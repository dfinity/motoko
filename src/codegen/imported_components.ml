(* imported_components.ml *)

open Ir_def
open Mo_types.Type

type imported_function = { function_name : string; args : Import_components_ir.arg_data list; return_type : typ }

(* This module manages the imported components in a map where each key is a component name
   and the value is a set of function names that are imported from that component. *)

module ImportedFunctionOrd : Set.OrderedType with type t = imported_function = struct
  type t = imported_function
  let compare a b =
    let c = String.compare a.function_name b.function_name in
    if c <> 0 then c
    else
      let c_args = compare a.args b.args in
      if c_args <> 0 then c_args
      else compare a.return_type b.return_type
end

module FunctionSet = Set.Make(ImportedFunctionOrd)
module StringMap = Map.Make(String)

type t = FunctionSet.t StringMap.t

module TypeMap = Map.Make(Ord)

type variant_def = { name : string; fields : field list }

let empty : t = StringMap.empty

let add_imported_component ~(component_name : string) ~(imported_function : imported_function) (map : t) : t =
  let existing_set =
    match StringMap.find_opt component_name map with
    | Some set -> set
    | None -> FunctionSet.empty in
  let updated_set = FunctionSet.add imported_function existing_set in
  StringMap.add component_name updated_set map

let imported_components_of_list (entries : (string * imported_function) list) : t =
  List.fold_left (fun acc (k, v) -> add_imported_component ~component_name:k ~imported_function:v acc) empty entries

let imported_components_to_list (map : t) : (string * imported_function) list =
  StringMap.bindings map
  |> List.concat_map (fun (k, set) ->
      FunctionSet.elements set |> List.map (fun v -> (k, v))
    )

let print_imported_components (map : t) =
  StringMap.iter (fun key data ->
    Printf.printf "%s -> [" key;
    let elements = FunctionSet.elements data in
    Printf.printf "%s" (String.concat ", " (List.map (fun e -> e.function_name) elements));
    Printf.printf "]\n"
  ) map

let rec map_motoko_type_to_wit (variants_ref : string TypeMap.t ref) (next_variant_idx : int ref) typ : string =
  match normalize typ with
  | Prim Blob -> "list<u8>"
  | Prim Text -> "string"
  | Prim Bool -> "bool"
  | Prim Char -> "char"
  | Prim Nat8 -> "u8"
  | Prim Int8 -> "s8"
  | Prim Nat16 -> "u16"
  | Prim Int16 -> "s16"
  | Prim Nat32 -> "u32"
  | Prim Int32 -> "s32"
  | Prim Nat64 -> "u64"
  | Prim Int64 -> "s64"
  | Prim Float -> "f64"
  | Array t -> "list<" ^ map_motoko_type_to_wit variants_ref next_variant_idx t ^ ">"
  | Variant _ as v ->
    begin match TypeMap.find_opt v !variants_ref with
    | Some name -> name
    | None ->
      let name = Printf.sprintf "v%d" !next_variant_idx in
      incr next_variant_idx;
      variants_ref := TypeMap.add v name !variants_ref;
      name
    end
  | _ -> failwith (Printf.sprintf "map_motoko_type_to_wit: unsupported type %s" (string_of_typ typ))

let map_motoko_name_to_wit (motoko_name : string) : string =
  String.map (fun c -> if c = '_' then '-' else c) motoko_name

(* Experimental API version, unused for now *)
let imported_components_to_wit_api (map : t) : string =
  (* Emit a minimal aggregator world that imports each component's exported
     `api` interface by package path. This avoids regenerating variants and
     functions. The generated WIT is intended to be used within a WIT package
     (e.g. alongside a deps/ tree), so dependency paths use slashes.

       import component:<component-name>/api;
  *)
  let imports =
    StringMap.bindings map
    |> List.map (fun (component_name, _functions) ->
         (* Alias must match the imported interface name Motoko expects at link time *)
         "  import component:" ^ component_name ^ "/api;"
       )
  in
  Printf.sprintf "package motoko:component;\n\nworld motoko {\n%s\n}\n" (String.concat "\n" imports)

let imported_components_to_wit (map : t) : string =
  let imports = StringMap.bindings map |> List.map (fun (component_name, functions) ->
    (* Initialize variant-name generator *)
    let variants_ref : string TypeMap.t ref = ref TypeMap.empty in
    let next_variant_idx = ref 1 in

    (* Process each function *)
    let fn_lines = FunctionSet.elements functions |> List.map (fun { function_name; args; return_type } ->
      let args_strings = args |> List.map (fun Import_components_ir.{ arg_name; arg_type } ->
        let wit_ty = map_motoko_type_to_wit variants_ref next_variant_idx arg_type in
        map_motoko_name_to_wit arg_name ^ ": " ^ wit_ty
      ) in
      let ret_ty = map_motoko_type_to_wit variants_ref next_variant_idx return_type in
      "    " ^ function_name ^ ": func(" ^ (String.concat ", " args_strings) ^ ") -> " ^ ret_ty ^ ";"
    ) in

    (* Generate variant declarations *)
    let variant_decls =
      if TypeMap.is_empty !variants_ref then ""
      else
        let defs = TypeMap.bindings !variants_ref |> List.map (fun (vt, name) ->
          let fs = as_variant vt in
          let case_strings = fs |> List.map (fun field ->
            match normalize field.typ with
            | Tup [] -> map_motoko_name_to_wit field.lab
            | t -> map_motoko_name_to_wit field.lab ^ "(" ^ map_motoko_type_to_wit variants_ref next_variant_idx t ^ ")"
          ) in
          "    variant " ^ name ^ " { " ^ String.concat ", " case_strings ^ " }\n"
        ) in
        String.concat "" defs
    in
    "  import " ^ component_name ^ ": interface {\n" ^ variant_decls ^ (String.concat "\n" fn_lines) ^ "\n  }"
  ) in
  Printf.sprintf "package motoko:component;\n\nworld motoko {\n%s\n}\n" (String.concat "\n" imports)

let imported_components_to_wac (map : t) : string =
  let imported_components =
    StringMap.bindings map
    |> List.map (fun (component_name, _functions) -> "let " ^ component_name ^ " = new component:" ^ component_name ^ " {};")
    |> String.concat "\n"
  in
  let components_in_motoko =
    StringMap.bindings map
    |> List.map (fun (component_name, _functions) -> "    " ^ component_name ^ " : " ^ component_name ^ ".api,")
    |> String.concat "\n"
  in
  let motoko_component = Printf.sprintf "let motoko = new motoko:component {\n%s\n    ...\n};" components_in_motoko in
  Printf.sprintf "package motoko:composition;\n\n%s\n\n%s\n\nexport motoko.run;\n" imported_components motoko_component
