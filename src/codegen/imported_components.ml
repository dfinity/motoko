(* imported_components.ml *)

open Ir_def
open Mo_types
open Mo_types.Type
open Wasm_exts.CustomModule
open Import_components_ir

type imported_function = {
  function_name : string;
  args : arg_data list;
  return_type : typ
}

module StringMap = Map.Make(String)
module TypeMap = Map.Make(Ord)

let map_motoko_name_to_wit motoko_name =
  String.map (fun c -> if c = '_' then '-' else c) motoko_name
  |> String.uncapitalize_ascii

let is_normalized_result t =
  match t with
  | Variant [ok; er] when er.lab = "err" && ok.lab = "ok" -> true
  | _ -> false

let is_normalized_special_variant t =
  is_normalized_result t

let normalize t =
  match normalize t with
  (* Special handling of the result type *)
  | Variant [er; ok] when er.lab = "err" && ok.lab = "ok" ->
    (* Flip to match Canonical ABI results *)
    Variant [ok; er]
  | Variant [ok; er] as t when is_normalized_result t -> t

  (* Arbitrary variants below: *)
  | Variant fields ->
    (* Order by their src *)
    let norm_fields = List.sort (fun a b -> Source.Region_ord.compare a.src.track_region b.src.track_region) fields in
    Variant norm_fields
  | t -> t

module CanonicalABI = struct
  open Wasm_exts.Types

  let normalize t = normalize t

  let discriminant_prim cases =
    let n = List.length cases in
    let open Stdlib.Float in
    match int_of_float (ceil (log2 (of_int n) /. 8.)) with
    | 0
    | 1 -> Nat8
    | 2 -> Nat16
    | 3 -> Nat32
    | _ -> assert false

  let discriminant_type cases = Prim (discriminant_prim cases)

  let rec flatten_type typ =
    match normalize typ with
    | Prim (Blob | Text)
    | Array _ -> [I32Type; I32Type] (* pointer + length *)
    | Variant cases -> flatten_variant cases
    | Tup ts -> flatten_tuple ts
    | Prim (Bool | Char | Nat8 | Int8 | Nat16 | Int16 | Nat32 | Int32) -> [I32Type]
    | Prim (Nat64 | Int64) -> [I64Type]
    | Prim Float -> [F64Type]
    | _ -> failwith (Printf.sprintf "flatten_type: unsupported type %s" (string_of_typ typ))

  and flatten_tuple ts =
    List.concat_map flatten_type ts

  and flatten_variant cases =
    let join a b =
      if a = b then a else
      match a, b with
      | I32Type, F32Type
      | F32Type, I32Type -> I32Type
      | _ -> I64Type
    in
    let flat = ref [] in
    cases |> List.iter (fun f ->
      flatten_type f.typ |> List.iteri (fun idx ft ->
        if idx < List.length !flat then
          flat := List.mapi (fun i t -> if idx = i then join t ft else t) !flat
        else
          flat := !flat @ [ft]));
    flatten_type (discriminant_type cases) @ !flat

  let needs_out_param = function
    (* Canonical ABI allows for max 1 return value *)
    | [_] | [] -> false
    (* If there are multiple return values, we need to use out-parameters via a pointer *)
    | _ -> true

  let flatten_return_type typ =
    let flat = flatten_type typ in
    if needs_out_param flat then
      [I32Type], []
    else
      [], flat
end

open CanonicalABI

let is_kind_def con =
  match Cons.kind con with
  | Def _ -> true
  | _ -> false

let is_unit t = is_unit (normalize t)

let rec map_motoko_type_to_wit variants_ref typ =
  match typ with
  | Named (_, t) -> map_motoko_type_to_wit variants_ref t
  | Con (con, _) as t when is_kind_def con ->
    let name = map_motoko_name_to_wit (Cons.name con) in
    map_named_type_to_wit name variants_ref (normalize t)
  | _ ->
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
    | Array t ->
      if normalize t = Prim Nat8 then failwith "Motoko [Nat8] should not be used, replace it with Blob instead";
      Printf.sprintf "list<%s>" (map_motoko_type_to_wit variants_ref  t)
    | Opt t -> Printf.sprintf "option<%s>" (map_motoko_type_to_wit variants_ref  t)
    | Tup ts ->
      assert (List.length ts > 0);
      Printf.sprintf "tuple<%s>" (String.concat ", " (List.map (map_motoko_type_to_wit variants_ref ) ts))
    | Variant [ok; er] as typ when is_normalized_result typ ->
      if is_unit ok.typ then
        if is_unit er.typ then "result"
        else Printf.sprintf "result<_, %s>" (map_motoko_type_to_wit variants_ref er.typ)
      else if is_unit er.typ then
        Printf.sprintf "result<%s>" (map_motoko_type_to_wit variants_ref ok.typ)
      else
        Printf.sprintf "result<%s, %s>" (map_motoko_type_to_wit variants_ref ok.typ) (map_motoko_type_to_wit variants_ref er.typ)
    | _ -> failwith (Printf.sprintf "map_motoko_type_to_wit: unsupported type %s" (string_of_typ typ))

and map_named_type_to_wit name variants_ref typ =
  match normalize typ with
  | Variant _ as v when not (is_normalized_special_variant v) ->
    (* Note that special variants, e.g. result, should not be emitted *)
    begin match TypeMap.find_opt v !variants_ref with
    | Some name -> name
    | None ->
      variants_ref := TypeMap.add v name !variants_ref;
      name
    end
  | _ -> map_motoko_type_to_wit variants_ref typ

(* Experimental API version, unused for now *)
let imported_components_to_wit_api map =
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

let imported_components_to_wit map =
  let imports = StringMap.bindings map |> List.map (fun (component_name, functions) ->
    (* Initialize variant-name generator *)
    let variants_ref : string TypeMap.t ref = ref TypeMap.empty in

    (* Process each function *)
    let fn_lines = StringMap.bindings functions |> List.map (fun (_, { function_name; args; return_type }) ->
      let args_strings = args |> List.map (fun { arg_name; arg_type } ->
        let wit_ty = map_motoko_type_to_wit variants_ref arg_type in
        map_motoko_name_to_wit arg_name ^ ": " ^ wit_ty
      ) in
      let ret_ty =
        if is_unit return_type then ""
        else Printf.sprintf " -> %s" (map_motoko_type_to_wit variants_ref return_type)
      in
      Printf.sprintf
        "    %s: func(%s)%s;"
        function_name
        (String.concat ", " args_strings)
        ret_ty
    ) in

    (* Generate variant declarations *)
    let variant_decls =
      if TypeMap.is_empty !variants_ref then ""
      else
        let defs = TypeMap.bindings !variants_ref |> List.map (fun (vt, name) ->
          let fs = as_variant vt in
          let case_strings = fs |> List.map (fun field ->
            let args = if is_unit field.typ then "" else
              "(" ^ map_motoko_type_to_wit variants_ref field.typ ^ ")"
            in
            map_motoko_name_to_wit field.lab ^ args
          ) in
          "    variant " ^ name ^ " { " ^ String.concat ", " case_strings ^ " }\n"
        ) in
        String.concat "" defs
    in
    "  import " ^ component_name ^ ": interface {\n" ^ variant_decls ^ (String.concat "\n" fn_lines) ^ "\n  }"
  ) in
  Printf.sprintf "package motoko:component;\n\nworld motoko {\n%s\n}\n" (String.concat "\n" imports)

let imported_components_to_wac map =
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

let add_imported_component ~component_name ~imported_function map =
  let functions =
    match StringMap.find_opt component_name !map with
    | Some set -> set
    | None -> StringMap.empty
  in
  let updated_set = StringMap.add imported_function.function_name imported_function functions in
  map := StringMap.add component_name updated_set !map

let add_imports_and_generate_wit_wac on_import prog =
  let imported_components = ref StringMap.empty in
  (* Add imports to the environment *)
  let add_import component_name function_name args return_type = 
    add_imported_component
      ~component_name 
      ~imported_function:{ function_name; args; return_type }
      imported_components;
    let wasm_args = List.concat_map (fun arg -> flatten_type arg.arg_type) args in
    let extra_out_param, wasm_results = flatten_return_type return_type in
    let wasm_args = wasm_args @ extra_out_param in
    on_import component_name function_name wasm_args wasm_results
  in

  (* Traverse the program to add wasm component imports *)
  traverse add_import prog;
  { wit_file_content = imported_components_to_wit !imported_components
  ; wac_file_content = imported_components_to_wac !imported_components
  }
