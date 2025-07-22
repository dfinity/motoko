(* imported_components.ml *)

open Ir_def

type imported_function = { function_name : string; args : Import_components_ir.arg_data list; return_type : Mo_types.Type.typ }

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


let map_motoko_type_to_wit (motoko_type : Mo_types.Type.typ): string = match Mo_types.Type.string_of_typ motoko_type with
    | "Blob" -> "list<u8>"
    | _ -> failwith (Printf.sprintf "map_motoko_type_to_wit: unsupported type %s" (Mo_types.Type.string_of_typ motoko_type))

let map_motoko_name_to_wit (motoko_name : string) : string =
  String.map (fun c -> if c = '_' then '-' else c) motoko_name

let imported_components_to_wit (map : t) : string =
  let imports = StringMap.bindings map
  |> List.map (fun (component_name, functions) ->
      let imported_functions = FunctionSet.elements functions |> 
          List.map (fun (e) -> 
              "    " ^ e.function_name ^ ": func(" ^ 
            (String.concat ", " (List.map (fun arg -> map_motoko_name_to_wit arg.Import_components_ir.arg_name ^ ": " ^ map_motoko_type_to_wit arg.Import_components_ir.arg_type) e.args)) ^ ") -> " ^ (map_motoko_type_to_wit e.return_type) ^ ";") in
       "  import " ^ component_name ^ ": interface {\n" ^ (String.concat "\n" imported_functions) ^ "\n  }"
     )
  |> String.concat "\n" in
  Printf.sprintf "package motoko:component;\n\nworld motoko {\n%s\n}\n" imports

(* Example usage *)