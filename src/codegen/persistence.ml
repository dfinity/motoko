(* Support for orthogonal persistence using stable heap and in-place memory upgrades 
   without serialization or deserialization to secondary stable memory. *)

(*
  // The first type denotes the stable actor object.
  // Non-primitive types are referred to by a positive index according to the type table.
  // Primitive types are encoded by negative indices.
  // All numbers (type indices etc.) are encoded as little endian i32.
  <type_table> ::= length:i32 (<type>)^length
  <type> ::= <object>
  <object> ::= 1l field_list
  <field_list> ::= length:i32 (<field>)^length
  <field> ::= label_hash:i32 type_index:i32
  
  Predefined primitive type indices:
  Nat -1l
*)

open Mo_types

let encode_i32 number =
  let buffer = Buffer.create 4 in
  Buffer.add_int32_le buffer number;
  Buffer.contents buffer

let list_to_string list = List.fold_left (^) "" list

module TypeTable = struct
  let empty = []

  let length table = List.length table

  let index_of table typ =
    match Lib.List.index_of typ table with
    | Some index -> index
    | None -> assert false

  let contains_type table typ =
    match List.find_opt (fun entry -> entry = typ) table with
    | Some _ -> true
    | None -> false

  let add_type table typ =
    let open Type in
    match typ with
    | Prim _ -> table
    | _ when contains_type table typ -> table
    | _ -> List.append table [typ]
end

let rec collect_type table typ =
  let table = TypeTable.add_type table typ in
  let open Type in
  match typ with
  | Prim _ -> table
  | Obj (Object, field_list) ->
    let field_types = List.map (fun field -> field.typ) field_list in
    collect_types table field_types
  | _ -> 
    Printf.printf "UNSUPPORTED PERSISTENT TYPE %s\n" (Type.string_of_typ typ);
    assert false

and collect_types table type_list =
  match type_list with
  | [] -> table
  | first::remainder -> collect_types (collect_type table first) remainder
  
let encode_list encode_element list = 
  let length = Int32.of_int (List.length list) in
  let elements = List.map encode_element list in
  encode_i32 length ^ list_to_string elements

let type_index table typ =
  let open Type in
  match typ with
  | Prim Nat -> -1l
  | Prim _ -> assert false
  | _ -> Int32.of_int (TypeTable.index_of table typ)

let encode_field table field =
  let open Type in
  let field_hash = Hash.hash field.lab in
  encode_i32 field_hash ^ 
  encode_i32 (type_index table field.typ)
  
let encode_complex_type table typ =
  let open Type in
  match typ with
  | Obj (Object, field_list) -> 
    encode_i32 1l ^ 
    encode_list (encode_field table) field_list
  | _ -> assert false

let encode_type_table table =
  encode_list (encode_complex_type table) table

let unwrap_optional typ =
  let open Type in
  match typ with
  | Opt inner_type -> inner_type
  | _ -> assert false

(* Encode the stable type to enable a memory compatibility check on upgrade. *)
(* See `persistence::compatibility` in the runtime system for the encoding format. *)
let encode_stable_type (stable_type: Type.typ) : string =
  let open Type in 
  match stable_type with
  | Obj (Memory, field_list) -> 
      let unwrap_field field = {lab = field.lab; typ = unwrap_optional field.typ; depr = field.depr} in
      let stable_fields = List.map unwrap_field field_list in
      let stable_actor = Obj (Object, stable_fields) in
      let table = collect_type TypeTable.empty stable_actor in
      encode_type_table table
  | _ -> assert false
