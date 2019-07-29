open As_types

type value_decl = { name : string; typ: Type.typ }
type type_decl = { name : string; typ: Type.typ }

type ide_decl =
  | ValueDecl of value_decl
  | TypeDecl of type_decl

(** For debugging purposes *)
let string_of_ide_decl = function
  | ValueDecl value ->
     "ValueDecl{ name = "
       ^ String.escaped value.name
       ^ ", typ = "
       ^ Type.string_of_typ value.typ
       ^ " }"
  | TypeDecl ty ->
     "TypeDecl{ name = "
       ^ String.escaped ty.name
       ^ ", typ = "
       ^ Type.string_of_typ ty.typ
       ^ " }"

let name_of_ide_decl (d : ide_decl) : string =
  match d with
  | ValueDecl value -> value.name
  | TypeDecl ty -> ty.name

module Index = Map.Make(String)
type declaration_index = (ide_decl list) Index.t

let string_of_list f xs =
  List.map f xs
  |> String.concat "; "
  |> fun x -> "[ " ^ x ^ " ]"

let string_of_index index =
  Index.bindings index
  |> string_of_list
       (fun (path, decls) ->
         path
         ^ " =>\n    "
         ^ string_of_list string_of_ide_decl decls
         ^ "\n")

let lib_files () : string list =
  let lib_dir = "lib" in
  Sys.readdir lib_dir
  |> Array.to_list
  |> List.filter (fun file -> String.equal (Filename.extension file) ".as")
  |> List.map (fun file -> Filename.concat lib_dir file)

let read_single_module_lib (ty: Type.typ): ide_decl list option =
  match ty with
  | Type.Obj (Type.Module, fields) ->
     fields
     |> List.map (fun Type.{ lab = name; typ } -> ValueDecl { name; typ })
     |> Lib.Option.some
  | _ -> None

let make_index (): declaration_index =
  let (libraries, scope) =
    Diag.run
      (Pipeline.chase_imports
         Pipeline.initial_stat_env
         (Pipeline__.Resolve_import.S.of_list (lib_files ()))) in
  Type.Env.fold
    (fun path ty acc ->
      Index.add
        path
        (ty
         |> read_single_module_lib
         |> Lib.Fun.flip Lib.Option.get [])
        acc)
    scope.Scope.lib_env
    Index.empty
