open As_frontend
open As_types
open Printf

module Index = Map.Make(String)
type completion_index = (string list) Index.t

let string_of_list f xs =
  List.map f xs
  |> String.concat "; "
  |> fun x -> "[ " ^ x ^ " ]"

let lib_files () : string list =
  let lib_dir = "lib" in
  Sys.readdir lib_dir
  |> Array.to_list
  |> List.filter (fun file -> String.equal (Filename.extension file) ".as")
  |> List.map (fun file -> Filename.concat lib_dir file)

let read_single_module_lib (ty: Type.typ): string list option =
  match ty with
  | Type.Obj (Type.Module, fields) ->
     fields
     |> List.map (fun Type.{ lab = lab; typ = typ } -> lab)
     |> Lib.Option.pure
  | _ -> None

let make_index (): completion_index =
  let (libraries, scope) =
    Diag.run
      (Pipeline.chase_imports
         Scope.empty
         (Pipeline__.Resolve_import.S.of_list (lib_files ()))) in
  Type.Env.fold
    (fun path ty acc ->
      Index.add
        path
        (ty |> read_single_module_lib |> Base.Option.value ~default:[])
        acc)
    scope.Scope.lib_env
    Index.empty

let string_of_index index =
  Index.bindings index
  |> string_of_list
       (fun (path, decls) ->
         path
         ^ " =>\n    "
         ^ string_of_list Base.Fn.id decls
         ^ "\n")

let complete_test () = make_index () |> string_of_index |> printf "%s\n"
