open As_types
open As_frontend
module Lsp_t = Lsp.Lsp_t

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

(* Given the source of a module, figure out under what names what
   modules have been imported. Normalizes the imported modules
   filepaths relative to the project root *)
let find_imported_modules file =
  [ ("List", "lib/ListLib.as")
  ; ("ListFns", "lib/ListFuncs.as")
  ]

(* Given a source file and a cursor position in that file, figure out
   the prefix relevant to searching completions. For example, given:

   List.| (where | is the cursor) return `List.` *)
let find_completion_prefix logger file line column =
  (* The LSP sends 0 based line numbers *)
  let line = line + 1 in
  let lexbuf = Lexing.from_string file in
  let next () = Lexer.token Lexer.Normal lexbuf in
  let rec loop = function
    | Parser.ID ident ->
        (match next () with
        | Parser.DOT ->
            (match next () with
            | Parser.EOF -> Some ident
            | tkn ->
               let position = (Lexer.region lexbuf).Source.left in
               let _ =
                 logger
                   "completion_prefix"
                   (Printf.sprintf
                      "%d:%d::%s\n"
                      position.Source.line
                      position.Source.column
                      ident) in
               if position.Source.line > line
                  || (position.Source.line = line
                      && position.Source.column >= column)
               then Some ident
               else loop tkn)
        | tkn -> loop tkn)
    | Parser.EOF -> None
    | _ -> loop (next ()) in
  loop (next ())

(* TODO(Christoph): Don't recompute the index whenever completions are
   requested *)
let completions (* index *) logger file line column =
  let index = make_index () in
  let imported = find_imported_modules file in
  match find_completion_prefix logger file line column with
  | None -> [ "ERROR: Couldn't determine completion prefix "]
  | Some prefix ->
     let module_path =
       imported
       |> List.find_opt (fun (mn, _) -> String.equal mn prefix) in
     let index_keys =
       Index.bindings index
       |> List.map fst
       |> string_of_list Base.Fn.id in
     match module_path with
     | Some mp ->
        (match Index.find_opt (snd mp) index with
         | Some decls ->
            decls
         | None ->
            [ "ERROR: Couldn't find module in index: " ^ index_keys ])
     | None ->
        [ "ERROR: Couldn't find module for prefix: " ^ prefix ]

let completion_handler logger file position =
  let line = position.Lsp_t.position_line in
  let column = position.Lsp_t.position_character in
  let completion_item lbl = Lsp_t.{ completion_item_label = lbl } in
  `CompletionResponse
    (List.map completion_item (completions logger file line column))

let complete_test () =
  make_index () |> string_of_index |> Printf.printf "%s\n"
