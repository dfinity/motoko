open Mo_types
open Mo_frontend
module Lsp_t = Lsp.Lsp_t
module DI = Declaration_index

let string_of_list f xs =
  List.map f xs
  |> String.concat "; "
  |> fun x -> "[ " ^ x ^ " ]"

let template_of_ide_decl decl =
  let supply = ref 0 in
  let fresh () = supply := !supply + 1; string_of_int !supply in
  match decl with
  | DI.ValueDecl value ->
     (match value.DI.typ with
      | Type.Func(_, _, binds, ty_list1, ty_list2) ->
         let ty_args =
           binds
           |> List.filter Type.(fun { sort; _ } -> sort = Type)
           |> List.map (fun Type.{ var = var; bound = bound; _} ->
                  Printf.sprintf "${%s:%s}" (fresh ()) var)
           |> String.concat ", " in
         let args =
           ty_list1
           |> List.map (fun _ -> Printf.sprintf "$%s" (fresh ()))
           |> String.concat ", " in
         let ty_args = if ty_args = "" then "" else "<" ^ ty_args ^ ">" in
         Printf.sprintf "%s%s(%s)" value.DI.name ty_args args
      | _ -> value.DI.name)
  | DI.TypeDecl ty ->
     ty.DI.name

let string_of_item (item : Lsp_t.completion_item) : string =
  item.Lsp_t.completion_item_label

let item_of_ide_decl (d : DI.ide_decl) : Lsp_t.completion_item =
  let tmpl = template_of_ide_decl d in
  match d with
  | DI.ValueDecl value ->
     Lsp_t.{
        completion_item_label = value.DI.name;
        completion_item_kind = 3;
        completion_item_insertText = tmpl;
        completion_item_insertTextFormat = 2;
        completion_item_detail = Some(Type.string_of_typ value.DI.typ);
     }
  | DI.TypeDecl ty ->
     let con = ty.DI.typ in
     let eq, params, typ = Type.strings_of_kind (Con.kind con) in
     Lsp_t.{
        completion_item_label = ty.DI.name;
        completion_item_kind = 7;
        completion_item_insertText = tmpl;
        completion_item_insertTextFormat = 2;
        completion_item_detail =
          Some
            (Printf.sprintf
               "type %s%s"
               ty.DI.name
               params);
     }

let import_relative_to_project_root root module_path dependency =
  match Lib.FilePath.relative_to root module_path with
  | None -> None
  | Some root_to_module ->
     root_to_module
     |> Filename.dirname
     |> Fun.flip Filename.concat dependency
     |> Lib.FilePath.normalise
     |> Option.some

(* Given a source file and a cursor position in that file, figure out
   the prefix relevant to searching completions. For example, given:

   List.fi| (where | is the cursor) return `Some ("List", "fi")` *)
let find_completion_prefix logger file line column: (string * string) option =
  let open Source in
  (* The LSP sends 0 based line numbers *)
  let line = line + 1 in
  let lexbuf = Lexing.from_string file in
  let next () = Lexer.token Lexer.Normal lexbuf in
  let pos_eq_cursor pos =
    pos.line = line && pos.column = column in
  let pos_past_cursor pos =
    pos.line > line
    || (pos.line = line && pos.column > column) in
  let rec loop = function
    | _ when (pos_past_cursor (Lexer.region lexbuf).right) -> None
    | Parser.ID ident ->
       let next_token_end = (Lexer.region lexbuf).right in
       if pos_eq_cursor next_token_end
       then Some("", ident)
       else
       (match next () with
        | Parser.DOT ->
           let next_token = next () in
           let next_token_start = (Lexer.region lexbuf).left in
           if pos_eq_cursor next_token_start
              || pos_past_cursor next_token_start
           then Some (ident, "")
           else
             (match next_token with
              | Parser.EOF -> Some (ident, "")
              | Parser.ID prefix ->
                 let next_token_start = (Lexer.region lexbuf).left in
                 let next_token_end = (Lexer.region lexbuf).right in
                 if pos_eq_cursor next_token_start
                    || pos_past_cursor next_token_start
                 then Some (ident, "")
                 else if pos_eq_cursor next_token_end
                 then Some (ident, prefix)
                 else loop (Parser.ID prefix)
              | tkn -> loop tkn)
        | tkn -> loop tkn)
    | Parser.EOF -> None
    | _ -> loop (next ()) in
  try loop (next ()) with _ -> None

let has_prefix (prefix : string) (ide_decl : DI.ide_decl): bool =
  ide_decl
  |> DI.name_of_ide_decl
  |> Lib.String.chop_prefix prefix
  |> Option.is_some

let opt_bind f = function
  | None -> None
  | Some x -> f x

let completions index logger project_root file_path file_contents line column =
  let imported = Source_file.parse_module_header project_root file_path file_contents in
  let current_uri_opt = Lib.FilePath.relative_to project_root file_path in
  let toplevel_decls =
     let current_module_decls =
       current_uri_opt
       |> opt_bind (fun uri -> DI.lookup_module (Filename.remove_extension uri) index)
       |> Option.fold ~none:[] ~some:snd in
     current_module_decls
  in
  let module_alias_completion_item alias =
    Lsp_t.{
        completion_item_label = alias;
        completion_item_kind = 9;
        completion_item_insertText = alias;
        completion_item_insertTextFormat = 1;
        completion_item_detail = None;
    } in
  match find_completion_prefix logger file_contents line column with
  | None ->
     (* If we don't have any prefix to work with, just suggest the
        imported module aliases, as well as top-level definitions in
        the current file *)
     let decls = List.map item_of_ide_decl toplevel_decls  in
     decls @ List.map (fun (alias, _) -> module_alias_completion_item alias) imported
  | Some ("", prefix) ->
     (* Without an alias but with a prefix we filter the toplevel
        identifiers of the current module *)
     toplevel_decls
     |> List.filter (has_prefix prefix)
     |> List.map item_of_ide_decl
  | Some (alias, prefix) ->
     let module_path =
       imported
       |> List.find_opt (fun (mn, _) -> String.equal mn alias) in
     match module_path with
     | Some mp ->
        (match DI.lookup_module (snd mp) index with
         | Some (_, decls) ->
            decls
            |> List.filter (has_prefix prefix)
            |> List.map item_of_ide_decl
         | None ->
            (* The matching import references a module we haven't loaded *)
            [])
     | None ->
        (* No module with the given prefix was found *)
        []

let completion_handler index logger project_root file_path file_contents position =
  let line = position.Lsp_t.position_line in
  let column = position.Lsp_t.position_character in
  `CompletionResponse
    (completions index logger project_root file_path file_contents line column)
