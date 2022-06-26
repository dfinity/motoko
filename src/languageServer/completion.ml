open Mo_types
open Mo_frontend
module Lsp_t = Lsp.Lsp_t
module DI = Declaration_index

let string_of_list f xs =
  List.map f xs |> String.concat "; " |> fun x -> "[ " ^ x ^ " ]"

let string_of_item (item : Lsp_t.completion_item) : string =
  item.Lsp_t.completion_item_label

let markup_content s =
  Lsp_t.{ markup_content_kind = "markdown"; markup_content_value = s }

let item_of_ide_decl (d : DI.ide_decl) : Lsp_t.completion_item =
  let comp = DI.name_of_ide_decl d in
  match d with
  | DI.ValueDecl value ->
      Lsp_t.
        {
          completion_item_label = value.DI.name;
          completion_item_kind = 3;
          completion_item_insertText = Some comp;
          completion_item_insertTextFormat = Some 2;
          completion_item_additionalTextEdits = None;
          completion_item_documentation =
            Option.map markup_content value.DI.doc_comment;
          completion_item_detail = Some (Pretty.string_of_typ value.DI.typ);
        }
  | DI.TypeDecl ty ->
      let con = ty.DI.typ in
      let eq, params, typ = Type.strings_of_kind (Cons.kind con) in
      Lsp_t.
        {
          completion_item_label = ty.DI.name;
          completion_item_kind = 7;
          completion_item_insertText = Some comp;
          completion_item_insertTextFormat = Some 2;
          completion_item_additionalTextEdits = None;
          completion_item_documentation =
            Option.map markup_content ty.DI.doc_comment;
          completion_item_detail =
            Some (Printf.sprintf "type %s%s" ty.DI.name params);
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
let find_completion_prefix file line column : (string * string) option =
  let open Source in
  (* The LSP sends 0 based line numbers *)
  let line = line + 1 in
  let lexbuf = Lexing.from_string file in
  let tokenizer, _ = Lexer.tokenizer Lexer.mode lexbuf in
  let next () =
    let t, start, end_ = tokenizer () in
    (t, Lexer.convert_pos start, Lexer.convert_pos end_)
  in
  let pos_eq_cursor pos = pos.line = line && pos.column = column in
  let pos_past_cursor pos =
    pos.line > line || (pos.line = line && pos.column > column)
  in
  let rec loop (tkn, start, end_) =
    match tkn with
    | _ when pos_past_cursor end_ -> None
    | Parser.ID ident -> (
        if pos_eq_cursor end_ then Some ("", ident)
        else
          match next () with
          | Parser.DOT, _, _ -> (
              let ((tkn, start, end_) as next_token) = next () in
              if pos_eq_cursor start || pos_past_cursor start then
                Some (ident, "")
              else
                match tkn with
                | Parser.EOF -> Some (ident, "")
                | Parser.ID prefix ->
                    if pos_eq_cursor start || pos_past_cursor start then
                      Some (ident, "")
                    else if pos_eq_cursor end_ then Some (ident, prefix)
                    else loop next_token
                | _ -> loop next_token)
          | tkn -> loop tkn)
    | Parser.EOF -> None
    | _ -> loop (next ())
  in
  try loop (next ()) with _ -> None

let has_prefix (prefix : string) (ide_decl : DI.ide_decl) : bool =
  ide_decl
  |> DI.name_of_ide_decl
  |> Lib.String.chop_prefix prefix
  |> Option.is_some

let opt_bind f = function None -> None | Some x -> f x

let is_capital : string -> bool = fun s -> String.capitalize_ascii s = s

let completions index project_root file_path file_contents line column =
  let imported =
    Source_file.parse_module_header project_root file_path file_contents
  in
  let current_uri_opt = Lib.FilePath.relative_to project_root file_path in
  let toplevel_decls =
    let current_module_decls =
      current_uri_opt
      |> opt_bind (fun uri ->
             DI.lookup_module project_root (Filename.remove_extension uri) index)
      |> Option.fold ~none:[] ~some:snd
    in
    current_module_decls
  in
  let module_alias_completion_item alias =
    Lsp_t.
      {
        completion_item_label = alias;
        completion_item_kind = 9;
        completion_item_insertText = Some alias;
        completion_item_insertTextFormat = Some 1;
        completion_item_additionalTextEdits = None;
        completion_item_documentation = None;
        completion_item_detail = None;
      }
  in
  match find_completion_prefix file_contents line column with
  | None ->
      (* If we don't have any prefix to work with, just suggest the
         imported module aliases/fields, as well as top-level definitions in
         the current file *)
      let decls = List.map item_of_ide_decl toplevel_decls in
      decls
      @ List.map
          (function
            | Source_file.AliasImport (ident, _)
            | Source_file.FieldImport (_, ident, _) ->
                module_alias_completion_item ident)
          imported
  | Some ("", prefix) ->
      (* Without an alias but with a prefix we filter the toplevel
         identifiers of the current module *)
      toplevel_decls
      |> List.filter (has_prefix prefix)
      |> List.map item_of_ide_decl
  | Some (alias, prefix) -> (
      let module_path =
        imported
        |> List.map (function
               | Source_file.AliasImport (ident, path)
               | Source_file.FieldImport (_, ident, path)
               -> (ident, path))
        |> List.find_opt (fun (ident, _) -> String.equal alias ident)
      in
      match module_path with
      | Some mp -> (
          match DI.lookup_module project_root (snd mp) index with
          | Some (_, decls) ->
              decls
              |> List.filter (has_prefix prefix)
              |> List.map item_of_ide_decl
          | None ->
              (* The matching import references a module we haven't loaded *)
              [])
      (* We only try to add imports for capital aliases. Once we've got
       *  proper scoping information this can be improved *)
      | None when is_capital alias ->
          (* No import with the given alias was found *)
          let import_edit = Imports.add_import file_contents alias in
          let possible_imports =
            DI.find_with_prefix prefix
              (Lib.FilePath.make_absolute project_root file_path)
              index
          in
          let completions =
            List.concat_map
              (fun (p, ds) ->
                if p = Filename.basename file_path then
                  (* Self-imports are not allowed *)
                  []
                else
                  List.map
                    (fun d ->
                      let item = item_of_ide_decl d in
                      Lsp_t.
                        {
                          item with
                          completion_item_additionalTextEdits =
                            Some [ import_edit p ];
                          completion_item_detail =
                            Option.map
                              (fun ty ->
                                Printf.sprintf "%s (import from \"%s\")" ty p)
                              item.completion_item_detail;
                        })
                    ds)
              possible_imports
          in
          completions
      | None -> [])

let completion_handler index project_root file_path file_contents position =
  let line = position.Lsp_t.position_line in
  let column = position.Lsp_t.position_character in
  `CompletionResponse
    (completions index project_root file_path file_contents line column)
