open As_frontend
open As_types
open Declaration_index
module Lsp = Lsp.Lsp_t

type hover_target =
   | Ident of string
   | Qualified of string * string

let hovered_identifier
    (position : Lsp.position)
    (file_contents : string)
    : hover_target option =
  let line = position.Lsp.position_line + 1 in
  let column = position.Lsp.position_character + 1 in
  let lexbuf = Lexing.from_string file_contents in
  let next () = Lexer.token Lexer.Normal lexbuf in
  let pos_past_cursor pos =
    pos.Source.line > line
    || (pos.Source.line = line && pos.Source.column >= column) in
  let rec loop = function
    | _ when (pos_past_cursor (Lexer.region lexbuf).Source.left) -> None
    | Parser.ID ident ->
       (match next () with
        | Parser.DOT ->
           (match next () with
            | Parser.ID prefix ->
               let next_token_end = (Lexer.region lexbuf).Source.right in
               if pos_past_cursor next_token_end
               then Some (Qualified (ident, prefix))
               else loop (Parser.ID prefix)
            | tkn ->
               let next_token_start = (Lexer.region lexbuf).Source.left in
               if pos_past_cursor next_token_start
               then Some (Ident ident)
               else loop tkn)
        | _ when pos_past_cursor (Lexer.region lexbuf).Source.left ->
           Some (Ident ident)
        | tkn -> loop tkn)
    | Parser.EOF -> None
    | _ -> loop (next ()) in
  loop (next ())

let hover_detail = function
  | ValueDecl value ->
     value.name ^ " : " ^ Type.string_of_typ value.typ
  | TypeDecl ty ->
     ty.name ^ Type.string_of_typ ty.typ

let hover_handler (* index *) position file_contents project_root file_path =
  (* TODO(Christoph): Don't rebuild index on every hover *)
  let index = make_index () in
  let imported =
    Completion.parse_module_header
      project_root
      file_path
      file_contents in
  let hover_result =
    Lib.Option.bind (hovered_identifier position file_contents) (function
        | Ident s ->
           List.find_opt (fun (alias, _) -> alias = s) imported
           |> Lib.Option.map (fun (_, path) ->
                  Lsp.{ hover_result_contents = path })
        | Qualified (qual, ident) ->
           List.find_opt (fun (alias, _) -> alias = qual) imported
           |> Lib.Fun.flip Lib.Option.bind (fun (_, path) ->
                  Index.find_opt path index)
           |> Lib.Fun.flip Lib.Option.bind (fun decls ->
                List.find_opt
                  (fun d -> name_of_ide_decl d = ident)
                  decls)
           |> Lib.Option.map (fun ide_decl ->
                Lsp.{ hover_result_contents = hover_detail ide_decl })
    ) in
  let result = `TextDocumentHoverResponse hover_result in
  result
