(** Import management for Motoko's language server *)
open Mo_frontend

open Mo_def
open Source
module Lsp = Lsp.Lsp_t

type import = string * string

let parse_with mode lexbuf parser =
  let tokenizer, _ = Lexer.tokenizer mode lexbuf in
  Ok (Parsing.parse 0 (parser lexbuf.Lexing.lex_curr_p) tokenizer lexbuf)

let parse_string s =
  try
    let lexer = Lexing.from_string s in
    let parse = Parser.Incremental.parse_module_header in
    ignore (parse_with Lexer.mode lexer parse);
    []
  with
  | Parser_lib.Imports is -> is
  | ex -> []

let match_import : Syntax.dec -> string * string =
 fun dec ->
  let open Syntax in
  match dec.it with
  | LetD ({ it = VarP { it = name; _ }; _ }, { it = ImportE (s, _); _ }) ->
      (name, s)
  | _ -> ("Can't", "deal with this import format")

let print_import : import -> string =
 fun (alias, path) -> Printf.sprintf "import %s \"%s\";" alias path

(** Formats an import section. Eg. sorting imports alphabetically *)
let format_imports : import list -> string =
 fun imports ->
  if imports = [] then ""
  else
    String.concat "\n"
      (List.map print_import
         (List.sort (fun (a1, _) (a2, _) -> compare a1 a2) imports))
    ^ "\n"

let mk_range : int * int -> int * int -> Lsp.range =
 fun (sl, sc) (el, ec) ->
  Lsp.
    {
      range_start = { position_line = sl; position_character = sc };
      range_end_ = { position_line = el; position_character = ec };
    }

let add_import : string -> string -> string -> Lsp.text_edit =
 fun input ->
  (* We do the parsing as soon as we get the input so the curried
     usage of this function doesn't duplicate work *)
  match parse_string input with
  | [] ->
      fun alias import_path ->
        Lsp.
          {
            text_edit_range = mk_range (0, 0) (0, 0);
            text_edit_newText = format_imports [ (alias, import_path) ];
          }
  | decls ->
      let open Source in
      let start_line = (List.hd decls).at.left.line - 1 in
      let end_line = (Lib.List.last decls).at.right.line - 1 in
      let imports = List.map match_import decls in
      let import_range = mk_range (start_line, 0) (end_line + 1, 0) in
      fun alias import_path ->
        let new_imports = (alias, import_path) :: imports in
        Lsp.
          {
            text_edit_range = import_range;
            text_edit_newText = format_imports new_imports;
          }
