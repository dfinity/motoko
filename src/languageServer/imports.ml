(** Import management for Motoko's language server *)
open Mo_frontend
open Mo_def
open Source

type import = string * string


let parse_with mode lexer parser =
    Ok (Parsing.parse 0 (parser lexer.Lexing.lex_curr_p) (Lexer.token mode) lexer)

let parse_string s =
  try
    (let lexer = Lexing.from_string s in
    let parse = Parser.Incremental.parse_module_header in
    ignore (parse_with Lexer.Normal lexer parse); [])
  with Mo_def.Syntax.Imports is ->
    is

let match_import : Syntax.dec -> string * string =
  fun dec ->
  let open Syntax in
  match dec.it with
  | LetD ({it=VarP {it=name;_};_}, {it=ImportE(s, _);_}) -> (name, s)
  | _ -> ("Can't", "deal with this import format")

let slice_imports : string -> string list * import list * string list =
  fun input ->
  let lines = String.split_on_char '\n' input in
  match parse_string input with
  | [] ->
     (* No imports yet, we'll just start at the top of the file?
        (Ideally we'd skip the module documentation comment?)*)
     ([], [], lines)
  | decls ->
     let open Source in
     (* let _ = List.iter (fun d -> print_endline (Source.string_of_region' d.at)) decls in *)
     let start_line = (List.hd decls).at.left.line - 1 in
     let end_line = (Lib.List.last decls).at.right.line - 1 in
     (* let _ = Printf.printf "start_line: %d\nend_line: %d\n" start_line end_line in *)
     let (before, lines) = Lib.List.split_at start_line lines in
     let (_, after) = Lib.List.split_at (end_line - start_line + 1) lines in
     (before, List.map match_import decls, after)
