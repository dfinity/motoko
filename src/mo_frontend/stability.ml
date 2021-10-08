open Mo_def

(*module Pretty = Type.MakePretty(struct let show_stamps = true end); *)

let parse_with mode lexer parser name : Syntax.sig_ Diag.result =
  let open Diag.Syntax in
  lexer.Lexing.lex_curr_p <-
    {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
  let tokenizer, triv_table = Lexer.tokenizer mode lexer in
  let* mk_sig =
    try
      Parser_lib.triv_table := triv_table;
      Parsing.parse 0 (parser lexer.Lexing.lex_curr_p) tokenizer lexer
    with Lexer.Error (at, msg) -> Diag.error at"M0002" "syntax" msg
  in
  let sig_ = mk_sig name in
  Diag.return sig_

let parse_sig s name  =
  let open Diag.Syntax in
  let mode = {Lexer.privileged = true} in (* TODO: make false *)
  let lexer = Lexing.from_string s in
  let parse = Parser.Incremental.parse_sig in
  let* sig_ = parse_with mode lexer parse name in
  Diag.return sig_



