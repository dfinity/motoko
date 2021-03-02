(** This module is all you need to import for lexing Motoko source files.

  We lex in two phases:

  1. source_lexer.mll emits source_tokens, which also includes tokens for
     trivia (whitespace + comments)
  2. lexer.ml is a stream processor over the emitted source tokens. It:
     - Converts them into parser tokens
     - Disambiguates various tokens based on whitespace
     - Collects trivia per parser token and makes it available via a side channel
*)

module ST = Source_token
open Mo_def.Trivia

include module type of Lexer_lib

type parser_token = Parser.token * Lexing.position * Lexing.position

(**
  Given a mode and a lexbuf returns a tuple of a lexing function
  and an accessor function for the collected trivia indexed by
  the start position for every token.
*)
val tokenizer : mode -> Lexing.lexbuf ->
    (unit -> parser_token) * triv_table
