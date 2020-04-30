module ST = Source_token

include module type of Lexer_lib

type trivia_info = {
  leading_trivia : ST.line_feed ST.trivia list;
  trailing_trivia : ST.void ST.trivia list;
}

type parser_token = Parser.token * Lexing.position * Lexing.position

val tokenizer : mode -> Lexing.lexbuf ->
    (int * int -> trivia_info option) * (unit -> parser_token)


