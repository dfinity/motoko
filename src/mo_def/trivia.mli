type line_feed = LF | CRLF

type 'l trivia = Comment of string | Space of int | Tab of int | Line of 'l

type void

val absurd : void -> 'a

val map_trivia : ('a -> 'b) -> 'a trivia -> 'b trivia

val string_of_line_feed : line_feed -> string

val string_of_trivia : ('a -> string) -> 'a trivia -> string

val string_of_trivia_lf : line_feed trivia -> string

type trivia_info = {
  leading_trivia : line_feed trivia list;
  trailing_trivia : void trivia list;
}

val deprecated_of_trivia_info : trivia_info -> string option

val doc_comment_of_trivia_info : trivia_info -> string option

type pos = { line : int; column : int }

val pos_of_lexpos : Lexing.position -> pos

module PosHashtbl : Hashtbl.S with type key = pos

type triv_table = trivia_info PosHashtbl.t

val empty_triv_table : triv_table

val find_trivia : triv_table -> Source.region -> trivia_info
