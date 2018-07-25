exception Syntax of Source.region * string

val convert_pos : Lexing.position -> Source.pos

val token : Lexing.lexbuf -> Parser.token  (* raises Source.Error *)

(*
val string_of_token : Parser.token->string
*)

val region : Lexing.lexbuf -> Source.region

val line : Lexing.lexbuf -> string option * bool  
