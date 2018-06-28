exception Syntax of Source.region * string

val convert_pos : Lexing.position -> Source.pos

val token : Lexing.lexbuf -> Parser.token  (* raises Source.Error *)

val token_to_string : Parser.token->string

val region : Lexing.lexbuf -> Source.region

val line : Lexing.lexbuf -> string option * bool  
