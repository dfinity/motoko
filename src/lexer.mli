val convert_pos : Lexing.position -> Source.pos

val token : Lexing.lexbuf -> Parser.token  (* raises Source.Error *)

val token_to_string : Parser.token->string

val line : Lexing.lexbuf -> string option * bool  
