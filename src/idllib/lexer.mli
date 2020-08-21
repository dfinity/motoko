val token : Lexing.lexbuf -> Parser.token  (* raise Source.ParseError *)
val region : Lexing.lexbuf -> Source.region
