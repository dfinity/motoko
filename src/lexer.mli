exception Error of Source.region * string

val token : Lexing.lexbuf -> Parser.token  (* raise Error *)
val region : Lexing.lexbuf -> Source.region
