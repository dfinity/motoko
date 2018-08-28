exception Error of Source.region * string

val convert_pos : Lexing.position -> Source.pos

val token : Lexing.lexbuf -> Parser.token  (* raise Error *)
val region : Lexing.lexbuf -> Source.region
