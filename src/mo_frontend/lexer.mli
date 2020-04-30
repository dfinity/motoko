exception Error of Source.region * string

type mode = Normal | Privileged

val token : mode -> Lexing.lexbuf -> Source_token.token  (* raise Error *)
val convert_pos : Lexing.position -> Source.pos
val region : Lexing.lexbuf -> Source.region
