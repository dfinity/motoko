exception Error of Source.region * string

type mode = Normal | Privileged

val token : mode -> Lexing.lexbuf -> Parser.token  (* raise Error *)
val region : Lexing.lexbuf -> Source.region
