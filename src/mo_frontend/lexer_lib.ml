(**
  This module exists so it can be included by lexer.ml. This way
  source_lexer.ml can use these definitions but stay internal to
  lexer.ml.
*)
type mode = {
  privileged : bool;
}

let mode : mode = {
  privileged = Option.is_some (Sys.getenv_opt "MOC_UNLOCK_PRIM");
}

let mode_priv : mode = { mode with privileged = true }


exception Error of Source.region * string

let convert_pos pos =
  { Source.file = pos.Lexing.pos_fname;
    Source.line = pos.Lexing.pos_lnum;
    Source.column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
  }

