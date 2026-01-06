exception Imports of Mo_def.Syntax.dec list

(* Temporary hack! *)
let msg_store : Diag.msg_store option ref = ref None
let mode : Lexer_lib.mode option ref = ref None

let triv_table : Trivia.triv_table ref = ref Trivia.empty_triv_table
