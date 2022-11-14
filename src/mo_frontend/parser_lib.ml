exception Imports of Mo_def.Syntax.dec list

(* Temporary hack! *)
let msg_store : Diag.msg_store option ref = ref None

let triv_table : Mo_def.Trivia.triv_table ref = ref Mo_def.Trivia.empty_triv_table
