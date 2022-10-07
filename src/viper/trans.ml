open Source
open Syntax


let rec prog (p : Mo_def.Syntax.prog) : Syntax.prog =
  let (it, note)  = prog' p in
  { it;
    at = p.at; (* annotate with source location. *)
    note       (* and returned info *)
  }

and prog' p =
  match p.it with
  | _ ->
     (Prog, NoInfo)
