(*
This module implements the syntactic check if a file can used as a module.

In general, an actor file needs to end with an actor class declaration.

As short forms, it may end with an actor expression (implicitly taking no
parameters), or with an expression of type unit (appending an implicit `actor
{}`)

It needs to run on the type-checked AST, to know whether the last expression
is of type unit.
*)

open Source
open Syntax

let err m at text =
  let open Diag in
  add_msg m {
    sev = Diag.Error;
    cat = "type";
    at;
    text;
  }

let top_level m ds =
  let rec go = function
    | [] -> () (* Empty program; that is ok *)
    | [d] -> (* last declaration... *)
      begin match d.it with
      | ClassD (_, _, s, _, _, _) when s.it = Type.Actor-> ()
      | ExpD { it = ObjE (s, _); _} when s.it = Type.Actor -> ()
      | LetD ({ it = VarP n; _}, { it = ObjE (s, _); _}) when s.it = Type.Actor -> ()
      | _ when Type.is_unit d.note.note_typ -> ()
      | _ -> err m d.at "an actor file must end with an actor class, actor or expression of type unit"
      end
    | d::ds -> go ds
  in go ds

let prog (p : Syntax.prog) =
  Diag.with_message_store (fun m -> top_level m p.it; Some ())
