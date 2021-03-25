(*
This modules provides an interface to a SAT solver for boolean satisfiability
problems with Horn clauses.

Or, put less fancy: one can define boolean variables and implications between
them. Variables are optimistically believed to be true unless there is
evidence that they have to be false. The user of this module is then notified that
they are indeed false.
*)

(** Abstract type for these boolean variables *)
type t

(* Intro forms: *)

(* A variable that may be false *)
val maybe_false : unit -> t

(* Definite variants *)
val surely_false : t
val surely_true : t

(*
Defining implications:

  required_for a b

means that a must be true for b to be true (i.e. b ⟶ a).

So if we have evidence that a must be false, we have evidence that b must be
false.

The graph described by `required_for` maybe cyclic.
*)
val required_for : t -> t -> unit

(*
An abbreviation that defines a new variable that requires all the given
variables to be false.
*)
val all : t list -> t

(*
To obtaint the result, install a callback for each logical variable of
interested using `when_false v k`

If there is evidence that `v` must be false, the callback `k` is invoked
exactly once. This happens during the execution of `when_false v k` or
`required_for`.

If the callback is not invoked ater all variables and impliciations have been
registered, then the variable is certain to be true.
*)
val when_false : t -> unit Lazy.t -> unit
