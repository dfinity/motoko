open Mo_types

open Ir
open Type

(* A miscellany of helpers to construct typed terms from typed terms *)

(* For convenience, fresh identifiers are returned as expressions, and binders
   take expressions (that must be variables) as arguments.
   This makes code transformations easier to write and read,
   at the loss of some precision in OCaml typing.
*)

type var = exp

(* Field names *)

val nameN : string -> Type.lab
val nextN : Type.lab

(* Identifiers *)

val fresh_id : string -> unit -> id
val fresh_var : string -> typ -> var
val fresh_vars : string -> typ list -> var list

val clone : exp -> exp

val idE : id -> typ -> exp
val id_of_exp : var -> id
val arg_of_exp : var -> arg
val exp_of_arg : arg -> var

(* Patterns *)

val varP : var -> pat
val tupP :  pat list -> pat

val seqP : pat list -> pat

(* Expressions *)

val primE : Ir.prim -> exp list -> exp
val selfRefE : typ -> exp
val asyncE : typ -> typ -> exp -> exp
val assertE : exp -> exp
val awaitE : typ -> exp -> exp -> exp
val ic_replyE : typ list -> exp -> exp
val ic_rejectE : exp -> exp
val ic_callE : exp -> exp -> exp -> exp -> exp
val projE : exp -> int -> exp
val optE : exp -> exp
val tagE : id -> exp -> exp
val blockE : dec list -> exp -> exp
val textE : string -> exp
val blobE : string -> exp
val letE : var -> exp -> exp -> exp
val ignoreE : exp -> exp

val unitE : unit -> exp
val boolE : bool -> exp

val callE : exp -> typ list -> exp -> exp

val ifE : exp -> exp -> exp -> typ -> exp
val dotE : exp -> Type.lab -> typ -> exp
val switch_optE : exp -> exp -> pat -> exp -> typ -> exp
val switch_variantE : exp -> (id * pat * exp) list -> typ -> exp
val tupE : exp list -> exp
val breakE: id -> exp -> exp
val retE: exp -> exp
val immuteE: exp -> exp
val assignE : exp -> exp -> exp
val labelE : id -> typ -> exp -> exp
val loopE : exp -> exp
val forE : pat -> exp -> exp -> exp
val loopWhileE : exp -> exp -> exp
val whileE : exp -> exp -> exp

val declare_idE : id -> typ -> exp -> exp
val define_idE : id -> mut -> exp -> exp
val newObjE : obj_sort -> Ir.field list -> typ -> exp

val unreachableE : unit -> exp

(* Declarations *)

val letP : pat -> exp -> dec
val letD : var -> exp -> dec
val varD : id -> typ -> exp -> dec
val expD : exp -> dec
val funcD : var -> var -> exp -> dec
val nary_funcD : var  -> var list -> exp -> dec

val let_no_shadow : var -> exp -> dec list -> dec list

(* Continuations *)

val answerT : typ
val contT : typ -> typ
val err_contT : typ
val cpsT : typ -> typ

(* Sequence expressions *)

val seqE : exp list -> exp

(* Lambdas *)

val (-->) : var -> exp -> exp
val (-->*) : var list -> exp -> exp (* n-ary local *)
val forall : typ_bind list -> exp -> exp (* generalization *)
val (-*-) : exp -> exp -> exp       (* application *)
