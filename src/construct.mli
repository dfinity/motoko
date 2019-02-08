open Ir   
open Type

(* A miscellany of helpers to construct typed terms from typed terms *)

(* For convenience, fresh identifiers are returned as expressions, and binders
   take expressions (that must be variables) as arguments.
   This makes code transformations easier to write and read,
   at the loss of some precision in OCaml typing.
*)

type var = exp

(* Mutabilities *)

val varM : Syntax.mut
val constM : Syntax.mut

(* Field names *)

val nameN : string -> Syntax.name
val nextN : Syntax.name

(* Identifiers *)

val fresh_lab : unit -> Syntax.id
val fresh_id : typ -> var

val idE : Syntax.id -> typ -> exp
val id_of_exp : exp -> Syntax.id

(* Patterns *)

val varP : var -> pat
val tupP :  pat list -> pat

val seqP : pat list -> pat
val as_seqP : pat -> pat list

(* Expressions *)

val primE : string -> typ -> exp
val projE : exp ->  int -> exp
val decE : dec -> exp
val blockE : dec list -> exp
val textE : string -> exp
val letE : var -> exp -> exp -> exp

val unitE : exp
val boolE : bool -> exp

val callE : exp -> typ list -> exp -> typ -> exp

val ifE : exp -> exp -> exp -> typ -> exp
val dotE : exp -> Syntax.name -> typ -> exp
val switch_optE : exp -> exp -> pat -> exp -> typ -> exp
val tupE : exp list -> exp
val breakE: Syntax.id -> exp -> typ -> exp
val retE: exp -> typ -> exp
val assignE : exp -> exp -> exp
val labelE : Syntax.id -> typ -> exp -> exp
val loopE: exp -> exp option -> exp

val whileE' : exp -> exp -> exp'
val loopWhileE' : exp -> exp -> exp'
val forE' : pat -> exp -> exp -> exp'

val declare_idE : Syntax.id -> typ -> exp -> exp
val define_idE : Syntax.id -> Syntax.mut -> exp -> exp
val newObjE : Syntax.obj_sort -> (Syntax.name * Syntax.id) list -> typ -> exp

(* Declarations *)

val letP : pat -> exp -> dec   (* TBR: replace letD? *)

val letD : var -> exp -> dec
val varD : Syntax.id -> exp -> dec
val expD : exp -> dec
val funcD : var -> var -> exp -> dec
val nary_funcD : var  -> var list -> exp -> dec

(* Continuations *)

val answerT : typ
val contT : typ -> typ
val cpsT : typ -> typ
val fresh_cont : typ -> var

(* Sequence expressions *)

val seqE : exp list -> exp
val as_seqE : exp -> exp list

(* Lambdas *)

val (-->) : var -> exp -> exp
val (-->*) : var list -> exp -> exp (* n-ary local *)
val (-@>*) : var list -> exp -> exp (* n-ary shared *)
val (-*-) : exp -> exp -> exp       (* application *)


(* intermediate, cps-based @async and @await primitives,
   introduced by await(opt).ml to be removed by async.ml *)

val prim_async : typ -> exp

val prim_await : typ -> exp

