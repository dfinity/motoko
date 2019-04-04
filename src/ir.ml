(* Patterns *)
type type_note = Syntax.typ_note = {note_typ : Type.typ; note_eff : Type.eff}

type 'a phrase = ('a, Syntax.typ_note) Source.annotated_phrase

type typ_bind' = {con : Type.con; bound : Type.typ}
type typ_bind = typ_bind' Source.phrase

type id = Syntax.id
type lit = Syntax.lit
type unop = Syntax.unop
type binop = Syntax.binop
type relop = Syntax.relop
type mut = Syntax.mut
type vis = Syntax.vis

type pat = (pat', Type.typ) Source.annotated_phrase
and pat' =
  | WildP                                      (* wildcard *)
  | VarP of id                                 (* variable *)
  | LitP of lit                                (* literal *)
  | TupP of pat list                           (* tuple *)
  | OptP of pat                                (* option *)
  | AltP of pat * pat                          (* disjunctive *)

(* Like id, but with a type attached *)
type arg = (string, Type.typ) Source.annotated_phrase

(* Expressions *)

type exp = exp' phrase
and exp' =
  | PrimE of string                            (* primitive *)
  | VarE of id                                 (* variable *)
  | LitE of lit                                (* literal *)
  | UnE of Type.typ * unop * exp               (* unary operator *)
  | BinE of Type.typ * exp * binop * exp       (* binary operator *)
  | RelE of Type.typ * exp * relop * exp       (* relational operator *)
  | TupE of exp list                           (* tuple *)
  | ProjE of exp * int                         (* tuple projection *)
  | OptE of exp                                (* option injection *)
  | DotE of exp * name                         (* object projection *)
  | ActorDotE of exp * name                    (* actor field access *)
  | AssignE of exp * exp                       (* assignment *)
  | ArrayE of mut * Type.typ * exp list        (* array *)
  | IdxE of exp * exp                          (* array indexing *)
  | CallE of                                   (* function call *)
      Value.call_conv * exp * Type.typ list * exp
  | BlockE of (dec list * exp)                 (* block *)
  | IfE of exp * exp * exp                     (* conditional *)
  | SwitchE of exp * case list                 (* switch *)
  | LoopE of exp                               (* do-while loop *)
  | LabelE of id * Type.typ * exp              (* label *)
  | BreakE of id * exp                         (* break *)
  | RetE of exp                                (* return *)
  | AsyncE of exp                              (* async *)
  | AwaitE of exp                              (* await *)
  | AssertE of exp                             (* assertion *)
  | DeclareE of id * Type.typ * exp            (* local promise *)
  | DefineE of id * mut * exp                  (* promise fulfillment *)
  | FuncE of                                   (* function *)
      string * Value.call_conv * typ_bind list * arg list * Type.typ list * exp
  | ActorE of id * dec list * field list * Type.typ (* actor *)
  | NewObjE of  Type.obj_sort * field list * Type.typ  (* make an object *)

and field = (field', Type.typ) Source.annotated_phrase
and field' = {name : name; var : id} (* the var is by reference, not by value *)

and name = name' Source.phrase
and name' = Name of string

and case = case' Source.phrase
and case' = {pat : pat; exp : exp}


(* Declarations *)

and dec = dec' Source.phrase
and dec' =
  | LetD of pat * exp                          (* immutable *)
  | VarD of id * exp                           (* mutable *)
  | TypD of Type.con                           (* type *)


(* Flavor *)

(*
We have a bunch of flavors of the IR, where some constructors are not
allowed in some flavors. In an ideal world, we would have different IRs for
that (or maybe GADTs). But for now we simply track that on the value level. The
main purpose of tracking that is to inform `Check_ir` about the invariants that
should hold.
*)

type flavor = {
  has_async_typ : bool; (* AsyncT *)
  has_await : bool; (* AwaitE and AsyncE *)
  serialized : bool; (* Shared function arguments are serialized *)
}


(* Program *)

type prog = (dec list * exp) * flavor
