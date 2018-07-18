(* Variables *)

type var = string Source.phrase
type var_ref = (string,Types.mut) Source.annotated_phrase

(* Types *)

type mut = Types.mut Source.phrase

type actor = Types.actor Source.phrase

type typ = typ' Source.phrase
and typ' =
  | VarT of var * typ list                     (* constructor *)
  | PrimT of Types.prim                        (* primitive *)
  | ObjT of actor * typ_field list             (* object *)
  | ArrayT of mut * typ                        (* array *)
  | OptT of typ                                (* option *)
  | TupT of typ list                           (* tuple *)
  | FuncT of typ_bind list * typ * typ         (* function *)
  | AsyncT of typ                              (* future *)
  | LikeT of typ                               (* expansion *)
  | AnyT                                       (* top *)
(*
  | UnionT of type * typ                       (* union *)
  | AtomT of string                            (* atom *)
*)

and typ_field = typ_field' Source.phrase
and typ_field' = {var : var; typ : typ; mut : mut}

and typ_bind = typ_bind' Source.phrase
and typ_bind' = {var : var; bound : typ}


(* Patterns *)

type pat = pat' Source.phrase
and pat' =
  | WildP                                      (* wildcard *)
  | VarP of var                                (* variable *)
  | TupP of pat list                           (* tuple *)
  | AnnotP of pat * typ                        (* type annotation *)
  | LitP of Types.lit ref                          (* literal *) (* only in switch case, for now *)
(*
  | ObjP of pat_field list                     (* object *)
  | AsP of pat * pat                           (* conjunctive *)
  | OrP of pat * pat                           (* disjunctive *)

and pat_field = pat_field' Source.phrase
and pat_field' = {var : var; pat : pat}
*)


(* Expressions *)



type priv = priv' Source.phrase
and priv' = Public | Private

type exp = (exp',Types.typ) Source.annotated_phrase
and exp' =
  | VarE of var_ref                                (* variable *)
  | LitE of  Types.lit ref                     (* literal *)
  | UnE of Types.unop * exp                    (* unary operator *)
  | BinE of exp * Types.binop * exp            (* binary operator *)
  | RelE of exp * Types.relop * exp            (* relational operator *)
  | TupE of exp list                           (* tuple *)
  | ProjE of exp * Types.nat                   (* tuple projection *)
  | ObjE of actor * var option * exp_field list  (* object *)
  | DotE of exp * var_ref                       (* object projection *)
  | AssignE of exp * exp                       (* assignment *)
  | ArrayE of exp list                         (* array *)
  | IdxE of exp * exp                          (* array indexing *)
  | CallE of exp * exp                         (* function call *)
  | BlockE of exp list                         (* block *)
  | NotE of exp                                (* negation *)
  | AndE of exp * exp                          (* conjunction *)
  | OrE of exp * exp                           (* disjunction *)
  | IfE of exp * exp * exp                     (* conditional *)
  | SwitchE of exp * case list                 (* switch *)
  | WhileE of exp * exp                        (* while-do loop *)
  | LoopE of exp * exp option                  (* do-while loop *)
  | ForE of pat * exp * exp                    (* iteration *)
  | LabelE of var * exp                        (* label *)
  | BreakE of var * exp                        (* break *)
  | ContE of var                               (* continue *)
  | RetE of exp                                (* return *)
  | AsyncE of exp                              (* async *)
  | AwaitE of exp                              (* await *)
  | AssertE of exp                             (* assertion *)
  | IsE of exp * typ                           (* instance-of *)
  | AnnotE of exp * typ                        (* type annotation *)
  | DecE of dec                                (* declaration *)
(*
  | ThrowE of exp list                         (* throw exception *)
  | TryE of exp * case list                    (* catch eexception *)
  | FinalE of exp * exp                        (* finally *)
  | AtomE of string                            (* atom *)
*)

and exp_field = exp_field' Source.phrase
and exp_field' = {var : var; exp : exp; mut : mut; priv : priv}

and case = case' Source.phrase
and case' = {pat : pat; exp : exp}


(* Declarations *)

and dec = dec' Source.phrase
and dec' =
  | LetD of pat * exp                                  (* immutable *)
  | VarD of var * typ * exp option                     (* mutable *)  (* why do we need the type when given an exp *)
  | FuncD of var * typ_bind list * pat * typ * exp     (* function *) (* why do we need the range type? *)
  | TypD of var * typ_bind list * typ                  (* type *)
(* | ClassD of actor * var * typ_bind list * pat * exp (* class *) *)
  | ClassD of actor * var * typ_bind list * pat * exp_field list (* class *)


(* Program *)

type prog = prog' Source.phrase
and prog' = dec list
