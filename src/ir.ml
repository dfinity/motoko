(* Patterns *)

type pat = pat' Source.phrase
and pat' =
  | WildP                                      (* wildcard *)
  | VarP of Syntax.id                          (* variable *)
  | LitP of Syntax.lit                         (* literal *)
  | TupP of pat list                           (* tuple *)
  | OptP of pat                                (* option *)
  | AltP of pat * pat                          (* disjunctive *)

(* Expressions *)

type exp = exp' Source.phrase
and exp' =
  | PrimE of string                            (* primitive *)
  | VarE of Syntax.id                          (* variable *)
  | LitE of Syntax.lit                         (* literal *)
  | UnE of Type.prim * Syntax.unop * exp                   (* unary operator *)
  | BinE of Type.prim * exp * Syntax.binop * exp           (* binary operator *)
  | RelE of Type.prim * exp * Syntax.relop * exp           (* relational operator *)
  | TupE of exp list                           (* tuple *)
  | ProjE of exp * int                         (* tuple projection *)
  | OptE of exp                                (* option injection *)
  | ActorE of Syntax.id * exp_field list       (* actor *)
  | DotE of exp * Syntax.name                  (* object projection *)
  | AssignE of exp * exp                       (* assignment *)
  | ArrayE of Syntax.mut * exp list            (* array *)
  | IdxE of exp * exp                          (* array indexing *)
  | CallE of Value. call_conv * exp * Syntax.typ list * exp (* function call *)
  | BlockE of dec list                         (* block *)
  | IfE of exp * exp * exp                     (* conditional *)
  | SwitchE of exp * case list                 (* switch *)
  | WhileE of exp * exp                        (* while-do loop *)
  | LoopE of exp * exp option                  (* do-while loop *)
  | ForE of pat * exp * exp                    (* iteration *)
  | LabelE of Syntax.id * Syntax.typ * exp     (* label *)
  | BreakE of Syntax.id * exp                  (* break *)
  | RetE of exp                                (* return *)
  | AsyncE of exp                              (* async *)
  | AwaitE of exp                              (* await *)
  | AssertE of exp                             (* assertion *)
  | IsE of exp * exp                           (* instance-of *)
  | DeclareE of Syntax.id * Type.typ * exp     (* local promise (internal) *)
  | DefineE of Syntax.id * Syntax.mut * exp    (* promise fulfillment (internal) *)
  | NewObjE of Syntax.obj_sort * (Syntax.name * Syntax.id) list (* make an object, preserving mutable identity (internal) *)

and exp_field = exp_field' Source.phrase
and exp_field' = {name : Syntax.name; id : Syntax.id; exp : exp; mut : Syntax.mut; priv : Syntax.priv}

and case = case' Source.phrase
and case' = {pat : pat; exp : exp}


(* Declarations *)

and dec = dec' Source.phrase
and dec' =
  | ExpD of exp                                        (* plain expression *)
  | LetD of pat * exp                                  (* immutable *)
  | VarD of Syntax.id * exp                                   (* mutable *)
  | FuncD of Value.call_conv * Syntax.id * Syntax.typ_bind list * pat * Syntax.typ * exp (* function *)
  | TypD of Syntax.id * Syntax.typ_bind list * Syntax.typ                   (* type *)


(* Program *)

type prog = prog' Source.phrase
and prog' = dec list
