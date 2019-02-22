(* Notes *)

type typ_note = {note_typ : Type.typ; note_eff : Type.eff}

let empty_typ_note = {note_typ = Type.Pre; note_eff = Type.Triv}


(* Identifiers *)

type id = string Source.phrase
type con_id = (string, Type.con option) Source.annotated_phrase


(* Types *)

type sharing = Type.sharing Source.phrase
type obj_sort = Type.obj_sort Source.phrase

type mut = mut' Source.phrase
and mut' = Const | Var

type typ = (typ', Type.typ) Source.annotated_phrase
and typ' =
  | PrimT of string                                (* primitive *)
  | VarT of id * typ list                          (* constructor *)
  | ObjT of obj_sort * typ_field list              (* object *)
  | ArrayT of mut * typ                            (* array *)
  | OptT of typ                                    (* option *)
  | TupT of typ list                               (* tuple *)
  | FuncT of sharing * typ_bind list * typ * typ   (* function *)
  | AsyncT of typ                                  (* future *)
  | ParT of typ                                    (* parentheses, used to control function arity only *)
(*
  | UnionT of type * typ                           (* union *)
  | AtomT of string                                (* atom *)
*)

and typ_field = typ_field' Source.phrase
and typ_field' = {id : id; typ : typ; mut : mut}

and typ_bind = (typ_bind', Type.con option) Source.annotated_phrase
and typ_bind' = {var : id; bound : typ}


(* Literals *)

type lit =
  | NullLit
  | BoolLit of bool
  | NatLit of Value.Nat.t
  | IntLit of Value.Int.t
  | Word8Lit of Value.Word8.t
  | Word16Lit of Value.Word16.t
  | Word32Lit of Value.Word32.t
  | Word64Lit of Value.Word64.t
  | FloatLit of Value.Float.t
  | CharLit of Value.unicode
  | TextLit of string
  | PreLit of string * Type.prim


(* Operators *)

type unop =
  | PosOp                                       (* +x *)
  | NegOp                                       (* -x *)
  | NotOp                                       (* bitwise negation *)

type binop =
  | AddOp                                       (* x+y *)
  | SubOp                                       (* x-y *)
  | MulOp                                       (* x*y *)
  | DivOp                                       (* x/y *)
  | ModOp                                       (* x%y *)
  | PowOp                                       (* x^y *)
  | AndOp                                       (* bitwise operators... *)
  | OrOp
  | XorOp
  | ShLOp
  | ShROp
  | RotLOp
  | RotROp
  | CatOp                                       (* concatenation *)

type relop =
  | EqOp                                        (* x=y *)
  | NeqOp                                       (* x!=y *)
  | LtOp                                        (* x<y *)
  | GtOp                                        (* x>y *)
  | LeOp                                        (* x<=y *)
  | GeOp                                        (* x>=y *)


(* Patterns *)

type pat = (pat', Type.typ) Source.annotated_phrase
and pat' =
  | WildP                                      (* wildcard *)
  | VarP of id                                 (* variable *)
  | LitP of lit ref                            (* literal *)
  | SignP of unop * lit ref                    (* signed literal *)
  | TupP of pat list                           (* tuple *)
  | OptP of pat                                (* option *)
  | AltP of pat * pat                          (* disjunctive *)
  | AnnotP of pat * typ                        (* type annotation *)
(*
  | AsP of pat * pat                           (* conjunctive *)
  | ObjP of pat_field list                     (* object *)

and pat_field = pat_field' Source.phrase
and pat_field' = {id : id; pat : pat}
*)


(* Expressions *)

type vis = vis' Source.phrase
and vis' = Public | Private

(* Filled in for overloaded operators and blocks during type checking. Initially Type.Pre. *)
type op_typ = Type.typ ref

type exp = (exp', typ_note) Source.annotated_phrase
and exp' =
  | PrimE of string                            (* primitive *)
  | VarE of id                                 (* variable *)
  | LitE of lit ref                            (* literal *)
  | UnE of op_typ * unop * exp                 (* unary operator *)
  | BinE of op_typ * exp * binop * exp         (* binary operator *)
  | RelE of op_typ * exp * relop * exp         (* relational operator *)
  | TupE of exp list                           (* tuple *)
  | ProjE of exp * int                         (* tuple projection *)
  | OptE of exp                                (* option injection *)
  | ObjE of obj_sort * id * exp_field list     (* object *)
  | DotE of exp * Type.obj_sort ref * id       (* object projection *)
  | AssignE of exp * exp                       (* assignment *)
  | ArrayE of mut * exp list                   (* array *)
  | IdxE of exp * exp                          (* array indexing *)
  | CallE of exp * typ list * exp              (* function call *)
  | BlockE of dec list                         (* block (with type after avoidance)*)
  | NotE of exp                                (* negation *)
  | AndE of exp * exp                          (* conjunction *)
  | OrE of exp * exp                           (* disjunction *)
  | IfE of exp * exp * exp                     (* conditional *)
  | SwitchE of exp * case list                 (* switch *)
  | WhileE of exp * exp                        (* while-do loop *)
  | LoopE of exp * exp option                  (* do-while loop *)
  | ForE of pat * exp * exp                    (* iteration *)
  | LabelE of id * typ * exp                   (* label *)
  | BreakE of id * exp                         (* break *)
  | RetE of exp                                (* return *)
  | AsyncE of exp                              (* async *)
  | AwaitE of exp                              (* await *)
  | AssertE of exp                             (* assertion *)
  | AnnotE of exp * typ                        (* type annotation *)
(*
  | ThrowE of exp list                         (* throw exception *)
  | TryE of exp * case list                    (* catch eexception *)
  | FinalE of exp * exp                        (* finally *)
  | AtomE of string                            (* atom *)
*)

and exp_field = exp_field' Source.phrase
and exp_field' = {dec : dec; vis : vis}

and case = case' Source.phrase
and case' = {pat : pat; exp : exp}


(* Declarations *)

and dec = (dec', typ_note) Source.annotated_phrase
and dec' =
  | ExpD of exp                                (* plain expression *)
  | LetD of pat * exp                          (* immutable *)
  | VarD of id * exp                           (* mutable *)
  | FuncD of                                   (* function *)
      sharing * id * typ_bind list * pat * typ * exp
  | TypD of con_id * typ_bind list * typ       (* type *)
  | ClassD of                                  (* class *)
      id * con_id * typ_bind list * obj_sort * pat * id * exp_field list


(* Program *)

type prog = prog' Source.phrase
and prog' = dec list


(* n-ary arguments/result sequences *)

let seqT ts =
  match ts with
  | [t] -> t
  | ts ->
    { Source.it = TupT ts;
      at = Source.no_region;
      Source.note = Type.Tup (List.map (fun t -> t.Source.note) ts) }

let as_seqT t =
  match t.Source.it with
  | TupT ts -> ts
  | _ -> [t]

