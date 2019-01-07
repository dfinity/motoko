(* Notes *)

type typ_note = {note_typ : Type.typ; note_eff : Type.eff}

let empty_typ_note = {note_typ = Type.Pre; note_eff = Type.Triv}

(* Identifiers *)

type id = string Source.phrase

(* Names (not alpha-convertible), used for field and class names *)
type name = name' Source.phrase
and name' = Name of string                
let string_of_name (Name s ) = s              


(* Types *)

type sharing = Type.sharing Source.phrase
type obj_sort = Type.obj_sort Source.phrase
type func_sort = Type.func_sort Source.phrase

type mut = mut' Source.phrase
and mut' = Const | Var

type typ = (typ',typ_note) Source.annotated_phrase
and typ' =
  | PrimT of string                                (* primitive *)
  | VarT of id * typ list                          (* constructor *)
  | ObjT of obj_sort * typ_field list              (* object *)
  | ArrayT of mut * typ                            (* array *)
  | OptT of typ                                    (* option *)
  | TupT of typ list                               (* tuple *)
  | FuncT of func_sort * typ_bind list * typ * typ (* function *)
  | AsyncT of typ                                  (* future *)
  | LikeT of typ                                   (* expansion *)
  | ParT of typ                                    (* parentheses, used to control function arity only *)
(*
  | UnionT of type * typ                           (* union *)
  | AtomT of string                                (* atom *)
*)

and typ_field = typ_field' Source.phrase
and typ_field' = {id : id; typ : typ; mut : mut}

and typ_bind = (typ_bind', Con.t option ref) Source.annotated_phrase
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

type pat = (pat', typ_note) Source.annotated_phrase
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

type priv = priv' Source.phrase
and priv' = Public | Private

(* type instantiations *)
type inst = (typ, Type.typ ref) Source.annotated_phrase

(* Filled in for overloaded operators during type checking. Initially Type.Pre. *)
type op_type = Type.typ ref

type exp = (exp', typ_note) Source.annotated_phrase
and exp' =
  | PrimE of string                            (* primitive *)
  | VarE of id                                 (* variable *)
  | LitE of lit ref                            (* literal *)
  | UnE of op_type * unop * exp                (* unary operator *)
  | BinE of op_type * exp * binop * exp        (* binary operator *)
  | RelE of op_type * exp * relop * exp        (* relational operator *)
  | TupE of exp list                           (* tuple *)
  | ProjE of exp * int                         (* tuple projection *)
  | OptE of exp                                (* option injection *)
  | ObjE of obj_sort * id * exp_field list     (* object *)
  | DotE of exp * name                         (* object projection *)
  | AssignE of exp * exp                       (* assignment *)
  | ArrayE of mut * exp list                   (* array *)
  | IdxE of exp * exp                          (* array indexing *)
  | CallE of exp * inst list * exp             (* function call *)
  | BlockE of dec list                         (* block *)
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
  | IsE of exp * exp                           (* instance-of *)
  | AnnotE of exp * typ                        (* type annotation *)
  | DecE of dec                                (* declaration *)
  | DeclareE of id * Type.typ * exp            (* local promise (internal) *)
  | DefineE of id * mut * exp                  (* promise fulfillment (internal) *)
  | NewObjE of obj_sort * (name * id) list     (* make an object, preserving mutable identity (internal) *)
(*
  | ThrowE of exp list                         (* throw exception *)
  | TryE of exp * case list                    (* catch eexception *)
  | FinalE of exp * exp                        (* finally *)
  | AtomE of string                            (* atom *)
*)

and exp_field = exp_field' Source.phrase
and exp_field' = {name : name; id : id; exp : exp; mut : mut; priv : priv}

and case = case' Source.phrase
and case' = {pat : pat; exp : exp}


(* Declarations *)

and dec = (dec', typ_note) Source.annotated_phrase
and dec' =
  | ExpD of exp                                        (* plain expression *)
  | LetD of pat * exp                                  (* immutable *)
  | VarD of id * exp                                   (* mutable *)
  | FuncD of sharing * id * typ_bind list * pat * typ * exp (* function *)
  | TypD of id * typ_bind list * typ                   (* type *)
  | ClassD of id (*term id*) * id (*type id*) * typ_bind list * obj_sort * pat * id * exp_field list (* class *)


(* Program *)

type prog = prog' Source.phrase
and prog' = dec list


(* n-ary arguments/result sequences *)

 
let seqT ts =
  match ts with
  | [t] -> t
  | ts -> {Source.it = TupT ts;
           at = Source.no_region;
           Source.note = {note_typ = Type.Tup (List.map (fun t -> t.Source.note.note_typ) ts);
                          note_eff = Type.Triv}}

let as_seqT t =
  match t.Source.it with
  | TupT ts -> ts
  | _ -> [t]
           
