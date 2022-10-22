type info = NoInfo

type id = (string, info) Source.annotated_phrase

type prog = (item list, info) Source.annotated_phrase

and item = (item', info) Source.annotated_phrase
and item' =
  (* | import path *)
  | FieldI of id * typ
  | MethodI of id * par list * par list * exp list * exp list * seqn option
  | InvariantI of string * exp

and par = id * typ

and seqn = (decl list * stmt list, info) Source.annotated_phrase

and decl = (id * typ, info) Source.annotated_phrase

and exp = (exp', info) Source.annotated_phrase

and exp' =
  | LocalVar of id * typ
  | Result of typ
  | BoolLitE of bool
  | NullLitE
  | IntLitE of Mo_values.Numerics.Int.t
  | AddE of exp * exp
  | SubE of exp * exp
  | MulE of exp * exp
  | DivE of exp * exp
  | ModE of exp * exp
  | LtCmpE of exp * exp
  | LeCmpE of exp * exp
  | GtCmpE of exp * exp
  | GeCmpE of exp * exp
  | EqCmpE of exp * exp
  | NeCmpE of exp * exp
  | MinusE of exp
  | NotE of exp
  | AndE of exp * exp
  | OrE of exp * exp
  | Implies of exp * exp
  | FldAcc of fldacc
  | PermE of perm          (* perm_amount *)
  | AccE of fldacc * exp   (* acc((rcvr: exp).field, (exp: perm_amount)) *)
  | MacroCall of string * exp

and perm = (perm', info) Source.annotated_phrase

and perm' =
  | NoP                       (* 0 / 1 *)
  | FullP                     (* 1 / 1 *)
  | WildcardP                 (* 1 / N for some N *)
  | FractionalP of exp * exp  (* (a: exp) / (b: exp) *)

and invariants = exp list

and stmt = (stmt', info) Source.annotated_phrase

and tmpl = (tmpl', info) Source.annotated_phrase
and tmpl' = (Mo_def.Syntax.exp -> exp) -> exp

and fldacc = exp * id

and stmt' =
  | MethodCallS of id * exp list * id list
  | ExhaleS of exp
  | InhaleS of exp
  | AssertS of exp
  | AssumeS of exp
  | SeqnS of seqn
  | VarAssignS of id * exp
  | FieldAssignS of fldacc * exp
  | IfS of exp * seqn * seqn
  | WhileS of exp * invariants * seqn
  | LabelS of id * invariants
  (* TODO: these are temporary helper terms  that should not appear in the final translation 
       we should avoid introducing them in the first place if possible, so they can be removed *)
  | PreconditionS of exp
  | PostconditionS of exp
  | ConcurrencyS of string * exp * tmpl


and typ = (typ', info) Source.annotated_phrase

and typ' =
  | IntT
  | BoolT
  | RefT

