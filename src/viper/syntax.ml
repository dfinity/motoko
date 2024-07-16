type info =
  | NoInfo
  | ActorInit
  | PublicFunction of string
  | PrivateFunction of string

type id = (string, info) Source.annotated_phrase

type prog = (item list, info) Source.annotated_phrase

and item = (item', info) Source.annotated_phrase
and item' =
  (* | import path *)
  | AdtI of id * id list * adt_con list
  | FieldI of id * typ
  | MethodI of id * par list * par list * exp list * exp list * seqn option
  | InvariantI of string * exp

and adt_con = { con_name : id; con_fields : (id * typ) list }

and par = id * typ

and seqn = (seqn', info) Source.annotated_phrase
and seqn' = decl list * stmt list

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
  | Old of exp
  | FldAcc of fldacc
  | PermE of perm          (* perm_amount *)
  | AccE of fldacc * exp   (* acc((rcvr: exp).field, (exp: perm_amount)) *)
  | FldE of string               (* top-level field name, e.g. to be passed as a macro argument *)
  | CallE of string * exp list   (* macro or func call *)
  | ForallE of (id * typ) list * exp
  | ExistsE of (id * typ) list * exp

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
  | MethodCallS of id list * id * exp list
  | ExhaleS of exp
  | InhaleS of exp
  | AssertS of exp
  | AssumeS of exp
  | SeqnS of seqn
  | VarAssignS of id * exp
  | FieldAssignS of fldacc * exp
  | IfS of exp * seqn * seqn
  | WhileS of exp * invariants * seqn
  | LabelS of id
  | GotoS of id
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
  | ArrayT
  | TupleT of typ list
  | OptionT of typ
  | ConT of id * typ list

