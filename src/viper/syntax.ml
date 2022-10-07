type info = NoInfo

type id = (string, info) Source.annotated_phrase

type prog = (prog', info) Source.annotated_phrase

and prog' =
  Prog

and item = (item', info) Source.annotated_phrase
and item' =
  (* | import path *)
  | FieldI of id * typ
  | MethodI of id * localvardecl list * exp list * exp list * seqn option

and localvardecl = id * typ

and seqn = (stmt list * decl list, info ) Source.annotated_phrase

and decl = ((id * typ), info) Source.annotated_phrase

and exp = (exp', info) Source.annotated_phrase

and exp' =
  | BoolE of bool
  | IntN of int (* Num.Big_int.t *)
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Div of exp * exp
  | Mod of exp * exp
  | Lt of exp * exp
  | Le of exp * exp
  | Gt of exp * exp
  | Ge of exp * exp
  | Eq of exp * exp
  | Ne of exp * exp
  | FldAcc of fldacc

and invariants = exp list

and stmt = (stmt', info) Source.annotated_phrase

and fldacc = (exp * id)

and stmt' =
  | MethodCallS of id * exp list * id list
  | ExhaleS of exp
  | InhaleS of exp
  | AssertS of exp
  | AssumeS of exp
  | SeqnS of seqn
  | VarAssignE of id * exp
  | FieldAssignE of fldacc * exp
  | IfS of exp * seqn * seqn
  | WhileE of exp * invariants * seqn
  | LabelE of id * invariants


and typ = (typ', info) Source.annotated_phrase

and typ' =
  | IntT

