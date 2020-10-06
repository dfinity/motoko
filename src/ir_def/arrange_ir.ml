open Mo_types
open Mo_values
open Source
open Ir
open Wasm.Sexpr

let ($$) head inner = Node (head, inner)

let id i = Atom i

let typ t = Atom (Type.string_of_typ t)
let prim_ty p = typ (Type.Prim p)
let kind k = Atom (Type.string_of_kind k)

let rec exp e = match e.it with
  | VarE i              -> "VarE"    $$ [id i]
  | LitE l              -> "LitE"    $$ [lit l]
  | PrimE (p, es)       -> "PrimE"   $$ [prim p] @ List.map exp es
  | AssignE (le1, e2)   -> "AssignE" $$ [lexp le1; exp e2]
  | BlockE (ds, e1)     -> "BlockE"  $$ List.map dec ds @ [exp e1]
  | IfE (e1, e2, e3)    -> "IfE"     $$ [exp e1; exp e2; exp e3]
  | SwitchE (e, cs)     -> "SwitchE" $$ [exp e] @ List.map case cs
  | LoopE e1            -> "LoopE"   $$ [exp e1]
  | LabelE (i, t, e)    -> "LabelE"  $$ [id i; typ t; exp e]
  | AsyncE (tb, e, t)   -> "AsyncE"  $$ [typ_bind tb; exp e; typ t]
  | DeclareE (i, t, e1) -> "DeclareE" $$ [id i; exp e1]
  | DefineE (i, m, e1)  -> "DefineE" $$ [id i; mut m; exp e1]
  | FuncE (x, s, c, tp, as_, ts, e) ->
    "FuncE" $$ [Atom x; func_sort s; control c] @ List.map typ_bind tp @ args as_ @ [ typ (Type.seq ts); exp e]
  | SelfCallE (ts, exp_f, exp_k, exp_r) ->
    "SelfCallE" $$ [typ (Type.seq ts); exp exp_f; exp exp_k; exp exp_r]
  | ActorE (ds, fs, u, t) -> "ActorE"  $$ List.map dec ds @ fields fs @ [upgrade u; typ t]
  | NewObjE (s, fs, t)  -> "NewObjE" $$ (Arrange_type.obj_sort s :: fields fs @ [typ t])
  | TryE (e, cs)        -> "TryE" $$ [exp e] @ List.map case cs

and upgrade { pre; post } =
  "Upgrade" $$ ["Pre" $$ [exp pre]; "Post" $$ [exp post]]

and lexp le = match le.it with
  | VarLE i             -> "VarLE" $$ [id i]
  | IdxLE (e1, e2)      -> "IdxLE" $$ [exp e1; exp e2]
  | DotLE (e1, n)       -> "DotLE" $$ [exp e1; Atom n]

and fields fs = List.fold_left (fun flds (f : field) -> (f.it.name $$ [ id f.it.var ]):: flds) [] fs

and args = function
 | [] -> []
 | as_ -> ["params" $$ List.map arg as_]

and arg a = Atom a.it

and prim = function
  | CallPrim ts       -> "CallPrim" $$ List.map typ ts
  | UnPrim (t, uo)    -> "UnPrim"     $$ [typ t; Arrange_ops.unop uo]
  | BinPrim (t, bo)   -> "BinPrim"    $$ [typ t; Arrange_ops.binop bo]
  | RelPrim (t, ro)   -> "RelPrim"    $$ [typ t; Arrange_ops.relop ro]
  | TupPrim           -> Atom "TupPrim"
  | ProjPrim i        -> "ProjPrim"   $$ [Atom (string_of_int i)]
  | OptPrim           -> Atom "OptPrim"
  | TagPrim i         -> "TagE" $$ [id i]
  | DotPrim n         -> "DotPrim" $$ [Atom n]
  | ActorDotPrim n    -> "ActorDotPrim" $$ [Atom n]
  | ArrayPrim (m, t)  -> "ArrayPrim"  $$ [mut m; typ t]
  | IdxPrim           -> Atom "IdxPrim"
  | BreakPrim i       -> "BreakPrim"  $$ [id i]
  | RetPrim           -> Atom "RetPrim"
  | AwaitPrim         -> Atom "AwaitPrim"
  | AssertPrim        -> Atom "AssertPrim"
  | ThrowPrim         -> Atom "ThrowPrim"
  | ShowPrim t        -> "ShowPrim" $$ [typ t]
  | SerializePrim t   -> "SerializePrim" $$ List.map typ t
  | DeserializePrim t -> "DeserializePrim" $$ List.map typ t
  | NumConvPrim (t1, t2) -> "NumConvPrim" $$ [prim_ty t1; prim_ty t2]
  | CastPrim (t1, t2) -> "CastPrim" $$ [typ t1; typ t2]
  | ActorOfIdBlob t   -> "ActorOfIdBlob" $$ [typ t]
  | BlobOfIcUrl       -> Atom "BlobOfIcUrl"
  | IcUrlOfBlob       -> Atom "IcUrlOfBlob"
  | SelfRef t         -> "SelfRef"    $$ [typ t]
  | SystemTimePrim    -> Atom "SystemTimePrim"
  | SystemFundsAddPrim -> Atom "SystemFundsAcceptPrim"
  | SystemFundsAcceptPrim -> Atom "SystemFundsAcceptPrim"
  | SystemFundsAvailablePrim -> Atom "SystemFundsAvailablePrim"
  | SystemFundsBalancePrim -> Atom "SystemFundsBalancePrim"
  | SystemFundsRefundedPrim -> Atom "SystemFundsRefundedPrim"
  | OtherPrim s       -> Atom s
  | CPSAwait          -> Atom "CPSAwait"
  | CPSAsync t        -> "CPSAsync" $$ [typ t]
  | ICReplyPrim ts    -> "ICReplyPrim" $$ List.map typ ts
  | ICRejectPrim      -> Atom "ICRejectPrim"
  | ICCallerPrim      -> Atom "ICCallerPrim"
  | ICCallPrim        -> Atom "ICCallPrim"
  | ICStableWrite t   -> "ICStableWrite" $$ [typ t]
  | ICStableRead t    -> "ICStableRead" $$ [typ t]


and mut = function
  | Const -> Atom "Const"
  | Var   -> Atom "Var"

and pat p = match p.it with
  | WildP           -> Atom "WildP"
  | VarP i          -> "VarP"       $$ [ id i ]
  | TupP ps         -> "TupP"       $$ List.map pat ps
  | ObjP pfs        -> "ObjP"       $$ List.map pat_field pfs
  | LitP l          -> "LitP"       $$ [ lit l ]
  | OptP p          -> "OptP"       $$ [ pat p ]
  | TagP (i, p)     -> "TagP"       $$ [ id i; pat p ]
  | AltP (p1,p2)    -> "AltP"       $$ [ pat p1; pat p2 ]

and lit (l:lit) = match l with
  | NullLit       -> Atom "NullLit"
  | BoolLit true  -> "BoolLit"   $$ [ Atom "true" ]
  | BoolLit false -> "BoolLit"   $$ [ Atom "false" ]
  | NatLit n      -> "NatLit"    $$ [ Atom (Value.Nat.to_pretty_string n) ]
  | Nat8Lit w     -> "Nat8Lit"   $$ [ Atom (Value.Nat8.to_pretty_string w) ]
  | Nat16Lit w    -> "Nat16Lit"  $$ [ Atom (Value.Nat16.to_pretty_string w) ]
  | Nat32Lit w    -> "Nat32Lit"  $$ [ Atom (Value.Nat32.to_pretty_string w) ]
  | Nat64Lit w    -> "Nat64Lit"  $$ [ Atom (Value.Nat64.to_pretty_string w) ]
  | IntLit i      -> "IntLit"    $$ [ Atom (Value.Int.to_pretty_string i) ]
  | Int8Lit w     -> "Int8Lit"   $$ [ Atom (Value.Int_8.to_pretty_string w) ]
  | Int16Lit w    -> "Int16Lit"  $$ [ Atom (Value.Int_16.to_pretty_string w) ]
  | Int32Lit w    -> "Int32Lit"  $$ [ Atom (Value.Int_32.to_pretty_string w) ]
  | Int64Lit w    -> "Int64Lit"  $$ [ Atom (Value.Int_64.to_pretty_string w) ]
  | Word8Lit w    -> "Word8Lit"  $$ [ Atom (Value.Word8.to_pretty_string w) ]
  | Word16Lit w   -> "Word16Lit" $$ [ Atom (Value.Word16.to_pretty_string w) ]
  | Word32Lit w   -> "Word32Lit" $$ [ Atom (Value.Word32.to_pretty_string w) ]
  | Word64Lit w   -> "Word64Lit" $$ [ Atom (Value.Word64.to_pretty_string w) ]
  | FloatLit f    -> "FloatLit"  $$ [ Atom (Value.Float.to_pretty_string f) ]
  | CharLit c     -> "CharLit"   $$ [ Atom (string_of_int c) ]
  | TextLit t     -> "TextLit"   $$ [ Atom t ]
  | BlobLit b     -> "BlobLit"   $$ [ Atom (Printf.sprintf "%S" b) ] (* hex might be nicer *)

and pat_field pf = pf.it.name $$ [pat pf.it.pat]

and case c = "case" $$ [pat c.it.pat; exp c.it.exp]

and func_sort s = Atom (Arrange_type.func_sort s)

and control s = Atom (Arrange_type.control s)

and dec d = match d.it with
  | LetD (p, e) -> "LetD" $$ [pat p; exp e]
  | VarD (i, t, e) -> "VarD" $$ [id i; typ t; exp e]

and typ_bind (tb : typ_bind) =
  Con.to_string tb.it.con $$ [typ tb.it.bound]

and comp_unit = function
  | LibU (ds, e) -> "LibU" $$ List.map dec ds @ [ exp e ]
  | ProgU ds -> "ProgU" $$ List.map dec ds
  | ActorU (None, ds, fs, u, t) -> "ActorU"  $$ List.map dec ds @ fields fs @ [upgrade u; typ t]
  | ActorU (Some as_, ds, fs, u, t) -> "ActorU"  $$ List.map arg as_ @ List.map dec ds @ fields fs @ [upgrade u; typ t]

and prog (cu, _flavor) = comp_unit cu
