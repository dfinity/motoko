open As_types
open As_values
open Source
open Ir
open Wasm.Sexpr

let ($$) head inner = Node (head, inner)

let id i = Atom i

let typ t = Atom (Type.string_of_typ t)

let con c = Atom (Type.string_of_con c)

let rec exp e = match e.it with
  | VarE i              -> "VarE"    $$ [id i]
  | LitE l              -> "LitE"    $$ [lit l]
  | UnE (t, uo, e)      -> "UnE"     $$ [typ t; Arrange_ops.unop uo; exp e]
  | BinE (t, e1, bo, e2)-> "BinE"    $$ [typ t; exp e1; Arrange_ops.binop bo; exp e2]
  | RelE (t, e1, ro, e2)-> "RelE"    $$ [typ t; exp e1; Arrange_ops.relop ro; exp e2]
  | ShowE (t, e)        -> "ShowE"   $$ [typ t; exp e]
  | TupE es             -> "TupE"    $$ List.map exp es
  | ProjE (e, i)        -> "ProjE"   $$ [exp e; Atom (string_of_int i)]
  | DotE (e, n)         -> "DotE"    $$ [exp e; Atom n]
  | ActorDotE (e, n)    -> "ActorDotE" $$ [exp e; Atom n]
  | AssignE (e1, e2)    -> "AssignE" $$ [exp e1; exp e2]
  | ArrayE (m, t, es)   -> "ArrayE"  $$ [mut m; typ t] @ List.map exp es
  | IdxE (e1, e2)       -> "IdxE"    $$ [exp e1; exp e2]
  | CallE (cc, e1, ts, e2) -> "CallE" $$ [call_conv cc; exp e1] @ List.map typ ts @ [exp e2]
  | BlockE (ds, e1)     -> "BlockE"  $$ List.map dec ds @ [exp e1]
  | IfE (e1, e2, e3)    -> "IfE"     $$ [exp e1; exp e2; exp e3]
  | SwitchE (e, cs)     -> "SwitchE" $$ [exp e] @ List.map case cs
  | LoopE e1            -> "LoopE"   $$ [exp e1]
  | LabelE (i, t, e)    -> "LabelE"  $$ [id i; typ t; exp e]
  | BreakE (i, e)       -> "BreakE"  $$ [id i; exp e]
  | RetE e              -> "RetE"    $$ [exp e]
  | AsyncE e            -> "AsyncE"  $$ [exp e]
  | AwaitE e            -> "AwaitE"  $$ [exp e]
  | AssertE e           -> "AssertE" $$ [exp e]
  | OptE e              -> "OptE"    $$ [exp e]
  | TagE (i, e)         -> "TagE" $$ [id i; exp e]
  | PrimE p             -> "PrimE"   $$ [Atom p]
  | DeclareE (i, t, e1) -> "DeclareE" $$ [id i; exp e1]
  | DefineE (i, m, e1)  -> "DefineE" $$ [id i; mut m; exp e1]
  | FuncE (x, cc, tp, as_, ts, e) ->
    "FuncE" $$ [Atom x; call_conv cc] @ List.map typ_bind tp @ args as_@ [ typ (Type.seq ts); exp e]
  | ActorE (i, ds, fs, t) -> "ActorE"  $$ [id i] @ List.map dec ds @ fields fs @ [typ t]
  | NewObjE (s, fs, t)  -> "NewObjE" $$ (Arrange_type.obj_sort s :: fields fs @ [typ t])

and fields fs = List.fold_left (fun flds (f : field) -> (f.it.name $$ [ id f.it.var ]):: flds) [] fs

and args = function
 | [] -> []
 | as_ -> ["params" $$ List.map arg as_]

and arg a = Atom a.it

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

and pat_field pf = pf.it.name $$ [pat pf.it.pat]

and case c = "case" $$ [pat c.it.pat; exp c.it.exp]

and call_conv cc = Atom (Call_conv.string_of_call_conv cc)

and dec d = match d.it with
  | LetD (p, e) -> "LetD" $$ [pat p; exp e]
  | VarD (i, e) -> "VarD" $$ [id i; exp e]
  | TypD c -> "TypD" $$ [con c]

and typ_bind (tb : typ_bind) =
  Con.to_string tb.it.con $$ [typ tb.it.bound]


and prog ((ds, e), _flavor)= "BlockE"  $$ List.map dec ds @ [ exp e ]
