open Source
open Syntax
open Wasm.Sexpr

let ($$) head inner = Node (head, inner)

let rec exp e = match e.it with
  | VarE i              -> "VarE"    $$ [id i]
  | LitE l              -> "LitE"    $$ [lit !l]
  | UnE (ot, uo, e)     -> "UnE"     $$ [operator_type !ot; unop uo; exp e]
  | BinE (ot, e1, bo, e2) -> "BinE"    $$ [operator_type !ot; exp e1; binop bo; exp e2]
  | RelE (ot, e1, ro, e2) -> "RelE"    $$ [operator_type !ot; exp e1; relop ro; exp e2]
  | TupE es             -> "TupE"    $$ List.map exp es
  | ProjE (e, i)        -> "ProjE"   $$ [exp e; Atom (string_of_int i)]
  | ObjE (s, i, efs)    -> "ObjE"    $$ [obj_sort s; id i] @ List.map exp_field efs
  | DotE (e, sr, n)         -> "DotE"    $$ [exp e; obj_sort' !sr; name n]
  | AssignE (e1, e2)    -> "AssignE" $$ [exp e1; exp e2]
  | ArrayE (m, es)      -> "ArrayE"  $$ [mut m] @ List.map exp es
  | IdxE (e1, e2)       -> "IdxE"    $$ [exp e1; exp e2]
  | CallE (e1, ts, e2)  -> "CallE"   $$ [exp e1] @ List.map typ ts @ [exp e2]
  | BlockE (ds)         -> "BlockE"  $$ List.map dec ds
  | NotE e              -> "NotE"    $$ [exp e]
  | AndE (e1, e2)       -> "AndE"    $$ [exp e1; exp e2]
  | OrE (e1, e2)        -> "OrE"     $$ [exp e1; exp e2]
  | IfE (e1, e2, e3)    -> "IfE"     $$ [exp e1; exp e2; exp e3]
  | SwitchE (e, cs)     -> "SwitchE" $$ [exp e] @ List.map case cs
  | WhileE (e1, e2)     -> "WhileE"  $$ [exp e1; exp e2]
  | LoopE (e1, None)    -> "LoopE"   $$ [exp e1]
  | LoopE (e1, Some e2) -> "LoopE"   $$ [exp e1; exp e2]
  | ForE (p, e1, e2)    -> "ForE"    $$ [pat p; exp e1; exp e2]
  | LabelE (i, t, e)    -> "LabelE"  $$ [id i; typ t; exp e]
  | BreakE (i, e)       -> "BreakE"  $$ [id i; exp e]
  | RetE e              -> "RetE"    $$ [exp e]
  | AsyncE e            -> "AsyncE"  $$ [exp e]
  | AwaitE e            -> "AwaitE"  $$ [exp e]
  | AssertE e           -> "AssertE" $$ [exp e]
  | AnnotE (e, t)       -> "AnnotE"  $$ [exp e; typ t]
  | OptE e              -> "OptE"    $$ [exp e]
  | PrimE p             -> "PrimE"   $$ [Atom p]

and pat p = match p.it with
  | WildP         -> Atom "WildP"
  | VarP i        -> "VarP"       $$ [ id i]
  | TupP ps       -> "TupP"       $$ List.map pat ps
  | AnnotP (p, t) -> "AnnotP"     $$ [ pat p; typ t]
  | LitP l        -> "LitP"       $$ [ lit !l ]
  | SignP (uo, l) -> "SignP"      $$ [ unop uo ; lit !l ]
  | OptP p        -> "OptP"       $$ [ pat p ]
  | AltP (p1,p2)  -> "AltP"       $$ [ pat p1; pat p2 ]

and lit (l:lit) = match l with
  | NullLit       -> Atom "NullLit"
  | BoolLit true  -> "BoolLit"   $$ [ Atom "true" ]
  | BoolLit false -> "BoolLit"   $$ [ Atom "false" ]
  | NatLit n      -> "NatLit"    $$ [ Atom (Value.Nat.to_string n) ]
  | IntLit i      -> "IntLit"    $$ [ Atom (Value.Int.to_string i) ]
  | Word8Lit w    -> "Word8Lit"  $$ [ Atom (Value.Word8.to_string_u w) ]
  | Word16Lit w   -> "Word16Lit" $$ [ Atom (Value.Word16.to_string_u w) ]
  | Word32Lit w   -> "Word32Lit" $$ [ Atom (Value.Word32.to_string_u w) ]
  | Word64Lit w   -> "Word64Lit" $$ [ Atom (Value.Word64.to_string_u w) ]
  | FloatLit f    -> "FloatLit"  $$ [ Atom (Value.Float.to_string f) ]
  | CharLit c     -> "CharLit"   $$ [ Atom (string_of_int c) ]
  | TextLit t     -> "TextLit"   $$ [ Atom t ]
  | PreLit (s,p)  -> "PreLit"    $$ [ Atom s; prim p]

and unop uo = match uo with
  | PosOp -> Atom "PosOp"
  | NegOp -> Atom "NegOp"
  | NotOp -> Atom "NotOp"

and binop bo = match bo with
  | AddOp  -> Atom "AddOp"
  | SubOp  -> Atom "SubOp"
  | MulOp  -> Atom "MulOp"
  | DivOp  -> Atom "DivOp"
  | ModOp  -> Atom "ModOp"
  | AndOp  -> Atom "AndOp"
  | OrOp   -> Atom "OrOp"
  | XorOp  -> Atom "XorOp"
  | ShLOp  -> Atom "ShiftLOp"
  | ShROp  -> Atom "ShiftROp"
  | RotLOp -> Atom "RotLOp"
  | RotROp -> Atom "RotROp"
  | CatOp  -> Atom "CatOp"
  | PowOp  -> Atom "PowOp"

and relop ro = match ro with
  | EqOp  -> Atom "EqOp"
  | NeqOp -> Atom "NeqOp"
  | LtOp  -> Atom "LtOp"
  | GtOp  -> Atom "GtOp"
  | LeOp  -> Atom "LeOp"
  | GeOp  -> Atom "GeOp"

and case c = "case" $$ [pat c.it.pat; exp c.it.exp]

and prim p = Atom (Type.string_of_prim p)

and sharing sh = match sh with
  | Type.Local -> "Local"
  | Type.Sharable -> "Sharable"

and control c = match c with
  | Type.Returns -> "Returns"
  | Type.Promises -> "Promises"

and obj_sort' s = match s with
  | Type.Object sh -> Atom ("Object " ^ sharing sh)
  | Type.Actor -> Atom "Actor"

and obj_sort s = obj_sort' s.it

and mut m = match m.it with
  | Const -> Atom "Const"
  | Var   -> Atom "Var"

and vis v = match v.it with
  | Public  -> Atom "Public"
  | Private -> Atom "Private"

and typ_field (tf : typ_field)
  = tf.it.id.it $$ [typ tf.it.typ; mut tf.it.mut]

and typ_bind (tb : typ_bind)
  = tb.it.var.it $$ [typ tb.it.bound]

and exp_field (ef : exp_field)
  = "Field" $$ [dec ef.it.dec; vis ef.it.vis]

and operator_type t = Atom (Type.string_of_typ t)

and typ t = match t.it with
  | VarT (s, ts)        -> "VarT" $$ [id s] @ List.map typ ts
  | PrimT p             -> "PrimT" $$ [Atom p]
  | ObjT (s, ts)        -> "ObjT" $$ [obj_sort s] @ List.map typ_field ts
  | ArrayT (m, t)       -> "ArrayT" $$ [mut m; typ t]
  | OptT t              -> "OptT" $$ [typ t]
  | TupT ts             -> "TupT" $$ List.map typ ts
  | FuncT (s, tbs, at, rt) -> "FuncT" $$ [Atom (sharing s.it)] @ List.map typ_bind tbs @ [ typ at; typ rt]
  | AsyncT t            -> "AsyncT" $$ [typ t]
  | ParT t              -> "ParT" $$ [typ t]

and id i = Atom i.it
and con_id i = Atom i.it         

and name n = Atom (string_of_name n.it)

and dec d = match d.it with
  | ExpD e ->      "ExpD" $$ [exp e ]
  | LetD (p, e) -> "LetD" $$ [pat p; exp e]
  | VarD (i, e) -> "VarD" $$ [id i; exp e]
  | FuncD (s, i, tp, p, t, e) ->
    "FuncD" $$ [
      Atom (Type.string_of_typ d.note.note_typ);
      Atom (sharing s.it);
      id i] @
      List.map typ_bind tp @ [
      pat p;
      typ t;
      exp e
    ]
  | TypD (i, tp, t) ->
    "TypD" $$ [con_id i] @ List.map typ_bind tp @ [typ t]
  | ClassD (i, j, tp, s, p, i', efs) ->
    "ClassD" $$ id i :: con_id j :: List.map typ_bind tp @ [obj_sort s; pat p; id i'] @ List.map exp_field efs

and prog prog = "BlockE"  $$ List.map dec prog.it
