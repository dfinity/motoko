open Source
open Arrange_type (* currently not used *)
open Ir
open Wasm.Sexpr

(* for concision, we shadow the imported definition of [Arrange_type.typ] and pretty print types instead *)

let typ t = Atom (Type.string_of_typ t)
let kind k = Atom (Type.string_of_kind k)

let rec exp e = match e.it with
  | VarE i              -> "VarE"    $$ [id i]
  | LitE l              -> "LitE"    $$ [Arrange.lit l]
  | UnE (t, uo, e)      -> "UnE"     $$ [typ t; Arrange.unop uo; exp e]
  | BinE (t, e1, bo, e2)-> "BinE"    $$ [typ t; exp e1; Arrange.binop bo; exp e2]
  | RelE (t, e1, ro, e2)-> "RelE"    $$ [typ t; exp e1; Arrange.relop ro; exp e2]
  | TupE es             -> "TupE"    $$ List.map exp es
  | ProjE (e, i)        -> "ProjE"   $$ [exp e; Atom (string_of_int i)]
  | ActorE (i, efs, t)  -> "ActorE"  $$ [id i] @ List.map exp_field efs @ [typ t]
  | DotE (e, n)         -> "DotE"    $$ [exp e; Atom (name n)]
  | ActorDotE (e, n)    -> "ActorDotE" $$ [exp e; Atom (name n)]
  | AssignE (e1, e2)    -> "AssignE" $$ [exp e1; exp e2]
  | ArrayE (m, t, es)   -> "ArrayE"  $$ [Arrange.mut m; typ t] @ List.map exp es
  | IdxE (e1, e2)       -> "IdxE"    $$ [exp e1; exp e2]
  | CallE (cc, e1, ts, e2) -> "CallE" $$ [call_conv cc; exp e1] @ List.map typ ts @ [exp e2]
  | BlockE (ds, e1)     -> "BlockE"  $$ List.map dec ds @ [exp e1]
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
  | OptE e              -> "OptE"    $$ [exp e]
  | PrimE p             -> "PrimE"   $$ [Atom p]
  | DeclareE (i, t, e1) -> "DeclareE" $$ [id i; exp e1]
  | DefineE (i, m, e1)  -> "DefineE" $$ [id i; Arrange.mut m; exp e1]
  | FuncE (x, cc, tp, p, t, e) ->
    "FuncE" $$ [Atom x; call_conv cc] @ List.map typ_bind tp @ [pat p; typ t; exp e]
  | NewObjE (s, nameids, t)-> "NewObjE" $$ (Arrange.obj_sort' s ::
                                              List.fold_left (fun flds (n,i) ->
                                                  Atom (name n)::(id i):: flds) [typ t] nameids)

and pat p = match p.it with
  | WildP         -> Atom "WildP"
  | VarP i        -> "VarP"       $$ [ id i]
  | TupP ps       -> "TupP"       $$ List.map pat ps
  | LitP l        -> "LitP"       $$ [ Arrange.lit l ]
  | OptP p        -> "OptP"       $$ [ pat p ]
  | AltP (p1,p2)  -> "AltP"       $$ [ pat p1; pat p2 ]

and case c = "case" $$ [pat c.it.pat; exp c.it.exp]

and exp_field (ef : exp_field)
  = name ef.it.name $$ [id ef.it.id; exp ef.it.exp; Arrange.mut ef.it.mut; Arrange.vis ef.it.vis]

and name n = match n.it with
  | Name l -> l

and call_conv cc = Atom (Value.string_of_call_conv cc)

and dec d = match d.it with
  | ExpD e ->      "ExpD" $$ [exp e ]
  | LetD (p, e) -> "LetD" $$ [pat p; exp e]
  | VarD (i, e) -> "VarD" $$ [id i; exp e]
  | TypD c ->
    "TypD" $$ [con c; kind (Con.kind c)]

and typ_bind (tb : typ_bind) =
  Con.to_string tb.it.con $$ [typ tb.it.bound]


and prog ((ds, e), _flavor)= "BlockE"  $$ List.map dec ds @ [ exp e ]
