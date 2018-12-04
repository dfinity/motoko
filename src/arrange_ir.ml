open Source
open Ir
open Wasm.Sexpr

let ($$) head inner = Node (head, inner)

let rec exp e = match e.it with
  | VarE i              -> "VarE"    $$ [id i]
  | LitE l              -> "LitE"    $$ [Arrange.lit l]
  | UnE (uo, e)         -> "UnE"     $$ [Arrange.unop uo; exp e]
  | BinE (e1, bo, e2)   -> "BinE"    $$ [exp e1; Arrange.binop bo; exp e2]
  | RelE (e1, ro, e2)   -> "RelE"    $$ [exp e1; Arrange.relop ro; exp e2]
  | TupE es             -> "TupE"    $$ List.map exp es
  | ProjE (e, i)        -> "ProjE"   $$ [exp e; Atom (string_of_int i)]
  | ObjE (s, i, efs)    -> "ObjE"    $$ [Arrange.obj_sort s; id i] @ List.map exp_field efs
  | DotE (e, n)         -> "DotE"    $$ [exp e; name n]
  | AssignE (e1, e2)    -> "AssignE" $$ [exp e1; exp e2]
  | ArrayE es           -> "ArrayE"  $$ List.map exp es
  | IdxE (e1, e2)       -> "IdxE"    $$ [exp e1; exp e2]
  | CallE (e1, ts, e2)  -> "CallE"   $$ [exp e1] @ List.map Arrange.typ ts @ [exp e2]
  | BlockE ds           -> "BlockE"  $$ List.map dec ds
  | IfE (e1, e2, e3)    -> "IfE"     $$ [exp e1; exp e2; exp e3]
  | SwitchE (e, cs)     -> "SwitchE" $$ [exp e] @ List.map case cs
  | WhileE (e1, e2)     -> "WhileE"  $$ [exp e1; exp e2]
  | LoopE (e1, None)    -> "LoopE"   $$ [exp e1]
  | LoopE (e1, Some e2) -> "LoopE"   $$ [exp e1; exp e2]
  | ForE (p, e1, e2)    -> "ForE"    $$ [pat p; exp e1; exp e2]
  | LabelE (i, t, e)    -> "LabelE"  $$ [id i; Arrange.typ t; exp e]
  | BreakE (i, e)       -> "BreakE"  $$ [id i; exp e]
  | RetE e              -> "RetE"    $$ [exp e]
  | AsyncE e            -> "AsyncE"  $$ [exp e]
  | AwaitE e            -> "AwaitE"  $$ [exp e]
  | AssertE e           -> "AssertE" $$ [exp e]
  | IsE (e1, e2)        -> "IsE"     $$ [exp e1; exp e2]
  | DecE d              -> "DecE"    $$ [dec d]
  | OptE e              -> "OptE"    $$ [exp e]
  | PrimE p             -> "PrimE"   $$ [Atom p]
  | DeclareE (i, t, e1) -> "DeclareE" $$ [id i; exp e1]
  | DefineE (i, m, e1)  -> "DefineE" $$ [id i; Arrange.mut m; exp e1]
  | NewObjE (s, nameids)-> "NewObjE" $$ (Arrange.obj_sort s ::
                                              List.fold_left (fun flds (n,i) ->
                                                  (name n)::(id i):: flds) [] nameids)
and pat p = match p.it with
  | WildP         -> Atom "WildP"
  | VarP i        -> "VarP"       $$ [ id i]
  | TupP ps       -> "TupP"       $$ List.map pat ps
  | LitP l        -> "LitP"       $$ [ Arrange.lit l ]
  | SignP (uo, l) -> "SignP"      $$ [ Arrange.unop uo ; Arrange.lit l ]
  | OptP p        -> "OptP"       $$ [ pat p ]
  | AltP (p1,p2)  -> "AltP"       $$ [ pat p1; pat p2 ]

and case c = "case" $$ [pat c.it.pat; exp c.it.exp]

and prim p = Atom (Type.string_of_prim p)

and exp_field (ef : exp_field)
  = (Syntax.string_of_name ef.it.name.it) $$ [id ef.it.id; exp ef.it.exp; Arrange.mut ef.it.mut; Arrange.priv ef.it.priv]

and id i = Atom i.it

and name n = Atom (Syntax.string_of_name n.it)

and dec d = match d.it with
  | ExpD e ->      "ExpD" $$ [exp e ]
  | LetD (p, e) -> "LetD" $$ [pat p; exp e]
  | VarD (i, e) -> "VarD" $$ [id i; exp e]
  | FuncD (s, i, tp, p, t, e) ->
    "FuncD" $$ [Atom (Arrange.sharing s.it); id i] @ List.map Arrange.typ_bind tp @ [pat p; Arrange.typ t; exp e]
  | TypD (i, tp, t) ->
    "TypD" $$ [id i] @ List.map Arrange.typ_bind tp @ [Arrange.typ t]
  | ClassD (i, j, tp, s, p, i', efs) ->
    "ClassD" $$ id i :: id j :: List.map Arrange.typ_bind tp @ [Arrange.obj_sort s; pat p; id i'] @ List.map exp_field efs

and prog prog = "BlockE"  $$ List.map dec prog.it                                                                       
