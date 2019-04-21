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
  | ShowE (t, e)        -> "ShowE"   $$ [typ t; exp e]
  | TupE es             -> "TupE"    $$ List.map exp es
  | ProjE (e, i)        -> "ProjE"   $$ [exp e; Atom (string_of_int i)]
  | DotE (e, n)         -> "DotE"    $$ [exp e; Atom (name n)]
  | ActorDotE (e, n)    -> "ActorDotE" $$ [exp e; Atom (name n)]
  | AssignE (e1, e2)    -> "AssignE" $$ [exp e1; exp e2]
  | ArrayE (m, t, es)   -> "ArrayE"  $$ [Arrange.mut m; typ t] @ List.map exp es
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
  | VariantE (i, e)     -> "VariantE" $$ [id i; exp e]
  | PrimE p             -> "PrimE"   $$ [Atom p]
  | DeclareE (i, t, e1) -> "DeclareE" $$ [id i; exp e1]
  | DefineE (i, m, e1)  -> "DefineE" $$ [id i; Arrange.mut m; exp e1]
  | FuncE (x, cc, tp, as_, ts, e) ->
    "FuncE" $$ [Atom x; call_conv cc] @ List.map typ_bind tp @ args as_@ [ typ (Type.seq ts); exp e]
  | ActorE (i, ds, fs, t) -> "ActorE"  $$ [id i] @ List.map dec ds @ fields fs @ [typ t]
  | NewObjE (s, fs, t)  -> "NewObjE" $$ (Arrange.obj_sort' s :: fields fs @ [typ t])

and fields fs = List.fold_left (fun flds f -> (name f.it.name $$ [ id f.it.var ]):: flds) [] fs

and args = function
 | [] -> []
 | as_ -> ["params" $$ List.map arg as_]

and arg a = Atom a.it

and pat p = match p.it with
  | WildP           -> Atom "WildP"
  | VarP i          -> "VarP"       $$ [ id i ]
  | TupP ps         -> "TupP"       $$ List.map pat ps
  | LitP l          -> "LitP"       $$ [ Arrange.lit l ]
  | OptP p          -> "OptP"       $$ [ pat p ]
  | VariantP (i, p) -> "VariantP"   $$ [ id i; pat p ]
  | AltP (p1,p2)    -> "AltP"       $$ [ pat p1; pat p2 ]

and case c = "case" $$ [pat c.it.pat; exp c.it.exp]

and name n = match n.it with
  | Name l -> l

and call_conv cc = Atom (Value.string_of_call_conv cc)

and dec d = match d.it with
  | LetD (p, e) -> "LetD" $$ [pat p; exp e]
  | VarD (i, e) -> "VarD" $$ [id i; exp e]
  | TypD c -> "TypD" $$ [con c; kind (Con.kind c)]

and typ_bind (tb : typ_bind) =
  Con.to_string tb.it.con $$ [typ tb.it.bound]


and prog ((ds, e), _flavor)= "BlockE"  $$ List.map dec ds @ [ exp e ]
