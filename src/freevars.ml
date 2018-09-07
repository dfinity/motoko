open Source
open Syntax

module S = Set.Make(String)


type t = S.t
let (++) x y = S.union x y
let unions f xs = List.fold_left S.union S.empty (List.map f xs)

let (//) x y = S.remove y x

let (///) x (f,b) = f ++ S.diff x b
let (+++) (f,b) x = (S.union f x, b)
let (++++) (f1, b1) (f2,b2) = (S.union f1 f2, S.union b1 b2)
let union_binders f xs = List.fold_left (++++) (S.empty, S.empty) (List.map f xs)
let close (f,b) = S.diff f b

let rec exp e : t = match e.it with
  | VarE i              -> S.singleton i.it
  | LitE l              -> S.empty
  | UnE (uo, e)         -> exp e
  | BinE (e1, bo, e2)   -> exps [e1; e2]
  | RelE (e1, ro, e2)   -> exps [e1; e2]
  | TupE es             -> exps es
  | ProjE (e, i)        -> exp e
  | ObjE (s, i, efs)    -> close (union_binders exp_field efs) // i.it
  | DotE (e, i)         -> exp e
  | AssignE (e1, e2)    -> exps [e1; e2]
  | ArrayE es           -> exps es
  | IdxE (e1, e2)       -> exps [e1; e2]
  | CallE (e1, ts, e2)  -> exps [e1; e2]
  | BlockE ds           -> close (decs ds)
  | NotE e              -> exp e
  | AndE (e1, e2)       -> exps [e1; e2]
  | OrE (e1, e2)        -> exps [e1; e2]
  | IfE (e1, e2, e3)    -> exps [e1; e2; e3]
  | SwitchE (e, cs)     -> exp e ++ cases cs
  | WhileE (e1, e2)     -> exps [e1; e2]
  | LoopE (e1, None)    -> exp e1
  | LoopE (e1, Some e2) -> exps [e1; e2]
  | ForE (p, e1, e2)    -> exp e1 ++ (exp e2 /// pat p)
  | LabelE (i, t, e)    -> exp e
  | BreakE (i, e)       -> exp e
  | RetE e              -> exp e
  | AsyncE e            -> exp e
  | AwaitE e            -> exp e
  | AssertE e           -> exp e
  | IsE (e, t)          -> exp e
  | AnnotE (e, t)       -> exp e
  | DecE d              -> close (dec d)
  | OptE e              -> exp e

and exps es : t = unions exp es

and pat p : (t * t) = match p.it with
  | WildP         -> (S.empty, S.empty)
  | VarP i        -> (S.empty, S.singleton i.it)
  | TupP ps       -> pats ps
  | AnnotP (p, t) -> pat p
  | LitP l        -> (S.empty, S.empty)
  | SignP (uo, l) -> (S.empty, S.empty)
  | OptP p        -> pat p
  | AltP (p1, p2) -> pat p1 ++++ pat p2

and pats ps : (t * t) = union_binders pat ps

and case (c : case) = exp c.it.exp /// pat c.it.pat

and cases cs : t = unions case cs

and exp_field (ef : exp_field) : t * t
  = (exp ef.it.exp, S.singleton ef.it.id.it)

and exp_fields efs : (t * t) = union_binders exp_field efs

and id i = S.singleton i.it

and dec d = match d.it with
  | ExpD e -> (exp e, S.empty)
  | LetD (p, e) -> pat p +++ exp e
  | VarD (i, e) -> (S.empty, S.singleton i.it) +++ exp e
  | FuncD (i, tp, p, t, e) ->
    (S.empty, S.singleton i.it) +++ (exp e /// pat p)
  | TypD (i, tp, t) -> (S.empty, S.empty)
  | ClassD (i, tp, s, p, efs) ->
    (S.empty, S.singleton i.it) +++ (close (exp_fields efs) /// pat p)

(* The variables captured by a function. May include the function itself! *)
and captured p e = S.elements (exp e /// pat p)

and decs ps : (t * t) = union_binders dec ps
