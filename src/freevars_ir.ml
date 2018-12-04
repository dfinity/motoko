open Source
open Ir

module S = Set.Make(String)


(* A set of free variables *)
type f = S.t

(* Operations: Union and removal *)
let (++) x y = S.union x y
let unions f xs = List.fold_left S.union S.empty (List.map f xs)
let (//) x y = S.remove y x

(* A combined set of free variables and defined variables,
   e.g. in patterns and declaration *)
type fb = S.t * S.t

(* Operations: *)

(* This adds a set of free variables to a combined set *)
let (+++) ((f,b) : fb)  x = (S.union f x, b)
(* This takes the union of two combined sets *)
let (++++) (f1, b1) (f2,b2) = (S.union f1 f2, S.union b1 b2)
let union_binders f xs = List.fold_left (++++) (S.empty, S.empty) (List.map f xs)

(* The bound variables from the second argument scope over the first *)
let (///) (x : f) ((f,b) : fb) = f ++ S.diff x b

(* This closes a combined set over itself (recursion or mutual recursion) *)
let close (f,b) = S.diff f b

(* One traversal for each syntactic category, named by that category *)

let rec exp e : f = match e.it with
  | VarE i              -> S.singleton i.it
  | LitE l              -> S.empty
  | PrimE _             -> S.empty
  | UnE (uo, e)         -> exp e
  | BinE (e1, bo, e2)   -> exps [e1; e2]
  | RelE (e1, ro, e2)   -> exps [e1; e2]
  | TupE es             -> exps es
  | ProjE (e, i)        -> exp e
  | ObjE (s, i, efs)    -> close (exp_fields efs) // i.it
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
  | DecE d              -> close (dec d)
  | OptE e              -> exp e
  | DeclareE (i, t, e)  -> exp e  // i.it
  | DefineE (i, m, e)   -> (id i) ++ exp e
  | NewObjE (_,ids)    -> unions id (List.map (fun (lab,id) -> id) ids)                                        

and exps es : f = unions exp es

and pat p : fb = match p.it with
  | WildP         -> (S.empty, S.empty)
  | VarP i        -> (S.empty, S.singleton i.it)
  | TupP ps       -> pats ps
  | LitP l        -> (S.empty, S.empty)
  | SignP (uo, l) -> (S.empty, S.empty)
  | OptP p        -> pat p
  | AltP (p1, p2) -> pat p1 ++++ pat p2

and pats ps : fb = union_binders pat ps

and case (c : case) = exp c.it.exp /// pat c.it.pat

and cases cs : f = unions case cs

and exp_field (ef : exp_field) : fb
  = (exp ef.it.exp, S.singleton ef.it.id.it)

and exp_fields efs : fb = union_binders exp_field efs

and id i = S.singleton i.it

and dec d = match d.it with
  | ExpD e -> (exp e, S.empty)
  | LetD (p, e) -> pat p +++ exp e
  | VarD (i, e) -> (S.empty, S.singleton i.it) +++ exp e
  | FuncD (s, i, tp, p, t, e) ->
    (S.empty, S.singleton i.it) +++ (exp e /// pat p)
  | TypD (i, tp, t) -> (S.empty, S.empty)
  | ClassD (i, l, tp, s, p, i', efs) ->
    (S.empty, S.singleton i.it) +++ (close (exp_fields efs) /// pat p // i'.it)

(* The variables captured by a function. May include the function itself! *)
and captured p e = S.elements (exp e /// pat p)

(* The variables captured by a class function. May include the function itself! *)
and captured_exp_fields p efs = S.elements (close (exp_fields efs) /// pat p)


and decs ps : fb = union_binders dec ps
