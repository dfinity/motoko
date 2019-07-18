open Source
open Ir

(* We collect a few things along the way *)

(* We want to know:
   Is this variable used (potentially) captured?
*)
type usage_info = { captured : bool }

let join u1 u2 = { captured = u1.captured || u2.captured }

module M = Env.Make(String)
module S = Set.Make(String)


(* A set of free variables *)
type f = usage_info M.t

(* Operations: Union and removal *)
let (++) : f -> f -> f = M.union (fun _ u1 u2 -> Some (join u1 u2))
let unions f xs = List.fold_left (++) M.empty (List.map f xs)
let (//) x y = M.remove y x

(* A combined set of free variables and defined variables,
   e.g. in patterns and declaration *)
type defs = S.t
type fd = f * defs

(* Operations: *)

(* This adds a set of free variables to a combined set *)
let (+++) ((f,d) : fd)  x = ((++) f x, d)
(* This takes the union of two combined sets *)
let (++++) (f1, d1) (f2,d2) = ((++) f1 f2, S.union d1 d2)
let union_binders f xs = List.fold_left (++++) (M.empty, S.empty) (List.map f xs)

let diff f d = M.filter (fun k _ -> not (S.mem k d)) f

(* The bound variables from the second argument scope over the first *)
let (///) (x : f) ((f,d) : fd) = f ++ diff x d

(* Usage tracking:

   We track which variables may be captured.
   Initially, variables are not captured.
   All variables under a lambda become captured.
*)
let under_lambda : f -> f = M.map (fun _ -> { captured = true })

let captured_vars : f -> S.t =
  fun f -> S.of_list (List.map fst (List.filter (fun (k,u) -> u.captured) (M.bindings f)))

(* This closes a combined set over itself (recursion or mutual recursion) *)
let close (f,d) = diff f d

(* One traversal for each syntactic category, named by that category *)

let rec exp e : f = match e.it with
  | VarE i              -> M.singleton i {captured = false}
  | LitE l              -> M.empty
  | PrimE (_, es)       -> exps es
  | TupE es             -> exps es
  | ProjE (e, i)        -> exp e
  | DotE (e, i)         -> exp e
  | ActorDotE (e, i)    -> exp e
  | AssignE (e1, e2)    -> exps [e1; e2]
  | ArrayE (m, t, es)   -> exps es
  | IdxE (e1, e2)       -> exps [e1; e2]
  | CallE (_, e1, ts, e2) -> exps [e1; e2]
  | BlockE (ds, e1)     -> close (decs ds +++ exp e1)
  | IfE (e1, e2, e3)    -> exps [e1; e2; e3]
  | SwitchE (e, cs)     -> exp e ++ cases cs
  | LoopE e1            -> exp e1
  | LabelE (i, t, e)    -> exp e
  | BreakE (i, e)       -> exp e
  | RetE e              -> exp e
  | AsyncE e            -> exp e
  | AwaitE e            -> exp e
  | AssertE e           -> exp e
  | OptE e              -> exp e
  | TagE (_, e)         -> exp e
  | DeclareE (i, t, e)  -> exp e  // i
  | DefineE (i, m, e)   -> id i ++ exp e
  | FuncE (x, cc, tp, as_, t, e) -> under_lambda (exp e /// args as_)
  | ActorE (i, ds, fs, _) -> close (decs ds +++ fields fs) // i
  | NewObjE (_, fs, _)  -> fields fs

and fields fs = unions (fun f -> id f.it.var) fs

and exps es : f = unions exp es

and arg a : fd = (M.empty, S.singleton a.it)

and args as_ : fd = union_binders arg as_

and pat p : fd = match p.it with
  | WildP           -> (M.empty, S.empty)
  | VarP i          -> (M.empty, S.singleton i)
  | TupP ps         -> pats ps
  | ObjP pfs        -> pats (pats_of_obj_pat pfs)
  | LitP l          -> (M.empty, S.empty)
  | OptP p          -> pat p
  | TagP (i, p)     -> pat p
  | AltP (p1, p2)   -> pat p1 ++++ pat p2

and pats ps : fd = union_binders pat ps

and case (c : case) = exp c.it.exp /// pat c.it.pat

and cases cs : f = unions case cs

and id i = M.singleton i {captured = false}

and dec d = match d.it with
  | LetD (p, e) -> pat p +++ exp e
  | VarD (i, e) -> (M.empty, S.singleton i) +++ exp e
  | TypD c -> (M.empty, S.empty)

(* The variables captured by a function. May include the function itself! *)
and captured e =
  List.map fst (M.bindings (exp e))

and decs ps : fd = union_binders dec ps
