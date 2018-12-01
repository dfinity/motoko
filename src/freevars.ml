open Source
open Syntax

(* We collect a few things along the way *)

(* We want to know:
   Is this variable used (potentially) eager?
   Is this variable used (potentially) captured?
   We actually need both, hence a product lattice.
*)
type usage_info = { eager: bool; captured : bool }

let join u1 u2 =
  { eager = u1.eager || u2.eager ;
    captured = u1.captured || u2.captured 
  }

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

   We distinguish between eager and delayed variables.
   Initially variables are eager.
   Eager variables become delayed
   - inside lambda
   Delayed variables may stay delayed
   - when storing variables in data structures (tuples, objects, lists)
   Delayed variables become eager again
   - when used in an application
   - when a block uses any of its own variables eagerly (see function `close`)

   Additionally, we track which variables may be captured.
   Initially, variables are not captured.
   All variables under a lambda become captured.
*)
let delayify : f -> f = M.map (fun _ -> { eager = false; captured = true })
let eagerify : f -> f = M.map (fun u -> { eager = true; captured = u.captured })

let eager_vars : f -> S.t =
  fun f -> S.of_list (List.map fst (List.filter (fun (k,u) -> u.eager) (M.bindings f)))
let delayed_vars : f -> S.t =
  fun f -> S.of_list (List.map fst (List.filter (fun (k,u) -> not u.eager) (M.bindings f)))
let captured_vars : f -> S.t =
  fun f -> S.of_list (List.map fst (List.filter (fun (k,u) -> u.captured) (M.bindings f)))

(* This closes a combined set over itself (recursion or mutual recursion) *)
let close (f,d) =
  if S.is_empty (S.inter d (eager_vars f))
  then diff f d
  else eagerify (diff f d)

(* One traversal for each syntactic category, named by that category *)

let rec exp e : f = match e.it with
  | VarE i              -> M.singleton i.it {eager = true; captured = false}
  | LitE l              -> M.empty
  | PrimE _             -> M.empty
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
  | CallE (e1, ts, e2)  -> eagerify (exps [e1; e2])
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
  | DeclareE (i, t, e)  -> exp e  // i.it
  | DefineE (i, m, e)   -> id i ++ exp e
  | NewObjE (_,ids)     -> unions id (List.map (fun (lab,id) -> id) ids)

and exps es : f = unions exp es

and pat p : fd = match p.it with
  | WildP         -> (M.empty, S.empty)
  | VarP i        -> (M.empty, S.singleton i.it)
  | TupP ps       -> pats ps
  | AnnotP (p, t) -> pat p
  | LitP l        -> (M.empty, S.empty)
  | SignP (uo, l) -> (M.empty, S.empty)
  | OptP p        -> pat p
  | AltP (p1, p2) -> pat p1 ++++ pat p2

and pats ps : fd = union_binders pat ps

and case (c : case) = exp c.it.exp /// pat c.it.pat

and cases cs : f = unions case cs

and exp_field (ef : exp_field) : fd
  = (exp ef.it.exp, S.singleton ef.it.id.it)

and exp_fields efs : fd = union_binders exp_field efs

and id i = M.singleton i.it {eager = true; captured = false}

and dec d = match d.it with
  | ExpD e -> (exp e, S.empty)
  | LetD (p, e) -> pat p +++ exp e
  | VarD (i, e) ->
    (M.empty, S.singleton i.it) +++ exp e
  | FuncD (s, i, tp, p, t, e) ->
    (M.empty, S.singleton i.it) +++ delayify (exp e /// pat p)
  | TypD (i, tp, t) -> (M.empty, S.empty)
  | ClassD (i, l, tp, s, p, i', efs) ->
    (M.empty, S.singleton i.it) +++ delayify (close (exp_fields efs) /// pat p // i'.it)

(* The variables captured by a function. May include the function itself! *)
and captured p e =
  List.map fst (M.bindings (exp e /// pat p))

(* The variables captured by a class function. May include the function itself! *)
and captured_exp_fields p efs =
  List.map fst (M.bindings (close (exp_fields efs) /// pat p))

and decs ps : fd = union_binders dec ps
