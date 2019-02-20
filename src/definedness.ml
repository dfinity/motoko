(*
This module implements a check that rules out use-before define.
It is a compositional algorithm that returns, for each subexpression,
 * Which variables are used eagerly and
 * which are delayd
these sets are disjoint and their union makes up the set of free variables, so
the structure of this module is very similar to that of freevars.ml.

We keep it still separate, because we are doing much more interesting things
here in for blocks (function decs).
*)

open Source
open Syntax

(* We collect a few things along the way *)
type usage_info = Eager | Delayed

let join u1 u2 = match u1, u2 with
  | Eager, _ -> Eager
  | _, Eager -> Eager
  | Delayed, Delayed -> Delayed


module M = Env.Make(String)
module S = Set.Make(String)

let map_of_set x s = S.fold (fun v m -> M.add v x m) s M.empty
let set_unions = List.fold_left S.union S.empty

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

(* Usage tracking. We distinguish between eager and delayed variable use.
   Eager variables become delayed
   - inside lambda
   Delayed variables may stay delayed
   - when storing variables in data structures (tuples, objects, arrays)
   Delayed variables become eager
   - when occurs in an application
   - when a block uses some of its own variables eagerly
*)
let delayify : f -> f = M.map (fun _ -> Delayed)
let eagerify : f -> f = M.map (fun _ -> Eager)

let eager_vars : f -> S.t =
  fun f -> S.of_list (M.keys (M.filter (fun _ u -> u = Eager) f))
let delayed_vars : f -> S.t =
  fun f -> S.of_list (M.keys (M.filter (fun _ u -> u = Delayed) f))

(* This closes a combined set over itself (recursion or mutual recursion) *)
let close (f,d) =
  if S.is_empty (S.inter d (eager_vars f))
  then diff f d
  else eagerify (diff f d)

(* One traversal for each syntactic category, named by that category *)

let rec exp msgs e : f = match e.it with
  | VarE i              -> M.singleton i.it Eager
  | LitE l              -> M.empty
  | PrimE _             -> M.empty
  | UnE (_, uo, e)      -> exp msgs e
  | BinE (_, e1, bo, e2)-> exps msgs [e1; e2]
  | RelE (_, e1, ro, e2)-> exps msgs [e1; e2]
  | TupE es             -> exps msgs es
  | ProjE (e, i)        -> exp msgs e
  | ObjE (s, i, efs)    ->
    let f = close (exp_fields msgs efs) // i.it in
    begin match s.it with
    | Type.Actor -> eagerify f
    | Type.Object _ -> f
    end
  | DotE (e, _t, i)     -> exp msgs e
  | AssignE (e1, e2)    -> exps msgs [e1; e2]
  | ArrayE (m, es)      -> exps msgs es
  | IdxE (e1, e2)       -> exps msgs [e1; e2]
  | CallE (e1, ts, e2)  -> eagerify (exps msgs [e1; e2])
  | BlockE (ds, _)      -> decs msgs ds
  | NotE e              -> exp msgs e
  | AndE (e1, e2)       -> exps msgs [e1; e2]
  | OrE (e1, e2)        -> exps msgs [e1; e2]
  | IfE (e1, e2, e3)    -> exps msgs [e1; e2; e3]
  | SwitchE (e, cs)     -> exp msgs e ++ cases msgs cs
  | WhileE (e1, e2)     -> exps msgs [e1; e2]
  | LoopE (e1, None)    -> exp msgs e1
  | LoopE (e1, Some e2) -> exps msgs [e1; e2]
  | ForE (p, e1, e2)    -> exp msgs e1 ++ (exp msgs e2 /// pat msgs p)
  | LabelE (i, t, e)    -> exp msgs e
  | BreakE (i, e)       -> exp msgs e
  | RetE e              -> exp msgs e
  | AsyncE e            -> exp msgs e
  | AwaitE e            -> exp msgs e
  | AssertE e           -> exp msgs e
  | AnnotE (e, t)       -> exp msgs e
  | DecE (d, t)         -> close (dec msgs d)
  | OptE e              -> exp msgs e

and exps msgs es : f = unions (exp msgs) es

and pat msgs p : fd = match p.it with
  | WildP         -> (M.empty, S.empty)
  | VarP i        -> (M.empty, S.singleton i.it)
  | TupP ps       -> pats msgs ps
  | AnnotP (p, _)
  | ParP p        -> pat msgs p
  | LitP l        -> (M.empty, S.empty)
  | SignP (uo, l) -> (M.empty, S.empty)
  | OptP p        -> pat msgs p
  | AltP (p1, p2) -> pat msgs p1 ++++ pat msgs p2

and pats msgs ps : fd = union_binders (pat msgs) ps

and case msgs (c : case) = exp msgs c.it.exp /// pat msgs c.it.pat

and cases msgs cs : f = unions (case msgs) cs

and exp_field msgs (ef : exp_field) : fd
  = (exp msgs ef.it.exp, S.singleton ef.it.id.it)

and exp_fields msgs efs : fd = union_binders (exp_field msgs) efs

and dec msgs d = match d.it with
  | ExpD e -> (exp msgs e, S.empty)
  | LetD (p, e) -> pat msgs p +++ exp msgs e
  | VarD (i, e) ->
    (M.empty, S.singleton i.it) +++ exp msgs e
  | FuncD (s, i, tp, p, t, e) ->
    (M.empty, S.singleton i.it) +++ delayify (exp msgs e /// pat msgs p)
  | TypD (i, tp, t) -> (M.empty, S.empty)
  | ClassD (i, l, tp, s, p, i', efs) ->
    (M.empty, S.singleton i.it) +++ delayify (close (exp_fields msgs efs) /// pat msgs p // i'.it)

and decs msgs decs : f =
  (* Annotate the declarations with the analysis results *)
  let decs' = List.map (fun d ->
    let (f, defs) = dec msgs d in
    (d.at, defs, eager_vars f, delayed_vars f)
  ) decs in
  (* Create a map from declared variable to their definition point *)
  let defWhen = M.disjoint_unions (List.mapi (fun i (_, defs, _, _) -> map_of_set i defs) decs') in
  (* Calculate the relation R *)
  let r = NameRel.unions (List.map (fun (_, defs, _, delayed) -> NameRel.cross defs delayed) decs') in
  (* Check for errors *)
  List.iteri (fun i (at, _, eager, _) ->
    NameRel.iter (fun x y ->
      match M.find_opt y defWhen with
      | Some j ->
        (* At position i, we are evaluating something that requires y, which is
           defined after j *)
        if j < i
        then () (* all izz well *)
        else Diag.(add_msg msgs
          { sev = Error; at; cat = "definedness";
            text = Printf.sprintf "cannot use %s before %s has been defined" x y
          })
      | None ->
        (* External variable, ok for now *)
        ()
    ) (NameRel.restricted_rtcl eager r)
  ) decs';
  (* Now calculate the analysis result: Eager is everything that is eager, or
     used by eager things *)
  let e = set_unions (List.map (fun (_,_,eager,_) ->
    NameRel.range (NameRel.restricted_rtcl eager r)
  ) decs') in
  (* Every thing else is lazy *)
  let d = S.diff (set_unions (List.map (fun (_,_,_,delayed) -> delayed) decs')) e in
  (* And remove whats defined here  *)
  M.disjoint_union (map_of_set Eager e) (map_of_set Delayed d) |>
    M.filter (fun v _ -> M.mem v defWhen = false)

let check_prog msgs prog = ignore (decs msgs prog.it)
