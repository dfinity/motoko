(*
This module implements a check that rules out use-before define.
It is a compositional algorithm that returns, for each subexpression,
 * Which variables are used eagerly and
 * which are delayed
these sets are disjoint and their union makes up the set of free variables, so
the structure of this module is very similar to that of freevars.ml.

We keep it still separate, because we are doing much more interesting things
here in for blocks (function decs).
*)

open Mo_def
open Mo_types
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

(* The analsys result of a recursive group, before tying the knot *)
type group = (Source.region * S.t * S.t * S.t) list

(* Operations: Union and removal *)
let (++) : f -> f -> f = M.union (fun _ u1 u2 -> Some (join u1 u2))
let unions f xs = List.fold_left (++) M.empty (List.map f xs)

(* A combined set of free variables and defined variables,
   e.g. in patterns and declaration *)
type defs = S.t
type fd = f * defs


(* Operations: *)

(* This adds a set of free variables to a combined set *)
let (+++) ((f, d) : fd) x = ((++) f x, d)
(* This takes the union of two combined sets *)
let (++++) (f1, d1) (f2, d2) = ((++) f1 f2, S.union d1 d2)
let union_binders f xs = List.fold_left (++++) (M.empty, S.empty) (List.map f xs)

let diff f d = M.filter (fun k _ -> not (S.mem k d)) f

(* The bound variables from the second argument scope over the first *)
let (///) (x : f) ((f, d) : fd) = f ++ diff x d

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

(* One traversal for each syntactic category, named by that category *)

let rec exp msgs e : f = match e.it with
  (* Eager uses are either first-class uses of a variable: *)
  | VarE i              -> M.singleton i.it Eager
  (* Or anything that is occurring in a call (as this may call a closure): *)
  | CallE (e1, ts, e2)  -> eagerify (exps msgs [e1; e2])
  (* And break, return, throw can be thought of as calling a continuation: *)
  | BreakE (i, e)       -> eagerify (exp msgs e)
  | RetE e              -> eagerify (exp msgs e)
  | ThrowE e            -> eagerify (exp msgs e)
  (* Uses are delayed by function expressions *)
  | FuncE (_, sp, tp, p, t, _, e) ->
      delayify ((exp msgs e /// pat msgs p) /// shared_pat msgs sp)
  (* The rest remaining cases just collect the uses of subexpressions: *)
  | LitE _ | ActorUrlE _
  | PrimE _ | ImportE _ -> M.empty
  | UnE (_, uo, e)      -> exp msgs e
  | BinE (_, e1, bo, e2)-> exps msgs [e1; e2]
  | RelE (_, e1, ro, e2)-> exps msgs [e1; e2]
  | ShowE (_, e)        -> exp msgs e
  | TupE es             -> exps msgs es
  | ProjE (e, i)        -> exp msgs e
  | ObjBlockE (s, dfs)       ->
    (* For actors, this may be too permissive; to be revised when we work on actors again *)
    group msgs (dec_fields msgs dfs)
  | ObjE efs            -> exp_fields msgs efs
  | DotE (e, i)         -> exp msgs e
  | AssignE (e1, e2)    -> exps msgs [e1; e2]
  | ArrayE (m, es)      -> exps msgs es
  | IdxE (e1, e2)       -> exps msgs [e1; e2]
  | BlockE ds           -> group msgs (decs msgs ds)
  | NotE e              -> exp msgs e
  | AndE (e1, e2)       -> exps msgs [e1; e2]
  | OrE (e1, e2)        -> exps msgs [e1; e2]
  | IfE (e1, e2, e3)    -> exps msgs [e1; e2; e3]
  | SwitchE (e, cs)     -> exp msgs e ++ cases msgs cs
  | TryE (e, cs)        -> exp msgs e ++ cases msgs cs
  | WhileE (e1, e2)     -> exps msgs [e1; e2]
  | LoopE (e1, None)    -> exp msgs e1
  | LoopE (e1, Some e2) -> exps msgs [e1; e2]
  | ForE (p, e1, e2)    -> exp msgs e1 ++ (exp msgs e2 /// pat msgs p)
  | LabelE (i, t, e)    -> exp msgs e
  | DebugE e            -> exp msgs e
  | AsyncE (_, e)    -> exp msgs e
  | AwaitE e            -> exp msgs e
  | AssertE e           -> exp msgs e
  | AnnotE (e, t)       -> exp msgs e
  | OptE e              -> exp msgs e
  | DoOptE e            -> exp msgs e
  | BangE e             -> exp msgs e
  | TagE (_, e)         -> exp msgs e
  | IgnoreE e           -> exp msgs e

and exps msgs es : f = unions (exp msgs) es

and exp_fields msgs efs : f = unions (exp_field msgs) efs
and exp_field msgs ef : f = exp msgs ef.it.exp

and pat msgs p : fd = match p.it with
  | WildP         -> (M.empty, S.empty)
  | VarP i        -> (M.empty, S.singleton i.it)
  | TupP ps       -> pats msgs ps
  | ObjP pfs      -> pat_fields msgs pfs
  | AnnotP (p, _)
  | ParP p        -> pat msgs p
  | LitP l        -> (M.empty, S.empty)
  | SignP (uo, l) -> (M.empty, S.empty)
  | OptP p
  | TagP (_, p)   -> pat msgs p
  | AltP (p1, p2) -> pat msgs p1 ++++ pat msgs p2

and pats msgs ps : fd = union_binders (pat msgs) ps

and pat_fields msgs pfs = union_binders (fun (pf : pat_field) -> pat msgs pf.it.pat) pfs

and shared_pat msgs shared_pat =
  match shared_pat.it with
  | Type.Local ->
    (M.empty, S.empty)
  | Type.Shared (_, p1) ->
    pat msgs p1

and case msgs (c : case) = exp msgs c.it.exp /// pat msgs c.it.pat

and cases msgs cs : f = unions (case msgs) cs

and dec_fields msgs dfs =
  decs msgs (List.map (fun df -> df.it.dec) dfs)

and dec msgs d = match d.it with
  | ExpD e -> (exp msgs e, S.empty)
  | LetD (p, e) -> pat msgs p +++ exp msgs e
  | VarD (i, e) -> (M.empty, S.singleton i.it) +++ exp msgs e
  | TypD (i, tp, t) -> (M.empty, S.empty)
  | ClassD (csp, i, tp, p, t, s, i', dfs) ->
    (M.empty, S.singleton i.it) +++ delayify (
      group msgs (dec_fields msgs dfs @ class_self d.at i') /// pat msgs p /// shared_pat msgs csp
    )

(* The class self binding is treated as defined at the very end of the group *)
and class_self at i : group = [(at, S.singleton i.it, S.empty, S.empty)]

and decs msgs decs : group =
  (* Annotate the declarations with the analysis results *)
  List.map (fun d ->
    let (f, defs) = dec msgs d in
    (d.at, defs, eager_vars f, delayed_vars f)
  ) decs

and group msgs (grp : group) : f =
  (* Create a map from declared variable to their definition point *)
  let defWhen = M.disjoint_unions (List.mapi (fun i (_, defs, _, _) -> map_of_set i defs) grp) in
  (* Calculate the relation R *)
  let r = NameRel.unions (List.map (fun (_, defs, _, delayed) -> NameRel.cross defs delayed) grp) in
  (* Check for errors *)
  List.iteri (fun i (at, _, eager, _) ->
    NameRel.iter (fun x y ->
      match M.find_opt y defWhen with
      | Some j ->
        (* At position i, we are evaluating something that requires y, which is
           defined after j *)
        if j < i
        then () (* all izz well *)
        else
          Diag.add_msg
            msgs
            (Diag.error_message
               at
               "M0016"
               "definedness"
               (Printf.sprintf "cannot use %s before %s has been defined" x y))
      | None ->
        (* External variable, ok for now *)
        ()
    ) (NameRel.restricted_rtcl eager r)
  ) grp;
  (* Now calculate the analysis result: Eager is everything that is eager, or
     used by eager things *)
  let e = set_unions (List.map (fun (_,_,eager,_) ->
    NameRel.range (NameRel.restricted_rtcl eager r)
  ) grp) in
  (* Everything else is lazy *)
  let d = S.diff (set_unions (List.map (fun (_,_,_,delayed) -> delayed) grp)) e in
  (* And remove whats defined here  *)
  M.disjoint_union (map_of_set Eager e) (map_of_set Delayed d) |>
    M.filter (fun v _ -> M.mem v defWhen = false)

let check_prog prog =
  Diag.with_message_store (fun msgs ->
    ignore (group msgs (decs msgs prog.it));
    Some ()
  )

let check_lib lib =
  Diag.with_message_store (fun msgs ->
    let (imp_ds, ds) = CompUnit.decs_of_lib lib in
    ignore (group msgs (decs msgs (imp_ds @ ds)));
    Some ()
  )

