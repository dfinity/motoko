open Mo_types
open Ir_def
open Source
open Ir

(*
  This module identifies subexpressions that can be compiled statically. This
  means each subexpression must be a constant, immutable value for which the backend can create
  the memory representation statically.

  This module should stay in sync with the `compile_const_exp` function in
  `codegen/compile.ml`.

  If we didn't have recursion, this would be simple: We'd pass down an environment
  mapping all free variables to whether they are constant or not (bool E.t), use this
  in the VarE case, and otherwise const-able expressions are constant when their
  subexpressions are.

  As always, recursion makes things hard. But not too much: We pass down a custom type
  called `lazy_bool`. It denotes a boolean value, just we do not know which one yet.
  But we can still do operations like implications and conjunction on these values.

  Internally, these lazy_bool values keep track of their dependencies, and
  propagate more knowledge automatically. When one of them knows it is going to
  be surely false, then it updates the corresponding `note.const` field.

  This works even in the presence of recursion, because it is monotonic: We start
  with true (possibly constant) and only use implications to connect these values, so
  all propagations of “false” eventually terminate.

  This analysis relies on the fact that AST notes are mutable. So sharing AST
  nodes would be bad.  Check_ir checks for the absence of sharing.
*)

(* The lazy bool value type *)

type lazy_bool' =
  | SurelyTrue
  | SurelyFalse
  | MaybeFalse of (unit -> unit) (* who to notify when turning false *)
type lazy_bool = lazy_bool' ref

let set_false (l : lazy_bool) =
  match !l with
  | SurelyTrue -> assert false
  | SurelyFalse -> ()
  | MaybeFalse when_false ->
    l := SurelyFalse; (* do this first, this breaks cycles *)
    when_false ()

let when_false (l : lazy_bool) (act : unit -> unit) =
  match !l with
  | SurelyTrue -> ()
  | SurelyFalse -> act ()
  | MaybeFalse when_false ->
    l := MaybeFalse (fun () -> act (); when_false ())

let surely_true = ref SurelyTrue (* sharing is ok *)
let surely_false = ref SurelyFalse (* sharing is ok *)
let maybe_false () = ref (MaybeFalse (fun () -> ()))

let required_for (a : lazy_bool) (b : lazy_bool) =
  when_false a (fun () -> set_false b)

let all (xs : lazy_bool list) : lazy_bool =
  if xs = [] then surely_true else
  let b = maybe_false () in
  List.iter (fun a -> required_for a b) xs;
  b

(* The environment *)

type lvl = TopLvl | NotTopLvl

module S = Freevars.S

module M = Env.Make(String)

type info = {
  loc_known : bool;
  const : lazy_bool;
}
type env = info M.t


let no_info = { loc_known = false; const = surely_false }
let arg env a = M.add a.it no_info env
let args env as_ = List.fold_left arg env as_

let rec pat env p = match p.it with
  | WildP
  | LitP _ -> env
  | VarP id -> M.add id no_info env
  | TupP pats -> List.fold_left pat env pats
  | ObjP pfs -> List.fold_left pat env (pats_of_obj_pat pfs)
  | AltP (pat1, _) | OptP pat1 | TagP (_, pat1) -> pat env pat1

let find v env = match M.find_opt v env with
  | None -> raise (Invalid_argument (Printf.sprintf "Unbound var: %s\n" v))
  | Some lb -> lb


(* Setting the notes *)

let set_const e b =
  if e.note.Note.const != b
  then e.note <- Note.{ e.note with const = b }

let set_lazy_const e lb =
  set_const e true;
  when_false lb (fun () -> set_const e false)

(* Traversals *)

let rec exp lvl (env : env) e : lazy_bool =
  let lb =
    match e.it with
    | VarE v -> (find v env).const
    | FuncE (x, s, c, tp, as_ , ts, body) ->
      exp_ NotTopLvl (args env as_) body;
      begin match lvl with
      | TopLvl -> surely_true (* top-level functions can always be const *)
      | NotTopLvl ->
        let lb = maybe_false () in
        Freevars.M.iter (fun v _ ->
          let info = find v env in
          if info.loc_known then () else (* static definitions are ok *)
          required_for info.const lb
        ) (Freevars.exp e);
        lb
      end
    | NewObjE (Type.(Object | Module), fs, t) when Type.is_immutable_obj t ->
      let lb = maybe_false () in
      List.iter (fun f -> required_for (find f.it.var env).const lb) fs;
      lb
    | BlockE (ds, body) ->
      block lvl env (ds, body)
    | PrimE (DotPrim n, [e1]) ->
      exp lvl env e1

    (* All the following expressions cannot be const, but we still need to descend *)
    | PrimE (_, es) ->
      List.iter (exp_ lvl env) es;
      surely_false
    | LitE _ ->
      surely_false
    | DeclareE (id, _, e1) ->
      exp_ lvl (M.add id no_info env) e1;
      surely_false
    | LoopE e1 | AsyncE (_, e1, _) ->
      exp_ NotTopLvl env e1;
      surely_false
    | AssignE (_, e1) | LabelE (_, _, e1) | DefineE (_, _, e1) ->
      exp_ lvl env e1;
      surely_false
    | IfE (e1, e2, e3) ->
      exp_ lvl env e1;
      exp_ lvl env e2;
      exp_ lvl env e3;
      surely_false
    | SelfCallE (_, e1, e2, e3) ->
      exp_ NotTopLvl env e1;
      exp_ lvl env e2;
      exp_ lvl env e3;
      surely_false
    | SwitchE (e1, cs) | TryE (e1, cs) ->
      exp_ lvl env e1;
      List.iter (case_ lvl env) cs;
      surely_false
    | NewObjE _ -> (* mutable objects *)
      surely_false
    | ActorE (ds, fs, {pre; post}, _typ) ->
      (* this may well be “the” top-level actor, so don’t update lvl here *)
      let (env', _) = decs lvl env ds in
      exp_ lvl env' pre;
      exp_ lvl env' post;
      surely_false
  in
  set_lazy_const e lb;
  lb

and exp_ lvl env e : unit = ignore (exp lvl env e)
and case_ lvl env c : unit =
  exp_ lvl (pat env c.it.pat) c.it.exp

and gather_dec lvl scope dec : env =
  let mk_info const = { loc_known = lvl = TopLvl; const } in
  match dec.it with
  | LetD ({it = VarP v; _}, e) ->
    M.add v (mk_info (maybe_false ())) scope
  | _ ->
    let vs = snd (Freevars.dec dec) in (* TODO: implement gather_dec more directly *)
    S.fold (fun v scope -> M.add v (mk_info surely_false) scope) vs scope

and gather_decs lvl ds : env =
  List.fold_left (gather_dec lvl) M.empty ds

and check_dec lvl env dec : lazy_bool = match dec.it with
  | LetD ({it = VarP v; _}, e) ->
    let lb = exp lvl env e in
    required_for lb (M.find v env).const;
    lb
  | LetD (_, e) | VarD (_, _, e) ->
    exp_ lvl env e;
    surely_false

and check_decs lvl env ds : lazy_bool =
  let lbs = List.map (check_dec lvl env) ds in
  all lbs

and decs lvl env ds : (env * lazy_bool) =
  let scope = gather_decs lvl ds in
  let env' = M.adjoin env scope in
  let could_be = check_decs lvl env' ds in
  (env', could_be)

and block lvl env (ds, body) =
  let (env', decs_const) = decs lvl env ds in
  let exp_const = exp lvl env' body in
  all [decs_const; exp_const]

let analyze scope ((b, _flavor) : prog) =
  (*
  We assume everything in scope is static. Right now, this is only the prelude,
  which is static. It will blow up in compile if we get this wrong.
  *)
  let static_info = { loc_known = true; const = surely_true } in
  let env = M.of_seq (Seq.map (fun (v, _typ) -> (v, static_info)) (Type.Env.to_seq scope.Scope.val_env)) in
  ignore (block TopLvl env b)
