open As_types
open As_values

open Source
module T = Type
module E = Ir_effect

(* TODO: make note immutable, perhaps just using type abstraction *)

(* TODO:
   dereferencing is still implicit in the IR (see immut_typ below);
   consider making it explicit as part of desugaring.
*)

(* TODO: enforce second-class nature of T.Mut? in check_typ *)

(* TODO: check escape of free mutables via actors *)

(* Helpers *)

let (==>) p q = not p || q

let typ = E.typ

let immute_typ p =
  assert (not (T.is_mut (typ p)));
  (typ p)

(* Scope *)

type val_env = T.typ T.Env.t
type con_env = T.ConSet.t

type scope =
  { val_env : val_env;
    con_env : con_env;
  }

let empty_scope : scope =
  { val_env = T.Env.empty;
    con_env = T.ConSet.empty
  }

(* Contexts (internal) *)

type lab_env = T.typ T.Env.t
type ret_env = T.typ option

type env =
  { flavor : Ir.flavor;
    vals : val_env;
    cons : con_env;
    labs : lab_env;
    rets : ret_env;
    async : bool;
  }

let env_of_scope scope flavor : env =
  { flavor;
    vals = scope.Scope.val_env;
    cons = scope.Scope.con_env;
    labs = T.Env.empty;
    rets = None;
    async = false;
  }


(* More error bookkeeping *)

exception CheckFailed of string

let type_error at text : Diag.message = Diag.{ sev = Diag.Error; at; cat = "IR type"; text }

let error env at fmt =
    Printf.ksprintf (fun s -> raise (CheckFailed (Diag.string_of_message (type_error at s)))) fmt

let check env at p fmt =
  if p
  then Printf.ikfprintf (fun () -> ()) () fmt
  else error env at fmt



let add_lab c x t = {c with labs = T.Env.add x t c.labs}

let add_typs c cs =
  { c with
    cons = List.fold_right (fun c -> T.ConSet.disjoint_add c) cs c.cons;
  }

let adjoin c scope =
  { c with
    vals = T.Env.adjoin c.vals scope.val_env;
    cons = T.ConSet.(*disjoint_*)union c.cons scope.con_env;
  }

let adjoin_vals c ve = {c with vals = T.Env.adjoin c.vals ve}

let adjoin_cons c ce =
  { c with
    cons = T.ConSet.disjoint_union c.cons ce;
  }

let disjoint_union env at fmt env1 env2 =
  try T.Env.disjoint_union env1 env2
  with T.Env.Clash k -> error env at fmt k

(* Types *)

let check_sub env at t1 t2 =
  check env at (T.sub t1 t2) "subtype violation:\n  %s\n  %s\n"
    (T.string_of_typ_expand t1) (T.string_of_typ_expand t2)

let check_shared env at t =
  check env at (T.shared t)
    "message argument is not sharable:\n  %s" (T.string_of_typ_expand t)

let check_concrete env at t =
  check env at (T.concrete t)
    "message argument is not concrete:\n  %s" (T.string_of_typ_expand t)

let rec check_typ env typ : unit =
  match typ with
  | T.Pre ->
    error env no_region "illegal T.Pre type"
  | T.Var (s, i) ->
    error env no_region "free type variable %s, index %i" s  i
  | T.Con (c, typs) ->
    List.iter (check_typ env) typs;
    begin
      match Con.kind c with
      | T.Def (tbs,_) ->
        if not (T.ConSet.mem c env.cons) then
          (* an anonymous recursive type, check its def but beware recursion
             future: use a visited set *)
          check_con {env with cons = T.ConSet.add c env.cons} c;
        check_typ_bounds env tbs typs no_region
      | T.Abs (tbs, _) ->
        check env no_region (T.ConSet.mem c env.cons) "free type constructor";
        check_typ_bounds env tbs typs no_region
    end
  | T.Any -> ()
  | T.Non -> ()
  | T.Prim _ -> ()
  | T.Array typ ->
    check_typ env typ
  | T.Tup typs ->
    List.iter (check_typ env) typs
  | T.Func (sort, control, binds, ts1, ts2) ->
    let cs, ce = check_typ_binds env binds in
    let env' = adjoin_cons env ce in
    let ts = List.map (fun c -> T.Con (c, [])) cs in
    let ts1 = List.map (T.open_ ts) ts1 in
    let ts2 = List.map (T.open_ ts) ts2 in
    List.iter (check_typ env') ts1;
    List.iter (check_typ env') ts2;
    if control = T.Promises && env.flavor.Ir.has_async_typ then begin
      match ts2 with
      | [T.Async _ ] -> ()
      | _ ->
        let t2 = T.seq ts2 in
        error env no_region "promising function with non-async result type \n  %s"
          (T.string_of_typ_expand t2)
    end;
    if sort = T.Shared then begin
      List.iter (fun t -> check_shared env no_region t) ts1;
      match ts2 with
      | [] -> ()
      | [T.Async t2] when env.flavor.Ir.has_async_typ->
        check env' no_region (T.shared t2)
          "message result is not sharable:\n  %s" (T.string_of_typ_expand t2)
      | ts2 when not env.flavor.Ir.has_async_typ && control <> T.Returns ->
        List.iter (fun t -> check_shared env no_region t) ts2;
      | _ ->
        error env no_region "oneway function with non-empty return type \n  %s"
          (T.string_of_typ typ)
    end
  | T.Opt typ ->
    check_typ env typ
  | T.Async typ ->
    check env no_region env.flavor.Ir.has_async_typ "async in non-async flavor";
    let t' = T.promote typ in
    check_shared env no_region t'
  | T.Obj (sort, fields) ->
    List.iter (check_typ_field env (Some sort)) fields;
    (* fields strictly sorted (and) distinct *)
    if not (Lib.List.is_strictly_ordered T.compare_field fields) then
      error env no_region "object type's fields are not distinct and sorted %s" (T.string_of_typ typ)
  | T.Variant fields ->
    List.iter (check_typ_field env None) fields;
    if not (Lib.List.is_strictly_ordered T.compare_field fields) then
      error env no_region "variant type's fields are not distinct and sorted %s" (T.string_of_typ typ)
  | T.Mut typ ->
    check_typ env typ
  | T.Typ c ->
    check_con env c

and check_con env c =
  let env = {env with cons = T.ConSet.add c env.cons} in
  let T.Abs (binds,typ) | T.Def (binds, typ) = Con.kind c in
  let cs, ce = check_typ_binds env binds in
  let ts = List.map (fun c -> T.Con (c, [])) cs in
  let env' = adjoin_cons env ce in
  check_typ env' (T.open_ ts typ)

and check_typ_field env s typ_field : unit =
  let T.{lab; typ} = typ_field in
  check_typ env typ;
  if not (T.is_typ typ) then begin
    check env no_region
      (s <> Some T.Actor || T.is_func (T.promote typ))
      "actor field has non-function type";
    check env no_region
      (s <> Some T.Actor || T.shared typ)
      "actor field has non-shared type"
  end

and check_typ_binds_acyclic env cs ts  =
  let n = List.length cs in
  let ce = List.fold_right2 T.ConEnv.add cs ts T.ConEnv.empty in
  let chase c =
    let rec chase i ts c' =
      if i > n then
        error env no_region "type parameter %s has cyclic bounds %s"
          (T.string_of_con c)
          (String.concat " <: " (List.map T.string_of_typ ts)) (List.rev ts)
      else
        match T.ConEnv.find_opt c' ce with
        | None -> ()
        | Some t ->
          (match T.normalize t with
           | T.Con (c'', []) as t' ->
             chase (i+1) (t'::ts) c''
           | _ -> ())
    in chase 0 [] c
  in List.iter chase cs

and check_typ_binds env typ_binds : T.con list * con_env =
  let ts = Type.open_binds typ_binds in
  let cs = List.map (function T.Con (c, []) -> c | _ -> assert false) ts in
  let env' = add_typs env cs in
  let bds = List.map (fun typ_bind -> T.open_ ts typ_bind.T.bound) typ_binds in
  check_typ_binds_acyclic env' cs bds;
  List.iter (check_typ env') bds;
  cs, T.ConSet.of_list cs

and check_typ_bounds env (tbs : T.bind list) typs at : unit =
  let pars = List.length tbs in
  let args = List.length typs in
  if pars < args then
    error env at "too many type arguments";
  if pars > args then
    error env at "too few type arguments";
  List.iter2
    (fun tb typ ->
      check env at (T.sub typ (T.open_ typs tb.T.bound))
        "type argument does not match parameter bound")
    tbs typs


and check_inst_bounds env tbs typs at =
  List.iter (check_typ env) typs;
  check_typ_bounds env tbs typs at

(* Literals *)

open Ir

let type_lit env lit at : T.prim =
  match lit with
  | NullLit -> T.Null
  | BoolLit _ -> T.Bool
  | NatLit _ -> T.Nat
  | Nat8Lit _ -> T.Nat8
  | Nat16Lit _ -> T.Nat16
  | Nat32Lit _ -> T.Nat32
  | Nat64Lit _ -> T.Nat64
  | IntLit _ -> T.Int
  | Int8Lit _ -> T.Int8
  | Int16Lit _ -> T.Int16
  | Int32Lit _ -> T.Int32
  | Int64Lit _ -> T.Int64
  | Word8Lit _ -> T.Word8
  | Word16Lit _ -> T.Word16
  | Word32Lit _ -> T.Word32
  | Word64Lit _ -> T.Word64
  | FloatLit _ -> T.Float
  | CharLit _ -> T.Char
  | TextLit _ -> T.Text


(* Expressions *)

let isAsyncE exp =
  match exp.it with
  | AsyncE _ (* pre await transformation *)
  | PrimE(OtherPrim "@async", [_]) (* post await transformation *)
    -> true
  | _ -> false

let rec check_exp env (exp:Ir.exp) : unit =
  (* helpers *)
  let check p = check env exp.at p in
  let (<:) t1 t2 = check_sub env exp.at t1 t2 in
  let (<~) t1 t2 = (if T.is_mut t2 then t1 else T.as_immut t1) <: t2 in
  (* check type annotation *)
  let t = E.typ exp in
  check_typ env t;
  (* check effect annotation *)
  check (E.infer_effect_exp exp <= E.eff exp)
    "inferred effect not a subtype of expected effect";
  (* check typing *)
  match exp.it with
  | VarE id ->
    let t0 = try T.Env.find id env.vals with
             |  Not_found -> error env exp.at "unbound variable %s" id
    in
      t0 <~ t
  | LitE lit ->
    T.Prim (type_lit env lit exp.at) <: t
  | PrimE (p, es) ->
    begin match p, es with
    | UnPrim (ot, op), [exp1] ->
      check (Operator.has_unop op ot) "unary operator is not defined for operand type";
      check_exp env exp1;
      typ exp1 <: ot;
      ot <: t
    | BinPrim (ot, op), [exp1; exp2] ->
      check (Operator.has_binop op ot) "binary operator is not defined for operand type";
      check_exp env exp1;
      check_exp env exp2;
      typ exp1 <: ot;
      typ exp2 <: ot;
      ot <: t
    | RelPrim (ot, op), [exp1; exp2] ->
      check (Operator.has_relop op ot) "relational operator is not defined for operand type";
      check_exp env exp1;
      check_exp env exp2;
      typ exp1 <: ot;
      typ exp2 <: ot;
      T.bool <: t
    | ShowPrim ot, [exp1] ->
      check env.flavor.has_show "show expression in non-show flavor";
      check (Show.can_show ot) "show is not defined for operand type";
      check_exp env exp1;
      typ exp1 <: ot;
      T.Prim T.Text <: t
    | OtherPrim _, _ -> ()
    | _ ->
      error env exp.at "PrimE with wrong number of arguments"
    end
  | TupE exps ->
    List.iter (check_exp env) exps;
    T.Tup (List.map typ exps) <: t
  | OptE exp1 ->
    check_exp env exp1;
    T.Opt (typ exp1) <: t
  | TagE (i, exp1) ->
    check_exp env exp1;
    T.Variant [{T.lab = i; typ = typ exp1}] <: t
  | ProjE (exp1, n) ->
    check_exp env exp1;
    let t1 = T.promote (immute_typ exp1) in
    let ts = try T.as_tup_sub n t1
             with Invalid_argument _ ->
               error env exp1.at "expected tuple type, but expression produces type\n  %s"
                 (T.string_of_typ_expand t1) in
    let tn = try List.nth ts n with
             | Invalid_argument _ ->
               error env exp.at "tuple projection %n is out of bounds for type\n  %s"
                 n (T.string_of_typ_expand t1) in
    tn <: t
  | ActorDotE(exp1, n)
  | DotE (exp1, n) ->
    begin
      check_exp env exp1;
      let t1 = typ exp1 in
      let sort, tfs =
        try T.as_obj_sub [n] t1 with Invalid_argument _ ->
          error env exp1.at "expected object type, but expression produces type\n  %s"
            (T.string_of_typ_expand t1)
      in
      check (match exp.it with
             | ActorDotE _ -> sort = T.Actor
             | DotE _ -> sort <> T.Actor
             | _ -> false) "sort mismatch";
      try T.lookup_val_field n tfs <~ t with Invalid_argument _ ->
        error env exp1.at "field name %s does not exist in type\n  %s"
          n (T.string_of_typ_expand t1)
    end
  | AssignE (exp1, exp2) ->
    check_exp env exp1;
    check_exp env exp2;
    let t2 = try T.as_mut  (typ exp1) with
               Invalid_argument _ -> error env exp.at "expected mutable assignment target"
    in
    typ exp2 <: t2;
    T.unit <: t
  | ArrayE (mut, t0, exps) ->
    List.iter (check_exp env) exps;
    List.iter (fun e -> typ e <: t0) exps;
    let t1 = T.Array (match mut with Const -> t0 | Var -> T.Mut t0) in
    t1 <: t
  | IdxE (exp1, exp2) ->
    check_exp env exp1;
    check_exp env exp2;
    let t1 = T.promote (typ exp1) in
    let t2 = try T.as_array_sub t1 with
             | Invalid_argument _ ->
               error env exp1.at "expected array type, but expression produces type\n  %s"
                                       (T.string_of_typ_expand t1)
    in
    typ exp2 <: T.nat;
    t2 <~ t
  | CallE (call_conv, exp1, insts, exp2) ->
    check_exp env exp1;
    check_exp env exp2;
    (* TODO: check call_conv (assuming there's something to check) *)
    let t1 = T.promote (typ exp1) in
    let _, tbs, t2, t3 =
      try T.as_func_sub call_conv.Call_conv.sort (List.length insts) t1 with
      |  Invalid_argument _ ->
         error env exp1.at "expected function type, but expression produces type\n  %s"
           (T.string_of_typ_expand t1)
    in
    check_inst_bounds env tbs insts exp.at;
    check_exp env exp2;
    let t_arg = T.open_ insts t2 in
    let t_ret = T.open_ insts t3 in
    if (call_conv.Call_conv.sort = T.Shared) then begin
      check_concrete env exp.at t_arg;
      check_concrete env exp.at t_ret;
    end;
    typ exp2 <: t_arg;
    t_ret <: t
  | BlockE (ds, exp1) ->
    let scope = gather_block_decs env ds in
    let env' = adjoin env scope in
    check_decs env' ds;
    check_exp env' exp1;
    typ exp1 <: t
  | IfE (exp1, exp2, exp3) ->
    check_exp env exp1;
    typ exp1 <: T.bool;
    check_exp env exp2;
    typ exp2 <: t;
    check_exp env exp3;
    typ exp3 <: t
  | SwitchE (exp1, cases) ->
    check_exp env exp1;
    let t1 = T.promote (typ exp1) in
(*    if not env.pre then
      if not (Coverage.check_cases env.cons cases t1) then
        warn env exp.at "the cases in this switch do not cover all possible values";
 *)
    check_cases env t1 t cases
  | LoopE exp1 ->
    check_exp env exp1;
    typ exp1 <: T.unit;
    T.Non <: t (* vacuously true *)
  | LabelE (id, t0, exp1) ->
    assert (t0 <> T.Pre);
    check_typ env t0;
    check_exp (add_lab env id t0) exp1;
    typ exp1 <: t0;
    t0 <: t
  | BreakE (id, exp1) ->
    begin
      match T.Env.find_opt id env.labs with
      | None ->
        error env exp.at "unbound label %s" id
      | Some t1 ->
        check_exp env exp1;
        typ exp1 <: t1;
    end;
    T.Non <: t (* vacuously true *)
  | RetE exp1 ->
    begin
      match env.rets with
      | None ->
        error env exp.at "misplaced return"
      | Some t0 ->
        assert (t0 <> T.Pre);
        check_exp env exp1;
        typ exp1 <: t0;
    end;
    T.Non <: t (* vacuously true *)
  | AsyncE exp1 ->
    check env.flavor.has_await "async expression in non-await flavor";
    let t1 = typ exp1 in
    let env' =
      {env with labs = T.Env.empty; rets = Some t1; async = true} in
    check_exp env' exp1;
    t1 <: T.Any;
    T.Async t1 <: t
  | AwaitE exp1 ->
    check env.flavor.has_await "await in non-await flavor";
    check env.async "misplaced await";
    check_exp env exp1;
    let t1 = T.promote (typ exp1) in
    let t2 = try T.as_async_sub t1
             with Invalid_argument _ ->
               error env exp1.at "expected async type, but expression has type\n  %s"
                 (T.string_of_typ_expand t1)
    in
    t2 <: t
  | AssertE exp1 ->
    check_exp env exp1;
    typ exp1 <: T.bool;
    T.unit <: t
  | DeclareE (id, t0, exp1) ->
    check_typ env t0;
    let env' = adjoin_vals env (T.Env.singleton id t0) in
    check_exp env' exp1;
    (typ exp1) <: t
  | DefineE (id, mut, exp1) ->
    check_exp env exp1;
    begin
      match T.Env.find_opt id env.vals with
      | None -> error env exp.at "unbound variable %s" id
      | Some t0 ->
        match mut with
        | Const ->
          typ exp1 <: t0
        | Var ->
          let t0 = try T.as_mut t0 with
                   | Invalid_argument _ ->
                     error env exp.at "expected mutable %s" (T.string_of_typ t0)
          in
          typ exp1 <: t0
    end;
    T.unit <: t
  | FuncE (x, cc, typ_binds, args, ret_tys, exp) ->
    let cs, tbs, ce = check_open_typ_binds env typ_binds in
    let env' = adjoin_cons env ce in
    let ve = check_args env' args in
    List.iter (check_typ env') ret_tys;
    check ((cc.Call_conv.sort = T.Shared && Type.is_async (T.seq ret_tys))
           ==> isAsyncE exp)
      "shared function with async type has non-async body";
    check (cc.Call_conv.n_args = List.length args)
      "calling convention arity does not match number of parameters";
    check (if not env.flavor.Ir.has_async_typ && cc.Call_conv.control = T.Promises
           then cc.Call_conv.n_res = 0
           else cc.Call_conv.n_res = List.length ret_tys)
      "calling convention arity does not match number of return types";
    if (cc.Call_conv.sort = T.Shared) then List.iter (check_concrete env exp.at) ret_tys;
    let env'' =
      {env' with labs = T.Env.empty; rets = Some (T.seq ret_tys); async = false} in
    check_exp (adjoin_vals env'' ve) exp;
    if not env.flavor.Ir.has_async_typ && cc.Call_conv.control = T.Promises then
      check_sub env' exp.at (typ exp) T.unit
    else check_sub env' exp.at (typ exp) (T.seq ret_tys);
    (* Now construct the function type and compare with the annotation *)
    let ts1 = List.map (fun a -> a.note) args in
    if (cc.Call_conv.sort = T.Shared) then List.iter (check_concrete env exp.at) ts1;
    let fun_ty = T.Func
      ( cc.Call_conv.sort, cc.Call_conv.control
      , tbs, List.map (T.close cs) ts1, List.map (T.close cs) ret_tys
      ) in
    fun_ty <: t
  | ActorE (id, ds, fs, t0) ->
    let env' = { env with async = false } in
    let ve0 = T.Env.singleton id t0 in
    let scope0 = { empty_scope with val_env = ve0 } in
    let scope1 = List.fold_left (gather_dec env') scope0 ds in
    let env'' = adjoin env' scope1 in
    check_decs env'' ds;
    check (T.is_obj t0) "bad annotation (object type expected)";
    let (s0, tfs0) = T.as_obj t0 in
    let val_tfs0 = List.filter (fun tf -> not (T.is_typ tf.T.typ)) tfs0 in
    (type_obj env'' T.Actor fs) <: (T.Obj (s0, val_tfs0));
    t0 <: t;
  | NewObjE (s, fs, t0) ->
    check (T.is_obj t0) "bad annotation (object type expected)";
    let (s0, tfs0) = T.as_obj t0 in
    let val_tfs0 = List.filter (fun tf -> not (T.is_typ tf.T.typ)) tfs0 in
    (type_obj env s fs) <: (T.Obj (s0, val_tfs0));
    t0 <: t

(* Cases *)

and check_cases env t_pat t cases =
  List.iter (check_case env t_pat t) cases

and check_case env t_pat t {it = {pat; exp}; _} =
  let ve = check_pat env pat in
  check_sub env pat.at t_pat pat.note;
  check_exp (adjoin_vals env ve) exp;
  check env pat.at (T.sub (typ exp) t) "bad case"

(* Arguments *)

and check_args env args =
  let rec go ve = function
    | [] -> ve
    | a::as_ ->
      check env a.at (not (T.Env.mem a.it ve))
        "duplicate binding for %s in argument list" a.it;
      check_typ env a.note;
      go (T.Env.add a.it a.note ve) as_
  in go T.Env.empty args

(* Patterns *)

and gather_pat env ve0 pat : val_env =
  let rec go ve pat =
    match pat.it with
    | WildP
    | LitP _ ->
      ve
    | VarP id ->
      check env pat.at (not (T.Env.mem id ve0))
        "duplicate binding for %s in block" id;
      T.Env.add id pat.note ve (*TBR*)
    | TupP pats ->
      List.fold_left go ve pats
    | ObjP pfs ->
      List.fold_left go ve (pats_of_obj_pat pfs)
    | AltP (pat1, pat2) ->
      ve
    | OptP pat1
    | TagP (_, pat1) ->
      go ve pat1
  in T.Env.adjoin ve0 (go T.Env.empty pat)

and check_pat_exhaustive env pat : val_env =
  let  ve = check_pat env pat in
  (* TODO: actually check exhaustiveness *)
  ve

and check_pat env pat : val_env =
  assert (pat.note <> T.Pre);
  let (<:) = check_sub env pat.at in
  let t = pat.note in
  match pat.it with
  | WildP -> T.Env.empty
  | VarP id -> T.Env.singleton id pat.note
  | LitP NullLit ->
    t <: T.Opt T.Any;
    T.Env.empty
  | LitP lit ->
    let t1 = T.Prim (type_lit env lit pat.at) in
    t <: t1;
    T.Env.empty
  | TupP pats ->
    let ve = check_pats pat.at env pats T.Env.empty in
    let ts = List.map (fun pat -> pat.note) pats in
    t <: T.Tup ts;
    ve
  | ObjP pfs ->
    let ve = check_pats pat.at env (pats_of_obj_pat pfs) T.Env.empty in
    check_pat_fields env t pfs;
    ve
  | OptP pat1 ->
    let ve = check_pat env pat1 in
    t <: T.Opt pat1.note;
    ve
  | TagP (l, pat1) ->
    let ve = check_pat env pat1 in
    check_pat_tag env t l pat1;
    ve
  | AltP (pat1, pat2) ->
    let ve1 = check_pat env pat1 in
    let ve2 = check_pat env pat2 in
    t <: pat1.note;
    t <: pat2.note;
    check env pat.at (T.Env.is_empty ve1 && T.Env.is_empty ve2)
      "variables are not allowed in pattern alternatives";
    T.Env.empty

and check_pats at env pats ve : val_env =
  match pats with
  | [] -> ve
  | pat::pats' ->
    let ve1 = check_pat env pat in
    let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
    check_pats at env pats' ve'

and check_pat_fields env t = List.iter (check_pat_field env t)

and check_pat_field env t (pf : pat_field) =
  let lab = pf.it.name in
  let tf = T.{lab; typ=pf.it.pat.note} in
  let s, tfs = T.as_obj_sub [lab] t in
  let (<:) = check_sub env pf.it.pat.at in
  t <: T.Obj (s, [tf]);
  if T.is_mut (T.lookup_val_field lab tfs) then
    error env pf.it.pat.at "cannot match mutable field %s" lab

and check_pat_tag env t l pat =
  let (<:) = check_sub env pat.at in
  T.lookup_val_field l (T.as_variant_sub l t) <: pat.note

(* Objects *)

and type_obj env s fs : T.typ =
  let tfs = type_exp_fields env s fs in
  T.Obj (s, tfs)

and type_exp_fields env s fs : T.field list =
  let tfs = List.map (type_exp_field env s) fs in
  List.sort T.compare_field tfs

and type_exp_field env s f : T.field =
  let {name; var} = f.it in
  let t = try T.Env.find var env.vals with
          | Not_found -> error env f.at "field typing for %s not found" name
  in
  assert (t <> T.Pre);
  check_sub env f.at t f.note;
  if not (T.is_typ t) then begin
    check env f.at ((s = T.Actor) ==> T.is_func t)
      "public actor field is not a function";
    check env f.at ((s = T.Actor) ==> T.shared t)
      "public actor field has non-shared type";
  end;
  T.{lab = name; typ = t}

(* Declarations *)

and check_open_typ_binds env typ_binds =
  let cs = List.map (fun tp -> tp.it.con) typ_binds in
  let ce = List.fold_right (fun c ce -> T.ConSet.disjoint_add c ce) cs T.ConSet.empty in
  let tbs = close_typ_binds cs (List.map (fun tb -> tb.it) typ_binds) in
  let _ = check_typ_binds env tbs in
  cs, tbs, ce

and close_typ_binds cs tbs =
  List.map (fun {con; bound} -> {Type.var = Con.name con; bound = Type.close cs bound}) tbs

and check_dec env dec  =
  (* helpers *)
  let check p = check env dec.at p in
  let (<:) t1 t2 = check_sub env dec.at t1 t2 in
  match dec.it with
  | LetD (pat, exp) ->
    ignore (check_pat_exhaustive env pat);
    check_exp env exp;
    typ exp <: pat.note
  | VarD (id, exp) ->
    let t0 = try T.Env.find id env.vals with
             |  Not_found -> error env dec.at "unbound variable %s" id
    in
    check (T.is_mut t0) "variable in VarD is not immutable";
    check_exp env exp;
    typ exp <: T.as_immut t0
  | TypD c ->
    check (T.ConSet.mem c env.cons) "free type constructor";
    check_con env c

and check_decs env decs  =
  List.iter (check_dec env) decs;

and gather_block_decs env decs =
  List.fold_left (gather_dec env) empty_scope decs

and gather_dec env scope dec : scope =
  match dec.it with
  | LetD (pat, exp) ->
    let ve = gather_pat env scope.val_env pat in
    let ce' = gather_typ env scope.con_env exp.note.note_typ in
    { val_env = ve; con_env = ce'}
  | VarD (id, exp) ->
    check env dec.at
      (not (T.Env.mem id scope.val_env))
      "duplicate variable definition in block";
    let ve =  T.Env.add id (T.Mut (typ exp)) scope.val_env in
    { scope with val_env = ve}
  | TypD c ->
    check env dec.at
      (not (T.ConSet.mem c scope.con_env))
      "duplicate definition of type in block";
    let ce' = T.ConSet.disjoint_add c scope.con_env in
    { scope with con_env = ce' }

and gather_typ env ce typ =
   match typ with
   | T.Obj(_, fs) ->
     List.fold_right (fun {T.lab;T.typ = typ1} ce ->
         match typ1 with
         | T.Typ c -> T.ConSet.add c ce
         | _ -> gather_typ env ce typ1
       ) fs ce
   | _ -> ce

(* Programs *)

let check_prog verbose scope phase (((ds, exp), flavor) as prog) : unit =
  let env = env_of_scope scope flavor in
  try
    let scope = gather_block_decs env ds in
    let env' = adjoin env scope in
    check_decs env' ds;
    check_exp env' exp;
  with CheckFailed s ->
    let bt = Printexc.get_backtrace () in
    if verbose
    then begin
      Printf.eprintf "Ill-typed intermediate code after %s:\n" phase;
      Printf.eprintf "%s" (Wasm.Sexpr.to_string 80 (Arrange_ir.prog prog));
      Printf.eprintf "%s" s;
      Printf.eprintf "%s" bt;
    end else begin
      Printf.eprintf "Ill-typed intermediate code after %s (use -v to see dumped IR):\n" phase;
      Printf.eprintf "%s" s;
      Printf.eprintf "%s" bt;
    end;
    exit 1

