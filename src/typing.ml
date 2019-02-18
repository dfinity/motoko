open Syntax
open Source

module T = Type
module A = Effect


(* Error recovery *)

exception Recover

let recover_with (x : 'a) (f : 'b -> 'a) (y : 'b) = try f y with Recover -> x
let recover_opt f y = recover_with None (fun y -> Some (f y)) y
let recover f y = recover_with () f y


(* Scopes *)

type val_env = T.typ T.Env.t
type typ_env = T.con T.Env.t
type con_env = T.Con.Set.t


type scope =
  { val_env : val_env;
    typ_env : typ_env;
    con_env : con_env;
  }

let empty_scope : scope =
  { val_env = T.Env.empty;
    typ_env = T.Env.empty;
    con_env = T.Con.Set.empty
  }

let adjoin_scope scope1 scope2 =
  { val_env = T.Env.adjoin scope1.val_env scope2.val_env;
    typ_env = T.Env.adjoin scope1.typ_env scope2.typ_env;
    con_env = T.Con.Set.disjoint_union scope1.con_env scope2.con_env;
  }


(* Contexts (internal) *)

type lab_env = T.typ T.Env.t
type ret_env = T.typ option

type env =
  { vals : val_env;
    typs : typ_env;
    cons : con_env;
    labs : lab_env;
    rets : ret_env;
    async : bool;
    pre : bool;
    msgs : Diag.msg_store;
  }

let env_of_scope msgs scope =
  { vals = scope.val_env;
    typs = scope.typ_env;
    cons = scope.con_env;
    labs = T.Env.empty;
    rets = None;
    async = false;
    pre = false;
    msgs;
  }


(* Error bookkeeping *)

let type_error at text : Diag.message = Diag.{sev = Diag.Error; at; cat = "type"; text}
let type_warning at text : Diag.message = Diag.{sev = Diag.Warning; at; cat = "type"; text}

let local_error env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_error at s)) fmt
let error env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_error at s); raise Recover) fmt
let warn env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_warning at s)) fmt


(* Context extension *)

let add_lab env x t = {env with labs = T.Env.add x t env.labs}
let add_val env x t = {env with vals = T.Env.add x t env.vals}

let add_typs env xs cs =
  { env with
    typs = List.fold_right2 T.Env.add xs cs env.typs;
    cons = List.fold_right T.Con.Set.disjoint_add cs env.cons;
  }

let adjoin env scope =
  { env with
    vals = T.Env.adjoin env.vals scope.val_env;
    typs = T.Env.adjoin env.typs scope.typ_env;
    cons = T.Con.Set.disjoint_union env.cons scope.con_env;
  }

let adjoin_vals env ve = {env with vals = T.Env.adjoin env.vals ve}
let adjoin_typs env te ce =
  { env with
    typs = T.Env.adjoin env.typs te;
    cons = T.Con.Set.disjoint_union env.cons ce;
  }

let disjoint_union env at fmt env1 env2 =
  try T.Env.disjoint_union env1 env2
  with T.Env.Clash k -> error env at fmt k


(* Types *)

let check_ids env ids = ignore
  (List.fold_left
    (fun dom id ->
      if List.mem id.it dom
      then error env id.at "duplicate field name %s in object type" id.it
      else id.it::dom
    ) [] ids
  )

let infer_mut mut : T.typ -> T.typ =
  match mut.it with
  | Const -> fun t -> t
  | Var -> fun t -> T.Mut t

let rec check_typ env typ : T.typ =
  let t = check_typ' env typ in
  typ.note <- t;
  t

and check_typ' env typ : T.typ =
  match typ.it with
  | VarT (id, typs) ->
    (match T.Env.find_opt id.it env.typs with
    | Some con ->
      let T.Def (tbs, t) | T.Abs (tbs, t) = T.Con.kind con in
      let ts = check_typ_bounds env tbs typs typ.at in
      T.Con (con, ts)
    | None -> error env id.at "unbound type identifier %s" id.it
    )
  | PrimT "Any" -> T.Any
  | PrimT "None" -> T.Non
  | PrimT "Shared" -> T.Shared
  | PrimT s ->
    (try T.Prim (T.prim s) with Invalid_argument _ ->
      error env typ.at "unknown primitive type"
    )
  | ArrayT (mut, typ) ->
    let t = check_typ env typ in
    T.Array (infer_mut mut t)
  | TupT typs ->
    T.Tup (List.map (check_typ env) typs)
  | FuncT (sort, binds, typ1, typ2) ->
    let cs, ts, te, ce = check_typ_binds env binds in
    let env' = adjoin_typs env te ce in
    let typs1 = as_seqT typ1 in
    let typs2 = as_seqT typ2 in
    let ts1 = List.map (check_typ env') typs1 in
    let ts2 = List.map (check_typ env') typs2 in
    let c = match typs2 with [{it = AsyncT _; _}] -> T.Promises | _ -> T.Returns in
    if sort.it = T.Sharable then begin
      let t1 = T.seq ts1 in
      if not (T.sub t1 T.Shared) then
        error env typ1.at "shared function has non-shared parameter type\n  %s"
          (T.string_of_typ_expand t1);
      (match ts2 with
      | [] -> ()
      | [T.Async t2] ->
        if not (T.sub t2 T.Shared) then
          error env typ1.at "shared function has non-shared result type\n  %s"
            (T.string_of_typ_expand t2);
      | _ -> error env typ1.at "shared function has non-async result type\n  %s"
          (T.string_of_typ_expand (T.seq ts2))
      )
    end;
    let tbs = List.map2 (fun c t -> {T.var = T.Con.name c; bound = t}) cs ts in
    T.Func (sort.it, c, T.close_binds cs tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2)
  | OptT typ ->
    T.Opt (check_typ env typ)
  | AsyncT typ ->
    let t = check_typ env typ in
    let t' = T.promote t in
    if t' <> T.Pre && not (T.sub t' T.Shared) then
      error env typ.at "async type has non-shared parameter type\n  %s"
        (T.string_of_typ_expand t');
    T.Async t
  | ObjT (sort, fields) ->
    check_ids env (List.map (fun (field : typ_field) -> field.it.id) fields);
    let fs = List.map (check_typ_field env sort.it) fields in
    T.Obj (sort.it, List.sort T.compare_field fs)
  | ParT typ ->
    check_typ env typ

and check_typ_field env s typ_field : T.field =
  let {id; mut; typ} = typ_field.it in
  let t = infer_mut mut (check_typ env typ) in
  if s = T.Actor && not (T.is_func (T.promote t)) then
    error env typ.at "actor field %s has non-function type\n  %s"
      id.it (T.string_of_typ_expand t);
  if s <> T.Object T.Local && not (T.sub t T.Shared) then
    error env typ.at "shared object or actor field %s has non-shared type\n  %s"
      id.it (T.string_of_typ_expand t);
  T.{lab = id.it; typ = t}

and check_typ_binds env typ_binds : T.con list * T.typ list * typ_env * con_env =
  let xs = List.map (fun typ_bind -> typ_bind.it.var.it) typ_binds in
  let cs = List.map (fun n -> T.Con.fresh n (T.Abs ([], T.Pre))) xs in
  let te = List.fold_left2 (fun te typ_bind c ->
      let id = typ_bind.it.var in
      if T.Env.mem id.it te then
        error env id.at "duplicate type name %s in type parameter list" id.it;
      T.Env.add id.it c te
    ) T.Env.empty typ_binds cs in
  let pre_env' = add_typs {env with pre = true} xs cs  in
  let ts = List.map (fun typ_bind -> check_typ pre_env' typ_bind.it.bound) typ_binds in
  let ks = List.map (fun t -> T.Abs ([], t)) ts in
  List.iter2 T.Con.set_kind cs ks;
  let env' = add_typs env xs cs in
  let _ = List.map (fun typ_bind -> check_typ env' typ_bind.it.bound) typ_binds in
  List.iter2 (fun typ_bind c -> typ_bind.note <- Some c) typ_binds cs;
  cs, ts, te, T.Con.Set.of_list cs

and check_typ_bounds env (tbs : T.bind list) typs at : T.typ list =
  match tbs, typs with
  | tb::tbs', typ::typs' ->
    let t = check_typ env typ in
    if not env.pre then begin
      if not (T.sub t tb.T.bound) then
        local_error env typ.at "type argument\n  %s\ndoes not match parameter bound\n  %s"
          (T.string_of_typ_expand t)
          (T.string_of_typ_expand tb.T.bound)
    end;
    let ts' = check_typ_bounds env tbs' typs' at in
    t::ts'
  | [], [] -> []
  | [], _ -> local_error env at "too many type arguments"; []
  | _, [] -> error env at "too few type arguments"

and check_inst_bounds env tbs typs at =
  let tys = check_typ_bounds env tbs typs at  in
  tys

(* Literals *)

let check_lit_val env t of_string at s =
  try of_string s with _ ->
    error env at "literal out of range for type %s"
      (T.string_of_typ (T.Prim t))

let check_nat env = check_lit_val env T.Nat Value.Nat.of_string
let check_int env = check_lit_val env T.Int Value.Int.of_string
let check_word8 env = check_lit_val env T.Word8 Value.Word8.of_string_u
let check_word16 env = check_lit_val env T.Word16 Value.Word16.of_string_u
let check_word32 env = check_lit_val env T.Word32 Value.Word32.of_string_u
let check_word64 env = check_lit_val env T.Word64 Value.Word64.of_string_u
let check_float env = check_lit_val env T.Float Value.Float.of_string


let infer_lit env lit at : T.prim =
  match !lit with
  | NullLit -> T.Null
  | BoolLit _ -> T.Bool
  | NatLit _ -> T.Nat
  | IntLit _ -> T.Int
  | Word8Lit _ -> T.Word8
  | Word16Lit _ -> T.Word16
  | Word32Lit _ -> T.Word32
  | Word64Lit _ -> T.Word64
  | FloatLit _ -> T.Float
  | CharLit _ -> T.Char
  | TextLit _ -> T.Text
  | PreLit (s, T.Nat) ->
    lit := NatLit (check_nat env at s); (* default *)
    T.Nat
  | PreLit (s, T.Int) ->
    lit := IntLit (check_int env at s); (* default *)
    T.Int
  | PreLit (s, T.Float) ->
    lit := FloatLit (check_float env at s); (* default *)
    T.Float
  | PreLit _ ->
    assert false

let check_lit env t lit at =
  match T.normalize t, !lit with
  | T.Opt _, NullLit -> ()
  | T.Prim T.Nat, PreLit (s, T.Nat) ->
    lit := NatLit (check_nat env at s)
  | T.Prim T.Int, PreLit (s, (T.Nat | T.Int)) ->
    lit := IntLit (check_int env at s)
  | T.Prim T.Word8, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word8Lit (check_word8 env at s)
  | T.Prim T.Word16, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word16Lit (check_word16 env at s)
  | T.Prim T.Word32, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word32Lit (check_word32 env at s)
  | T.Prim T.Word64, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word64Lit (check_word64 env at s)
  | T.Prim T.Float, PreLit (s, (T.Nat | T.Int | T.Float)) ->
    lit := FloatLit (check_float env at s)
  | t, _ ->
    let t' = T.Prim (infer_lit env lit at) in
    if not (T.sub t' t) then
      local_error env at "literal of type\n  %s\ndoes not have expected type\n  %s"
        (T.string_of_typ t') (T.string_of_typ_expand t)


(* Expressions *)

let isAsyncE exp =
  match exp.it with
  | AsyncE _ -> true
  | _ -> false

let rec infer_exp env exp : T.typ =
  infer_exp' T.as_immut env exp

and infer_exp_mut env exp : T.typ =
  infer_exp' Lib.Fun.id env exp

and infer_exp_promote env exp : T.typ =
  let t = infer_exp env exp in
  let t' = T.promote t in
  if t' = T.Pre then
    error env exp.at "cannot infer type of expression while trying to infer surrounding class type,\nbecause its type is a forward reference to type\n  %s"
      (T.string_of_typ_expand  t);
  t'

and infer_exp' f env exp : T.typ =
  assert (exp.note.note_typ = T.Pre);
  let t = infer_exp'' env exp in
  assert (t <> T.Pre);
  let t' = f t in
  if not env.pre then begin
    assert (T.normalize t' <> T.Pre);
    let e = A.infer_effect_exp exp in
    exp.note <- {note_typ = T.normalize t'; note_eff = e}
  end;
  t'

and infer_exp'' env exp : T.typ =
  match exp.it with
  | PrimE _ ->
    error env exp.at "cannot infer type of primitive"
  | VarE id ->
    (match T.Env.find_opt id.it env.vals with
    | Some T.Pre ->
      error env id.at "cannot infer type of forward variable %s" id.it;
    | Some t -> t
    | None -> error env id.at "unbound variable %s" id.it
    )
  | LitE lit ->
    T.Prim (infer_lit env lit exp.at)
  | UnE (ot, op, exp1) ->
    let t1 = infer_exp_promote env exp1 in
    (* Special case for subtyping *)
    let t = if t1 = T.Prim T.Nat then T.Prim T.Int else t1 in
    if not env.pre then begin
      assert (!ot = Type.Pre);
      if not (Operator.has_unop t op) then
        error env exp.at "operator is not defined for operand type\n  %s"
          (T.string_of_typ_expand t);
      ot := t;
    end;
    t
  | BinE (ot, exp1, op, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    let t2 = infer_exp_promote env exp2 in
    let t = T.lub t1 t2 in
    if not env.pre then begin
      assert (!ot = Type.Pre);
      if not (Operator.has_binop t op) then
        error env exp.at "operator not defined for operand types\n  %s and\n  %s"
          (T.string_of_typ_expand t1)
          (T.string_of_typ_expand t2);
      ot := t
    end;
    t
  | RelE (ot, exp1, op, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    let t2 = infer_exp_promote env exp2 in
    let t = T.lub t1 t2 in
    if not env.pre then begin
      assert (!ot = Type.Pre);
      if not (Operator.has_relop t op) then
        error env exp.at "operator not defined for operand types\n  %s and\n  %s"
          (T.string_of_typ_expand t1)
          (T.string_of_typ_expand t2);
      ot := t;
    end;
    T.bool
  | TupE exps ->
    let ts = List.map (infer_exp env) exps in
    T.Tup ts
  | OptE exp1 ->
    let t1 = infer_exp env exp1 in
    T.Opt t1
  | ProjE (exp1, n) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let ts = T.as_tup_sub n t1 in
      match List.nth_opt ts n with
      | Some t -> t
      | None ->
        error env exp.at "tuple projection %n is out of bounds for type\n  %s"
          n (T.string_of_typ_expand t1)
    with Invalid_argument _ ->
      error env exp1.at "expected tuple type, but expression produces type\n  %s"
        (T.string_of_typ_expand t1)
    )
  | ObjE (sort, id, fields) ->
    let env' = if sort.it = T.Actor then {env with async = false} else env in
    infer_obj env' sort.it id T.Pre fields exp.at
  | DotE (exp1, sr, {it = Name l; _}) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let s, tfs = T.as_obj_sub l t1 in
      sr := s;
      match List.find_opt (fun T.{lab; _} -> lab = l) tfs with
      | Some {T.typ = t; _} -> t
      | None ->
        error env exp1.at "field name %s does not exist in type\n  %s"
          l (T.string_of_typ_expand t1)
    with Invalid_argument _ ->
      error env exp1.at "expected object type, but expression produces type\n  %s"
        (T.string_of_typ_expand t1)
    )
  | AssignE (exp1, exp2) ->
    if not env.pre then begin
      let t1 = infer_exp_mut env exp1 in
      try
        let t2 = T.as_mut t1 in
        check_exp env t2 exp2
      with Invalid_argument _ ->
        error env exp.at "expected mutable assignment target";
    end;
    T.unit
  | ArrayE (mut, exps) ->
    let ts = List.map (infer_exp env) exps in
    let t1 = List.fold_left T.lub T.Non ts in
    if
      t1 = T.Any && List.for_all (fun t -> T.promote t <> T.Any) ts
    then
      warn env exp.at "this array has type %s because elements have inconsistent types"
        (T.string_of_typ (T.Array t1));
    T.Array (match mut.it with Const -> t1 | Var -> T.Mut t1)
  | IdxE (exp1, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let t = T.as_array_sub t1 in
      if not env.pre then check_exp env T.nat exp2;
      t
    with Invalid_argument _ ->
      error env exp1.at "expected array type, but expression produces type\n  %s"
        (T.string_of_typ_expand t1)
    )
  | CallE (exp1, insts, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let tbs, t2, t = T.as_func_sub (List.length insts) t1 in
      let ts = check_inst_bounds env tbs insts exp.at in
      if not env.pre then check_exp env (T.open_ ts t2) exp2;
      T.open_ ts t
    with Invalid_argument _ ->
      error env exp1.at "expected function type, but expression produces type\n  %s"
        (T.string_of_typ_expand t1)
    )
  | BlockE decs ->
    let t, scope = infer_block env decs exp.at in
    (try T.avoid scope.con_env t with T.Unavoidable c ->
      error env exp.at "local class type %s is contained in inferred block type\n  %s"
        (T.Con.to_string c)
        (T.string_of_typ_expand t)
    )
  | NotE exp1 ->
    if not env.pre then check_exp env T.bool exp1;
    T.bool
  | AndE (exp1, exp2) ->
    if not env.pre then begin
      check_exp env T.bool exp1;
      check_exp env T.bool exp2
    end;
    T.bool
  | OrE (exp1, exp2) ->
    if not env.pre then begin
      check_exp env T.bool exp1;
      check_exp env T.bool exp2
    end;
    T.bool
  | IfE (exp1, exp2, exp3) ->
    if not env.pre then check_exp env T.bool exp1;
    let t2 = infer_exp env exp2 in
    let t3 = infer_exp env exp3 in
    let t = T.lub t2 t3 in
    if
      t = T.Any &&
      T.promote t2 <> T.Any && T.promote t3 <> T.Any
    then
      warn env exp.at "this if has type %s because branches have inconsistent types,\ntrue produces\n  %s\nfalse produces\n  %s"
        (T.string_of_typ t)
        (T.string_of_typ_expand t2)
        (T.string_of_typ_expand t3);
    t
  | SwitchE (exp1, cases) ->
    let t1 = infer_exp_promote env exp1 in
    let t = infer_cases env t1 T.Non cases in
    if not env.pre then
      if not (Coverage.check_cases cases t1) then
        warn env exp.at "the cases in this switch do not cover all possible values";
    t
  | WhileE (exp1, exp2) ->
    if not env.pre then begin
      check_exp env T.bool exp1;
      check_exp env T.unit exp2
    end;
    T.unit
  | LoopE (exp1, expo) ->
    if not env.pre then begin
      check_exp env T.unit exp1;
      Lib.Option.app (check_exp env T.bool) expo
    end;
    T.Non
  | ForE (pat, exp1, exp2) ->
    if not env.pre then begin
      let t1 = infer_exp_promote env exp1 in
      (try
        let _, tfs = T.as_obj_sub "next" t1 in
        let t = T.lookup_field "next" tfs in
        let t1, t2 = T.as_mono_func_sub t in
        if not (T.sub T.unit t1) then raise (Invalid_argument "");
        let t2' = T.as_opt_sub t2 in
        let ve = check_pat_exhaustive env t2' pat in
        check_exp (adjoin_vals env ve) T.unit exp2
      with Invalid_argument _ ->
        local_error env exp1.at "expected iterable type, but expression has type\n  %s"
          (T.string_of_typ_expand t1)
      );
    end;
    T.unit
  | LabelE (id, typ, exp1) ->
    let t = check_typ env typ in
    if not env.pre then check_exp (add_lab env id.it t) t exp1;
    t
  | BreakE (id, exp1) ->
    (match T.Env.find_opt id.it env.labs with
    | Some t ->
      if not env.pre then check_exp env t exp1
    | None ->
      let name =
        match String.split_on_char ' ' id.it with
        | ["continue"; name] -> name
        | _ -> id.it
      in local_error env id.at "unbound label %s" name
    );
    T.Non
  | RetE exp1 ->
    if not env.pre then begin
      match env.rets with
      | Some T.Pre ->
        local_error env exp.at "cannot infer return type"
      | Some t ->
        check_exp env t exp1
      | None ->
        local_error env exp.at "misplaced return"
    end;
    T.Non
  | AsyncE exp1 ->
    let env' =
      {env with labs = T.Env.empty; rets = Some T.Pre; async = true} in
    let t = infer_exp env' exp1 in
    if not (T.sub t T.Shared) then
      error env exp1.at "async type has non-shared parameter type\n  %s"
        (T.string_of_typ_expand t);
    T.Async t
  | AwaitE exp1 ->
    if not env.async then
      error env exp.at "misplaced await";
    let t1 = infer_exp_promote env exp1 in
    (try
      T.as_async_sub t1
    with Invalid_argument _ ->
      error env exp1.at "expected async type, but expression has type\n  %s"
        (T.string_of_typ_expand t1)
    )
  | AssertE exp1 ->
    if not env.pre then check_exp env T.bool exp1;
    T.unit
  | AnnotE (exp1, typ) ->
    let t = check_typ env typ in
    if not env.pre then check_exp env t exp1;
    t

and check_exp env t exp =
  assert (not env.pre);
  assert (exp.note.note_typ = T.Pre);
  assert (t <> T.Pre);
  let t' = check_exp' env (T.normalize t) exp in
  let e = A.infer_effect_exp exp in
  exp.note <- {note_typ = t'; note_eff = e}

and check_exp' env t exp : T.typ =
  match exp.it, t with
  | PrimE s, T.Func _ ->
    t
  | LitE lit, _ ->
    check_lit env t lit exp.at;
    t
  | UnE (ot, op, exp1), _ when Operator.has_unop t op ->
    ot := t;
    check_exp env t exp1;
    t
  | BinE (ot, exp1, op, exp2), _ when Operator.has_binop t op ->
    ot := t;
    check_exp env t exp1;
    check_exp env t exp2;
    t
  | TupE exps, T.Tup ts when List.length exps = List.length ts ->
    List.iter2 (check_exp env) ts exps;
    t
  | OptE exp1, _ when T.is_opt t ->
    check_exp env (T.as_opt t) exp1;
    t
  | ArrayE (mut, exps), T.Array t' ->
    if (mut.it = Var) <> T.is_mut t' then
      local_error env exp.at "%smutable array expression cannot produce expected type\n  %s"
        (if mut.it = Const then "im" else "")
        (T.string_of_typ_expand (T.Array t'));
    List.iter (check_exp env (T.as_immut t')) exps;
    t
  | AsyncE exp1, T.Async t' ->
    let env' = {env with labs = T.Env.empty; rets = Some t'; async = true} in
    check_exp env' t' exp1;
    t
  | BlockE decs, _ ->
    ignore (check_block env t decs exp.at);
    t
  | IfE (exp1, exp2, exp3), _ ->
    check_exp env T.bool exp1;
    check_exp env t exp2;
    check_exp env t exp3;
    t
  | SwitchE (exp1, cases), _ ->
    let t1 = infer_exp_promote env exp1 in
    check_cases env t1 t cases;
    if not (Coverage.check_cases cases t1) then
      warn env exp.at "the cases in this switch do not cover all possible values";
    t
  | _ ->
    let t' = infer_exp env exp in
    if not (T.sub t' t) then
      local_error env exp.at "expression of type\n  %s\ncannot produce expected type\n  %s"
        (T.string_of_typ_expand t')
        (T.string_of_typ_expand t);
    t'


(* Cases *)

and infer_cases env t_pat t cases : T.typ =
  List.fold_left (infer_case env t_pat) t cases

and infer_case env t_pat t {it = {pat; exp}; at; _} =
  let ve = check_pat env t_pat pat in
  let t' = recover_with T.Non (infer_exp (adjoin_vals env ve)) exp in
  let t'' = T.lub t t' in
  if
    t'' = T.Any &&
    T.promote t <> T.Any && T.promote t' <> T.Any
  then
    warn env at "the switch has type %s because branches have inconsistent types,\nthis case produces type\n  %s\nthe previous produce type\n  %s"
      (T.string_of_typ t'')
      (T.string_of_typ_expand t)
      (T.string_of_typ_expand t');
  t''

and check_cases env t_pat t cases =
  List.iter (check_case env t_pat t) cases

and check_case env t_pat t {it = {pat; exp}; _} =
  let ve = check_pat env t_pat pat in
  recover (check_exp (adjoin_vals env ve) t) exp


(* Patterns *)

and infer_pat_exhaustive env pat : T.typ * val_env =
  let t, ve = infer_pat env pat in
  if not env.pre then
    if not (Coverage.check_pat pat t) then
      warn env pat.at "this pattern does not cover all possible values";
  t, ve

and infer_pat env pat : T.typ * val_env =
  assert (pat.note = T.Pre);
  let t, ve = infer_pat' env pat in
  if not env.pre then
    pat.note <- T.normalize t;
  t, ve

and infer_pat' env pat : T.typ * val_env =
  match pat.it with
  | WildP ->
    error env pat.at "cannot infer type of wildcard"
  | VarP _ ->
    error env pat.at "cannot infer type of variable"
  | LitP lit ->
    T.Prim (infer_lit env lit pat.at), T.Env.empty
  | SignP (op, lit) ->
    let t1 = T.Prim (infer_lit env lit pat.at) in
    (* Special case for subtyping *)
    let t = if t1 = T.Prim T.Nat then T.Prim T.Int else t1 in
    if not (Operator.has_unop t op) then
      local_error env pat.at "operator is not defined for operand type\n  %s"
        (T.string_of_typ_expand t);
    t, T.Env.empty
  | TupP pats ->
    let ts, ve = infer_pats pat.at env pats [] T.Env.empty in
    T.Tup ts, ve
  | OptP pat1 ->
    let t1, ve = infer_pat env pat1 in
    T.Opt t1, ve
  | AltP (pat1, pat2) ->
    let t1, ve1 = infer_pat env pat1 in
    let t2, ve2 = infer_pat env pat2 in
    let t = T.lub t1 t2 in
    if ve1 <> T.Env.empty || ve2 <> T.Env.empty then
      error env pat.at "variables are not allowed in pattern alternatives";
    t, T.Env.empty
  | AnnotP (pat1, typ) ->
    let t = check_typ env typ in
    t, check_pat env t pat1

and infer_pats at env pats ts ve : T.typ list * val_env =
  match pats with
  | [] -> List.rev ts, ve
  | pat::pats' ->
    let t, ve1 = infer_pat env pat in
    let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
    infer_pats at env pats' (t::ts) ve'


and check_pat_exhaustive env t pat : val_env =
  let ve = check_pat env t pat in
  if not env.pre then
    if not (Coverage.check_pat pat t) then
      warn env pat.at "this pattern does not cover all possible values";
  ve

and check_pat env t pat : val_env =
  assert (pat.note = T.Pre);
  if t = T.Pre then snd (infer_pat env pat) else
  let t' = T.normalize t in
  let ve = check_pat' env t' pat in
  if not env.pre then pat.note <- t';
  ve

and check_pat' env t pat : val_env =
  assert (t <> T.Pre);
  match pat.it with
  | WildP ->
    T.Env.empty
  | VarP id ->
    T.Env.singleton id.it t
  | LitP lit ->
    if not env.pre then check_lit env t lit pat.at;
    T.Env.empty
  | SignP (op, lit) ->
    if not env.pre then begin
      let t' = T.normalize t in
      if not (Operator.has_unop t op) then
        local_error env pat.at "operator cannot consume expected type\n  %s"
          (T.string_of_typ_expand t');
      check_lit env t' lit pat.at
    end;
    T.Env.empty
  | TupP pats ->
    (try
      let ts = T.as_tup_sub (List.length pats) t in
      check_pats env ts pats T.Env.empty pat.at
    with Invalid_argument _ ->
      error env pat.at "tuple pattern cannot consume expected type\n  %s"
        (T.string_of_typ_expand t)
    )
  | OptP pat1 ->
    (try
      let t1 = T.as_opt t in
      check_pat env t1 pat1
    with Invalid_argument _ ->
      error env pat.at "option pattern cannot consume expected type\n  %s"
        (T.string_of_typ_expand t)
    )
  | AltP (pat1, pat2) ->
    let ve1 = check_pat env t pat1 in
    let ve2 = check_pat env t pat2 in
    if ve1 <> T.Env.empty || ve2 <> T.Env.empty then
      error env pat.at "variables are not allowed in pattern alternatives";
    T.Env.empty
  | _ ->
    let t', ve = infer_pat env pat in
    if not (T.sub t t') then
      error env pat.at "pattern of type\n  %s\ncannot consume expected type\n  %s"
        (T.string_of_typ_expand t')
        (T.string_of_typ_expand t);
    ve

and check_pats env ts pats ve at : val_env =
  match pats, ts with
  | [], [] -> ve
  | pat::pats', t::ts' ->
    let ve1 = check_pat env t pat in
    let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
    check_pats env ts' pats' ve' at
  | [], ts ->
    local_error env at "tuple pattern has %i fewer components than expected type"
      (List.length ts); ve
  | ts, [] ->
    error env at "tuple pattern has %i more components than expected type"
      (List.length ts)


(* Objects *)

and pub_fields fields : region T.Env.t * region T.Env.t =
  List.fold_right pub_field fields (T.Env.empty, T.Env.empty)

and pub_field field xs : region T.Env.t * region T.Env.t =
  match field.it with
  | {vis; dec} when vis.it = Public -> pub_dec dec xs
  | _ -> xs

and pub_dec dec xs : region T.Env.t * region T.Env.t =
  match dec.it with
  | ExpD _ -> xs
  | LetD (pat, _) -> pub_pat pat xs
  | VarD (id, _)
  | FuncD (_, id, _, _, _, _) -> pub_val_id id xs
  | ClassD (id, typ_id, _, _, _, _, _) -> pub_val_id id (pub_typ_id typ_id xs)
  | TypD (id, _, _) -> pub_typ_id id xs

and pub_pat pat xs : region T.Env.t * region T.Env.t =
  match pat.it with
  | WildP | LitP _ | SignP _ -> xs
  | VarP id -> pub_val_id id xs
  | TupP pats -> List.fold_right pub_pat pats xs
  | AltP (pat1, _)
  | OptP pat1
  | AnnotP (pat1, _) -> pub_pat pat1 xs

and pub_typ_id id (xs, ys) : region T.Env.t * region T.Env.t =
  (T.Env.add id.it id.at xs, ys)

and pub_val_id id (xs, ys) : region T.Env.t * region T.Env.t =
  (xs, T.Env.add id.it id.at ys)


and infer_obj env s id t fields at : T.typ =
  let decs = List.map (fun (field : exp_field) -> field.it.dec) fields in
  let env' = add_val env id.it t in
  (* Prepass to infer type for id *)
  let _, scope = infer_block {env' with pre = true} decs at in
  let pub_typ, pub_val = pub_fields fields in
  (* TODO: type fields *)
  T.Env.iter (fun _ at' ->
    local_error env at' "public type fields not supported yet"
  ) pub_typ;
  let dom = T.Env.keys pub_val in
  let tfs =
    List.map (fun lab -> T.{lab; typ = T.Env.find lab scope.val_env}) dom in
  if not env.pre && s <> T.Object T.Local then begin
    List.iter (fun T.{lab; typ} ->
      if not (T.sub typ T.Shared) then
        error env (T.Env.find lab pub_val)
          "public shared object or actor field %s has non-shared type\n  %s"
          lab (T.string_of_typ_expand typ)
    ) tfs
  end;
  let t' = T.Obj (s, tfs) in
  ignore (infer_block (add_val env id.it t') decs at);
  t'


(* Blocks and Declarations *)

and infer_block env decs at : T.typ * scope =
  let scope = infer_block_decs env decs in
  let t = infer_block_exps (adjoin env scope) decs in
  t, scope

and infer_block_decs env decs : scope =
  let scope = gather_block_typdecs env decs in
  let env' = adjoin {env with pre = true} scope in
  let ce = infer_block_typdecs env' decs in
  let env'' = adjoin env {scope with con_env = ce} in
  let _ce' = infer_block_typdecs env'' decs in
  (* TBR: assertion does not work for types with binders, due to stamping *)
  (* assert (ce = ce'); *)
  let pre_ve' = gather_block_valdecs env decs in
  let ve = infer_block_valdecs (adjoin_vals env'' pre_ve') decs in
  {scope with val_env = ve; con_env = ce}

and infer_block_exps env decs : T.typ =
  match decs with
  | [] -> T.unit
  | [dec] -> infer_dec env dec
  | dec::decs' ->
    if not env.pre then recover (check_dec env T.unit) dec;
    recover_with T.Non (infer_block_exps env) decs'

and infer_dec env dec : T.typ =
  let t =
  match dec.it with
  | ExpD exp ->
    infer_exp env exp
  | LetD (_, exp) | VarD (_, exp) ->
    if not env.pre then ignore (infer_exp env exp);
    T.unit
  | FuncD (sort, id, typ_binds, pat, typ, exp) ->
    let t = T.Env.find id.it env.vals in
    if not env.pre then begin
      let _cs, _ts, te, ce = check_typ_binds env typ_binds in
      let env' = adjoin_typs env te ce in
      let _, ve = infer_pat_exhaustive env' pat in
      let t2 = check_typ env' typ in
      let env'' =
        {env' with labs = T.Env.empty; rets = Some t2; async = false} in
      check_exp (adjoin_vals env'' ve) t2 exp
    end;
    t
  | ClassD (id, typ_id, typ_binds, sort, pat, self_id, fields) ->
    let t = T.Env.find id.it env.vals in
    if not env.pre then begin
      let cs, _ts, te, ce = check_typ_binds env typ_binds in
      let env' = adjoin_typs env te ce in
      let _, ve = infer_pat_exhaustive env' pat in
      let env'' =
        {env' with labs = T.Env.empty; rets = None; async = false} in
      let self_typ = T.Con (T.Env.find typ_id.it env.typs, List.map (fun c -> T.Con (c, [])) cs) in
      ignore (infer_obj (adjoin_vals env'' ve) sort.it self_id self_typ fields dec.at)
    end;
    t
  | TypD _ ->
    T.unit
  in
  let eff = A.infer_effect_dec dec in
  dec.note <- {note_typ = t; note_eff = eff};
  t


and check_block env t decs at : scope =
  let scope = infer_block_decs env decs in
  check_block_exps (adjoin env scope) t decs at;
  scope

and check_block_exps env t decs at =
  match decs with
  | [] ->
    if not (T.sub T.unit t) then
      local_error env at "empty block cannot produce expected type\n  %s"
        (T.string_of_typ_expand t)
  | [dec] ->
    check_dec env t dec
  | dec::decs' ->
    recover (check_dec env T.unit) dec;
    recover (check_block_exps env t decs') at

and check_dec env t dec =
  match dec.it with
  | ExpD exp ->
    check_exp env t exp;
    dec.note <- exp.note;
(* TBR: push in external type annotation;
   unfortunately, this isn't enough, because of the earlier recursive phases
  | FuncD (id, [], pat, typ, exp) ->
    (* TBR: special-case unit? *)
    if T.eq env.cons t T.unit then
      ignore (infer_dec env dec)
    else
    (match T.nonopt env.cons t with
    | T.Func ([], t1, t2)->
      let ve = check_pat env t1 pat in
      let t2' = check_typ env typ in
      (* TBR: infer return type *)
      if not (T.eq env.cons t2 t2') then
        error dec.at "expected return type %s but found %s"
          (T.string_of_typ t2) (T.string_of_typ t2');
      let env' =
        {env with labs = T.Env.empty; rets = Some t2; async = false} in
      check_exp (adjoin_vals env' ve) t2 exp
    | _ ->
      error exp.at "function expression cannot produce expected type %s"
        (T.string_of_typ t)
    )
*)
  | _ ->
    let t' = infer_dec env dec in
    (* TBR: special-case unit? *)
    if not (T.eq t T.unit || T.sub t' t) then
      local_error env dec.at "expression of type\n  %s\ncannot produce expected type\n  %s"
        (T.string_of_typ_expand t')
        (T.string_of_typ_expand t)

(* Pass 1: collect type identifiers and their arity *)
and gather_block_typdecs env decs : scope =
  List.fold_left (gather_dec_typdecs env) empty_scope decs

and gather_dec_typdecs env scope dec : scope =
  match dec.it with
  | ExpD _ | LetD _ | VarD _ | FuncD _ -> scope
  | TypD (id, binds, _) | ClassD (_, id, binds, _, _, _, _) ->
    if T.Env.mem id.it scope.typ_env then
      error env dec.at "duplicate definition for type %s in block" id.it;
    let pre_tbs = List.map (fun bind -> {T.var = bind.it.var.it; bound = T.Pre}) binds in
    let pre_k = T.Abs (pre_tbs, T.Pre) in
    let c = T.Con.fresh id.it pre_k in
    let te' = T.Env.add id.it c scope.typ_env in
    let ce' = T.Con.Set.disjoint_add c scope.con_env in
    {scope with typ_env = te'; con_env = ce'}


(* Pass 2 and 3: infer type definitions *)
and infer_block_typdecs env decs : con_env =
  List.fold_left
    (fun ce dec ->
      let ce' = infer_dec_typdecs env dec in
      T.Con.Set.disjoint_union ce ce'
    )  T.Con.Set.empty decs

and infer_dec_typdecs env dec : con_env =
  match dec.it with
  | ExpD _ | LetD _ | VarD _ | FuncD _ ->
    T.Con.Set.empty
  | TypD (id, binds, typ) ->
    let c = T.Env.find id.it env.typs in
    let cs, ts, te, ce = check_typ_binds {env with pre = true} binds in
    let env' = adjoin_typs env te ce in
    let t = check_typ env' typ in
    let tbs = List.map2 (fun c t -> {T.var = T.Con.name c; bound = T.close cs t}) cs ts in
    let k = T.Def (tbs, T.close cs t) in
    infer_id_typdecs env id c k
  | ClassD (_, id, binds, sort, pat, self_id, fields) ->
    let c = T.Env.find id.it env.typs in
    let cs, ts, te, ce = check_typ_binds {env with pre = true} binds in
    let env' = adjoin_typs {env with pre = true} te ce in
    let _, ve = infer_pat env' pat in
    let self_typ = T.Con (c, List.map (fun c -> T.Con (c, [])) cs) in
    let t = infer_obj (adjoin_vals env' ve) sort.it self_id self_typ fields dec.at in
    let tbs = List.map2 (fun c t -> {T.var = T.Con.name c; bound = T.close cs t}) cs ts in
    let k = T.Def (tbs, T.close cs t) in
    infer_id_typdecs env id c k

and infer_id_typdecs env id c k : con_env =
  assert (match k with T.Abs (_, T.Pre) -> false | _ -> true);
  (match T.Con.kind c with
  | T.Abs (_, T.Pre) ->
    T.Con.set_kind c k;
    id.note <- Some c
  | k' ->
    assert (T.eq_kind k' k)
  );
  T.Con.Set.singleton c


(* Pass 4: collect value identifiers *)
and gather_block_valdecs env decs : val_env =
  List.fold_left (gather_dec_valdecs env) T.Env.empty decs

and gather_dec_valdecs env ve dec : val_env =
  match dec.it with
  | ExpD _ | TypD _ ->
    ve
  | LetD (pat, _) ->
    gather_pat env ve pat
  | VarD (id, _)
  | FuncD (_, id, _, _, _, _)
  | ClassD (id, _ , _, _, _, _, _) ->
    gather_id env ve id

and gather_pat env ve pat : val_env =
  match pat.it with
  | WildP | LitP _ | SignP _ ->
    ve
  | VarP id ->
    gather_id env ve id
  | TupP pats ->
    List.fold_left (gather_pat env) ve pats
  | AltP (pat1, _)
  | OptP pat1
  | AnnotP (pat1, _) ->
    gather_pat env ve pat1

and gather_id env ve id : val_env =
  if T.Env.mem id.it ve then
    error env id.at "duplicate definition for %s in block" id.it;
  T.Env.add id.it T.Pre ve

(* Pass 5: infer value types *)
and infer_block_valdecs env decs : val_env =
  let _, ve =
    List.fold_left (fun (env, ve) dec ->
      let ve' = infer_dec_valdecs env dec in
      adjoin_vals env ve', T.Env.adjoin ve ve'
    ) (env, T.Env.empty) decs
  in ve

and infer_dec_valdecs env dec : val_env =
  match dec.it with
  | ExpD _ ->
    T.Env.empty
  | LetD (pat, exp) ->
    let t = infer_exp {env with pre = true} exp in
    let ve' = check_pat_exhaustive env t pat in
    ve'
  | VarD (id, exp) ->
    let t = infer_exp {env with pre = true} exp in
    T.Env.singleton id.it (T.Mut t)
  | FuncD (sort, id, typ_binds, pat, typ, exp) ->
    let cs, ts, te, ce = check_typ_binds env typ_binds in
    let env' = adjoin_typs env te ce in
    let t1, _ = infer_pat {env' with pre = true} pat in
    let t2 = check_typ env' typ in
    if not env.pre && sort.it = T.Sharable then begin
      if not (T.sub t1 T.Shared) then
        error env pat.at "shared function has non-shared parameter type\n  %s"
          (T.string_of_typ_expand t1);
      begin match t2 with
      | T.Tup [] -> ()
      | T.Async t2 ->
        if not (T.sub t2 T.Shared) then
          error env typ.at "shared function has non-shared result type\n  %s"
            (T.string_of_typ_expand t2);
        if not (isAsyncE exp) then
          error env dec.at "shared function with async type has non-async body"
      | _ -> error env typ.at "shared function has non-async result type\n  %s"
          (T.string_of_typ_expand t2)
      end;
    end;
    let ts1 = match pat.it with TupP ps -> T.as_seq t1 | _ -> [t1] in
    let ts2 = match typ.it with TupT _ -> T.as_seq t2 | _ -> [t2] in
    let c =
      match sort.it, typ.it with
      | T.Sharable, (AsyncT _) -> T.Promises  (* TBR: do we want this for T.Local too? *)
      | _ -> T.Returns
    in
    let tbs = List.map2 (fun c t -> {T.var = T.Con.name c; bound = T.close cs t}) cs ts in
    T.Env.singleton id.it
      (T.Func (sort.it, c, tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2))
  | TypD _ ->
    T.Env.empty
  | ClassD (id, typ_id, typ_binds, sort, pat, self_id, fields) ->
    let cs, ts, te, ce = check_typ_binds env typ_binds in
    let env' = adjoin_typs env te ce in
    let c = T.Env.find typ_id.it env.typs in
    let t1, _ = infer_pat {env' with pre = true} pat in
    let ts1 = match pat.it with TupP _ -> T.as_seq t1 | _ -> [t1] in
    let t2 = T.Con (c, List.map (fun c -> T.Con (c, [])) cs) in
    let tbs = List.map2 (fun c t -> {T.var = T.Con.name c; bound = T.close cs t}) cs ts in
    T.Env.singleton id.it (T.Func (T.Local, T.Returns, tbs, List.map (T.close cs) ts1, [T.close cs t2]))


(* Programs *)

let check_prog scope prog : scope Diag.result =
  Diag.with_message_store (fun msgs ->
    Definedness.check_prog msgs prog;
    let env = env_of_scope msgs scope in
    recover_opt (check_block env T.unit prog.it) prog.at)

let infer_prog scope prog : (T.typ * scope) Diag.result =
  Diag.with_message_store (fun msgs ->
    Definedness.check_prog msgs prog;
    let env = env_of_scope msgs scope in
    recover_opt (infer_block env prog.it) prog.at)
