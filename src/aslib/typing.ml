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
type lib_env = T.typ T.Env.t
type typ_env = T.con T.Env.t
type con_env = T.ConSet.t

type obj_env = scope T.Env.t  (* internal object scopes *)

and scope =
  { val_env : val_env;
    lib_env : lib_env;
    typ_env : typ_env;
    con_env : con_env;
    obj_env : obj_env;
  }

let empty_scope : scope =
  { val_env = T.Env.empty;
    lib_env = T.Env.empty;
    typ_env = T.Env.empty;
    con_env = T.ConSet.empty;
    obj_env = T.Env.empty;
  }

let adjoin_scope scope1 scope2 =
  { val_env = T.Env.adjoin scope1.val_env scope2.val_env;
    lib_env = T.Env.adjoin scope1.lib_env scope2.lib_env;
    typ_env = T.Env.adjoin scope1.typ_env scope2.typ_env;
    con_env = T.ConSet.union scope1.con_env scope2.con_env;
    obj_env = T.Env.adjoin scope1.obj_env scope2.obj_env;
  }


let adjoin_val_env scope ve = {scope with val_env = T.Env.adjoin scope.val_env ve}

let library_scope f t =
  { empty_scope with lib_env = T.Env.add f t empty_scope.lib_env }


(* Contexts (internal) *)

type lab_env = T.typ T.Env.t
type ret_env = T.typ option

type env =
  { vals : val_env;
    libs : lib_env;
    typs : typ_env;
    cons : con_env;
    objs : obj_env;
    labs : lab_env;
    rets : ret_env;
    async : bool;
    pre : bool;
    msgs : Diag.msg_store;
  }

let env_of_scope msgs scope =
  { vals = scope.val_env;
    libs = scope.lib_env;
    typs = scope.typ_env;
    cons = scope.con_env;
    objs = T.Env.empty;
    labs = T.Env.empty;
    rets = None;
    async = false;
    pre = false;
    msgs;
  }

(* Error bookkeeping *)

let type_error at text : Diag.message =
  Diag.{sev = Diag.Error; at; cat = "type"; text}
let type_warning at text : Diag.message =
  Diag.{sev = Diag.Warning; at; cat = "type"; text}

let local_error env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_error at s)) fmt

let error env at fmt =
  Printf.ksprintf
    (fun s -> Diag.add_msg env.msgs (type_error at s); raise Recover) fmt

let warn env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_warning at s)) fmt


(* Context extension *)

let add_lab env x t = {env with labs = T.Env.add x t env.labs}
let add_val env x t = {env with vals = T.Env.add x t env.vals}

let add_typs env xs cs =
  { env with
    typs = List.fold_right2 T.Env.add xs cs env.typs;
    cons = List.fold_right T.ConSet.disjoint_add cs env.cons;
  }

let adjoin env scope =
  { env with
    vals = T.Env.adjoin env.vals scope.val_env;
    libs = T.Env.adjoin env.libs scope.lib_env;
    typs = T.Env.adjoin env.typs scope.typ_env;
    cons = T.ConSet.union env.cons scope.con_env;
    objs = T.Env.adjoin env.objs scope.obj_env;
  }

let adjoin_vals env ve = {env with vals = T.Env.adjoin env.vals ve}
let adjoin_typs env te ce =
  { env with
    typs = T.Env.adjoin env.typs te;
    cons = T.ConSet.disjoint_union env.cons ce;
  }

let disjoint_union env at fmt env1 env2 =
  try T.Env.disjoint_union env1 env2
  with T.Env.Clash k -> error env at fmt k


(* Types *)

let check_ids env kind member ids = ignore
  (List.fold_left
    (fun dom id ->
      if List.mem id.it dom
      then error env id.at "duplicate %s name %s in %s type" member id.it kind
      else id.it::dom
    ) [] ids
  )

let infer_mut mut : T.typ -> T.typ =
  match mut.it with
  | Const -> fun t -> t
  | Var -> fun t -> T.Mut t

(* Paths *)

let rec check_obj_path env path : T.obj_sort * (T.field list) =
  match T.promote (check_obj_path' env path) with
  | T.Obj (s, fs) as t ->
    path.note <- t;
    (s, fs)
  | t ->
    error env path.at
      "expected actor, object or module type, but path expression produces type\n  %s"
      (T.string_of_typ_expand t)

and check_obj_path' env path : T.typ =
  match path.it with
  | IdH id ->
    (match T.Env.find_opt id.it env.vals with
     | Some T.Pre ->
       error env id.at "cannot infer type of forward variable reference %s" id.it
     | Some t -> t
     | None -> error env id.at "unbound variable %s" id.it
    )
  | DotH (path', id) ->
    let s, fs = check_obj_path env path' in
    match T.lookup_val_field id.it fs with
    | Some T.Pre ->
      error env id.at "cannot infer type of forward field reference %s" id.it
    | Some t -> t
    | None ->
      error env id.at "field %s does not exist in type\n  %s"
        id.it (T.string_of_typ_expand (T.Obj (s, fs)))

let rec check_typ_path env path : T.con =
  let c = check_typ_path' env path in
  path.note <- T.Typ c;
  c

and check_typ_path' env path : T.con =
  match path.it with
  | IdH id ->
    (match T.Env.find_opt id.it env.typs with
    | Some c -> c
    | None -> error env id.at "unbound type %s" id.it
    )
  | DotH (path', id) ->
    let s, fs = check_obj_path env path' in
    match T.lookup_typ_field id.it fs with
    | Some t -> t
    | None ->
      error env id.at "type field %s does not exist in type\n  %s"
        id.it (T.string_of_typ_expand (T.Obj (s, fs)))

let rec check_typ env typ : T.typ =
  let t = check_typ' env typ in
  typ.note <- t;
  t

and check_typ' env typ : T.typ =
  match typ.it with
  | PathT (path, typs) ->
    let c = check_typ_path env path in
    let T.Def (tbs, _) | T.Abs (tbs, _) = Con.kind c in
    let ts = check_typ_bounds env tbs typs typ.at in
    T.Con (T.Free c, ts)
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
    if sort.it = T.Sharable then
    if not env.pre then begin
      let t1 = T.seq ts1 in
      if not (T.sub t1 T.Shared) then
        error env typ1.at
          "shared function has non-shared parameter type\n  %s"
          (T.string_of_typ_expand t1);
      match ts2 with
      | [] -> ()
      | [T.Async t2] ->
        if not (T.sub t2 T.Shared) then
          error env typ2.at
            "shared function has non-shared result type\n  %s"
            (T.string_of_typ_expand t2);
      | _ ->
        error env typ2.at
          "shared function has non-async result type\n  %s"
          (T.string_of_typ_expand (T.seq ts2))
    end;
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = t}) cs ts in
    T.Func (sort.it, c, T.close_binds cs tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2)
  | OptT typ ->
    T.Opt (check_typ env typ)
  | VariantT tags ->
    check_ids env "variant" "tag"
      (List.map (fun (tag : typ_tag) -> tag.it.tag) tags);
    let fs = List.map (check_typ_tag env) tags in
    T.Variant (List.sort T.compare_field fs)
  | AsyncT typ ->
    let t = check_typ env typ in
    if not env.pre && not (T.sub t T.Shared) then
      error env typ.at "async type has non-shared parameter type\n  %s"
        (T.string_of_typ_expand t);
    T.Async t
  | ObjT (sort, fields) ->
    check_ids env "object" "field"
      (List.map (fun (field : typ_field) -> field.it.id) fields);
    let fs = List.map (check_typ_field env sort.it) fields in
    T.Obj (sort.it, List.sort T.compare_field fs)
  | ParT typ ->
    check_typ env typ

and check_typ_field env s typ_field : T.field =
  let {id; mut; typ} = typ_field.it in
  let t = infer_mut mut (check_typ env typ) in
  if not env.pre then begin
    if s = T.Actor && not (T.is_func (T.promote t)) then
      error env typ.at "actor field %s has non-function type\n  %s"
        id.it (T.string_of_typ_expand t);
    if s <> T.Object T.Local && not (T.sub t T.Shared) then
      error env typ.at
        "shared object or actor field %s has non-shared type\n  %s"
        id.it (T.string_of_typ_expand t)
  end;
  T.{lab = id.it; typ = t}

and check_typ_tag env typ_tag =
  let {tag; typ} = typ_tag.it in
  let t = check_typ env typ in
  T.{lab = tag.it; typ = t}

and check_typ_binds env typ_binds : T.con list * T.typ list * typ_env * con_env =
  let xs = List.map (fun typ_bind -> typ_bind.it.var.it) typ_binds in
  let cs = List.map2 (fun x tb ->
               match tb.note with
               | Some c -> c
               | None -> Con.fresh x (T.Abs ([], T.Pre))) xs typ_binds in
  let te = List.fold_left2 (fun te typ_bind c ->
      let id = typ_bind.it.var in
      if T.Env.mem id.it te then
        error env id.at "duplicate type name %s in type parameter list" id.it;
      T.Env.add id.it c te
    ) T.Env.empty typ_binds cs in
  let pre_env' = add_typs {env with pre = true} xs cs  in
  let ts = List.map (fun typ_bind -> check_typ pre_env' typ_bind.it.bound) typ_binds in
  let ks = List.map (fun t -> T.Abs ([], t)) ts in
  List.iter2 (fun c k ->
    match Con.kind c with
    | T.Abs(_,T.Pre) -> T.set_kind c k
    | k' -> (* assert (T.eq_kind k k') *) ()
    ) cs ks;
  let env' = add_typs env xs cs in
  let _ = List.map (fun typ_bind -> check_typ env' typ_bind.it.bound) typ_binds in
  List.iter2 (fun typ_bind c -> typ_bind.note <- Some c) typ_binds cs;
  cs, ts, te, T.ConSet.of_list cs

and check_typ_bounds env (tbs : T.bind list) typs at : T.typ list =
  match tbs, typs with
  | tb::tbs', typ::typs' ->
    let t = check_typ env typ in
    if not env.pre then begin
      if not (T.sub t tb.T.bound) then
        local_error env typ.at
          "type argument\n  %s\ndoes not match parameter bound\n  %s"
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
      local_error env at
        "literal of type\n  %s\ndoes not have expected type\n  %s"
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
    error env exp.at
      "cannot infer type of expression while trying to infer surrounding class type,\nbecause its type is a forward reference to type\n  %s"
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
    | None ->
      error env id.at "unbound variable %s" id.it
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
  | ShowE (ot, exp1) ->
    let t = infer_exp_promote env exp1 in
    if not env.pre then begin
      if not (Show.can_show t) then
        error env exp.at "show is not defined for operand type\n  %s"
          (T.string_of_typ_expand t);
      ot := t
    end;
    T.Prim T.Text
  | BinE (ot, exp1, op, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    let t2 = infer_exp_promote env exp2 in
    let t = T.lub t1 t2 in
    if not env.pre then begin
      assert (!ot = Type.Pre);
      if not (Operator.has_binop t op) then
        error env exp.at
          "operator not defined for operand types\n  %s and\n  %s"
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
        error env exp.at
          "operator not defined for operand types\n  %s and\n  %s"
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
  | TagE (id, exp1) ->
    T.Variant [T.{lab = id.it; typ = infer_exp env exp1}]
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
      error env exp1.at
        "expected tuple type, but expression produces type\n  %s"
        (T.string_of_typ_expand t1)
    )
  | ObjE (sort, fields) ->
    let env' = if sort.it = T.Actor then {env with async = false} else env in
    infer_obj env' sort.it fields exp.at
  | DotE (exp1, id) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let s, tfs = T.as_obj_sub id.it t1 in
      match T.lookup_val_field id.it tfs with
      | Some T.Pre ->
        error env exp.at "cannot infer type of forward field reference %s"
          id.it
      | Some t -> t
      | None ->
        error env exp1.at "field %s does not exist in type\n  %s"
           id.it (T.string_of_typ_expand t1)
     with Invalid_argument _ ->
       error env exp1.at
         "expected object type, but expression produces type\n  %s"
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
    if not env.pre && t1 = T.Any && List.for_all (fun t -> T.promote t <> T.Any) ts then
      warn env exp.at
        "this array has type %s because elements have inconsistent types"
        (T.string_of_typ (T.Array t1));
    T.Array (match mut.it with Const -> t1 | Var -> T.Mut t1)
  | IdxE (exp1, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let t = T.as_array_sub t1 in
      if not env.pre then check_exp env T.nat exp2;
      t
    with Invalid_argument _ ->
      error env exp1.at
        "expected array type, but expression produces type\n  %s"
        (T.string_of_typ_expand t1)
    )
  | FuncE (_, sort, typ_binds, pat, typ, exp) ->
    let cs, ts, te, ce = check_typ_binds env typ_binds in
    let env' = adjoin_typs env te ce in
    let t1, ve = infer_pat_exhaustive env' pat in
    let t2 = check_typ env' typ in
    if not env.pre then begin
      let env'' =
        {env' with labs = T.Env.empty; rets = Some t2; async = false} in
      check_exp (adjoin_vals env'' ve) t2 exp;
      if sort.it = T.Sharable then begin
        if not (T.sub t1 T.Shared) then
          error env pat.at
            "shared function has non-shared parameter type\n  %s"
            (T.string_of_typ_expand t1);
        if not (T.is_concrete t1) then
          error env pat.at
            "shared function parameter contains abstract type\n  %s"
            (T.string_of_typ_expand t1);
        match t2 with
        | T.Tup [] -> ()
        | T.Async t2 ->
          if not (T.sub t2 T.Shared) then
            error env typ.at
              "shared function has non-shared result type\n  %s"
              (T.string_of_typ_expand t2);
          if not (T.is_concrete t2) then
            error env typ.at
              "shared function result contains abstract type\n  %s"
              (T.string_of_typ_expand t2);
          if not (isAsyncE exp) then
            error env exp.at
              "shared function with async type has non-async body"
        | _ ->
          error env typ.at "shared function has non-async result type\n  %s"
            (T.string_of_typ_expand t2)
      end
    end;
    let ts1 = match pat.it with TupP ps -> T.as_seq t1 | _ -> [t1] in
    let ts2 = match typ.it with TupT _ -> T.as_seq t2 | _ -> [t2] in
    let c =
      match sort.it, typ.it with
      | T.Sharable, (AsyncT _) -> T.Promises  (* TBR: do we want this for T.Local too? *)
      | _ -> T.Returns
    in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close cs t}) cs ts in
    T.Func (sort.it, c, tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2)
  | CallE (exp1, insts, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    let sort, tbs, t_arg, t_ret =
      try T.as_func_sub T.Local (List.length insts) t1
      with Invalid_argument _ ->
        error env exp1.at
          "expected function type, but expression produces type\n  %s"
          (T.string_of_typ_expand t1)
      in
    let ts = check_inst_bounds env tbs insts exp.at in
    let t_arg = T.open_ ts t_arg in
    let t_ret = T.open_ ts t_ret in
    if not env.pre then begin
      check_exp env t_arg exp2;
      if sort = T.Sharable then begin
        if not (T.is_concrete t_arg) then
          error env exp1.at
            "shared function argument contains abstract type\n  %s"
            (T.string_of_typ_expand t_arg);
        if not (T.is_concrete t_ret) then
          error env exp2.at
            "shared function call result contains abstract type\n  %s"
            (T.string_of_typ_expand t_ret);
      end
    end;
    t_ret
  | BlockE decs ->
    let t, scope = infer_block env decs exp.at in
    (try T.avoid scope.con_env t with T.Unavoidable c ->
      error env exp.at
        "local class type %s is contained in inferred block type\n  %s"
        (Con.to_string c)
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
    if not env.pre && t = T.Any && T.promote t2 <> T.Any && T.promote t3 <> T.Any then
      warn env exp.at
        "this if has type %s because branches have inconsistent types,\ntrue produces\n  %s\nfalse produces\n  %s"
        (T.string_of_typ t)
        (T.string_of_typ_expand t2)
        (T.string_of_typ_expand t3);
    t
  | SwitchE (exp1, cases) ->
    let t1 = infer_exp_promote env exp1 in
    let t = infer_cases env t1 T.Non cases in
    if not env.pre then
      if not (Coverage.check_cases cases t1) then
        warn env exp.at
          "the cases in this switch do not cover all possible values";
    t
  | WhileE (exp1, exp2) ->
    if not env.pre then begin
      check_exp env T.bool exp1;
      check_exp env T.unit exp2
    end;
    T.unit
  | LoopE (exp1, None) ->
    if not env.pre then begin
      check_exp env T.unit exp1
    end;
    T.Non
  | LoopE (exp1, Some exp2) ->
    if not env.pre then begin
      check_exp env T.unit exp1;
      check_exp env T.bool exp2
    end;
    T.unit
  | ForE (pat, exp1, exp2) ->
    if not env.pre then begin
      let t1 = infer_exp_promote env exp1 in
      (try
        let _, tfs = T.as_obj_sub "next" t1 in
        let t = Lib.Option.value (T.lookup_val_field "next" tfs) in
        let t1, t2 = T.as_mono_func_sub t in
        if not (T.sub T.unit t1) then raise (Invalid_argument "");
        let t2' = T.as_opt_sub t2 in
        let ve = check_pat_exhaustive env t2' pat in
        check_exp (adjoin_vals env ve) T.unit exp2
      with Invalid_argument _ | Not_found ->
        local_error env exp1.at
          "expected iterable type, but expression has type\n  %s"
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
  | ImportE (_, fp) ->
    assert (!fp <> "");
    (match T.Env.find_opt !fp env.libs with
    | Some T.Pre ->
      error env exp.at "cannot infer type of forward import %s" !fp
    | Some t -> t
    | None -> error env exp.at "unresolved import %s" !fp
    )

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
      local_error env exp.at
        "%smutable array expression cannot produce expected type\n  %s"
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
    if not env.pre then
      if not (Coverage.check_cases cases t1) then
        warn env exp.at
          "the cases in this switch do not cover all possible values";
    t
  | _ ->
    let t' = infer_exp env exp in
    if not (T.sub t' t) then
      local_error env exp.at
        "expression of type\n  %s\ncannot produce expected type\n  %s"
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
  if not env.pre && t'' = T.Any && T.promote t <> T.Any && T.promote t' <> T.Any then
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
  | ObjP pfs ->
    let (s, tfs), ve = infer_pat_fields pat.at env pfs [] T.Env.empty in
    T.Obj (s, tfs), ve
  | OptP pat1 ->
    let t1, ve = infer_pat env pat1 in
    T.Opt t1, ve
  | TagP (id, pat1) ->
    let t1, ve = infer_pat env pat1 in
    T.Variant [T.{lab = id.it; typ = t1}], ve
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
  | ParP pat1 ->
    infer_pat env pat1

and infer_pats at env pats ts ve : T.typ list * val_env =
  match pats with
  | [] -> List.rev ts, ve
  | pat::pats' ->
    let t, ve1 = infer_pat env pat in
    let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
    infer_pats at env pats' (t::ts) ve'

and infer_pat_fields at env pfs ts ve : (T.obj_sort * T.field list) * val_env =
  match pfs with
  | [] -> (T.(Object Local), List.rev ts), ve
  | pf::pfs' ->
    let typ, ve1 = infer_pat env pf.it.pat in
    let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
    infer_pat_fields at env pfs' (T.{ lab = pf.it.id.it; typ }::ts) ve'

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
  | ObjP pfs ->
    (try
       let s, tfs = T.as_obj_sub "" t in
       if s = T.Actor then error env pat.at "object pattern cannot destructure actors";
       check_pat_fields env tfs (List.stable_sort compare_pat_field pfs) T.Env.empty pat.at
     with Invalid_argument _ ->
       error env pat.at "object pattern cannot consume expected type\n  %s"
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
  | TagP (id, pat1) ->
    (try
      let t1 = Lib.Option.value (T.lookup_val_field id.it (T.as_variant t)) in
      check_pat env t1 pat1
     with Invalid_argument _ | Not_found ->
       error env pat.at "variant pattern cannot consume expected type\n  %s"
         (T.string_of_typ_expand t)
    )
  | AltP (pat1, pat2) ->
    let ve1 = check_pat env t pat1 in
    let ve2 = check_pat env t pat2 in
    if ve1 <> T.Env.empty || ve2 <> T.Env.empty then
      error env pat.at "variables are not allowed in pattern alternatives";
    T.Env.empty
  | ParP pat1 ->
    check_pat env t pat1
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

and check_pat_fields env tfs pfs ve at : val_env =
  let repeated l = function
    | [] -> None
    | (pf : pat_field)::_ -> if l = pf.it.id.it then Some pf.at else None
  in
  match pfs, tfs with
  | [], [] -> ve
  | pf::pfs', T.{ lab; typ }::tfs' ->
    (match compare pf.it.id.it lab with
    | 0 ->
      if T.is_mut typ then error env pf.at "cannot pattern match mutable field %s" lab;
      let ve1 = check_pat env typ pf.it.pat in
      let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
      begin match repeated lab pfs' with
      | None -> check_pat_fields env tfs' pfs' ve' at
      | Some at -> error env at "cannot pattern match repeated field %s" lab
      end
    | c when c > 0 ->
      check_pat_fields env tfs' pfs ve at
    | _ ->
      error env pf.at "object pattern field %s is not contained in expected type" pf.it.id.it
    )
  | [], _ -> ve
  | pf::_, [] ->
    error env pf.at "object pattern field %s is not contained in expected type" pf.it.id.it

and compare_pat_field {it={id = l1; pat; _};_} {it={id = l2; pat; _};_} = compare l1.it l2.it


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
  | VarD (id, _) -> pub_val_id id xs
  | ClassD (id, _, _, _, _, _) ->
    pub_val_id {id with note = ()} (pub_typ_id id xs)
  | TypD (id, _, _) -> pub_typ_id id xs

and pub_pat pat xs : region T.Env.t * region T.Env.t =
  match pat.it with
  | WildP | LitP _ | SignP _ -> xs
  | VarP id -> pub_val_id id xs
  | TupP pats -> List.fold_right pub_pat pats xs
  | ObjP pfs -> List.fold_right pub_pat_field pfs xs
  | AltP (pat1, _)
  | OptP pat1
  | TagP (_, pat1)
  | AnnotP (pat1, _)
  | ParP pat1 -> pub_pat pat1 xs

and pub_pat_field pf xs =
  pub_pat pf.it.pat xs

and pub_typ_id id (xs, ys) : region T.Env.t * region T.Env.t =
  (T.Env.add id.it id.at xs, ys)

and pub_val_id id (xs, ys) : region T.Env.t * region T.Env.t =
  (xs, T.Env.add id.it id.at ys)

(* Object/Scope transformations *)

and gather_typ con_env t =
  match t with
  | T.Obj (s, tfs) -> List.fold_right gather_typ_field tfs con_env
  | _ -> con_env

and gather_typ_field T.{lab; typ} con_env =
  match typ with
  | T.Typ  c -> T.ConSet.add c con_env
  | t -> gather_typ con_env t

(* TODO: remove by merging conenv and valenv or by separating typ_fields *)

and object_of_scope env sort fields scope at =
  let pub_typ, pub_val = pub_fields fields in
  let typ_fields =
    T.Env.fold
      (fun id con flds ->
       if T.Env.mem id pub_typ then
         { T.lab = id; T.typ = T.Typ con }::flds
       else flds) scope.typ_env  []
  in
  let fields =
    T.Env.fold
      (fun id typ flds ->
        if T.Env.mem id pub_val then
          { T.lab = id; T.typ = typ }::flds
        else flds) scope.val_env typ_fields
  in
  let t = T.Obj (sort, List.sort T.compare_field fields) in
  let accessible_cons = gather_typ T.ConSet.empty t in
  let inaccessible_cons = T.ConSet.diff scope.con_env accessible_cons in
  try
    T.avoid_cons inaccessible_cons accessible_cons;
    T.avoid inaccessible_cons t
  with T.Unavoidable c ->
      error env at "local class type %s is contained in object or actor type\n  %s"
        (Con.to_string c)
        (T.string_of_typ_expand t)

and is_actor_method dec : bool = match dec.it with
  | LetD ({ it = VarP _; _}, {it = FuncE _; _} ) -> true
  | _ -> false

and is_typ_dec dec : bool = match dec.it with
  | TypD _ -> true
  | _ -> false


and infer_obj env s fields at : T.typ =
  let decs = List.map (fun (field : exp_field) -> field.it.dec) fields in
  let _, scope = infer_block env decs at in
  let t = object_of_scope env s fields scope at in
  let (_, tfs) = T.as_obj t in
  if not env.pre then begin
    if s = T.Object T.Sharable || s = T.Actor then
      List.iter (fun T.{lab; typ} ->
          if  (not (T.is_typ typ)) && not (T.sub typ T.Shared) then
            let _, pub_val = pub_fields fields in
            error env (T.Env.find lab pub_val)
              "public shared object or actor field %s has non-shared type\n  %s"
              lab (T.string_of_typ_expand typ)
        ) tfs;
    if s = T.Actor then
      List.iter (fun ef ->
          if ef.it.vis.it = Syntax.Public && not (is_actor_method ef.it.dec) && not (is_typ_dec ef.it.dec) then
            local_error env ef.it.dec.at "public actor field needs to be a manifest function"
        ) fields;
    if s = T.Module then Static.fields env.msgs fields
  end;
  t

(* Blocks and Declarations *)

and infer_block env decs at : T.typ * scope =
  let scope = infer_block_decs env decs in
  let t = infer_block_exps (adjoin env scope) decs in
  t, scope

and infer_block_decs env decs : scope =
  let scope = gather_block_decs env decs in
  let env' = adjoin {env with pre = true} scope in
  let scope_ce = infer_block_typdecs env' decs in
  let env'' = adjoin env scope_ce in
  let _scope_ce = infer_block_typdecs env'' decs in
  (* TBR: assertion does not work for types with binders, due to stamping *)
  (* assert (scope_ce = _scope_ce); *)
  infer_block_valdecs (adjoin env'' scope_ce) decs scope_ce

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
  | ExpD exp
  | LetD (_, exp) ->
    infer_exp env exp
  | VarD (_, exp) ->
    if not env.pre then ignore (infer_exp env exp);
    T.unit
  | ClassD (id, typ_binds, sort, pat, self_id, fields) ->
    let t = T.Env.find id.it env.vals in
    if not env.pre then begin
      let c = T.Env.find id.it env.typs in
      let cs, _ts, te, ce = check_typ_binds env typ_binds in
      let env' = adjoin_typs env te ce in
      let _, ve = infer_pat_exhaustive env' pat in
      let self_typ = T.Con (T.Free c, List.map (fun c -> T.Con (T.Free c, [])) cs) in
      let env'' =
        { (add_val (adjoin_vals env' ve) self_id.it self_typ) with
          labs = T.Env.empty;
          rets = None;
          async = false
        }
      in ignore (infer_obj env'' sort.it fields dec.at)
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
    dec.note <- exp.note
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
    if not (T.eq t T.unit || T.sub t' t) then
      local_error env dec.at "expression of type\n  %s\ncannot produce expected type\n  %s"
        (T.string_of_typ_expand t')
        (T.string_of_typ_expand t)

and infer_val_path env exp : T.typ option =
  match exp.it with
  | ImportE (_, fp) ->
    assert (!fp <> "");
    T.Env.find_opt !fp env.libs
  | VarE id ->
    T.Env.find_opt id.it env.vals
  | DotE (path, id) ->
    (match infer_val_path env path with
     | None -> None
     | Some t ->
       match T.promote t with
       | T.Obj ( _, flds) -> T.lookup_val_field id.it flds
       | _ -> None
    )
  | _ -> None


(* Pass 1: collect:
   * type identifiers and their arity,
   * object identifiers and their fields (if known) (recursively)
   * other value identifiers at type T.Pre
*)
and gather_block_decs env decs : scope =
  List.fold_left (gather_dec env) empty_scope decs

and gather_dec env scope dec : scope =
  match dec.it with
  | ExpD _ -> scope
  (* TODO: generalize beyond let <id> = <obje> *)
  | LetD (
      {it = VarP id; _},
      {it = ObjE ({it = sort; _}, fields); at; _}
    ) ->
    let decs = List.map (fun ef -> ef.it.dec) fields in
    if T.Env.mem id.it scope.val_env then
      error env dec.at "duplicate definition for value %s in block" id.it;
    let scope' = gather_block_decs env decs in
    let ve' = T.Env.add id.it (object_of_scope env sort fields scope' at) scope.val_env in
    let obj_env = T.Env.add id.it scope' scope.obj_env in
    {val_env = ve';
     typ_env = scope.typ_env;
     lib_env = scope.lib_env;
     con_env = scope.con_env;
     obj_env = obj_env }
  | LetD (pat, _) -> adjoin_val_env scope (gather_pat env scope.val_env pat)
  | VarD (id, _) -> adjoin_val_env scope (gather_id env scope.val_env id)
  | TypD (id, binds, _) | ClassD (id, binds, _, _, _, _) ->
    if T.Env.mem id.it scope.typ_env then
      error env dec.at "duplicate definition for type %s in block" id.it;
    let pre_tbs = List.map (fun bind -> {T.var = bind.it.var.it; bound = T.Pre}) binds in
    let pre_k = T.Abs (pre_tbs, T.Pre) in
    let c = match id.note with
      | None -> let c = Con.fresh id.it pre_k in id.note <- Some c; c
      | Some c -> c
    in
    let ve' = match dec.it with
      | ClassD _ -> T.Env.add id.it T.Pre scope.val_env
      | _ -> scope.val_env
    in
    { val_env = ve';
      typ_env = T.Env.add id.it c scope.typ_env;
      con_env = T.ConSet.disjoint_add c scope.con_env;
      lib_env = scope.lib_env;
      obj_env = scope.obj_env;
    }

and gather_pat env ve pat : val_env =
  match pat.it with
  | WildP | LitP _ | SignP _ -> ve
  | VarP id -> gather_id env ve id
  | TupP pats -> List.fold_left (gather_pat env) ve pats
  | ObjP pfs -> List.fold_left (gather_pat_field env) ve pfs
  | TagP (_, pat1) | AltP (pat1, _) | OptP pat1
  | AnnotP (pat1, _) | ParP pat1 -> gather_pat env ve pat1

and gather_pat_field env ve pf : val_env =
  gather_pat env ve pf.it.pat

and gather_id env ve id : val_env =
  if T.Env.find_opt id.it ve <> None then
    error env id.at "duplicate definition for %s in block" id.it;
  T.Env.add id.it T.Pre ve

(* Pass 2 and 3: infer type definitions *)
and infer_block_typdecs env decs : scope =
  let _env', scope =
    List.fold_left (fun (env, scope) dec ->
      let scope' = infer_dec_typdecs env dec in
      adjoin env scope', adjoin_scope scope scope'
    ) (env, empty_scope) decs
  in scope

and infer_dec_typdecs env dec : scope =
  match dec.it with
  (* TODO: generalize beyond let <id> = <obje> *)
  | LetD (
      {it = VarP id; _},
      {it = ObjE ({it = sort; _}, fields); at; _}
    ) ->
    let decs = List.map (fun {it = {vis;dec}; _} -> dec) fields in
    let scope = T.Env.find id.it env.objs in
    let env' = adjoin env scope in
    let obj_scope = infer_block_typdecs env' decs in
    { empty_scope with
      con_env = obj_scope.con_env;
      val_env = T.Env.singleton id.it (object_of_scope env sort fields obj_scope at);
      obj_env = T.Env.singleton id.it obj_scope
    }
  (* TODO: generalize beyond let <id> = <valpath> *)
  | LetD ({it = VarP id; _}, exp) ->
    (match infer_val_path env exp with
     | None -> empty_scope
     | Some t ->
       match T.promote t with
       | T.Obj (_, _) as t' -> { empty_scope with val_env = T.Env.singleton id.it t' }
       | _ -> { empty_scope with val_env = T.Env.singleton id.it T.Pre }
    )
  | LetD _ | ExpD _ | VarD _ ->
    empty_scope
  | TypD (con_id, binds, typ) ->
    let c = T.Env.find con_id.it env.typs in
    let cs, ts, te, ce = check_typ_binds {env with pre = true} binds in
    let env' = adjoin_typs env te ce in
    let t = check_typ env' typ in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close (c::cs) t}) cs ts in
    let k = T.Def (tbs, T.close (c::cs) t) in
    if false then
    begin
      let is_typ_param c =
        match Con.kind c with
        | T.Def _ -> false
        | T.Abs( _, T.Pre) -> false (* an approximated type constructor *)
        | T.Abs( _, _) -> true in
      let typ_params = T.ConSet.filter is_typ_param env.cons in
      let cs_k = T.cons_kind k in
      let free_params = T.ConSet.inter typ_params cs_k in
      if not (T.ConSet.is_empty free_params) then
        error env dec.at
          "type definition %s %s references type parameter(s) %s from an outer scope"
          con_id.it
          (T.string_of_kind k)
          (String.concat ", " (T.ConSet.fold (fun c cs -> T.string_of_con c::cs) free_params []))
    end;
    { empty_scope with
      typ_env = T.Env.singleton con_id.it c;
      con_env = infer_id_typdecs con_id c k;
    }
  | ClassD (id, binds, sort, pat, self_id, fields) ->
    let c = T.Env.find id.it env.typs in
    let cs, ts, te, ce = check_typ_binds {env with pre = true} binds in
    let env' = adjoin_typs {env with pre = true} te ce in
    let _, ve = infer_pat env' pat in
    let self_typ = T.Con (T.Free c, List.map (fun c -> T.Con (T.Free c, [])) cs) in
    let env'' = add_val (adjoin_vals env' ve) self_id.it self_typ in
    let t = infer_obj env'' sort.it fields dec.at in
    let tbs = List.map2 (fun c' t -> {T.var = Con.name c'; bound = T.close (c::cs) t}) cs ts in
    let k = T.Def (tbs, T.close (c::cs) t) in
    { empty_scope with
      typ_env = T.Env.singleton id.it c;
      con_env = infer_id_typdecs id c k;
    }

and infer_id_typdecs id c k : con_env =
  assert (match k with T.Abs (_, T.Pre) -> false | _ -> true);
  (match Con.kind c with
  | T.Abs (_, T.Pre) -> T.set_kind c k; id.note <- Some c
  | k' ->  (* assert (T.eq_kind k' k) *) ()
  );
  T.ConSet.singleton c

(* Pass 4: infer value types *)
and infer_block_valdecs env decs scope : scope =
  let _, scope' =
    List.fold_left (fun (env, scope) dec ->
      let scope' = infer_dec_valdecs env dec in
      adjoin env scope', adjoin_scope scope scope'
    ) (env, scope) decs
  in scope'

and infer_dec_valdecs env dec : scope =
  match dec.it with
  | ExpD _ ->
    empty_scope
  (* TODO: generalize beyond let <id> = <obje> *)
  | LetD (
      {it = VarP id; _} as pat,
      {it = ObjE ({it = sort; _}, fields); at; _}
    ) ->
    let decs = List.map (fun ef -> ef.it.dec) fields in
    let obj_scope = T.Env.find id.it env.objs in
    let obj_scope' =
      infer_block_valdecs
        (adjoin {env with pre = true} obj_scope)
        decs empty_scope
    in
    let obj_typ = object_of_scope env sort fields obj_scope' at in
    let _ve = check_pat env obj_typ pat in
    { empty_scope with
      val_env = T.Env.singleton id.it obj_typ }
  | LetD (pat, exp) ->
    let t = infer_exp {env with pre = true} exp in
    let ve' = check_pat_exhaustive env t pat in
    { empty_scope with val_env = ve' }
  | VarD (id, exp) ->
    let t = infer_exp {env with pre = true} exp in
    { empty_scope with val_env = T.Env.singleton id.it (T.Mut t) }
  | TypD (con_id, binds, typ) ->
    let c = match con_id.note with
      | Some c -> c | _ -> assert false in
    { empty_scope with
      typ_env = T.Env.singleton con_id.it c;
      con_env = T.ConSet.singleton c ;
    }
  | ClassD (id, typ_binds, sort, pat, self_id, fields) ->
    let cs, ts, te, ce = check_typ_binds env typ_binds in
    let env' = adjoin_typs env te ce in
    let c = T.Env.find id.it env.typs in
    let t1, _ = infer_pat {env' with pre = true} pat in
    let ts1 = match pat.it with TupP _ -> T.as_seq t1 | _ -> [t1] in
    let t2 = T.Con (T.Free c, List.map (fun c -> T.Con (T.Free c, [])) cs) in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close cs t}) cs ts in
    let t = T.Func (T.Local, T.Returns, tbs, List.map (T.close cs) ts1, [T.close cs t2]) in
    { empty_scope with
      val_env = T.Env.singleton id.it t;
      typ_env = T.Env.singleton id.it c;
      con_env = T.ConSet.singleton c;
    }

(* Programs *)

let infer_prog scope prog : (T.typ * scope) Diag.result =
  Diag.with_message_store
    (fun msgs ->
       recover_opt
         (fun prog ->
           let env = env_of_scope msgs scope in
           let res = infer_block env prog.it prog.at in
           res
         )
         prog
    )

let infer_library env prog at =
  let typ,scope = infer_block env prog at in
  match prog with
  |  [{it = Syntax.ExpD _;_}] ->
     typ
  (* HACK: to be removed once we insist on single expression imports *)
  |  ds ->
     object_of_scope env T.Module (List.map (fun d -> { vis = Public @@ no_region ; dec = d } @@ no_region) ds) scope at

let check_library scope (filename, prog) : scope Diag.result =
  Diag.with_message_store
    (fun msgs ->
    recover_opt
      (fun prog ->
        let env = env_of_scope msgs scope in
        let typ = infer_library env prog.it prog.at in
        library_scope filename typ
      )
      prog
    )
