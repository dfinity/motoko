open Syntax
open Source

module T = Type
module A = Await

(* Error Handling *)

exception Error of Source.region * string

let error at fmt =
  Printf.ksprintf (fun s -> raise (Error (at, s))) fmt

let warn at fmt =
  Printf.ksprintf (fun s ->
    Printf.eprintf "%s: warning, %s\n" (Source.string_of_region at) s;
  ) fmt


(* Contexts *)

type val_env = T.typ T.Env.t
type typ_env = T.con T.Env.t
type con_env = T.con_env
type lab_env = T.typ T.Env.t
type ret_env = T.typ option

type scope = val_env * typ_env * con_env

type env =
  { vals : val_env;
    typs : typ_env;
    cons : con_env;
    labs : lab_env;
    rets : ret_env;
    async : bool;
    pre : bool;
  }

let empty_env =
  { vals = T.Env.empty;
    typs = T.Env.empty;
    cons = Con.Env.empty;
    labs = T.Env.empty;
    rets = None;
    async = false;
    pre = false;
  }

let add_lab c x t = {c with labs = T.Env.add x t c.labs}
let add_val c x t = {c with vals = T.Env.add x t c.vals}
(*let add_con c con k = {c with cons = Con.Env.add con k c.cons}*)
let add_typ c x con k =
  { c with
    typs = T.Env.add x con c.typs;
    cons = Con.Env.add con k c.cons;
  }

let add_typs c xs cs ks =
  { c with
    typs = List.fold_right2 T.Env.add xs cs c.typs;
    cons = List.fold_right2 Con.Env.add cs ks c.cons;
  }

let adjoin c (ve, te, ce) =
  { c with
    vals = T.Env.adjoin c.vals ve;
    typs = T.Env.adjoin c.typs te;
    cons = Con.Env.adjoin c.cons ce;
  }

let adjoin_vals c ve = {c with vals = T.Env.adjoin c.vals ve}
let adjoin_cons c ce = {c with cons = Con.Env.adjoin c.cons ce}
let adjoin_typs c te ce =
  { c with
    typs = T.Env.adjoin c.typs te;
    cons = Con.Env.adjoin c.cons ce;
  }

let disjoint_union at fmt env1 env2 =
  try T.Env.disjoint_union env1 env2 with T.Env.Clash k -> error at fmt k


(* Type Analysis *)

(* Types one can iterate over using `for` *)
let is_iter_typ ce = function
  | T.Array _ -> true
  | _ -> false

(* Element type of iterable_type  *)
let elem_typ ce = function
  | T.Array t -> T.immutable t
  | _ -> assert false

(* TBR: the whole notion of sharable type needs to be reviewed in the presence of subtyping *)
let rec is_shared_typ ce t =
  match T.structural ce t with
  | T.Var _
  | T.Con _ -> false
  | T.Prim p -> true
  | T.Array t -> is_shared_typ ce t
  | T.Opt t -> is_shared_typ ce t
  | T.Tup ts -> List.for_all (is_shared_typ ce) ts 
    (* TBR: a function type should be non-sharable if it closes over non-shareable locals *)
  | T.Func _ as t' -> is_async_typ ce t'
  | T.Async _ as t' -> is_async_typ ce t'
  | T.Like t -> is_shared_typ ce t
  | T.Obj (T.Object, fs) ->
    (* TBR: this isn't stable with subtyping *)
    List.for_all (fun {T.name; typ} -> is_shared_typ ce typ) fs
  | T.Obj (T.Actor, fs) -> true
  | T.Mut _ -> false
  | T.Any -> false (* TBR *)
  | T.Pre -> assert false

(* Type of an actor field *)
and is_async_typ ce t =
  match T.structural ce t with
  | T.Func (tbs, t1, t2) ->
    let ts, ce' = T.open_binds ce tbs in
    is_shared_typ ce' (T.open_ ts t1) &&
    (match T.normalize ce' (T.open_ ts t2) with
    | T.Tup [] | T.Async _ -> true
    | _ -> false
    )
  | T.Async t -> is_shared_typ ce t
  | _ -> false


(* Types *)

let check_ids ids = ignore
  (List.fold_left
    (fun dom id ->
      if List.mem id.it dom
      then error id.at "duplicate field name %s in object type" id.it
      else id.it::dom
    ) [] ids
  )

let infer_mut mut : T.typ -> T.typ =
  match mut.it with
  | Const -> fun t -> t
  | Var -> fun t -> T.Mut t

let rec check_typ env typ : T.typ =
  match typ.it with
  | VarT (id, typs) ->
    (match T.Env.find_opt id.it env.typs with
    | Some c ->
      let T.Def (tbs, t) | T.Abs (tbs, t) = Con.Env.find c env.cons in
      let ts = check_typ_bounds env tbs typs typ.at in
	    T.Con (c, ts)
    | None -> error id.at "unbound type identifier %s" id.it
    )
  | PrimT "Any" ->
    T.Any
  | PrimT s ->
    (try T.Prim (T.prim s) with Invalid_argument _ ->
      error typ.at "unknown primitive type"
    )
  | ArrayT (mut, typ) ->
    let t = check_typ env typ in
    T.Array (infer_mut mut t)
  | TupT typs ->
    T.Tup (List.map (check_typ env) typs)
  | FuncT (binds, typ1, typ2) ->
    let cs, ts, te, ce = check_typ_binds env binds in
    let env' = adjoin_typs env te ce in
    let t1 = check_typ env' typ1 in
    let t2 = check_typ env' typ2 in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = t}) cs ts in
    T.Func (T.close_binds cs tbs, T.close cs t1, T.close cs t2)
  | OptT typ ->
    T.Opt (check_typ env typ)
  | AsyncT typ ->
    T.Async (check_typ env typ)
  | LikeT typ ->
    T.Like (check_typ env typ)
  | ObjT (sort, fields) ->
    check_ids (List.map (fun (field : typ_field) -> field.it.id) fields);
    let fs = List.map (check_typ_field env sort.it) fields in
    T.Obj (sort.it, List.sort compare fs)
  | AnyT ->
    T.Any

and check_typ_field env s typ_field : T.field =
  let {id; mut; typ} = typ_field.it in
  let t = infer_mut mut (check_typ env typ) in
  if s = T.Actor && not (is_async_typ env.cons t) then
    error typ.at "actor field %s has non-async type %s"
      id.it (T.string_of_typ t);
  {T.name = id.it; typ = t}

and check_typ_binds env typ_binds : T.con list * T.typ list * typ_env * con_env =
  let xs = List.map (fun typ_bind -> typ_bind.it.var.it) typ_binds in
  let cs = List.map (fun x -> Con.fresh x) xs in
  let te = List.fold_left2 (fun te typ_bind c ->
      let id = typ_bind.it.var in
      if T.Env.mem id.it te then
        error id.at "duplicate type name %s in type parameter list" id.it;
      T.Env.add id.it c te
    ) T.Env.empty typ_binds cs in
  let pre_ks = List.map (fun c -> T.Abs ([], T.Pre)) cs in
  let pre_env' = add_typs {env with pre = true} xs cs pre_ks in
  let ts = List.map (fun typ_bind -> check_typ pre_env' typ_bind.it.bound) typ_binds in
  let ks = List.map2 (fun c t -> T.Abs ([], t)) cs ts in
  let env' = add_typs env xs cs ks in
  let _ = List.map (fun typ_bind -> check_typ env' typ_bind.it.bound) typ_binds in
  cs, ts, te, Con.Env.from_list2 cs ks

and check_typ_bounds env (tbs : T.bind list) typs at : T.typ list =
  match tbs, typs with
  | tb::tbs', typ::typs' ->
    let t = check_typ env typ in
    if not env.pre then begin
      if not (T.sub env.cons t tb.T.bound) then
        error typ.at "type argument %s does not match parameter bound %s"
          (T.string_of_typ t) (T.string_of_typ tb.T.bound)
    end;
    let ts' = check_typ_bounds env tbs' typs' at in
    t::ts'
  | [], [] -> []
  | [], _ -> error at "too many type arguments"
  | _, [] -> error at "too few type arguments"


(* Literals *)

let check_lit_val t of_string at s =
  try of_string s with _ ->
    error at "literal out of range for type %s"
      (T.string_of_typ (T.Prim t))

let check_nat = check_lit_val T.Nat Value.Nat.of_string
let check_int = check_lit_val T.Int Value.Int.of_string
let check_word8 = check_lit_val T.Word8 Value.Word8.of_string_u
let check_word16 = check_lit_val T.Word16 Value.Word16.of_string_u
let check_word32 = check_lit_val T.Word32 Value.Word32.of_string_u
let check_word64 = check_lit_val T.Word64 Value.Word64.of_string_u
let check_float = check_lit_val T.Float Value.Float.of_string


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
    lit := NatLit (check_nat at s); (* default *)
    T.Nat
  | PreLit (s, T.Int) ->
    lit := IntLit (check_int at s); (* default *)
    T.Int
  | PreLit (s, T.Float) ->
    lit := FloatLit (check_float at s); (* default *)
    T.Float
  | PreLit _ ->
    assert false

let rec check_lit env t lit at =
  match T.normalize env.cons t, !lit with
  | T.Opt _, NullLit -> ()
  | T.Opt t', _ -> check_lit env t' lit at
  | T.Prim T.Nat, PreLit (s, T.Nat) ->
    lit := NatLit (check_nat at s)
  | T.Prim T.Int, PreLit (s, (T.Nat | T.Int)) ->
    lit := IntLit (check_int at s)
  | T.Prim T.Word8, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word8Lit (check_word8 at s)
  | T.Prim T.Word16, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word16Lit (check_word16 at s)
  | T.Prim T.Word32, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word32Lit (check_word32 at s)
  | T.Prim T.Word64, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word64Lit (check_word64 at s)
  | T.Prim T.Float, PreLit (s, (T.Nat | T.Int | T.Float)) -> 
    lit := FloatLit (check_float at s)
  | T.Prim t', _ when t' = infer_lit env lit at ->
    ()
  | t, _ ->
    error at "expected literal of type %s, found literal of type %s"
      (T.string_of_typ t)
      (T.string_of_typ (T.Prim (infer_lit env lit at)))


(* Expressions *)

let rec infer_exp env exp : T.typ =
  T.immutable (infer_exp_mut env exp)

and infer_exp_structural env exp : T.typ =
  let t = infer_exp env exp in
  let t' = T.structural env.cons t in
  if t' = T.Pre then
    error exp.at "cannot infer type of expression while trying to infer surrounding class type, because its type is a forward reference to type %s"
      (T.string_of_typ t);
  t'

and infer_exp_mut env exp : T.typ =
  assert (exp.note.note_typ = T.Pre);
  let t = infer_exp' env exp in
  assert (t <> T.Pre);
  if not env.pre then begin
    let e = A.infer_effect_exp exp in
    exp.note <- {note_typ = T.normalize env.cons t; note_eff = e}
  end;
  t

and infer_exp' env exp : T.typ =
  match exp.it with
  | PrimE _ ->
    error exp.at "cannot infer type of primitive"
  | VarE id ->
    (match T.Env.find_opt id.it env.vals with
    | Some T.Pre ->
      error id.at "cannot infer type of forward variable %s" id.it;
    | Some t -> t
    | None -> error id.at "unbound variable %s" id.it
    )
  | LitE lit ->
    T.Prim (infer_lit env lit exp.at)
  | UnE (op, exp1) ->
    let t1 = infer_exp_structural env exp1 in
    (* Special case for subtyping *)
    let t = if t1 = T.Prim T.Nat then T.Prim T.Int else t1 in
    if not env.pre then begin
      if not (Operator.has_unop t op) then
        error exp.at "operator is not defined for operand type %s"
          (T.string_of_typ t)
    end;
    t
  | BinE (exp1, op, exp2) ->
    let t1 = infer_exp_structural env exp1 in
    let t2 = infer_exp_structural env exp2 in
    let t = T.join env.cons t1 t2 in
    if not env.pre then begin
      if not (Operator.has_binop t op) then
        error exp.at "operator not defined for operand types %s and %s"
          (T.string_of_typ t1) (T.string_of_typ t2)
    end;
    t
  | RelE (exp1, op, exp2) ->
    let t1 = infer_exp_structural env exp1 in
    let t2 = infer_exp_structural env exp2 in
    let t = T.join env.cons t1 t2 in
    if not env.pre then begin
      if not (Operator.has_relop t op) then
        error exp.at "operator not defined for operand types %s and %s"
          (T.string_of_typ t1) (T.string_of_typ t2)
    end;
    T.bool
  | TupE exps ->
    let ts = List.map (infer_exp env) exps in
    T.Tup ts
  | OptE exp1 ->
    let t1 = infer_exp env exp1 in
    T.Opt t1
  | ProjE (exp1, n) ->
    let t1 = infer_exp_structural env exp1 in
    (match t1 with
    | T.Tup ts ->
      (match List.nth_opt ts n with
      | Some t -> t
      | None ->
        error exp.at "tuple projection %n >= %n is out-of-bounds"
          n (List.length ts)
      )
    | t1' ->
      error exp.at "expected tuple type, found %s" (T.string_of_typ t1')
    )
  | ObjE (sort, id, fields) ->
    fst (infer_obj env sort.it id fields)
  | DotE (exp1, id) ->
    let t1 = infer_exp_structural env exp1 in
    (match t1 with
    | T.Obj (_, tfs) as t ->
      (match List.find_opt (fun {T.name; _} -> name = id.it) tfs with
      | Some {T.typ = t; _} -> t
      | None ->
        error exp1.at "object of type %s has no field named %s"
          (T.string_of_typ t) id.it
      )
    | t1' ->
      error exp1.at "expected object type, found %s" (T.string_of_typ t1')
    )
  | AssignE (exp1, exp2) ->
    if not env.pre then begin
      match infer_exp_mut env exp1 with
      | T.Mut t2 ->
        check_exp env t2 exp2
      | _ ->
        error exp.at "expected mutable assignment target";
    end;
    T.unit
  | ArrayE [] ->
    (* TBR: T.Bottom? *)
    error exp.at "cannot infer type of empty array (use a type annotation)"
  | ArrayE exps ->
    let ts = List.map (infer_exp env) exps in
    let t1 = List.hd ts in
    (* TBR: join *)
    if not (List.for_all (T.eq env.cons t1) (List.tl ts)) then
      error exp.at "array contains elements of inconsistent types";
    T.Array t1
  | IdxE (exp1, exp2) ->
    let t1 = infer_exp_structural env exp1 in
    (match t1 with
    | T.Array t -> 
      if not env.pre then check_exp env T.nat exp2;
      t
    | t1' ->
      error exp1.at "expected array type, found %s" (T.string_of_typ t1')
    )
  | CallE (exp1, typs, exp2) ->
    let t1 = infer_exp_structural env exp1 in
    (match t1 with
    | T.Func (tbs, t2, t) ->
      let ts = check_typ_bounds env tbs typs exp.at in
      if not env.pre then check_exp env (T.open_ ts t2) exp2;
      T.open_ ts t
    | t1' ->
      error exp1.at "expected function type, found %s" (T.string_of_typ t1')
    )
  | BlockE decs ->
    let t, (_, _, ce) = infer_block env decs exp.at in
    (try T.avoid env.cons ce t with T.Unavoidable c ->
      error exp.at "inferred block type %s contains the local class type %s"
        (T.string_of_typ t) (Con.to_string c)
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
    let t = T.join env.cons t2 t3 in
    if
      t = T.Any &&
      T.structural env.cons t2 <> T.Any && T.structural env.cons t3 <> T.Any
    then
      warn exp.at "this if has type %s because branches have inconsistent types, true produces %s, false produces %s"
        (T.string_of_typ t) (T.string_of_typ t2) (T.string_of_typ t3);
    t
  | SwitchE (exp1, cases) ->
    let t1 = if env.pre then T.Pre else infer_exp_structural env exp1 in
    let t = infer_cases env t1 None cases in
    if not env.pre then
      if not (Coverage.check_cases cases) then
        warn exp.at "the cases in this switch do not cover all possible values";
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
    (* TBR: T.Bottom? *)
    T.unit
  | ForE (pat, exp1, exp2) ->
    if not env.pre then begin
      (* TBR: generalise beyond arrays *)
      let t1 = infer_exp_structural env exp1 in
      if not (is_iter_typ env.cons t1) then
        error exp1.at "expected iterable type, found %s" (T.string_of_typ t1);
      let ve = check_pat_exhaustive env (elem_typ env.cons t1) pat in
      check_exp (adjoin_vals env ve) T.unit exp2
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
      in error id.at "unbound label %s" name
    );
    (* TBR: T.Bottom? *)
    T.unit
  | RetE exp1 ->
    if not env.pre then begin
      match env.rets with
      | Some t ->
        check_exp env t exp1;
        (*TBR: T.Bottom? *)
      | None ->
        error exp.at "misplaced return"
    end;
    T.unit
  | AsyncE exp1 ->
    let env' =
      {env with labs = T.Env.empty; rets = Some T.Pre; async = true} in
    let t = infer_exp env' exp1 in
    T.Async t
  | AwaitE exp1 ->
    if not env.async then
      error exp.at "misplaced await";
    let t1 = infer_exp_structural env exp1 in
    (match t1 with
    | T.Async t -> t
    | t1' ->
      error exp1.at "expected async type, found %s" (T.string_of_typ t1')
    )
  | AssertE exp1 ->
    if not env.pre then check_exp env T.bool exp1;
    T.unit
  | IsE (exp1, typ) ->
    if not env.pre then begin
      (* TBR: what if T has free type variables? How will we check this, sans type passing? *) 
      let _t1 = infer_exp env exp1 in
      let _t = check_typ env typ in
      (* TBR: check that t <: t1 *)
      ()
    end;
    T.bool
  | AnnotE (exp1, typ) ->
    let t = check_typ env typ in
    if not env.pre then check_exp env t exp1;
    t
  | DecE dec ->
    let t, (_, _, ce) = infer_block env [dec] exp.at in
    (try T.avoid env.cons ce t with T.Unavoidable c ->
      error exp.at "inferred declaration type %s contains the local class type %s"
        (T.string_of_typ t) (Con.to_string c)
    )
    

and check_exp env t exp =
  assert (not env.pre);
  assert (exp.note.note_typ = T.Pre);
  assert (t <> T.Pre);
  let t' = T.normalize env.cons t in
  check_exp' env t' exp;
  let e = A.infer_effect_exp exp in
  exp.note <- {note_typ = t'; note_eff = e}

and check_exp' env t exp =
  match exp.it with
  | PrimE s ->
    (match T.nonopt env.cons t with
    | T.Func _ -> ()
    | _ ->
      error exp.at "primitive expression cannot produce expected type %s"
        (T.string_of_typ t)
    )
  | LitE lit ->
    check_lit env t lit exp.at
  | UnE (op, exp1) ->
    if not (Operator.has_unop t op) then
      error exp.at "operator cannot produce expected type %s"
        (T.string_of_typ t);
    check_exp env t exp1
  | BinE (exp1, op, exp2) ->
    if not (Operator.has_binop t op) then
      error exp.at "operator cannot produce expected type %s"
        (T.string_of_typ t);
    check_exp env t exp1;
    check_exp env t exp2
  | RelE (exp1, op, exp2) ->
    if not (T.sub env.cons t T.bool) then
      error exp.at "operator cannot produce expected type %s"
        (T.string_of_typ t);
    ignore (infer_exp env exp)
  | TupE exps ->
    (match T.nonopt env.cons t with
    | T.Tup ts when List.length ts = List.length exps ->
      List.iter2 (check_exp env) ts exps
    | _ ->
      error exp.at "%s expression cannot produce expected type %s"
        (if exps = [] then "empty" else "tuple")
        (T.string_of_typ t)
    )
  | OptE exp1 ->
    (match T.normalize env.cons t with
    | T.Opt t1 ->
      check_exp env t1 exp1
    | _ ->
      error exp.at "option expression cannot produce expected type %s"
        (T.string_of_typ t)
    )
  | ObjE (sort, id, fields) ->
    (match T.nonopt env.cons t with
    | T.Obj (s, tfs) when s = sort.it ->
      ignore (check_obj env s tfs id fields exp.at)
    | _ ->
      error exp.at "%s expression cannot produce expected type %s"
        (if sort.it = T.Actor then "actor" else "object")
        (T.string_of_typ t)
    )
  | ArrayE exps ->
    (match T.nonopt env.cons t with
    | T.Array t1 ->
      List.iter (check_exp env (T.immutable t1)) exps
    | _ ->
      error exp.at "array expression cannot produce expected type %s"
        (T.string_of_typ t)
    )
  | AsyncE exp1 ->
    (match T.nonopt env.cons t with
    | T.Async t ->
      let env' = {env with labs = T.Env.empty; rets = Some t; async = true} in
      check_exp env' t exp1
    | _ ->
      error exp.at "async expression cannot produce expected type %s"
        (T.string_of_typ t)
    )
  | BlockE decs ->
    ignore (check_block env t decs exp.at)
  | IfE (exp1, exp2, exp3) ->
    check_exp env T.bool exp1;
    check_exp env t exp2;
    check_exp env t exp3
  | SwitchE (exp1, cases) ->
    let t1 = infer_exp_structural env exp1 in
    check_cases env t1 t cases;
    if not (Coverage.check_cases cases) then
      warn exp.at "the cases in this switch do not cover all possible values";
  | LoopE _ | BreakE _ | RetE _ ->
    (* TBR: remove once we have T.Bottom and subtyping? *)
    ignore (infer_exp env exp)
  | _ ->
    let t' = infer_exp env exp in
    if not (T.sub env.cons t' t) then
      error exp.at "expected type %s, found %s"
        (T.string_of_typ t) (T.string_of_typ t')


(* Cases *)

and infer_cases env t1 t2o cases : T.typ =
  match cases with
  | [] ->
    (* TBR: default to T.Bottom? *)
    Lib.Option.get t2o T.unit
  | {it = {pat; exp}; at; _}::cases' ->
    let ve = check_pat env t1 pat in
    let t = infer_exp (adjoin_vals env ve) exp in
    let t' =
      match t2o with
      | None -> t
      | Some t'' ->
        let t' = T.join env.cons t t'' in
        if
          t' = T.Any &&
          T.structural env.cons t <> T.Any && T.structural env.cons t'' <> T.Any
        then
          warn at "the switch has type %s because branches have inconsistent types, this case produces %s, the previous %s"
            (T.string_of_typ t') (T.string_of_typ t) (T.string_of_typ t'');
        t'
    in infer_cases env t (Some t') cases'

and check_cases env t1 t2 cases =
  match cases with
  | [] -> ()
  | {it = {pat; exp}; _}::cases' ->
    let ve = check_pat env t1 pat in
    check_exp (adjoin_vals env ve) t2 exp;
    check_cases env t1 t2 cases'


(* Patterns *)

and gather_pat ve pat : val_env =
  match pat.it with
  | WildP | LitP _ | SignP _ ->
    ve
  | VarP id ->
    if T.Env.mem id.it ve then
      error pat.at "duplicate binding for %s in block" id.it;
    T.Env.add id.it T.Pre ve
  | TupP pats ->
    List.fold_left gather_pat ve pats
  | AltP (pat1, pat2) ->
    gather_pat ve pat1
  | OptP pat1
  | AnnotP (pat1, _) ->
    gather_pat ve pat1


and infer_pat_exhaustive env pat : T.typ * val_env =
  let t, ve = infer_pat env pat in
  if not env.pre then
    if not (Coverage.check_pat pat) then
      warn pat.at "this pattern does not cover all possible values";
  t, ve

and infer_pat env pat : T.typ * val_env =
  assert (pat.note.note_typ = T.Pre);
  let t, ve = infer_pat' env pat in
  if not env.pre then
    pat.note <- {note_typ = T.normalize env.cons t; note_eff = T.Triv};
  t, ve

and infer_pat' env pat : T.typ * val_env =
  match pat.it with
  | WildP ->
    error pat.at "cannot infer type of wildcard"
  | VarP _ ->
    error pat.at "cannot infer type of variable"
  | LitP lit ->
    T.Prim (infer_lit env lit pat.at), T.Env.empty
  | SignP (op, lit) ->
    let t1 = T.Prim (infer_lit env lit pat.at) in
    (* Special case for subtyping *)
    let t = if t1 = T.Prim T.Nat then T.Prim T.Int else t1 in
    if not (Operator.has_unop t op) then
      error pat.at "operator is not defined for operand type %s"
        (T.string_of_typ t);
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
    let t = T.join env.cons t1 t2 in
    if ve1 <> T.Env.empty || ve2 <> T.Env.empty then
      error pat.at "variables are not allowed in pattern alternatives";
    t, T.Env.empty
  | AnnotP (pat1, typ) ->
    let t = check_typ env typ in
    t, check_pat env t pat1

and infer_pats at env pats ts ve : T.typ list * val_env =
  match pats with
  | [] -> List.rev ts, ve
  | pat::pats' ->
    let t, ve1 = infer_pat env pat in
    let ve' = disjoint_union at "duplicate binding for %s in pattern" ve ve1 in
    infer_pats at env pats' (t::ts) ve'


and check_pat_exhaustive env t pat : val_env =
  let ve = check_pat env t pat in
  if not env.pre then
    if not (Coverage.check_pat pat) then
      warn pat.at "this pattern does not cover all possible values";
  ve

and check_pat env t pat : val_env =
  assert (pat.note.note_typ = T.Pre);
  if t = T.Pre then snd (infer_pat env pat) else
  let t' = T.normalize env.cons t in
  let ve = check_pat' env t pat in
  if not env.pre then pat.note <- {note_typ = t'; note_eff = T.Triv};
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
      let t' = T.normalize env.cons t in
      if not (Operator.has_unop t op) then
        error pat.at "operator cannot consume expected type %s"
          (T.string_of_typ t');
      check_lit env t' lit pat.at
    end;
    T.Env.empty
  | TupP pats ->
    (match t with
    | T.Tup ts ->
      check_pats env ts pats T.Env.empty pat.at
    | _ ->
      error pat.at "tuple pattern cannot consume expected type %s"
        (T.string_of_typ t)
    )
  | OptP pat1 ->
    (match t with
    | T.Opt t1 | t1 ->
      check_pat env t1 pat1
    )
  | AltP (pat1, pat2) ->
    let ve1 = check_pat env t pat1 in
    let ve2 = check_pat env t pat2 in
    if ve1 <> T.Env.empty || ve2 <> T.Env.empty then
      error pat.at "variables are not allowed in pattern alternatives";
    T.Env.empty
  | _ ->
    let t', ve = infer_pat env pat in
    if not (T.sub env.cons t' t) then
      error pat.at "expected type %s, found %s"
        (T.string_of_typ t) (T.string_of_typ t');
    ve

and check_pats env ts pats ve at : val_env =
  match pats, ts with
  | [], [] -> ve
  | pat::pats', t::ts ->
    let ve1 = check_pat env t pat in
    let ve' = disjoint_union at "duplicate binding for %s in pattern" ve ve1 in
    check_pats env ts pats' ve' at
  | [], ts ->
    error at "tuple pattern has %i fewer components than expected type"
      (List.length ts)
  | ts, [] ->
    error at "tuple pattern has %i more components than expected type"
      (List.length ts)


(* Objects *)

and infer_obj env s id fields : T.typ * T.typ =
  (* TBR: rethink private *)
(*Printf.printf "[object] gather fields, env:\n";
print_ce env.cons;
print_ve env.vals;*)
  let pre_ve = gather_exp_fields id.it fields in
(*Printf.printf "[object] pre-infer fields\n";*)
  let pre_env = adjoin_vals {env with pre = true} pre_ve in
  let tfs, tfs_inner, ve = infer_exp_fields pre_env s id.it T.Pre fields in
  let t_inner = T.Obj (s, tfs_inner) in
(*print_ve ve;
Printf.printf "[object] infer fields, env:\n";
print_ce env.cons;
print_ve (adjoin_vals (add_val env id.it ((*t*) t_inner, T.Const)) ve).vals;*)
  if not env.pre then begin
    let env' = adjoin_vals (add_val env id.it (*t*) t_inner) ve in
    ignore (infer_exp_fields env' s id.it (*t*) t_inner fields)
  end;
(*Printf.printf "[object] done\n";*)
  T.Obj (s, tfs), t_inner


and check_obj env s tfs id fields at : T.typ =
  (* TBR: rethink private *)
(*Printf.printf "[object] gather fields, env:\n";
print_ce env.cons;
print_ve env.vals;*)
  let pre_ve = gather_exp_fields id.it fields in
  let pre_ve' = List.fold_left (fun ve {T.name; typ = t} ->
      if not (T.Env.mem name ve) then
        error at "%s expression cannot produce expected type %s, field %s is missing"
          (if s = T.Actor then "actor" else "object")
          (T.string_of_typ t) name;
      T.Env.add name t ve
    ) pre_ve tfs
  in
(*Printf.printf "[object] pre-infer fields\n";*)
  let pre_env = adjoin_vals {env with pre = true} pre_ve' in
  let _, tfs_inner, ve = infer_exp_fields pre_env s id.it T.Pre fields in
  let t_inner = T.Obj (s, tfs_inner) in
(*print_ve ve;
Printf.printf "[object] infer fields, env:\n";
print_ce env.cons;
print_ve (adjoin_vals (add_val env id.it ((*t*) t_inner, T.Const)) ve).vals;*)
  let env' = adjoin_vals (add_val env id.it (*t*) t_inner) ve in
  ignore (infer_exp_fields env' s id.it (*t*) t_inner fields);
(*Printf.printf "[object] done\n";*)
  t_inner


and gather_exp_fields id fields : val_env =
  let ve0 = T.Env.singleton id T.Pre in
  List.fold_left gather_exp_field ve0 fields

and gather_exp_field ve field : val_env =
  let {id; _} : exp_field' = field.it in
  if T.Env.mem id.it ve then
    error id.at "duplicate field name %s in object" id.it;
  T.Env.add id.it T.Pre ve


and infer_exp_fields env s id t fields : T.field list * T.field list * val_env =
  let env' = add_val env id t in
  let tfs, tfs_inner, ve =
    List.fold_left (infer_exp_field env' s) ([], [], T.Env.empty) fields in
  List.sort compare tfs, List.sort compare tfs_inner, ve

and infer_exp_field env s (tfs, tfs_inner, ve) field : T.field list * T.field list * val_env =
  let {id; exp; mut; priv} = field.it in
  let t =
    match T.Env.find id.it env.vals with
    | T.Pre -> infer_mut mut (infer_exp (adjoin_vals env ve) exp)
    | t ->
      (* When checking object in analysis mode *)
      if not env.pre then
        check_exp (adjoin_vals env ve) (T.immutable t) exp;
      t
  in
  if not env.pre then begin
    if s = T.Actor && priv.it = Public && not (is_async_typ env.cons t) then
      error field.at "public actor field %s has non-async type %s"
        id.it (T.string_of_typ t)
  end;
  let ve' = T.Env.add id.it t ve in
  let tfs_inner' = {T.name = id.it; typ = t} :: tfs_inner in
  let tfs' =
    if priv.it = Private then tfs else {T.name = id.it; typ = t} :: tfs
  in tfs', tfs_inner', ve'


(*
and check_exp_fields env s tfs id t fields : T.field list * val_env =
  let env' = add_val env id t in
  let tfs_inner, ve =
    List.fold_left (check_exp_field env' s tfs) ([], T.Env.empty) fields in
  List.sort compare tfs_inner, ve

and check_exp_field env s tfs (tfs_inner, ve) field : T.field list * val_env =
  let {id; exp; mut; priv} = field.it in
  if priv = Private then begin
    let _, tfs_inner', ve' =
      infer_exp_field env s ([], tfs_inner, ve) field
    in tfs_inner', ve'
  end else begin
    check_exp (adjoin_vals env ve) (T.Env.find env.vals id.it) exp;
    if s = T.Actor && priv.it = Public && not (is_async_typ env t) then
      error field.at "public actor field %s has non-async type %s"
        id.it (T.string_of_typ t)
  end;
  let ve' = T.Env.add id.it t ve in
  let tfs_inner' = {T.name = id.it; typ = t} :: tfs_inner in
  tfs_inner', ve'
*)


(* Blocks and Declarations *)

and infer_block env decs at : T.typ * scope =
  let _, _, ce as scope, ce_inner = infer_block_decs env decs in
  let t = infer_block_exps (adjoin env scope) ce_inner decs in
  t, scope

and infer_block_exps env ce_inner decs : T.typ =
  match decs with
  | [] -> T.unit
  | [dec] -> infer_dec env ce_inner dec
  | dec::decs' ->
    if not env.pre then check_dec env ce_inner T.unit dec;
    infer_block_exps env ce_inner decs'

and infer_dec env ce_inner dec : T.typ =
  match dec.it with
  | ExpD exp ->
    infer_exp env exp
  | LetD (_, exp) | VarD (_, exp) ->
    if not env.pre then ignore (infer_exp env exp);
    T.unit
  | FuncD (id, typbinds, pat, typ, exp) ->
    let t = T.Env.find id.it env.vals in
    if not env.pre then begin
      let _cs, _ts, te, ce = check_typ_binds env typbinds in
      let env' = adjoin_typs env te ce in
      let _, ve = infer_pat_exhaustive env' pat in
      let t2 = check_typ env' typ in
      let env'' =
        {env' with labs = T.Env.empty; rets = Some t2; async = false} in
      check_exp (adjoin_vals env'' ve) t2 exp
    end;
    t
  | ClassD (id, typbinds, sort, pat, fields) ->
    let t = T.Env.find id.it env.vals in
    if not env.pre then begin
      let _cs, _ts, te, ce = check_typ_binds env typbinds in
      let env' = adjoin_typs env te ce in
      let c = T.Env.find id.it env.typs in
      let env' = (*env'*) add_typ env' id.it c (Con.Env.find c ce_inner) in
      let _, ve = infer_pat_exhaustive env' pat in
      let env'' =
        {env' with labs = T.Env.empty; rets = None; async = false} in
      ignore (infer_obj (adjoin_vals env'' ve) sort.it ("anon-self" @@ no_region) fields)
    end;
    t
  | TypD _ ->
    T.unit


and check_block env t decs at : scope =
  let scope, ce_inner = infer_block_decs env decs in
(*Printf.printf "[block] check expressions\n";*)
  check_block_exps (adjoin env scope) ce_inner t decs at;
(*Printf.printf "[block] done\n";*)
  scope

and check_block_exps env ce_inner t decs at =
  match decs with
  | [] ->
    if not (T.sub env.cons T.unit t) then
      error at "empty block cannot produce type %s" (T.string_of_typ t)
  | [dec] ->
    check_dec env ce_inner t dec
  | dec::decs' ->
    check_dec env ce_inner T.unit dec;
    check_block_exps env ce_inner t decs' at

and check_dec env ce_inner t dec =
  match dec.it with
  | ExpD exp -> check_exp env t exp
(* TBR: push in external type annotation;
   unfortunately, this is enough, because of the earlier recursive phases
  | FuncD (id, [], pat, typ, exp) ->
    (* TBR: special-case unit? *)
    if T.eq env.cons t T.unit then
      ignore (infer_dec env ce_inner dec)
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
    let t' = infer_dec env ce_inner dec in
    (* TBR: special-case unit? *)
    if not (T.eq env.cons t T.unit || T.sub env.cons t' t) then
      error dec.at "expected type %s, found %s"
        (T.string_of_typ t) (T.string_of_typ t');


(*
and print_ce =
  Con.Env.iter (fun c k ->
    Printf.printf "  type %s %s\n" (Con.to_string c) (Type.string_of_kind k)
  )
and print_ve =
  Type.Env.iter (fun x t ->
    Printf.printf "  %s : %s\n" x (Type.string_of_typ t)
  )
*)


and infer_block_decs env decs : scope * con_env =
(*Printf.printf "[block] gather types\n";*)
  let pre_ve, te, pre_ce = gather_block_typdecs decs in
(*Printf.printf "[block] pre-infer types\n";*)
  let env' = adjoin {env with pre = true} (pre_ve, te, pre_ce) in
  let ce, _ = infer_block_typdecs env' decs in
(*Printf.printf "[block] infer types\n";*)
  let env'' = adjoin env (pre_ve, te, ce) in
  let ce', ce_inner = infer_block_typdecs env'' decs in
  (* TBR: assertion does not work for types with binders, due to stamping *)
  (* assert (ce = ce'); *)
(*print_ce ce;*)
(*Printf.printf "[block] gather values\n";*)
  let pre_ve' = gather_block_valdecs decs in
(*Printf.printf "[block] infer values\n";*)
  let ve = infer_block_valdecs (adjoin_vals env'' pre_ve') decs in
(*print_ve ve;*)
  (ve, te, ce), ce_inner


(* Pass 1: collect type identifiers and their arity *)
and gather_block_typdecs decs : scope =
  List.fold_left gather_dec_typdecs
    (T.Env.empty, T.Env.empty, Con.Env.empty) decs

and gather_dec_typdecs (ve, te, ce) dec : scope =
  match dec.it with
  | ExpD _ | LetD _ | VarD _ | FuncD _ -> ve, te, ce
  | TypD (id, binds, _) | ClassD (id, binds, _, _, _) ->
    if T.Env.mem id.it te then
      error dec.at "duplicate definition for type %s in block" id.it;
    let cs =
      List.map (fun (bind : typ_bind) -> Con.fresh bind.it.var.it) binds in
    let pre_tbs = List.map (fun c -> {T.var = Con.name c; bound = T.Pre}) cs in
    let c = Con.fresh id.it in
    let pre_k = T.Abs (pre_tbs, T.Pre) in
    let ve' =
      match dec.it with
      | ClassD _ ->
        let t2 = T.Con (c, List.map (fun c' -> T.Con (c', [])) cs) in
        T.Env.add id.it (T.Func (pre_tbs, T.Pre, t2)) ve
      | _ -> ve
    in ve', T.Env.add id.it c te, Con.Env.add c pre_k ce


(* Pass 2 and 3: infer type definitions *)
and infer_block_typdecs env decs : con_env * con_env =
  let _env', ce, ce_inner =
    List.fold_left (fun (env, ce, ce_inner) dec ->
      let ce', ce_inner' = infer_dec_typdecs env dec in
      adjoin_cons env ce', Con.Env.adjoin ce ce',
        Con.Env.adjoin ce_inner ce_inner'
    ) (env, Con.Env.empty, Con.Env.empty) decs
  in ce, ce_inner

and infer_dec_typdecs env dec : con_env * con_env =
  match dec.it with
  | ExpD _ | LetD _ | VarD _ | FuncD _ ->
    Con.Env.empty, Con.Env.empty
  | TypD (id, binds, typ) ->
    let c = T.Env.find id.it env.typs in
    let cs, ts, te, ce = check_typ_binds {env with pre = true} binds in
    let env' = adjoin_typs env te ce in
    let t = check_typ env' typ in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close cs t}) cs ts in
    Con.Env.singleton c (T.Def (tbs, T.close cs t)), Con.Env.empty
  | ClassD (id, binds, sort, pat, fields) ->
    let c = T.Env.find id.it env.typs in
    let cs, ts, te, ce = check_typ_binds {env with pre = true} binds in
    let env' = adjoin_typs {env with pre = true} te ce in
    let _, ve = infer_pat env' pat in
    let t, t_inner = infer_obj (adjoin_vals env' ve) sort.it ("anon-self" @@ no_region) fields in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close cs t}) cs ts in
    Con.Env.singleton c (T.Abs (tbs, T.close cs t)),
    Con.Env.singleton c (T.Abs (tbs, T.close cs t_inner))


(* Pass 4: collect value identifiers *)
and gather_block_valdecs decs : val_env =
  List.fold_left gather_dec_valdecs T.Env.empty decs

and gather_dec_valdecs ve dec : val_env =
  match dec.it with
  | ExpD _ | TypD _ ->
    ve
  | LetD (pat, _) ->
    gather_pat ve pat
  | VarD (id, _) | FuncD (id, _, _, _, _) | ClassD (id, _, _, _, _) ->
    if T.Env.mem id.it ve then
      error dec.at "duplicate definition for %s in block" id.it;
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
  | FuncD (id, typbinds, pat, typ, _) ->
    let cs, ts, te, ce = check_typ_binds env typbinds in
    let env' = adjoin_typs env te ce in
    let t1, _ = infer_pat {env' with pre = true} pat in
    let t2 = check_typ env' typ in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close cs t}) cs ts in
    T.Env.singleton id.it (T.Func (tbs, T.close cs t1, T.close cs t2))
  | TypD _ ->
    T.Env.empty
  | ClassD (id, typbinds, sort, pat, fields) ->
    let cs, ts, te, ce = check_typ_binds env typbinds in
    let env' = adjoin_typs env te ce in
    let c = T.Env.find id.it env.typs in
    let t1, _ = infer_pat {env' with pre = true} pat in
    let t2 = T.Con (c, List.map (fun c -> T.Con (c, [])) cs) in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close cs t}) cs ts in
    T.Env.singleton id.it (T.Func (tbs, T.close cs t1, T.close cs t2))


(* Programs *)

let check_prog env prog : scope =
  check_block env T.unit prog.it prog.at

let infer_prog env prog : T.typ * scope =
  infer_block env prog.it prog.at
