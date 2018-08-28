open Syntax
open Source

module T = Type
module A = Await

(* Error Handling *)

exception Error of Source.region * string

let error region fmt =
  Printf.ksprintf (fun s -> raise (Error (region, s))) fmt


(* Contexts *)

type val_env = T.typ T.Env.t
type typ_env = T.con T.Env.t
type con_env = T.con_env
type lab_env = T.typ T.Env.t
type ret_env = T.typ option

type scope = val_env * typ_env * con_env

type context =
  {
    vals : val_env;
    typs : typ_env;
    cons : con_env;
    labs : lab_env;
    rets : ret_env;
    async : bool;
    pre : bool
  }

let empty_context =
  {
    vals = T.Env.empty;
    typs = T.Env.empty;
    cons = Con.Env.empty;
    labs = T.Env.empty;
    rets = None;
    async = false;
    pre = false
  }

let add_lab c x t = {c with labs = T.Env.add x t c.labs}
let add_val c x t = {c with vals = T.Env.add x t c.vals}
let add_typ c x con k =
  {c with typs = T.Env.add x con c.typs; cons = Con.Env.add con k c.cons}

let adjoin c (ve, te, ce) =
  { c with
    vals = T.Env.adjoin c.vals ve;
    typs = T.Env.adjoin c.typs te;
    cons = Con.Env.adjoin c.cons ce
  }

let adjoin_vals c ve = {c with vals = T.Env.adjoin c.vals ve}
let adjoin_typs c te ce =
  {c with typs = T.Env.adjoin c.typs te; cons = Con.Env.adjoin c.cons ce}

let disjoint_union at fmt env1 env2 =
  try T.Env.disjoint_union env1 env2 with T.Env.Overlap k -> error at fmt k
(* TBD
let disjoint_add at fmt k x env =
  try T.Env.disjoint_add k x env with T.Env.Overlap k -> error at fmt k

let disjoint_union_vals at c ve =   {c with vals = disjoint_union at "duplicate value %s"  c.vals ve}
let disjoint_union_tys at c te = {c with typs = disjoint_union at "duplicate type %s" c.typs te}
let disjoint_add_val at c v t = disjoint_union_vals at c (T.Env.singleton v t)
let disjoint_add_typ at c con = disjoint_union_typs at c (T.Env.singleton v con)
*)


(* Type Analysis *)

(*
let is_num_typ context t =
  match T.structural context.cons t with
  | T.Prim (T.Nat | T.Int | T.Word8 | T.Word16 | T.Word32 | T.Word64 | T.Float) -> true
  | _ -> false

let is_bit_typ context t =
  match T.structural context.cons t with
  | T.Prim (T.Word8 | T.Word16 | T.Word32 | T.Word64) -> true
  | _ -> false

let is_ord_typ context t =
  match T.structural context.cons t with
  | T.Prim (T.Nat | T.Int | T.Word8 | T.Word16 | T.Word32 | T.Word64 | T.Float | T.Text | T.Char) -> true
  | _ -> false

let is_eq_typ context t =
  match T.structural context.cons t with
  | T.Prim (T.Bool | T.Nat | T.Int | T.Word8 | T.Word16 | T.Word32 | T.Word64 | T.Float | T.Text | T.Char) -> true
    (* TBR do we really want = and != on floats ?*)
  | _ -> false
*)

(* types one can switch on - all primitives except floats *)
(* TBR: switch on option type? *)
let is_switch_typ context t =
  match T.structural context.cons t with
  | T.Prim p -> p <> T.Float
  | _ -> false

(* types one can iterate over using `for`  *)
let is_iter_typ context t =
  match T.structural context.cons t with
  | T.Array _ -> true
  | _ -> false

(* element type of iterable_type  *)
let elem_typ context t =
  match T.structural context.cons t with
  | T.Array t -> T.immutable t
  | _ -> assert false

(* TBR: the whole notion of sharable typ needs to be reviewed in the presence of subtyping *)
let rec is_shared_typ (context : context) (t : T.typ) : bool =
  match T.structural context.cons t with
  | T.Var (c, ts) -> false
  | T.Prim p -> true
  | T.Array t -> is_shared_typ context t
  | T.Opt t -> is_shared_typ context t
  | T.Tup ts -> List.for_all (is_shared_typ context) ts 
    (* TBR: a function type should be non-sharable if it closes over non-shareable locals *)
  | T.Func _ as func_t -> is_async_typ context func_t
  | T.Async _ as async_t -> is_async_typ context async_t
  | T.Like t -> is_shared_typ context t
  | T.Obj (T.Object, fs) ->
    (* TBR: this isn't stable with subtyping *)
    List.for_all (fun {T.name; typ} -> is_shared_typ context typ) fs
  | T.Obj (T.Actor, fs) -> true
  | T.Mut _ -> false
  | T.Any -> false (* TBR *)
  | T.Pre -> assert false

and is_shared_typ_binds context ts =
  List.fold_left
    (fun (te, ce) {T.con; bound} ->
      T.Env.add (Con.name con) con te,
      Con.Env.add con (T.Abs ([], bound)) ce
    ) (context.typs, context.cons) ts

(* type of an actor field *)
and is_async_typ context t =
  match T.structural context.cons t with
  | T.Func (tbs, t1, t2) ->
    let te, ce = is_shared_typ_binds context tbs in
    let context' = adjoin_typs context te ce in
    is_shared_typ context' t1 &&
    (match T.structural context'.cons t2 with
    | T.Tup [] | T.Async _ -> true
    | _ -> false
    )
  | T.Async t -> is_shared_typ context t
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

let rec check_typ context typ : T.typ =
  match typ.it with
  | VarT (id, typs) ->
    (match T.Env.find_opt id.it context.typs with
    | Some con ->
      let T.Def (tbs, t) | T.Abs (tbs, t) = Con.Env.find con context.cons in
      let ts = check_typ_bounds context tbs typs typ.at in
	    T.Var (con, ts)
    | None -> error id.at "unbound type identifier %s" id.it
    )
  | PrimT prim ->
    T.Prim prim
  | ArrayT (mut, typ) ->
    let t = check_typ context typ in
    T.Array (infer_mut mut t)
  | TupT typs ->
    T.Tup (List.map (check_typ context) typs)
  | FuncT (binds, typ1, typ2) ->
    let tbs, te, ce = check_typ_binds context binds in
    let context' = adjoin_typs context te ce in
    T.Func (tbs, check_typ context' typ1, check_typ context' typ2)
  | OptT typ ->
    T.Opt (check_typ context typ)
  | AsyncT typ ->
    T.Async (check_typ context typ)
  | LikeT typ ->
    T.Like (check_typ context typ)
  | ObjT (sort, fields) ->
    check_ids (List.map (fun (field : typ_field) -> field.it.id) fields);
    let fs = List.map (check_typ_field context sort.it) fields in
    T.Obj (sort.it, List.sort compare fs)
  | AnyT ->
    T.Any

and check_typ_field context s typ_field : T.field =
  let {id; mut; typ} = typ_field.it in
  let t = infer_mut mut (check_typ context typ) in
  if s = T.Actor && not (is_async_typ context t) then
    error typ.at "actor field %s has non-async type %s"
      id.it (T.string_of_typ t);
  {T.name = id.it; typ = t}

and check_typ_binds context typ_binds : T.bind list * typ_env * con_env =
  match typ_binds with
  | [] -> [], T.Env.empty, Con.Env.empty
  | typ_bind::typ_binds' ->
    let {var; bound} = typ_bind.it in
    let con = Con.fresh var.it in
    let tb = {T.con; bound = check_typ context bound} in
    let k = T.Abs ([], tb.T.bound) in
    let context' = add_typ context var.it con k in
    let tbs', te', ce' = check_typ_binds context' typ_binds' in
    tb::tbs', T.Env.add var.it con te', Con.Env.add con k ce'

and check_typ_bounds context (tbs : T.bind list) typs at : T.typ list =
  match tbs, typs with
  | tb::tbs', typ::typs' ->
    let t = check_typ context typ in
    if not context.pre then begin
      (* TBR: check against bound *)
      (* TBR: F-bounded checking with mutually recursive bounds? *)
    end;
    let sigma = T.make_subst [t] [tb] in
    let ts' = check_typ_bounds context (T.subst_binds sigma tbs') typs' at in
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


let infer_lit context lit at : T.prim =
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
    (* TBR: default to nat, or replace defaulting with subtyping? *)
    lit := IntLit (check_int at s); (* default *)
    T.Int
  | PreLit (s, T.Int) ->
    lit := IntLit (check_int at s); (* default *)
    T.Int
  | PreLit (s, T.Float) ->
    lit := FloatLit (check_float at s); (* default *)
    T.Float
  | PreLit _ ->
    assert false

let rec check_lit context t lit at =
  match T.structural context.cons t, !lit with
  | T.Opt _, NullLit -> ()
  | T.Opt t, _ -> check_lit context t lit at
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
  | T.Prim t', _ when t' = infer_lit context lit at ->
    ()
  | t, _ ->
    error at "expected expression of type %s, found literal of type %s"
      (T.string_of_typ t)
      (T.string_of_typ (T.Prim (infer_lit context lit at)))


(* Expressions *)

let rec infer_exp context exp : T.typ =
  T.immutable (infer_exp_mut context exp)

and infer_exp_mut context exp : T.typ =
  assert (exp.note.note_typ = T.Pre);
  let t = infer_exp' context exp in
  if not context.pre then begin
    let e = A.infer_effect_exp exp in
    exp.note <- {note_typ = T.structural context.cons t; note_eff = e}
  end;
  t

and infer_exp' context exp : T.typ =
  match exp.it with
  | VarE id ->
    (match T.Env.find_opt id.it context.vals with
    | Some t ->
      if t = T.Pre then
        error id.at "cannot infer type of forward variable %s" id.it;
      t
    | None -> error id.at "unbound variable %s" id.it
    )
  | LitE lit ->
    T.Prim (infer_lit context lit exp.at)
  | UnE (op, exp1) ->
    let t1 = infer_exp context exp1 in
    let t = T.structural context.cons t1 in
    if not context.pre then begin
      if not (Operator.has_unop t op) then
        error exp.at "operator is not defined for operand type %s"
          (T.string_of_typ t)
    end;
    t
  | BinE (exp1, op, exp2) ->
    let t1 = infer_exp context exp1 in
    let t2 = infer_exp context exp2 in
    (* TBR: join *)
    let t = T.structural context.cons t1 in
    if not context.pre then begin
      if not (T.eq context.cons t1 t2) then
        error exp.at "operands have consistent types, %s vs %s"
          (T.string_of_typ t1) (T.string_of_typ t2);
      if not (Operator.has_binop t op) then
        error exp.at "operator not defined for operand type %s"
          (T.string_of_typ t)
    end;
    t
  | RelE (exp1, op, exp2) ->
    let t1 = infer_exp context exp1 in
    let t2 = infer_exp context exp2 in
    (* TBR: join *)
    let t = T.structural context.cons t1 in
    if not context.pre then begin
      if not (T.eq context.cons t1 t2) then
        error exp.at "operands have inconsistent types, %s vs %s"
          (T.string_of_typ t1) (T.string_of_typ t2);
      if not (Operator.has_relop t op) then
        error exp.at "operator is not defined for operand type %s"
          (T.string_of_typ t)
    end;
    T.bool
  | TupE exps ->
    let ts = List.map (infer_exp context) exps in
    T.Tup ts
  | ProjE (exp1, n) ->
    let t1 = infer_exp context exp1 in
    (match T.structural context.cons t1 with
    | T.Tup ts ->
      (match List.nth_opt ts n with
      | Some t -> t
      | None ->
        error exp.at "tuple projection %n >= %n is out-of-bounds"
          n (List.length ts)
      )
    | t ->
      error exp.at "expected tuple type, found %s" (T.string_of_typ t)
    )
  | ObjE (sort, id, fields) ->
    fst (infer_obj context sort.it id fields)
  | DotE (exp1, id) ->
    let t1 = infer_exp context exp1 in
    (match T.structural context.cons t1 with
    | T.Obj (a, tfs) as t ->
      (match List.find_opt (fun {T.name; _} -> name = id.it) tfs with
      | Some {T.typ = t; _} -> t
      | None ->
        error exp1.at "object of type %s has no field named %s"
          (T.string_of_typ t) id.it
      )
    | t ->
      error exp1.at "expected object type, found %s" (T.string_of_typ t)
    )
  | AssignE (exp1, exp2) ->
    if not context.pre then begin
      match infer_exp_mut context exp1 with
      | T.Mut t2 ->
        check_exp context t2 exp2
      | _ ->
        error exp.at "expected mutable assignment target";
    end;
    T.unit
  | ArrayE [] ->
    (* TBR: T.Bottom? *)
    error exp.at "cannot infer type of empty array (use a type annotation)"
  | ArrayE exps ->
    let ts = List.map (infer_exp context) exps in
    let t1 = List.hd ts in
    (* TBR: join *)
    if not (List.for_all (T.eq context.cons t1) (List.tl ts)) then
      error exp.at "array contains elements of inconsistent types";
    T.Array t1
  | IdxE (exp1, exp2) ->
    let t1 = infer_exp context exp1 in
    (match T.structural context.cons t1 with
    | T.Array t -> 
      if not context.pre then check_exp context T.nat exp2;
      t
    | t1 ->
      error exp1.at "expected array type, found %s" (T.string_of_typ t1)
    )
  | CallE (exp1, typs, exp2) ->
    let t1 = infer_exp context exp1 in
    (match T.structural context.cons t1 with
    | T.Func (tbs, t2, t) ->
      (* TBR: polymorphic instantiation, perhaps by matching? *)
      let ts = check_typ_bounds context tbs typs exp.at in
      let sigma = T.make_subst ts tbs in
      if not context.pre then check_exp context (T.subst sigma t2) exp2;
      T.subst sigma t
    | t1 ->
      error exp1.at "expected function type, found %s" (T.string_of_typ t1)
    )
  | BlockE decs ->
    let t = infer_block context decs in
    t
  | NotE exp1 ->
    if not context.pre then check_exp context T.bool exp1;
    T.bool
  | AndE (exp1, exp2) ->
    if not context.pre then begin
      check_exp context T.bool exp1;
      check_exp context T.bool exp2
    end;
    T.bool
  | OrE (exp1, exp2) ->
    if not context.pre then begin
      check_exp context T.bool exp1;
      check_exp context T.bool exp2
    end;
    T.bool
  | IfE (exp1, exp2, exp3) ->
    if not context.pre then check_exp context T.bool exp1;
    let t2 = infer_exp context exp2 in
    let t3 = infer_exp context exp3 in
    (* TBR: join *)
    if not (T.eq context.cons t2 t3) then
      error exp.at "if branches have inconsistent types, true is %s, false is %s"
        (T.string_of_typ t2) (T.string_of_typ t3);
    t2
  | SwitchE (exp1, cases) ->
    let t1 = if context.pre then T.Pre else infer_exp context exp1 in
    if not context.pre && not (is_switch_typ context t1) then
      error exp1.at "expected switchable type, found %s" (T.string_of_typ t1);
    let t = infer_cases context t1 cases in
    t
  | WhileE (exp1, exp2) ->
    if not context.pre then begin
      check_exp context T.bool exp1;
      check_exp context T.unit exp2
    end;
    T.unit
  | LoopE (exp1, expo) ->
    if not context.pre then begin
      check_exp context T.unit exp1;
      Lib.Option.app (check_exp context T.bool) expo
    end;
    (* TBR: T.Bottom? *)
    T.unit
  | ForE (pat, exp1, exp2) ->
    if not context.pre then begin
      (* TBR: generalise beyond arrays *)
      let t1 = infer_exp context exp1 in
      if not (is_iter_typ context t1) then
        error exp1.at "expected iterable type, found %s" (T.string_of_typ t1);
      let ve = check_pat context (elem_typ context t1) pat in
      check_exp (adjoin_vals context ve) T.unit exp2
    end;
    T.unit
  | LabelE (id, typ, exp1) ->
    let t = check_typ context typ in
    if not context.pre then check_exp (add_lab context id.it t) t exp1;
    t
  | BreakE (id, exp1) ->
    (match T.Env.find_opt id.it context.labs with
    | Some t ->
      if not context.pre then check_exp context t exp1
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
    if not context.pre then begin
      match context.rets with
      | Some t ->
        check_exp context t exp1;
        (*TBR: T.Bottom? *)
      | None ->
        error exp.at "misplaced return in global context"
    end;
    T.unit
  | AsyncE exp1 ->
    let context' =
      {context with labs = T.Env.empty; rets = Some T.Pre; async = true} in
    let t = infer_exp context' exp1 in
    T.Async t
  | AwaitE exp1 ->
    if not context.async then
      error exp.at "misplaced await in synchronous context";
    let t1 = infer_exp context exp1 in
    (match T.structural context.cons t1 with
    | T.Async t -> t
    | t1' ->
      error exp1.at "expected async type, found %s" (T.string_of_typ t1')
    )
  | AssertE exp1 ->
    if not context.pre then check_exp context T.bool exp1;
    T.unit
  | IsE (exp1, typ) ->
    if not context.pre then begin
      (* TBR: what if T has free type variables? How will we check this, sans type passing? *) 
      let _t1 = infer_exp context exp1 in
      let _t = check_typ context typ in
      (* TBR: check that t <: t1 *)
      ()
    end;
    T.bool
  | AnnotE (exp1, typ) ->
    let t = check_typ context typ in
    if not context.pre then check_exp context t exp1;
    t
  | DecE dec ->
    let t = infer_block context [dec] in
    t
    

and check_exp context t exp =
  assert (not context.pre);
  assert (exp.note.note_typ = T.Pre);
  check_exp' context t exp;
  if exp.note.note_typ = T.Pre then begin
    let e = A.infer_effect_exp exp in
    exp.note <- {note_typ = t; note_eff = e}
  end

and check_exp' context t exp =
  match exp.it with
  | LitE lit ->
    check_lit context t lit exp.at
  | UnE (op, exp1) ->
    if not (Operator.has_unop t op) then
      error exp.at "operator cannot produce expected type %s"
        (T.string_of_typ t);
    check_exp context t exp1
  | BinE (exp1, op, exp2) ->
    if not (Operator.has_binop t op) then
      error exp.at "operator cannot produce expected type %s"
        (T.string_of_typ t);
    check_exp context t exp1;
    check_exp context t exp2
  | RelE (exp1, op, exp2) ->
    if not (T.eq context.cons t T.bool) then
      error exp.at "operator cannot produce expected type %s"
        (T.string_of_typ t);
    ignore (infer_exp context exp)
  | TupE exps ->
    (match T.structural context.cons t with
    | T.Tup ts when List.length ts = List.length exps ->
      List.iter2 (check_exp context) ts exps
    | _ ->
      error exp.at "%s expression cannot produce expected type %s"
        (if exps = [] then "empty" else "tuple")
        (T.string_of_typ t)
    )
  | ArrayE exps ->
    (match T.structural context.cons t with
    | T.Array t1 ->
      List.iter (check_exp context (T.immutable t1)) exps
    | _ ->
      error exp.at "array expression cannot produce expected type %s"
       (T.string_of_typ t)
    )
  (* TBR: propagate mutablility via subtyping?
  | IdxE (exp1, exp2) ->
    check_exp context (T.Array (T.Const, t)) exp1;
    check_exp context T.nat exp2
  *)
  | AsyncE exp1 ->
    (match T.structural context.cons t with
    | T.Async t ->
      let context' =
        {context with labs = T.Env.empty; rets = Some t; async = true}
      in check_exp context' t exp1
    | _ ->
      error exp.at "async expression cannot produce expected type %s"
        (T.string_of_typ t)
    )
  | BlockE decs ->
    ignore (check_block context t decs exp.at)
  | IfE (exp1, exp2, exp3) ->
    check_exp context T.bool exp1;
    check_exp context t exp2;
    check_exp context t exp3
  | SwitchE (exp1, cases) ->
    let t1 = infer_exp context exp1 in
    if not (is_switch_typ context t1) then
      error exp1.at "expected switchable type, found %s" (T.string_of_typ t1);
    check_cases context t1 t cases
  | LoopE _ | BreakE _ | RetE _ ->
    (* TBR: remove once we have T.Bottom and subtyping *)
    ignore (infer_exp context exp)
  | _ ->
    let t' = infer_exp context exp in
    (* TBR: use subtyping *)
    if not (T.eq context.cons t t') then
      error exp.at "expected type %s, found %s"
        (T.string_of_typ t) (T.string_of_typ t')


(* Objects *)

and infer_obj context s id fields : T.typ * T.typ =
  (* TBR: rethink private *)
(*Printf.printf "[object] gather fields, context:\n";
print_ce context.cons;
print_ve context.vals;*)
  let pre_ve = gather_exp_fields id.it fields in
(*Printf.printf "[object] pre-infer fields\n";*)
  let pre_context = adjoin_vals {context with pre = true} pre_ve in
  let tfs, tfs_inner, ve = infer_exp_fields pre_context s id.it T.Pre fields in
  let t_inner = T.Obj (s, tfs_inner) in
(*print_ve ve;
Printf.printf "[object] infer fields, context:\n";
print_ce context.cons;
print_ve (adjoin_vals (add_val context id.it ((*t*) t_inner, T.Const)) ve).vals;*)
  if not context.pre then begin
    let context' = adjoin_vals (add_val context id.it (*t*) t_inner) ve in
    ignore (infer_exp_fields context' s id.it (*t*) t_inner fields)
  end;
(*Printf.printf "[object] done\n";*)
  T.Obj (s, tfs), t_inner

and gather_exp_fields id fields : val_env =
  let ve0 = T.Env.singleton id T.Pre in
  List.fold_left gather_exp_field ve0 fields

and gather_exp_field ve field : val_env =
  let {id; _} : exp_field' = field.it in
  if T.Env.mem id.it ve then
    error id.at "duplicate field name %s in object" id.it;
  T.Env.add id.it T.Pre ve

and infer_exp_fields context s id t fields : T.field list * T.field list * val_env =
  let context' = add_val context id t in
  let tfs, tfs_inner, ve =
    List.fold_left (infer_exp_field context' s) ([], [], T.Env.empty) fields in
  List.sort compare tfs, List.sort compare tfs_inner, ve

and infer_exp_field context s (tfs, tfs_inner, ve) field : T.field list * T.field list * val_env =
  let {id; exp; mut; priv} = field.it in
  let t1 = infer_exp (adjoin_vals context ve) exp in
  let t = infer_mut mut t1 in
  if not context.pre then begin
    if s = T.Actor && priv.it = Public && not (is_async_typ context t) then
      error field.at "public actor field %s has non-async type %s"
        id.it (T.string_of_typ t)
  end;
  let ve' = T.Env.add id.it t ve in
  let tfs_inner' = {T.name = id.it; typ = t} :: tfs_inner in
  let tfs' =
    if priv.it = Private then tfs else {T.name = id.it; typ = t} :: tfs
  in tfs', tfs_inner', ve'


(* Cases *)

(* TBR: compute refutability of pats and enforce accordingly (refutable in all cases but last, irrefutable elsewhere) *)

and infer_cases context t1 cases : T.typ =
  match cases with
  | [] ->
    (* TBR: T.Bottom? *)
    T.unit
  | {it = {pat; exp}; _}::cases' ->
    let ve = check_pat context t1 pat in
    let t = infer_exp (adjoin_vals context ve) exp in
    let t' = infer_cases context t cases' in
    (* TBR: join *)
    if not (T.eq context.cons t t') then
      error exp.at
        "switch branches have inconsistent types, this is %s, next is %s"
        (T.string_of_typ t) (T.string_of_typ t');
    t

and check_cases context t1 t2 cases =
  match cases with
  | [] -> ()
  | {it = {pat; exp}; _}::cases' ->
    let ve = check_pat context t1 pat in
    check_exp (adjoin_vals context ve) t2 exp;
    check_cases context t1 t2 cases'


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
  | AnnotP (pat1, _) ->
    gather_pat ve pat1


and infer_pat context pat : T.typ * val_env =
  let t, ve = infer_pat' context pat in
  if not context.pre then
    pat.note <- {note_typ = T.structural context.cons t; note_eff = T.Triv};
  t, ve

and infer_pat' context pat : T.typ * val_env =
  match pat.it with
  | WildP ->
    error pat.at "cannot infer type of wildcard"
  | VarP _ ->
    error pat.at "cannot infer type of variable"
  | LitP lit ->
    T.Prim (infer_lit context lit pat.at), T.Env.empty
  | SignP (op, lit) ->
    let t = T.Prim (infer_lit context lit pat.at) in
    if not (Operator.has_unop t op) then
      error pat.at "operator is not defined for operand type %s"
        (T.string_of_typ t);
    t, T.Env.empty
  | TupP pats ->
    let ts, ve = infer_pats pat.at context pats [] T.Env.empty in
    T.Tup ts, ve
  | AnnotP (pat1, typ) ->
    let t = check_typ context typ in
    t, check_pat context t pat1

and infer_pats at context pats ts ve : T.typ list * val_env =
  match pats with
  | [] -> List.rev ts, ve
  | pat::pats' ->
    let t, ve1 = infer_pat context pat in
    let ve' = disjoint_union at "duplicate binding for %s in pattern" ve ve1 in
    infer_pats at context pats' (t::ts) ve'


and check_pat context t pat : val_env =
  let ve = check_pat' context t pat in
  if not context.pre then
    pat.note <- {note_typ = T.structural context.cons t; note_eff = T.Triv};
  ve

and check_pat' context t pat : val_env =
  match pat.it with
  | WildP ->
    T.Env.empty
  | VarP id ->
    T.Env.singleton id.it t
  | LitP lit ->
    check_lit context t lit pat.at;
    T.Env.empty
  | SignP (op, lit) ->
    if not (Operator.has_unop t op) then
      error pat.at "operator cannot produce expected type %s"
        (T.string_of_typ t);
    check_lit context t lit pat.at;
    T.Env.empty
  | TupP pats ->
    (match T.structural context.cons t with
    | T.Tup ts ->
      check_pats context ts pats T.Env.empty pat.at
    | _ ->
      error pat.at "tuple pattern cannot produce expected type %s"
        (T.string_of_typ t)
    )
  | _ ->
    let t', ve = infer_pat context pat in
    if not (T.eq context.cons t t') then
      error pat.at "expected type %s, found %s"
        (T.string_of_typ t) (T.string_of_typ t');
    ve

and check_pats context ts pats ve at : val_env =
  match pats, ts with
  | [], [] -> ve
  | pat::pats', t::ts ->
    let ve1 = check_pat context t pat in
    let ve' = disjoint_union at "duplicate binding for %s in pattern" ve ve1 in
    check_pats context ts pats' ve' at
  | [], ts ->
    error at "tuple pattern has %i fewer components than expected type"
      (List.length ts)
  | ts, [] ->
    error at "tuple pattern has %i more components than expected type"
      (List.length ts)


(* Blocks and Declarations *)

and infer_block context decs : T.typ =
  let scope, ce_inner = infer_block_decs context decs in
(*Printf.printf "[block] infer expressions\n";
let t =*)
  infer_block_exps (adjoin context scope) ce_inner decs
(*in Printf.printf "[block] done\n"; t*)

and infer_block_exps context ce_inner decs : T.typ =
  match decs with
  | [] -> T.unit
  | [dec] -> infer_dec context ce_inner dec
  | dec::decs' ->
    if not context.pre then check_dec context ce_inner T.unit dec;
    infer_block_exps context ce_inner decs'

and infer_dec context ce_inner dec : T.typ =
  match dec.it with
  | ExpD exp ->
    infer_exp context exp
  | LetD (_, exp) | VarD (_, exp) ->
    if not context.pre then ignore (infer_exp context exp);
    T.unit
  | FuncD (id, typbinds, pat, typ, exp) ->
    let t= T.Env.find id.it context.vals in
    if not context.pre then begin
      let tbs, te, ce = check_typ_binds context typbinds in
      let context' = adjoin_typs context te ce in
      let _, ve = infer_pat context' pat in
      let t2 = check_typ context' typ in
      let context'' =
        {context' with labs = T.Env.empty; rets = Some t2; async = false} in
      check_exp (adjoin_vals context'' ve) t2 exp
    end;
    t
  | ClassD (id, typbinds, sort, pat, fields) ->
    let t = T.Env.find id.it context.vals in
    if not context.pre then begin
      let tbs, te, ce = check_typ_binds context typbinds in
      let context' = adjoin_typs context te ce in
      let c = T.Env.find id.it context.typs in
      let context' = (*context'*) add_typ context' id.it c (Con.Env.find c ce_inner) in
      let _, ve = infer_pat context' pat in
      ignore (infer_obj (adjoin_vals context' ve) sort.it ("anon-self" @@ no_region) fields)
    end;
    t
  | TypD _ ->
    T.unit


and check_block context t decs at : scope =
  let scope, ce_inner = infer_block_decs context decs in
(*Printf.printf "[block] check expressions\n";*)
  check_block_exps (adjoin context scope) ce_inner t decs at;
(*Printf.printf "[block] done\n";*)
  scope

and check_block_exps context ce_inner t decs at =
  match decs with
  | [] ->
    if not (T.eq context.cons t T.unit) then
      error at "empty block cannot produce type %s" (T.string_of_typ t)
  | [dec] ->
    check_dec context ce_inner t dec
  | dec::decs' ->
    check_dec context ce_inner T.unit dec;
    check_block_exps context ce_inner t decs' at

and check_dec context ce_inner t dec =
  match dec.it with
  | ExpD exp -> check_exp context t exp
  | _ ->
    let t' = infer_dec context ce_inner dec in
    (* TBR: special-case unit? *)
    if not (T.eq context.cons t T.unit || T.eq context.cons t t') then
      error dec.at "expected type %s, found %s"
        (T.string_of_typ t) (T.string_of_typ t');


and print_ce =
  Con.Env.iter (fun c k ->
    Printf.printf "  type %s %s\n" (Con.to_string c) (Type.string_of_kind k)
  )
and print_ve =
  Type.Env.iter (fun x t ->
    Printf.printf "  %s : %s\n" x (Type.string_of_typ t)
  )

and infer_block_decs context decs : scope * con_env =
(*Printf.printf "[block] gather types\n";*)
  let pre_ve, te, pre_ce = gather_block_typdecs decs in
(*Printf.printf "[block] pre-infer types\n";*)
  let context' = adjoin {context with pre = true} (pre_ve, te, pre_ce) in
  let ce, _ = infer_block_typdecs context' te decs in
(*Printf.printf "[block] infer types\n";*)
  let context'' = adjoin context (pre_ve, te, ce) in
  let ce', ce_inner = infer_block_typdecs context'' te decs in
  (* TBR: assertion does not work for types with binders, due to stamping *)
  (* assert (ce = ce'); *)
(*print_ce ce;*)
(*Printf.printf "[block] gather values\n";*)
  let pre_ve' = gather_block_valdecs decs in
(*Printf.printf "[block] infer values\n";*)
  let ve = infer_block_valdecs (adjoin_vals context'' pre_ve') decs in
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
    let pre_tbs = List.map (fun con -> {T.con; bound = T.Pre}) cs in
    let c = Con.fresh id.it in
    let pre_k = T.Abs (pre_tbs, T.Any) in
    let ve' =
      match dec.it with
      | ClassD _ ->
        let t2 = T.Var (c, List.map (fun c' -> T.Var (c', [])) cs) in
        T.Env.add id.it (T.Func (pre_tbs, T.Pre, t2)) ve
      | _ -> ve
    in ve', T.Env.add id.it c te, Con.Env.add c pre_k ce


(* Pass 2 and 3: infer type definitions *)
and infer_block_typdecs context te decs : con_env * con_env =
  List.fold_left (infer_dec_typdecs context te)
    (Con.Env.empty, Con.Env.empty) decs

and infer_dec_typdecs context te (ce, ce_inner) dec : con_env * con_env =
  match dec.it with
  | ExpD _ | LetD _ | VarD _ | FuncD _ -> ce, ce_inner
  | TypD (id, binds, typ) ->
    let c = T.Env.find id.it te in
    let tbs, te', ce' = check_typ_binds {context with pre = true} binds in
    let context' = adjoin_typs context te' ce' in
    let t = check_typ context' typ in
    Con.Env.add c (T.Def (tbs, t)) ce, ce_inner
  | ClassD (id, binds, sort, pat, fields) ->
    let c = T.Env.find id.it te in
    let tbs, te', ce' = check_typ_binds {context with pre = true} binds in
    let context' = {(adjoin_typs context te' ce') with pre = true} in
    let _, ve = infer_pat context' pat in
    let t, t_inner = infer_obj (adjoin_vals context' ve) sort.it ("anon-self" @@ no_region) fields in
    Con.Env.add c (T.Abs (tbs, t)) ce,
    Con.Env.add c (T.Abs (tbs, t_inner)) ce_inner


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
and infer_block_valdecs context decs : val_env =
  List.fold_left (infer_dec_valdecs context) T.Env.empty decs

and infer_dec_valdecs context ve dec : val_env =
  match dec.it with
  | ExpD _ ->
    ve
  | LetD (pat, exp) ->
    let t = infer_exp (adjoin_vals {context with pre = true} ve) exp in
    let ve' = check_pat context t pat in 
    T.Env.adjoin ve ve'
  | VarD (id, exp) ->
    let t = infer_exp (adjoin_vals {context with pre = true} ve) exp in
    T.Env.add id.it (T.Mut t) ve
  | FuncD (id, typbinds, pat, typ, _) ->
    let tbs, te, ce = check_typ_binds context typbinds in
    let context' = adjoin_typs context te ce in
    let t1, _ = infer_pat context' pat in
    let t2 = check_typ context' typ in
    T.Env.add id.it (T.Func (tbs, t1, t2)) ve
  | TypD _ ->
    ve
  | ClassD (id, typbinds, sort, pat, fields) ->
    let tbs, te, ce = check_typ_binds context typbinds in
    let context' = adjoin_typs context te ce in
    let c = T.Env.find id.it context.typs in
    let t1, _ = infer_pat context' pat in
    let t2 = T.Var (c, List.map (fun tb -> T.Var (tb.T.con, [])) tbs) in
    T.Env.add id.it (T.Func (tbs, t1, t2)) ve


(* Programs *)

let check_prog context prog : scope =
  check_block context T.unit prog.it prog.at
