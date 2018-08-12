open Syntax
open Source

module T = Type


(* Error Handling *)

exception KindError of Source.region * string
exception TypeError of Source.region * string

let kind_error region fmt =
  Printf.ksprintf (fun s -> raise (KindError (region, s))) fmt

let type_error region fmt =
  Printf.ksprintf (fun s -> raise (TypeError (region, s))) fmt


(* Contexts *)

type pass =
  | TypDecPass  (* Declare type constructors (as abstract) *)
  | ValDecPass  (* Declare values and constructors *)
  | TypDefPass  (* Define type constructors *)
  | ValDefPass  (* Check value definition *)

type val_env = (T.typ * T.mut) T.Env.t
type typ_env = T.con T.Env.t
type con_env = T.con_env

type context =
  {
    vals : val_env;
    typs : typ_env;
    cons : con_env;
    labs : T.typ T.Env.t;
    rets : T.typ option;
    async : bool
  }

let empty_context =
  {
    vals = T.Env.empty;
    typs = T.Env.empty;
    cons = Con.Env.empty;
    labs = T.Env.empty;
    rets = None;
    async = false
  }

let adjoin_vals c ve = {c with vals = T.Env.adjoin c.vals ve}
let adjoin_typs c te = {c with typs = T.Env.adjoin c.typs te}
let adjoin_cons c ce = {c with cons = Con.Env.adjoin c.cons ce}

let add_lab c x t = {c with labs = T.Env.add x t c.labs}
let add_val c x t = {c with vals = T.Env.add x t c.vals}
let add_typ c x con k =
  {c with typs = T.Env.add x con c.typs; cons = Con.Env.add con k c.cons}

let disjoint_union at fmt env1 env2 =
  try T.Env.disjoint_union env1 env2 with T.Env.Overlap k -> type_error at fmt k
let disjoint_add at fmt k x env =
  try T.Env.disjoint_add k x env with T.Env.Overlap k -> type_error at fmt k

(* TBD
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
  match T.structural context.cons t  with
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
  | T.Array (_, t) -> t
  | _ -> assert false


(* TBR: the whole notion of sharable typ needs to be reviewed in the presence of subtyping *)
let rec is_shared_typ (context : context) (t : T.typ) : bool =
  match T.structural context.cons t with
  | T.Var (c, ts) ->
    (match Con.Env.find_opt c context.cons with
    | Some (T.Def (tbs, t) | T.Abs (tbs, t)) ->
      is_shared_typ context (T.subst (T.make_subst ts tbs) t) (* TBR *)
    | None -> assert false
    )
  | T.Prim p -> true
  | T.Array (m, t) ->
    m = T.Const && is_shared_typ context t
  | T.Tup ts ->
    List.for_all (is_shared_typ context) ts 
  | T.Func (tbs, t1, t2) ->
    (* TBR: a function type should be non-sharable if it closes over non-shareable locals *)
    let te, ce = is_shared_typ_binds context tbs in
    let context' = adjoin_cons (adjoin_typs context te) ce in
    List.for_all (fun tb -> is_shared_typ context' tb.T.bound) tbs && (* TBR *)
    is_shared_typ context' t1 &&
    (match T.structural context'.cons t2 with
    | T.Tup [] | T.Async _ -> true
    | _ -> false
    )
  | T.Opt t -> is_shared_typ context t
  | T.Async t -> is_shared_typ context t
  | T.Like t -> is_shared_typ context t
  | T.Obj (T.Object, fs) ->
    (* TBR: this isn't stable with subtyping *)
    List.for_all
      (fun {T.lab; typ; mut} -> mut = T.Const && is_shared_typ context typ) fs
  | T.Obj (T.Actor, fs) -> true
  | T.Any -> false (* TBR *)

and is_shared_typ_binds context ts =
  List.fold_left
    (fun (te, ce) {T.con; bound} ->
      T.Env.add (Con.name con) con te,
      Con.Env.add con (T.Abs ([], bound)) ce
    ) (context.typs, context.cons) ts



(* Types *)

let check_labs vars = ignore
  (List.fold_left
    (fun dom var ->
      if List.mem var.it dom
      then kind_error var.at "duplicate field name %s in object type" var.it
      else var.it::dom
    ) [] vars
  )

let rec check_typ context typ : T.typ =
  match typ.it with
  | VarT (var, typs) ->
    (match T.Env.find_opt var.it context.typs with
    | Some con ->
      let T.Def (tbs, t) | T.Abs (tbs, t) = Con.Env.find con context.cons in
      let ts = check_typ_bounds context tbs typs typ.at in
	    T.Var (con, ts)
    | None -> kind_error var.at "unbound type identifier %s" var.it
    )
  | PrimT prim ->
    T.Prim prim
  | ArrayT (mut, typ) ->
    T.Array (mut.it, check_typ context typ)
  | TupT typs ->
    T.Tup (List.map (check_typ context) typs)
  | FuncT (binds, typ1, typ2) ->
    let tbs, te, ce = check_typ_binds context binds in
    let context' = adjoin_cons (adjoin_typs context te) ce in
    T.Func (tbs, check_typ context' typ1, check_typ context' typ2)
  | OptT typ ->
    T.Opt (check_typ context typ)
  | AsyncT typ ->
    T.Async (check_typ context typ)
  | LikeT typ ->
    T.Like (check_typ context typ)
  | ObjT (actor, fields) ->
    (* fields distinct? *)
    check_labs (List.map (fun (field : Syntax.typ_field) -> field.it.var) fields);
    let fs = List.map (check_typ_field context actor.it) fields in
    T.Obj (actor.it, List.sort compare fs)
  | AnyT ->
    T.Any

and check_typ_field context actor typ_field : T.field =
  let {var; mut; typ} = typ_field.it in
  let t = check_typ context typ in
  if actor = T.Actor then begin
    if mut.it = T.Mut then
      type_error typ_field.at "public field %s of actor type is mutable"
        var.it;
    if not (is_shared_typ context t) then
      type_error typ_field.at
        "public field %s of actor type has non-shared type %s"
        var.it (T.string_of_typ t)
  end;
  {T.lab = var.it; mut = mut.it; typ = t}

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
    (* TBR: check against bound *)
    (* TBR: F-bounded checking with mutually recursive bounds? *)
    let sigma = T.make_subst [t] [tb] in
    let ts' = check_typ_bounds context (T.subst_binds sigma tbs') typs' at in
    t::ts'
  | [], [] -> []
  | [], _ -> type_error at "too many type arguments"
  | _, [] -> type_error at "too few type arguments"


(* Literals *)

let check_lit_val t of_string at s =
  try of_string s with _ ->
    type_error at "literal out of range for type %s"
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
    type_error at "expected expression of type %s, found literal of type %s"
      (T.string_of_typ t)
      (T.string_of_typ (T.Prim (infer_lit context lit at)))


(* Expressions *)

let rec infer_exp context exp : T.typ =
  let t, _ = infer_exp' context exp in
  exp.note <- T.structural context.cons t;
  t

and infer_exp_mut context exp : T.typ =
  let t, m = infer_exp' context exp in
  if m <> T.Mut then
    type_error exp.at "expected mutable assignment target";
  exp.note <- T.structural context.cons t;
  t

and infer_exp' context exp : T.typ * T.mut =
  match exp.it with
  | VarE id ->
    (match T.Env.find_opt id.it context.vals with
    | Some (t, mut) -> id.note <- mut; t, mut
    | None -> type_error id.at "unbound variable %s" id.it
    )
  | LitE lit ->
    T.Prim (infer_lit context lit exp.at), T.Const
  | UnE (op, exp1) ->
    let t = infer_exp context exp1 in
    if not (Operator.has_unop t op) then
      type_error exp.at "operator is not defined for operand type %s"
        (T.string_of_typ t);
    t, T.Const
  | BinE (exp1, op, exp2) ->
    let t1 = infer_exp context exp1 in
    let t2 = infer_exp context exp2 in
    if not (T.eq context.cons t1 t2) then
      type_error exp.at "operands have consistent types, %s vs %s"
        (T.string_of_typ t1) (T.string_of_typ t2);
    if not (Operator.has_binop t1 op) then
      type_error exp.at "operator not defined for operand type %s"
        (T.string_of_typ t1);
    t1, T.Const
  | RelE (exp1, op, exp2) ->
    let t1 = infer_exp context exp1 in
    let t2 = infer_exp context exp2 in
    if not (T.eq context.cons t1 t2) then
      type_error exp.at "operands have inconsistent types, %s vs %s"
        (T.string_of_typ t1) (T.string_of_typ t2);
    if not (Operator.has_relop exp1.note op) then
      type_error exp.at "operator is not defined for operand type %s"
        (T.string_of_typ t1);
    T.bool, T.Const
  | TupE exps ->
    let ts = List.map (infer_exp context) exps in
    T.Tup ts, T.Const
  | ProjE (exp1, n) ->
    (match T.structural context.cons (infer_exp context exp1) with
    | T.Tup ts ->
      (match List.nth_opt ts n with
      | Some t -> t, T.Const
      | None ->
        type_error exp.at "tuple projection %n >= %n is out-of-bounds"
          n (List.length ts)
      )
    | t ->
      type_error exp.at "expected tuple type, found %s" (T.string_of_typ t)
    )
  | ObjE (actor, _, _) ->
    (* TBR: infer object type *)
    assert false
  | DotE (exp1, id) ->
    (match T.structural context.cons (infer_exp context exp1) with
    | T.Obj (a, tfs) as t ->
      (match List.find_opt (fun {T.lab; _} -> lab = id.it) tfs with
      | Some {T.mut; typ = t; _} ->
        id.note <- mut;
        t, mut
      | None ->
        type_error exp1.at "object of type %s has no field named %s"
          (T.string_of_typ t) id.it
      )
    | t ->
      type_error exp1.at "expected object type, found %s" (T.string_of_typ t)
    )
  | AssignE (exp1, exp2) ->
    let t2 = infer_exp_mut context exp1 in
    check_exp context t2 exp2;
    T.unit, T.Const
  | ArrayE [] ->
    (* TBR: T.Bottom? *)
    type_error exp.at
      "cannot infer type of empty array (use a type annotation)"
  | ArrayE exps ->
    let ts = List.map (infer_exp context) exps in
    let t1 = List.hd ts in
    (* TBR: join *)
    if not (List.for_all (T.eq context.cons t1) (List.tl ts)) then
      type_error exp.at "array contains elements of inconsistent types";
    T.Array (T.Const, t1), T.Const
  | IdxE (exp1, exp2) ->
    (match T.structural context.cons (infer_exp context exp1) with
    | T.Array (m, t) -> 
      check_exp context T.nat exp2;
      t, m
    | t1 ->
      type_error exp1.at "expected array type, found %s" (T.string_of_typ t1)
    )
  | CallE (exp1, typs, exp2) ->
    (match T.structural context.cons (infer_exp context exp1) with
    | T.Func (tbs, t2, t) ->
      (* TBR: polymorphic instantiation, perhaps by matching? *)
      let ts = check_typ_bounds context tbs typs exp.at in
      let sigma = T.make_subst ts tbs in
      check_exp context (T.subst sigma t2) exp2;
      T.subst sigma t, T.Const
    | t1 ->
      type_error exp1.at "expected function type, found %s"
        (T.string_of_typ t1)
    )
  | BlockE exps ->
    check_block_local context T.unit exps exp.at;
    (* TBR: return last type *)
    T.unit, T.Const
  | NotE exp1 ->
    check_exp context T.bool exp1;
    T.bool, T.Const
  | AndE (exp1, exp2) ->
    check_exp context T.bool exp1;
    check_exp context T.bool exp2;
    T.bool, T.Const
  | OrE (exp1, exp2) ->
    check_exp context T.bool exp1;
    check_exp context T.bool exp2;
    T.bool, T.Const
  | IfE (exp1, exp2, exp3) ->
    check_exp context T.bool exp1;
    let t2 = infer_exp context exp2 in
    let t3 = infer_exp context exp3 in
    (* TBR: join *)
    if not (T.eq context.cons t2 t3) then
      type_error exp.at "if branches have inconsistent types, true is %s, false is %s"
        (T.string_of_typ t2) (T.string_of_typ t3);
    t2, T.Const
  | SwitchE (exp1, cases) ->
    let t1 = infer_exp context exp1 in
    if not (is_switch_typ context t1) then
      type_error exp1.at "expected switchable type, found %s"
        (T.string_of_typ t1);
    let t = infer_cases context t1 cases in
    t, T.Const
  | WhileE (exp1, exp2) ->
    check_exp context T.bool exp1;
    check_exp context T.unit exp2;
    T.unit, T.Const
  | LoopE (exp1, expo) ->
    check_exp context T.unit exp1;
    Lib.Option.app (check_exp context T.bool) expo;
    (* TBR: T.Bottom? *)
    T.unit, T.Const
  | ForE (pat, exp1, exp2)->
    (* TBR: generalise beyond arrays *)
    let t1 = infer_exp context exp1 in
    if not (is_iter_typ context t1) then
      type_error exp1.at "expected iterable type, found %s"
        (T.string_of_typ t1);
    let ve = check_pat context (elem_typ context t1) pat in
    check_exp (adjoin_vals context ve) T.unit exp2;
    T.unit, T.Const
  | LabelE (id, exp1) ->
    let t = infer_exp (add_lab context id.it T.unit) exp1 in
    t, T.Const
  | BreakE (id, exp1) ->
    (match T.Env.find_opt id.it context.labs with
    | Some t ->
      check_exp context t exp1;
      (* TBR: T.Bottom? *)
      T.unit, T.Const
    | None ->
      let name =
        match String.split_on_char ' ' id.it with
        | ["continue"; name] -> name
        | _ -> id.it
      in type_error id.at "unbound label %s" name
    )
  | RetE exp1 ->
    (match context.rets with
    | Some t ->
      check_exp context t exp1;
      (*TBR: T.Bottom? *)
      T.unit, T.Const
    | None ->
      type_error exp.at "misplaced return in global context"
    )
  | AsyncE exp1 ->
    let context' =
      {context with labs = T.Env.empty; rets = Some T.unit; async = true} in
    let t = infer_exp context' exp1 in
    T.Async t, T.Const
  | AwaitE exp1 ->
    if not context.async then
      type_error exp.at "misplaced await in synchronous context";
    (match T.structural context.cons (infer_exp context exp1) with
    | T.Async t -> t, T.Const
    | t1 ->
      type_error exp1.at "expected async type, found %s" (T.string_of_typ t1)
    )
  | AssertE exp1 ->
    check_exp context T.bool exp1;
    T.unit, T.Const
  | IsE (exp1, typ) ->
    (* TBR: what if T has free type variables? How will we check this, sans type passing? *) 
    let t1 = infer_exp context exp1 in
    let t = check_typ context typ in
    (* TBR: check that t <: t1 *)
    T.bool, T.Const
  | AnnotE (exp1, typ) ->
    let t = check_typ context typ in 
    check_exp context t exp1;
    t, T.Const
  | DecE {it = FuncD (id, _, _, _, _); _} ->
    (* TODO: don't special-case functions *)
    ignore (check_block context [exp]);
    (match T.Env.find_opt id.it context.vals with
    | Some (t, mut) -> t, mut
    | None -> assert false
    )
  | DecE _ ->
    let _ = check_block context [exp] in
    T.unit, T.Const
    

and check_exp context t exp =
  check_exp' context t exp;
  exp.note <- t

and check_exp' context t exp =
  match exp.it with
  | LitE lit ->
    check_lit context t lit exp.at
  | UnE (op, exp1) ->
    if not (Operator.has_unop t op) then
      type_error exp.at "operator cannot produce expected type %s"
        (T.string_of_typ t);
    check_exp context t exp1
  | BinE (exp1, op, exp2) ->
    if not (Operator.has_binop t op) then
      type_error exp.at "operator cannot produce expected type %s"
        (T.string_of_typ t);
    check_exp context t exp1;
    check_exp context t exp2
  | RelE (exp1, op, exp2) ->
    if not (T.eq context.cons t T.bool) then
      type_error exp.at "operator cannot produce expected type %s"
        (T.string_of_typ t);
    ignore (infer_exp context exp)
  | TupE exps ->
    (match T.structural context.cons t with
    | T.Tup ts when List.length ts = List.length exps ->
      List.iter2 (check_exp context) ts exps
    | _ ->
      type_error exp.at "tuple expression cannot produce expected type %s"
        (T.string_of_typ t)
    )
  | ArrayE exps ->
    (match T.structural context.cons t with
    | T.Array (m, t) ->
      List.iter (check_exp context t) exps
    | _ ->
      type_error exp.at "array expression cannot produce expected type %s"
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
      type_error exp.at "async expression cannot produce expected type %s"
        (T.string_of_typ t)
    )
  | BlockE exps ->
    check_block_local context t exps exp.at
  | IfE (exp1, exp2, exp3) ->
    check_exp context T.bool exp1;
    check_exp context t exp2;
    check_exp context t exp3
  | SwitchE (exp1, cases) ->
    let t1 = infer_exp context exp1 in
    if not (is_switch_typ context t1) then
      type_error exp1.at "expected switchable type, found %s"
        (T.string_of_typ t1);
    check_cases context t1 t cases
  | LabelE (id, exp) ->
    check_exp (add_lab context id.it t) t exp
  | LoopE _ | BreakE _ | RetE _ ->
    (* TBR: remove once we have T.Bottom and subtyping *)
    ignore (infer_exp context exp)
  | _ ->
    let t' = infer_exp context exp in
    if not (T.eq context.cons t t') then
      type_error exp.at "expected type %s, found %s"
        (T.string_of_typ t) (T.string_of_typ t')


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
      type_error exp.at
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

and infer_pat context pat : T.typ * val_env =
  match pat.it with
  | WildP ->
    type_error pat.at "cannot infer type of wildcard"
  | VarP _ ->
    type_error pat.at "cannot infer type of variable"
  | LitP lit ->
    T.Prim (infer_lit context lit pat.at), T.Env.empty
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
  match pat.it with
  | WildP ->
    T.Env.empty
  | VarP id ->
    T.Env.singleton id.it (t, T.Const)
  | LitP lit ->
    check_lit context t lit pat.at;
    T.Env.empty
  | TupP pats ->
    (match T.structural context.cons t with
    | T.Tup ts ->
      check_pats context ts pats T.Env.empty pat.at
    | _ ->
      type_error pat.at "tuple pattern cannot produce expected type %s"
        (T.string_of_typ t)
    )
  | _ ->
    let t', ve = infer_pat context pat in
    if not (T.eq context.cons t t') then
      type_error pat.at "expected type %s, found %s"
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
    type_error at "tuple pattern has %i fewer components than expected type"
      (List.length ts)
  | ts, [] ->
    type_error at "tuple pattern has %i more components than expected type"
      (List.length ts)

      
(* Blocks *)

and infer_block context es =
  match es with
  | [] -> T.unit
  | ({it = DecE _; _} as e)::es' ->
    let ve, te, ke = check_block context [e] in (* TBR: we currently check local decs sequentially, not recursively *)
    infer_block (adjoin_cons (adjoin_typs (adjoin_vals context ve) te) ke) es'
  | [e] -> infer_exp context e
  | e::es' ->
    check_exp context T.unit e; (* TBR: is this too strict? do we want to allow implicit discard? *)
    infer_block context es'

and check_block_local context t es at =
  match es with
  | [] ->
    if not (T.eq context.cons t T.unit)
    then type_error at "block  must end with expression of type" (T.string_of_typ t) 
  | ({it = DecE _; _} as e)::es' ->
    let ve, te, ke = check_block context [e] in (* TBR: we currently check local decs sequentially, not recursively *)
    check_block_local (adjoin_cons (adjoin_typs (adjoin_vals context ve) te) ke) t es' at
  | [e] -> check_exp context t e
  | e::es' ->
    check_exp context T.unit e; (* TBR: is this too strict? do we want to allow implicit discard? *)
    check_block_local context t es' at


(* Declarations *)

and check_dec pass context d =
  match d.it with
  | LetD (p, e) ->
    if pass < TypDefPass then T.Env.empty, T.Env.empty, Con.Env.empty else      
    let t = infer_exp context e in
    let ve = check_pat context t p in 
    ve, T.Env.empty, Con.Env.empty
  | VarD (v, t, e) ->
    if pass < TypDefPass then T.Env.empty, T.Env.empty, Con.Env.empty else
    let t = check_typ context t in
    check_exp context t e;
    T.Env.singleton v.it (t, T.Mut), T.Env.empty, Con.Env.empty
  | TypD (v, tps, t) ->
    let tbs, te_tbs, ke_tbs = check_typ_binds context tps in
    let con =
      if pass = TypDecPass
      then Con.fresh v.it
	    else T.Env.find v.it context.typs
    in
    let kind0 = T.Abs (tbs, T.Var (con, [])) in  (* dummy abstract type *)
    let te0 = T.Env.singleton v.it con in
    let ke0 = Con.Env.singleton con kind0 in
    if pass = TypDecPass then T.Env.empty, te0, ke0 else
    let context_tbs = adjoin_cons (adjoin_typs context te_tbs) ke_tbs in
    let t = check_typ context_tbs t in
    let kind1 = T.Def (tbs, t) in  (* dummy type *)
    let te1 = T.Env.singleton v.it con in
    let ke1 = Con.Env.singleton con kind1 in
    T.Env.empty, te1, ke1
  | FuncD (v, tps, p, t, e) ->
    if pass < TypDefPass then T.Env.empty, T.Env.empty, Con.Env.empty else
    let tbs, te, ke = check_typ_binds context tps in
    let context_te = adjoin_cons (adjoin_typs context te) ke in
    let t1, ve = infer_pat context_te p in
    let t2 = check_typ context_te t in
    let func_t = T.Func (tbs, t1, t2) in  (* TBR: we allow polymorphic recursion *)
    let context_te_ve_v =
      { (add_val (adjoin_vals context_te ve) v.it (func_t, T.Const)) with
	      labs = T.Env.empty;
        rets = Some t2;
        async = false
      } in
    check_exp context_te_ve_v t2 e;
    T.Env.singleton v.it (func_t, T.Const), T.Env.empty, Con.Env.empty
  | ClassD (a, v, tps, p, efs) ->
    let tbs, te_ts, ke_ts = check_typ_binds context tps in
    let con =
      if pass = TypDecPass
      then Con.fresh v.it
	    else T.Env.find v.it context.typs
    in
    let kind0 = T.Abs (tbs, T.Obj (a.it, [])) in
    let te0 = T.Env.singleton v.it con in
    let ke0 = Con.Env.singleton con kind0 in
    if pass = TypDecPass then T.Env.empty, te0, ke0 else
    let context_ts = adjoin_cons (adjoin_typs context te_ts) ke_ts in
    let context_ts_v = add_typ context_ts v.it con kind0 in
    let dom_t, ve = infer_pat context_ts_v p in
    let class_t = T.Var (con, List.map (fun tb -> T.Var (tb.T.con, [])) tbs) in
    let cons_t = T.Func (tbs, dom_t, class_t) (* TBR: we allow polymorphic recursion *) in
    let ve1te1ke1 = T.Env.singleton v.it (cons_t, T.Const), te0, ke0 in
    if pass = ValDecPass then ve1te1ke1 else
    let context_ts_v_dom =
      {context_ts_v with vals = T.Env.adjoin context_ts_v.vals ve} in
    let rec pre_members context field_env = function
      | [] -> field_env
      | {it = {var; mut; priv; exp = {it = AnnotE (e, t); _}}; _}::efs ->
        let t = check_typ context t in
        let field_env = disjoint_add var.at "duplicate field %s" var.it (mut.it, priv.it, t) field_env in
        pre_members context field_env efs
      | {it = {var; mut; priv; exp = {it = DecE {it = FuncD (v, us, p, t, e); _}; _}}; _}::efs ->
        let us, te_us, ke_us = check_typ_binds context us in
        let context_us = adjoin_cons (adjoin_typs context te_us) ke_us in
        let dom_t, _ = infer_pat context_us p in
        let rng_t = check_typ context_us t in
        let func_t = T.Func (us, dom_t, rng_t) in
        let field_env = disjoint_add var.at "duplicate field %s" var.it (mut.it, priv.it, func_t) field_env in
  	    pre_members context field_env efs
      | {it = {var; mut; priv; exp = e}; _}::efs ->
        let t = infer_exp context e in (* TBR: this feels wrong as we don't know all field types yet, just the ones to the left *)
        let field_env = disjoint_add var.at "duplicate field %s" var.it (mut.it, priv.it, t) field_env in
        pre_members context field_env efs
    in
    let pre_members = pre_members context_ts_v_dom T.Env.empty efs in
    let private_context = T.Env.map (fun (m, p, t) -> (t, m)) pre_members in
    let bindings = T.Env.bindings pre_members in
    let public_bindings = List.filter (fun (v, (m, p, t)) -> p = Public) bindings in
    let public_fields = List.map (fun (lab, (mut, _, typ)) -> {T.lab; typ; mut}) public_bindings in
    (* reject mutable or non-sharable public actor fields *)
    (* TBR: List.iter (check_typ_field context_ts_v_dom a.at a.it) public_fields; *)
    let kind2 = T.Abs (tbs, T.Obj (a.it, public_fields)) in
    let ve2, te2, ke2 = T.Env.singleton v.it (cons_t, T.Const), T.Env.singleton v.it con, Con.Env.singleton con kind2 in
    if pass = TypDefPass then ve2, te2, ke2 else
    let _ = assert (pass = ValDefPass) in
    let all_fields = List.map (fun (lab, (mut, _, typ)) -> {T.lab; typ; mut}) bindings in
    let kind3 = T.Abs (tbs, T.Obj (a.it, all_fields)) in
    let field_context =
      add_typ
        (add_val (adjoin_vals context_ts_v_dom private_context) v.it (cons_t, T.Const))
      	v.it con kind3
    in
    (* infer the fields *)
    List.iter (fun {it = {var; mut; exp; _}; _} -> ignore (infer_exp field_context exp)) efs;
    ve2, te2, ke2


and check_block context es =
  (* declare type constructors *)
  let ve0, te0, ke0 = check_block' TypDecPass context T.Env.empty T.Env.empty Con.Env.empty es in
  (* declare instance constructors, given type constructors *)
  let ve1, te1, ke1 = check_block' ValDecPass (adjoin_cons (adjoin_typs (adjoin_vals context ve0) te0) ke0) T.Env.empty T.Env.empty Con.Env.empty es in
  (* define type constructors (declare public member types) *)
  let ve2, te2, ke2 = check_block' TypDefPass (adjoin_cons (adjoin_typs (adjoin_vals context ve1) te1) ke1) T.Env.empty T.Env.empty Con.Env.empty es in
  (* check class definitions (check public and private member expressions *)
  let ve3, te3, ke3 = check_block' ValDefPass (adjoin_cons (adjoin_typs (adjoin_vals context ve2) te2) ke2) T.Env.empty T.Env.empty Con.Env.empty es in
  ve3, te3, ke3

and check_block' pass context ve te ke = function
  | [] -> ve, te, ke
  | e::es ->
    let d =
      match e.it with
      | DecE d -> d
      | _ -> LetD (WildP @@ e.at, e) @@ e.at
    in
    let ve1, te1, ke1 = check_dec pass context d in
    check_block' pass (adjoin_cons (adjoin_typs (adjoin_vals context ve1) te1) ke1) (T.Env.adjoin ve ve1) (T.Env.adjoin te te1) (Con.Env.adjoin ke ke1) es


(* Programs *)

let check_prog prog =
  check_block empty_context prog.it
