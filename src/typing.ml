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
    labels : T.typ T.Env.t;
    returns : T.typ option;
    awaitable : bool
  }

let empty_context =
  {
    vals = T.Env.empty;
    typs = T.Env.empty;
    cons = Con.Env.empty;
    labels = T.Env.empty;
    returns = None;
    awaitable = false
  }

let adjoin_vals c ve = {c with vals = T.Env.adjoin c.vals ve}
let adjoin_typs c te = {c with typs = T.Env.adjoin c.typs te}
let adjoin_cons c ce = {c with cons = Con.Env.adjoin c.cons ce}

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
  match T.normalize context.cons t with
  | T.Prim (T.Nat | T.Int | T.Word8 | T.Word16 | T.Word32 | T.Word64 | T.Float) -> true
  | _ -> false

let is_bit_typ context t =
  match T.normalize context.cons t  with
  | T.Prim (T.Word8 | T.Word16 | T.Word32 | T.Word64) -> true
  | _ -> false

let is_ord_typ context t =
  match T.normalize context.cons t with
  | T.Prim (T.Nat | T.Int | T.Word8 | T.Word16 | T.Word32 | T.Word64 | T.Float | T.Text | T.Char) -> true
  | _ -> false

let is_eq_typ context t =
  match T.normalize context.cons t with
  | T.Prim (T.Bool | T.Nat | T.Int | T.Word8 | T.Word16 | T.Word32 | T.Word64 | T.Float | T.Text | T.Char) -> true
    (* TBR do we really want = and != on floats ?*)
  | _ -> false
*)

(* types one can switch on - all primitives except floats *)
(* TBR - switch on option type? *)
let is_switch_typ context t =
  match T.normalize context.cons t with
  | T.Prim p -> p <> T.Float
  | _ -> false

(* types one can iterate over using `for`  *)
let is_iter_typ context t =
  match T.normalize context.cons t with
  | T.Array _ -> true
  | _ -> false

(* element type of iterable_type  *)
let elem_typ context t =
  match T.normalize context.cons t with
  | T.Array (_, t) -> t
  | _ -> assert false


(* TBR: the whole notion of sharable typ needs to be reviewed in the presence of subtyping *)
let rec is_shared_typ (context : context) (t : T.typ) : bool =
  match T.normalize context.cons t with
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
    let te, ce = is_shared_typ_binds context tbs in
    let context' = adjoin_cons (adjoin_typs context te) ce in
    List.for_all (fun tb -> is_shared_typ context' tb.T.bound) tbs && (* TBR *)
    is_shared_typ context' t1 &&
    (match T.normalize context'.cons t2 with
    | T.Tup [] | T.Async _ -> true
    | _ -> false
    )
    (* TBR: a function type should be non-sharable if it closes over non-shareable locals *)
  | T.Opt t -> is_shared_typ context t
  | T.Async t -> is_shared_typ context t
  | T.Like t -> is_shared_typ context t
  | T.Obj (T.Object, fs) ->
    (*TBR: this isn't stable with subtyping *)
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



(* Checking Types *)

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



(* Checking Literals *)

let check_lit_val p f at s =
  try f s with _ ->
    type_error at "literal out of range for type %s"
      (T.string_of_typ (T.Prim p))

let check_nat = check_lit_val T.Nat Value.Nat.of_string_u
let check_int = check_lit_val T.Int Value.Int.of_string_s
let check_word8 = check_lit_val T.Word8 Value.Word8.of_string_u
let check_word16 = check_lit_val T.Word16 Value.Word16.of_string_u
let check_word32 = check_lit_val T.Word32 Value.Word32.of_string_u
let check_word64 = check_lit_val T.Word64 Value.Word64.of_string_u

(* begin sanity test *)

let pow2 n = 1 lsl n
(*let lpow2 n = Int64.shift_left (Int64.of_int 1) n*)
let raises f r x = try f r x; false; with _ -> true

let _ =
  let module I64 = Wasm.I64 in
  (* text check_nat *)
  assert (check_nat no_region (string_of_int (pow2 Value.nat_width - 1)) = I64.of_int_u (pow2 Value.nat_width - 1));
  assert (check_nat no_region (string_of_int 0) = I64.of_int_u 0);
(*
  assert (raises check_nat no_region (string_of_int (pow2 63)));
  assert (raises check_nat no_region (string_of_int (pow2 64 + 1)));
*)

  (* test check_word16 *)
  assert (check_word16 no_region (string_of_int (pow2 16 - 1)) = Value.Word16.of_int_u (pow2 16 - 1));
  assert (check_word16 no_region (string_of_int 0) = Value.Word16.of_int_u 0);
  assert (raises check_word16 no_region (string_of_int (pow2 16)));
  assert (raises check_word16 no_region (string_of_int (pow2 16 + 1)));

  (* test check_int *)
  assert (check_int no_region (string_of_int (pow2 (Value.int_width - 1) - 1)) = I64.of_int_s (pow2 (Value.int_width - 1) - 1));
  assert (check_int no_region (string_of_int (- pow2 (Value.int_width - 1))) = I64.of_int_s (- pow2 (Value.int_width - 1)))
(*
  assert (raises check_int no_region (Int64.to_string (lpow2 (Value.int_width - 1))));
  assert (raises check_int no_region (Int64.to_string (Int64.neg (Int64.add (lpow2 (Value.int_width - 1)) 1L))));
*)
(* end sanity test *)


let infer_lit context rl =
  match !rl with
  | NullLit -> T.Prim T.Null
  | BoolLit _ -> T.Prim T.Bool
  | NatLit _ -> T.Prim T.Nat
  | IntLit _ -> T.Prim T.Int
  | Word8Lit _ -> T.Prim T.Word8
  | Word16Lit _ -> T.Prim T.Word16
  | Word32Lit _ -> T.Prim T.Word32
  | Word64Lit _ -> T.Prim T.Word64
  | FloatLit _ -> T.Prim T.Float
  | CharLit _ -> T.Prim T.Char
  | TextLit _ -> T.Prim T.Text
  | PreLit s ->
    rl := IntLit (Value.Int.of_string s); (* default *)
    T.Prim T.Int

let rec check_lit at context t rl =
  match T.normalize context.cons t, !rl with
  | T.Prim T.Null, NullLit
  | T.Prim T.Nat, NatLit _
  | T.Prim T.Int, IntLit _
  | T.Prim T.Word8, Word8Lit _
  | T.Prim T.Word16, Word16Lit _
  | T.Prim T.Word32, Word32Lit _
  | T.Prim T.Word64, Word64Lit _
  | T.Prim T.Float, FloatLit _ -> ()
  | T.Prim T.Nat, PreLit s -> rl := NatLit (check_nat at s)
  | T.Prim T.Int, PreLit s -> rl := IntLit (check_int at s)
  | T.Prim T.Word8, PreLit s -> rl := Word8Lit (check_word8 at s)
  | T.Prim T.Word16, PreLit s -> rl := Word16Lit (check_word16 at s)
  | T.Prim T.Word32, PreLit s -> rl := Word32Lit (check_word32 at s)
  | T.Prim T.Word64, PreLit s -> rl := Word64Lit (check_word64 at s)
  | T.Prim _, _ ->
    let u = infer_lit context rl in
    if not (T.eq context.cons t u) then
      type_error at "expect literal of type %s, found literal of type %s"
        (T.string_of_typ t) (T.string_of_typ u)
  | T.Opt _, NullLit -> ()
  | T.Opt t, _ -> check_lit at context t rl
  | _ -> type_error at "type %s has no literals" (T.string_of_typ t)



(* Checking Operators *)

let rec infer_binop context at e1 op e2 =
  let t1 = infer_exp context e1 in
  let t2 = infer_exp context e2 in
  if T.eq context.cons t1 t2
  then
    if Operator.has_binop e1.note op
    then t1
    else type_error at "binary operator not available at argument type %s" (T.string_of_typ t1)
  else type_error at "arguments to binary operator must have equivalent types, found distinct types %s and %s" (T.string_of_typ t1) (T.string_of_typ t2)
 
and check_binop context at t e1 op e2 =
  if Operator.has_binop t op
  then (check_exp context t e1;
        check_exp context t e2)
  else type_error at "binary operator not available at expected type %s" (T.string_of_typ t)
 
and infer_relop context at e1 op e2 =
  let t1 = infer_exp context e1 in
  let t2 = infer_exp context e2 in
  if T.eq context.cons t1 t2
  then if Operator.has_relop e1.note op
       then T.bool
       else type_error at "relational operator not available at argument type %s" (T.string_of_typ t1)
  else type_error at "arguments to relational operator must have equivalent types, found arguments of distinct types %s and %s" (T.string_of_typ t1) (T.string_of_typ t2)

and check_relop context at t e1 op e2 =
  if not (T.eq context.cons t T.bool)
  then type_error at "expecting value of non-boolean type %s, relational operator returns a value of Bool type" (T.string_of_typ t);
  ignore(infer_relop context at e1 op e2)
 
and infer_unop context at op e =
  let t = infer_exp context e in
  if Operator.has_unop e.note op
  then t
  else type_error at "unary operator not available at argument type %s" (T.string_of_typ t)
  
and check_unop context at t op e =
  check_exp context t e;
  if Operator.has_unop e.note op
  then ()
  else type_error at "unary operator not available at expected type %s" (T.string_of_typ t);


and infer_exp context e =
  let t = infer_exp' context e in
  e.note <- T.normalize context.cons t;
  t

and infer_exp' context e =
  match e.it with
  | VarE x ->
    (match T.Env.find_opt x.it context.vals with
    | Some (t, mut) -> x.note <- mut; t
    | None -> type_error x.at "unbound identifier %s" x.it
    )
  | LitE rl ->
    infer_lit context rl
  | UnE (uop, e1) ->
    infer_unop context e.at uop e1
  | BinE (e1, bop, e2) ->
    infer_binop context e.at e1 bop e2
  | RelE (e1, rop, e2) ->
    infer_relop context e.at e1 rop e2    
  | TupE es ->
    let ts = List.map (infer_exp context) es in
    T.Tup ts
  | ProjE (e, n) ->
    (match T.structural context.cons (infer_exp context e) with
    | T.Tup ts ->
      (try List.nth ts n
      with Failure _ -> type_error e.at "tuple projection %n >= %n is out-of-bounds" n (List.length ts))
    | t -> type_error e.at "expecting tuple type, found %s" (T.string_of_typ t)
    )
  | ObjE (actor, _, _) ->
    (* TBR *)
    assert false
  | DotE (e, v) ->
    (match T.structural context.cons (infer_exp context e) with
    | T.Obj (a, tfs) as t ->
      (try
        let {T.mut; typ; _} = List.find (fun {T.lab; _} -> lab = v.it) tfs in
  	    v.note <- mut;
        typ
      with Not_found -> type_error e.at "object of type %s has no field named %s" (T.string_of_typ t) v.it)
    | t -> type_error e.at "expecting object type, found %s" (T.string_of_typ t)
    )
  | AssignE (e1, e2) ->
    (*TBC: array and object update *)
    (match e1.it with
    | VarE v ->
      (match T.Env.find_opt v.it context.vals with
      | Some (t1, T.Mut) ->
        v.note <- T.Mut;
        check_exp context t1 e2;
  	    T.unit
      | Some (_, T.Const) ->
        type_error e.at "cannot assign to immutable location"
      | None ->
     	  type_error e1.at "unbound mutable identifier %s" v.it
      )
    | DotE (o, v) ->
      (match T.structural context.cons (infer_exp context o) with
      | T.Obj (a, tfs) as t ->
  	    (try
          let {T.mut; typ; _} = List.find (fun {T.lab; _} -> lab = v.it) tfs in
  	      v.note <- mut;
          match mut with 
          | T.Mut ->
            check_exp context typ e2;
  	        T.unit
          | T.Const ->
            type_error e.at "cannot assign to immutable field %s"  v.it
  	    with Not_found -> type_error e.at "object of type %s has no field named %s" (T.string_of_typ t) v.it)
      | t -> type_error e.at "expecting object type, found %s" (T.string_of_typ t)
      )
    | IdxE (ea, ei) ->
      (match T.structural context.cons (infer_exp context ea) with
      | T.Array (T.Mut, t) ->
        check_exp context T.nat ei;
        check_exp context t e2;
  	    T.unit
      | T.Array (T.Const, _) as t1  -> 
        type_error e.at "cannot assign to immutable array of type %s" (T.string_of_typ t1) 
      | t -> type_error e.at "expecting array type, found %s" (T.string_of_typ t)
      )
    | _ ->
      type_error e.at "illegal assignment: expecting variable, mutable object field or mutable array element"
    )
  | ArrayE (_, []) ->
    type_error e.at "cannot infer type of empty array (use a type annotation)"
  | ArrayE (mut, es) ->
    let ts = List.map (infer_exp context) es in
    let t1 = List.hd ts in
    if List.for_all (T.eq context.cons t1) (List.tl ts)
    then T.Array (mut.it, t1)
    else type_error e.at "array contains elements of distinct types"
  | IdxE (e1, e2) ->
    (match T.structural context.cons (infer_exp context e1) with
    | T.Array (_, t1) -> 
      check_exp context T.nat e2;
      t1
    | t -> type_error e.at "illegal indexing: expected an array, found %s" (T.string_of_typ t)
    )
  | CallE (e1, typs, e2) ->
    (match T.structural context.cons (infer_exp context e1) with
    | T.Func (tbs, t2, t) -> (* TBC polymorphic instantiation, perhaps by matching? *)
      let ts = check_typ_bounds context tbs typs e.at in
      let sigma = T.make_subst ts tbs in
      check_exp context (T.subst sigma t2) e2;
      T.subst sigma t
    | _ -> type_error e.at "illegal application: not a function"
    )
  | BlockE es ->
    check_block_local e.at context T.unit es;
    T.unit
  | NotE e ->
    check_exp context T.bool e;
    T.bool
  | AndE (e1, e2) ->
    check_exp context T.bool e1;
    check_exp context T.bool e2;
    T.bool
  | OrE (e1, e2) ->
    check_exp context T.bool e1;
    check_exp context T.bool e2;
    T.bool
  | IfE (e0, e1, e2) ->
    check_exp context T.bool e0;
    let t1 = infer_exp context e1 in
    let t2 = infer_exp context e2 in
    if T.eq context.cons t1 t2 
    then t1
    else type_error e.at "branches of if have different types"
  | SwitchE (e, cs) ->
    let t = infer_exp context e in
    if not (is_switch_typ context t) then type_error e.at "illegal type for switch";
    (match infer_cases context t cs None with
    | Some t -> t
    | None -> type_error e.at "couldn't infer type of case"
    )
  | WhileE (e0, e1) ->
    check_exp context T.bool e0;
    check_exp context T.unit e1;
    T.unit
  | LoopE (e, None) ->
    check_exp context T.unit e;
    T.unit (* TBR: T.bottom? *)
  | LoopE (e0, Some e1) ->
    check_exp context T.unit e0;
    (* TBR currently can't break or continue from guard *)
    check_exp context T.bool e1;
    T.unit
  | ForE (p, e0, e1)->
    let t = infer_exp context e0 in (*TBR is this for arrays only? If so, what about mutability*)
    if not (is_iter_typ context t) then type_error e.at "cannot iterate over this type";
    let ve = check_pat context p (elem_typ context t) in
    let context' = {context with vals = T.Env.adjoin context.vals ve} in
    check_exp context' T.unit e1;
    T.unit
  | LabelE (l, e) ->
    let context' = {context with labels = T.Env.add l.it T.unit context.labels} in
    infer_exp context' e
  | BreakE (l, e) ->
    (match T.Env.find_opt l.it context.labels  with
    | Some t ->
      check_exp context t e;
      T.unit (* TBR: T.bottom? *)
    | None ->
      match String.split_on_char ' ' l.it with
      | ["continue"; l'] -> type_error e.at "continue to unknown label %s" l'
      | _ -> type_error e.at "break to unknown label %s" l.it
    )
  | RetE e0 ->
    (match context.returns with
    | Some t ->
      check_exp context t e0;
      T.unit (*TBR: bottom? *)
    | None -> type_error e.at "illegal return"
    )
  | AsyncE e0 ->
    let context' =
      { context with
  		  labels = T.Env.empty;
  		  returns = Some T.unit; (* TBR *)
  		  awaitable = true
      } in
    let t = infer_exp context' e0 in
    T.Async t
  | AwaitE e0 ->
    if context.awaitable
    then
      match T.structural context.cons (infer_exp context e0) with
      | T.Async t -> t
      | t -> type_error e0.at "expecting expression of async type, found expression of type %s" (T.string_of_typ t)
    else type_error e.at "illegal await in synchronous context"
  | AssertE e ->
    check_exp context T.bool e;
    T.unit
  | IsE (e, t) ->
    let _ = infer_exp context e in
    let _ = check_typ context t in (*TBR what if T has free type variables? How will we check this, sans type passing *) 
    T.bool
  | AnnotE (e, t) ->
    let t = check_typ context t in 
    check_exp context t e;
    t
  | DecE {it = FuncD (v, _, _, _, _); _} ->
    (* TODO: don't special-case functions *)
    ignore (check_block context [e]);
    (match T.Env.find_opt v.it context.vals with
    | Some (t, mut) -> t
    | None -> assert false
    )
  | DecE _ ->
    let _ = check_block context [e] in
    T.unit
    

(* todo: compute refutability of pats and enforce accordingly (refutable in all cases but last, irrefutable elsewhere) *)

and infer_cases context pt cs t_opt  =
  match cs with
  | [] -> t_opt
  | {it = {pat; exp}; _}::cs ->
    let ve = check_pat context pat pt in
    let t =
    	match t_opt with
      | None -> infer_exp (adjoin_vals context ve) exp 
      | Some t -> check_exp (adjoin_vals context ve) t exp; t
    in infer_cases context pt cs (Some t)

and check_exp context t e =
  (match e.it with
  | LitE rl ->
    check_lit e.at context t rl
  | UnE (uop, e1) ->
    check_unop context e.at t uop e1
  | BinE (e1, bop, e2) ->
    check_binop context e.at t e1 bop e2
  | RelE (e1, rop, e2) ->
    check_relop context e.at t e1 rop e2
  | TupE es ->
    (match T.structural context.cons t with
    | T.Tup ts when List.length ts = List.length es ->
      List.iter2 (check_exp context) ts es
    | _ -> type_error e.at "tuple expression cannot produce expected type %s" (T.string_of_typ t)
    )
  | ArrayE (mut, es) ->
    (match T.structural context.cons t with
    | T.Array (mut', t) when mut' = mut.it ->
      List.iter (check_exp context t) es
    | _ -> type_error e.at "array expression cannot produce expected type %s" (T.string_of_typ t)
    )
(* | IdxE(e1, e2) ->
   TBR, unfortunately, we can't easily do IdxE in checking mode because we don't know whether to expect
   a mutable or immutable array type (we could do better on assignment of course),
   so we rely on inference instead
*)
  | AsyncE e0 ->
    (match T.structural context.cons t with
    | T.Async t ->
      let context =
        { context with
          labels = T.Env.empty;
		      returns = Some t; (* TBR *)
		      awaitable = true
        } in
      check_exp context t e0
    | _ -> type_error e.at "async expression cannot produce expected type %s" (T.string_of_typ t)
    )
  | LoopE (e, None) ->
    check_exp context T.unit e (*TBR: any? *)
  | BlockE es ->
    check_block_local e.at context t es
  | IfE (e0, e1, e2) ->
    check_exp context T.bool e0;
    check_exp context t e1;
    check_exp context t e2
  | SwitchE (e1, cs) ->
    let t1 = infer_exp context e1 in
    if not (is_switch_typ context t1) then type_error e1.at "illegal type for switch";
    (match infer_cases context t1 cs (Some t) with
    | Some t' -> assert (T.eq context.cons t t')
    | None -> assert false
    )
  | BreakE _ ->
    ignore (infer_exp context e)
  | RetE _ ->
    ignore (infer_exp context e)
  | LabelE (l, e) ->
    let context' = {context with labels = T.Env.add l.it t context.labels} in
    check_exp context' t e
  | _ ->
    let t' = infer_exp context e in
    if not (T.eq context.cons t t')
    then type_error e.at "expecting expression of type %s found expression of type %s" (T.string_of_typ t) (T.string_of_typ t')
  );
  e.note <- T.normalize context.cons t    
    
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

and check_block_local r context t es =
  match es with
  | [] ->
    if not (T.eq context.cons t T.unit)
    then type_error r "block  must end with expression of type" (T.string_of_typ t) 
  | ({it = DecE _; _} as e)::es' ->
    let ve, te, ke = check_block context [e] in (* TBR: we currently check local decs sequentially, not recursively *)
    check_block_local r (adjoin_cons (adjoin_typs (adjoin_vals context ve) te) ke) t es'
  | [e] -> check_exp context t e
  | e::es' ->
    check_exp context T.unit e; (* TBR: is this too strict? do we want to allow implicit discard? *)
    check_block_local r context t es'


and infer_pats at context ve ts ps =
  match ps with
  | [] -> ve, T.Tup (List.rev ts)
  | p::ps ->
    let ve', t = infer_pat context p in
    infer_pats at context (disjoint_union at "duplicate binding for %s in pattern" ve ve') (t::ts) ps

and infer_pat context p =
  match p.it with
  | WildP ->  type_error p.at "can't infer type of pattern"
  | VarP v -> type_error p.at "can't infer type of pattern"
  | LitP l ->
    T.Env.empty, infer_lit context l
  | TupP ps ->
    infer_pats p.at context T.Env.empty [] ps
  | AnnotP (p, t) ->
    let t = check_typ context t in
    check_pat context p t, t

and check_pats at context ve ps ts =
  match ps, ts with
  | [], [] -> ve
  | p::ps, t::ts ->
    let ve' = check_pat context p t in
    check_pats at context (disjoint_union at "duplicate binding for %s in pattern" ve ve') ps ts  (*TBR reject shadowing *)
  | [], ts -> type_error at "tuple pattern has %i fewer components than expected type" (List.length ts)
  | ts, [] -> type_error at "tuple pattern has %i more components than expected type" (List.length ts)
         
and check_pat context p t =
  match p.it with
  | WildP -> T.Env.empty
  | VarP v -> T.Env.singleton v.it (t, T.Const)
  | LitP rl ->
    check_lit p.at context t rl;
    T.Env.empty
  | TupP ps ->
    (match T.structural context.cons t with
    | T.Tup ts -> check_pats p.at context T.Env.empty ps ts 
    | _ -> type_error p.at "expected pattern of non-tuple type, found pattern of tuple type"
    )
  | AnnotP (p', t') ->
    let t' = check_typ context t' in
    if T.eq context.cons t t'
    then check_pat context p' t'
    else type_error p.at "expected pattern of one type, found pattern of unequal type"

      
and check_dec pass context d =
  match d.it with
  | LetD (p, e) ->
    if pass < TypDefPass then T.Env.empty, T.Env.empty, Con.Env.empty else      
    let t = infer_exp context e in
    let ve = check_pat context p t in 
    ve, T.Env.empty, Con.Env.empty
  | VarD (v, t, None) ->
    if pass < TypDefPass then T.Env.empty, T.Env.empty, Con.Env.empty else
    let t = check_typ context t in
    T.Env.singleton v.it (t, T.Mut), T.Env.empty, Con.Env.empty
  | VarD (v, t, Some e) ->
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
    let ve, t1 = infer_pat context_te p in
    let t2 = check_typ context_te t in
    let func_t = T.Func (tbs, t1, t2) in  (* TBR: we allow polymorphic recursion *)
    let context_te_ve_v =
      { (add_val (adjoin_vals context_te ve) v.it (func_t, T.Const)) with
	      labels = T.Env.empty;
        returns = Some t2;
        awaitable = false
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
    let ve, dom_t = infer_pat context_ts_v p in
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
        let _, dom_t = infer_pat context_us p in
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

let check_prog p =
  check_block empty_context p.it
