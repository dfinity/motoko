open Syntax
open Source
open Types
open Con

module I32 = Wasm.I32
module I64 = Wasm.I64


exception KindError of Source.region * string
exception TypeError of Source.region * string

let kind_error region fmt =
  Printf.ksprintf (fun s -> raise (KindError (region, s))) fmt

let type_error region fmt =
  Printf.ksprintf (fun s -> raise (TypeError (region, s))) fmt


(* TBR *)
let nat_width = 31
let int_width = 31

module Env = Map.Make(String)

let union env1 env2 = Env.union (fun k v1 v2 -> Some v2) env1 env2
let lookup env k =
  try Some (Env.find k env) with Not_found -> None (* TODO: use find_opt in 4.05 *)


type val_env = (typ * mut) Env.t
type con_env = con Env.t
type kind_env = kind ConEnv.t

type context =
  {
    values : val_env;
    constructors : con_env;
    kinds : kind_env;
    labels : typ Env.t;
    returns : typ option;
    awaitable : bool
  }

let union_conenv env1 env2 = ConEnv.union (fun k v1 v2 -> Some v2) env1 env2

let union_values c ve = {c with values = union c.values ve}
let add_value c v tm = {c with values = Env.add v tm c.values}
let union_constructors c ce = {c with constructors = union c.constructors ce}

let union_kinds c ke = {c with kinds = union_conenv c.kinds ke}
let add_constructor c v con kind = {c with constructors = Env.add v con c.constructors;
                                           kinds = ConEnv.add con kind c.kinds}

(* raises TypeError on duplicate entries *)
let disjoint_union at fmt env1 env2 = Env.union (fun k v1 v2 -> type_error at fmt k) env1 env2
let disjoint_add_field at c v f = disjoint_union at "duplicate field %s" c (Env.singleton v f)

(* TBD
let disjoint_union_values at c ve =   {c with values = disjoint_union at "duplicate value %s"  c.values ve }
let disjoint_add_value at c v tm = disjoint_union_values at c (Env.singleton v tm)
let disjoint_union_constructors at c ce = {c with constructors = disjoint_union at "duplicate constructor %s" c.constructors ce}
let disjoint_add_constructor at c d = disjoint_union_constructors at c (Env.singleton v d)
*)


let empty_context =
  {
    values = Env.empty;
    constructors = Env.empty;
	  kinds = ConEnv.empty;
	  labels = Env.empty;
	  returns = None;
	  awaitable = false
  }


let rec eq context eqs t1 t2 =
  match t1, t2 with
  | VarT (con1, ts1), VarT (con2, ts2) ->
    if List.mem (con1, con2) eqs
    then eq_all context eqs ts1 ts2
    else begin
      con1 = con2 && eq_all context eqs ts1 ts2 ||
      match lookup_con context.kinds con1, lookup_con context.kinds con2 with
      | Some (DefK (tbs, t)), _ -> (* TBR this may fail to terminate *)
       	eq context eqs (subst (substitute ts1 tbs) t) t2
      | _, Some (DefK (tbs, t)) -> (* TBR this may fail to terminate *)
       	eq context eqs t1 (subst (substitute ts2 tbs) t)
      | _, _ -> false
    end
  | VarT (con1, ts1), t2 ->
    (match lookup_con context.kinds con1 with
    | Some (DefK (tbs, t)) -> (* TBR this may fail to terminate *)
      eq context eqs (subst (substitute ts1 tbs) t) t2
    | _ -> false
    )
  | t1, VarT (con2, ts2) ->
    (match lookup_con context.kinds con2 with
    | Some (DefK (tbs, t)) -> (* TBR this may fail to terminate *)
      eq context eqs t1 (subst (substitute ts2 tbs) t)
    | _ -> false
    )
  | PrimT p1, PrimT p2 ->
    p1 = p2
  | ObjT (a1, tfs1), ObjT (a2, tfs2) ->
    a1 = a2 &&
    (* assuming tf1 and tf2 are sorted by var *)
    (try List.for_all2 (eq_typ_field context eqs) tfs1 tfs2 with _ -> false)
  | ArrayT (m1, t1), ArrayT (m2, t2) ->
    m1 = m2 && eq context eqs t1 t2
  | OptT (t1), OptT (t2) ->
    eq context eqs t1 t2
  | TupT (ts1), TupT (ts2) ->
    eq_all context eqs ts1 ts2
  | FuncT (tbs1, t11, t12), FuncT (tbs2, t21, t22) ->
    (match eq_typ_binds context eqs [] [] tbs1 tbs2 with
    | Some eqs' -> eq context eqs' t11 t21 && eq context eqs' t12 t22
    | None -> false
    )
  | AsyncT (t1), AsyncT (t2) ->
    eq context eqs t1 t2
  | LikeT (t1), LikeT (t2) ->
    eq context eqs t1 t2
  | AnyT, AnyT -> true
  | _, _ -> false

and eq_typ_field context eqs (tf1 : typ_field) (tf2 : typ_field) =
  tf1.var = tf2.var &&
  tf1.mut = tf2.mut &&
  eq context eqs tf1.typ tf2.typ

and eq_typ_binds context eqs bds1 bds2 tbs1 tbs2 =
  match tbs1, tbs2 with
  | [], [] ->
    if eq_all context eqs bds1 bds2
    then Some eqs
    else None
  | tb1::tbs1, tb2::tbs2 ->
    eq_typ_binds context ((tb1.var,tb2.var)::eqs) (tb1.bound::bds1) (tb2.bound::bds2) tbs1 tbs2
  | _, _ -> None
      
and eq_all context eqs ts1 ts2 =
  try List.for_all2 (eq context eqs) ts1 ts2 with Invalid_argument _ -> false
  

let eq_typ (context : context) ty1 ty2 : bool =
    eq context [] ty1 ty2 


let rec norm_typ context = function
  | VarT (con, ts) as t ->
    (match lookup_con context.kinds con with
    | Some kind ->
      (match kind with
	    | DefK (tbs, t) -> norm_typ context (subst (substitute ts tbs) t)
	    | ObjK _ | ParK _ -> t
      ) 
    | None -> assert false
  )
  | t -> t

let rec obj_typ context t =
  match norm_typ context t with
  | VarT (con, ts) ->
    (match lookup_con context.kinds con with
    | Some kind ->
      (match kind with
	    | DefK (tbs, t) -> assert false
	    | ObjK (tbs, actor, fs) -> subst (substitute ts tbs) (ObjT (actor, fs))
	    | ParK (tbs, bound) -> t (*TBR ?*)
      )
    | None -> assert false
    )
  | ObjT _ -> t
  | LikeT t -> norm_typ context t (*TBR*)
  | _ -> t

(* types one can switch on - all primitives except floats *)

(* TBR - switch on option type? *)
let switchable_typ context t =
  match norm_typ context t with
  | PrimT p -> p <> FloatT
  | _ -> false

(* types one can iterate over using `for`  *)
let iterable_typ context t =
  match norm_typ context t with
  | ArrayT _ -> true
  | _ -> false

(* element type of iterable_type  *)
let element_typ context t =
  match norm_typ context t with
  | ArrayT (_, t) -> t
  | _ -> assert false

let numeric_typ context t =
  match norm_typ context t with
  | PrimT (NatT | IntT | WordT _ | FloatT) -> true
  | _ -> false

let logical_typ context t =
  match norm_typ context t  with
  | PrimT (WordT _) -> true
  | _ -> false

let equatable_typ context t =
  match norm_typ context t with
  | PrimT (BoolT | NatT | IntT | WordT _ | FloatT | TextT | CharT) -> true
    (* TBR do we really want = and != on floats ?*)
  | _ -> false

let comparable_typ context t =
  match norm_typ context t with
  | PrimT (NatT | IntT | WordT _ | FloatT | TextT | CharT) -> true
  | _ -> false


(* TBR: the whole notion of sharable typ needs to be reviewed in the presence of subtyping *)
let rec sharable_typ (context : context) (t : typ) : bool =
  match norm_typ context t with
  | VarT (c, ts) ->
    (match lookup_con context.kinds c with
    | Some (DefK _) -> assert false
    | Some (ObjK (tbs, actor, fds)) ->
      sharable_typ context (subst (substitute ts tbs) (ObjT (actor, fds))) (* TBR *)
    | Some (ParK (tbs, bound)) -> false (*TBR: use bound? *)
    | None -> assert false
    )
  | PrimT p -> true
  | ArrayT (m, t) ->
    m = ConstMut && sharable_typ context t
  | TupT ts ->
    List.for_all (sharable_typ context) ts 
  | FuncT (tbs, t1, t2) ->
    let ce, ke = sharable_typ_binds context tbs in
    let context' = union_kinds (union_constructors context ce) ke in
    List.for_all (fun tb -> sharable_typ context' tb.bound) tbs && (* TBR *)
    sharable_typ context' t1 &&
    (match norm_typ context' t2 with
    | TupT [] | AsyncT _ -> true
    | _ -> false
    )
    (* TBR: a function type should be non-sharable if it closes over non-shareable locals *)
  | OptT t -> sharable_typ context t
  | AsyncT t -> sharable_typ context t
  | LikeT t -> sharable_typ context t
  | ObjT (Object, fs) ->
    (*TBR: this isn't stable with subtyping *)
    List.for_all (fun {var; typ; mut}-> mut = ConstMut && sharable_typ context typ) fs
  | ObjT (Actor, fs) -> true
  | AnyT -> false (* TBR *)

and sharable_typ_binds context ts =
  let ce = List.fold_left (fun c (bind : Types.typ_bind) -> Env.add bind.var.name bind.var c) context.constructors ts in
  let ke = List.fold_left (fun c (bind : Types.typ_bind) -> ConEnv.add bind.var (ParK ([], bind.bound)) c) context.kinds ts in
  ce, ke


(* let sprintf = Printf.sprintf *)

let check_bounds at ts bounds =
  if List.length bounds <> List.length ts then
    type_error at "expecting %i type arguments, found %i arguments" (List.length bounds) (List.length ts)

(* todo: compute refutability of pats and enforce accordingly (refutable in all cases but last, irrefutable elsewhere) *)
(* todo: type ObjE expressions (anonymous objects)*)


(* checking literal values are in bounds *)
let check_i32_u p bits =
  let module I = I32 in
  let max = I.shl (I.of_int_s 1) (I.of_int_s bits) in
  fun at s ->
    try
      let i = I.of_string s in
    	if  (I.gt_u max I.zero) && not (I.lt_u i max)
	    then type_error at "literal overflow for type %s" (string_of_typ (PrimT p))
	    else I.to_bits i
    with _ -> type_error at "bad literal for type %s" (string_of_typ (PrimT p))

let check_i64_u p bits =
  let module I = I64 in
  let max = I.shl (I.of_int_s 1) (I.of_int_s bits) in
  fun at s ->
    try
      let i = I.of_string s in
    	if  (I.gt_u max I.zero) && not (I.lt_u i max)
	    then type_error at "literal overflow for type %s" (string_of_typ (PrimT p))
	    else I.to_bits i
    with _ -> type_error at "bad literal for type %s" (string_of_typ (PrimT p))

let check_nat    = check_i32_u NatT nat_width
let check_word8  = check_i32_u (WordT Width8) 8
let check_word16 = check_i32_u (WordT Width16) 16
let check_word32 = check_i32_u (WordT Width32) 32
let check_word64 = check_i64_u (WordT Width64) 64

let check_i32_s p bits =
  let module I = I32 in
  let max = I.sub (I.shl (I.of_int_u 1) (I.of_int_u (bits-1))) (I.of_int_s 1) in
  let min = I.sub (I.sub I.zero max) (I.of_int_s 1) in
  fun at s ->
    try
      let i = I.of_string s in
    	if not (I.le_s min i && I.le_s i max)
	    then type_error at "literal under/overflow for type %s" (string_of_typ (PrimT p))
	    else I.to_bits i
    with _ -> type_error at "bad literal for type %s" (string_of_typ (PrimT p))

let check_int = check_i32_s IntT int_width



(* begin sanity test *)

let pow2 n = 1 lsl n
let raises f r x = try f r x; false; with _ -> true

let _ =
  (* text check_nat *)
  assert (check_nat no_region (string_of_int (pow2 31 - 1)) = I32.of_int_u (pow2 31 - 1));
  assert (check_nat no_region (string_of_int 0) = I32.of_int_u 0);
  assert (raises check_nat no_region (string_of_int (pow2 31)));
  assert (raises check_nat no_region (string_of_int (pow2 31 + 1)));

  (* test check_word16 *)
  assert (check_word16 no_region (string_of_int (pow2 16 - 1)) = I32.of_int_u (pow2 16 - 1));
  assert (check_word16 no_region (string_of_int 0) = I32.of_int_u 0);
  assert (raises check_word16 no_region (string_of_int (pow2 16)));
  assert (raises check_word16 no_region (string_of_int (pow2 16 + 1)));

  (* test check_int *)
  assert (check_int no_region (string_of_int (pow2 (int_width - 1) - 1)) = I32.of_int_s (pow2 (int_width - 1) - 1));
  assert (check_int no_region (string_of_int (- pow2 (int_width - 1))) = I32.of_int_s (- pow2 (int_width - 1)));
  assert (raises check_int no_region (string_of_int (pow2 (int_width - 1))));
  assert (raises check_int no_region (string_of_int (- pow2 (int_width - 1) - 1)))

(* end sanity test *)



type pass =
  | TypDecPass  (* Declare type constructors (as abstract) *)
  | ValDecPass  (* Declare values and constructors *)
  | TypDefPass  (* Define type constructors *)
  | ValDefPass  (* Check value definition *)


let rec check_typ_binds context ts =
  (* TBR: allow parameters in bounds? - not for now*)
  let bind_context = context in
  let ts =
    List.map (fun (bind : Syntax.typ_bind) ->
    	let v = bind.it.Syntax.var.it in
    	let con = Con.fresh v in
      {Types.var = con; bound = check_typ bind_context bind.it.Syntax.bound}
    ) ts in
  let ce =
    List.fold_left (fun c (bind : Types.typ_bind) ->
      Env.add bind.var.name bind.var c
    ) context.constructors ts in
  let ke =
    List.fold_left (fun c (bind : Types.typ_bind) ->
      ConEnv.add bind.var (ParK ([], bind.bound)) c
    ) context.kinds ts in
  ts, ce, ke

(*TBD do we want F-bounded checking with mutually recursive bounds? *)

and check_typ context t =
  match t.it with
  | Syntax.VarT (c, tys) ->
    let ts = List.map (check_typ context) tys in
    (match lookup context.constructors c.it with
    | Some con ->
      (match lookup_con context.kinds con with
      | Some (DefK (bounds, u)) ->
        check_bounds t.at ts bounds;
        (* subst ts bounds u ? *)
	      VarT (con, ts)
      | Some (ObjK (bounds, actor, ftys)) ->
        check_bounds t.at ts bounds;
        VarT(con,ts)
      | Some (ParK (bounds,bound)) ->
     	  check_bounds t.at ts bounds;
        VarT (con, ts)
      | None ->
        kind_error c.at "unbound constructor %s " (Con.to_string con)
      )
    | None -> kind_error c.at "unbound type identifier %s" c.it
    )
  | Syntax.PrimT p -> PrimT p      
  | Syntax.ArrayT (m, t) ->
    ArrayT (m.it, check_typ context t)
  | Syntax.TupT ts ->
    TupT (List.map (check_typ context) ts)
  | Syntax.FuncT(tbs, t1, t2) ->
    let ts, ce, ke = check_typ_binds context tbs in
    let context = union_kinds (union_constructors context ce) ke in
    FuncT (ts, check_typ context t1, check_typ context t2)
  | Syntax.OptT t -> OptT (check_typ context t)
  | Syntax.AsyncT t -> AsyncT (check_typ context t)
  | Syntax.LikeT t -> LikeT (check_typ context t)
  | Syntax.ObjT (a, fs) ->
    (* fields distinct? *)
    let _ = List.fold_left
      (fun (dom : string list) ({it = {var; typ; mut}; at} : Syntax.typ_field) ->
        if List.mem var.it dom
        then kind_error var.at "duplicate field name %s in object type" var.it
        else var.it::dom
      ) [] fs in
    let fs = List.map
      (fun (f : Syntax.typ_field) ->
    	  {var = f.it.var.it; typ = check_typ context f.it.typ; mut = f.it.mut.it}
      ) fs in
    List.iter (check_typ_field context a.at a.it) fs;
    (* sort by name (for indexed access *)
    let fs_sorted = List.sort (fun (f : typ_field) (g : typ_field) -> String.compare f.var g.var) fs in
    ObjT (a.it, fs_sorted)
  | Syntax.AnyT -> AnyT

and check_typ_field context at actor {var; mut; typ} =
  match actor with
  | Object -> ()
  | Actor when mut = VarMut ->
    type_error at "public field %s of actor is mutable (an actor's public fields must be immutable)" var
  | Actor ->
    if not (sharable_typ context typ)
    then type_error at "public field %s of actor has non-sharable type %s (the type of an actor's public field must be shareable)" var (string_of_typ typ)


and infer_lit context rl =
  match !rl with
  | NullLit -> PrimT NullT (* TBR *)
  | BoolLit _ -> PrimT BoolT
  | NatLit _ -> PrimT NatT
  | IntLit _ -> PrimT IntT
  | WordLit (Word8 _) -> PrimT (WordT Width8)
  | WordLit (Word16 _) -> PrimT (WordT Width16)
  | WordLit (Word32 _) -> PrimT (WordT Width32)
  | WordLit (Word64 _) -> PrimT (WordT Width64)
  | FloatLit _ -> PrimT FloatT
  | CharLit _ -> PrimT CharT
  | TextLit _ -> PrimT TextT
  | PreLit s ->
    rl := IntLit (Int32.to_int (Wasm.I32.of_string s)); (* default *)
    PrimT IntT


and check_lit at context t rl =
  match norm_typ context t, !rl with
  | OptT _, NullLit
  | PrimT NullT, NullLit
  | PrimT NatT, NatLit _
  | PrimT IntT, IntLit _
  | PrimT (WordT Width8), WordLit (Word8 _)
  | PrimT (WordT Width16), WordLit (Word16 _)
  | PrimT (WordT Width32), WordLit (Word32 _)
  | PrimT (WordT Width64), WordLit (Word64 _) -> ()

  | OptT t, _ -> check_lit at context t rl

  | PrimT NatT, PreLit s ->
    let v = check_nat at s in 
    rl := NatLit (Int32.to_int v)
  | PrimT IntT, PreLit s ->
    let v = check_int at s in
    rl := IntLit (Int32.to_int v)
  | PrimT (WordT Width8), PreLit s ->
    let v = check_word8 at s in
    rl := WordLit (Word8 (Int32.to_int v))
  | PrimT (WordT Width16), PreLit s ->
    let v = check_word16 at s in
    rl := WordLit (Word16 (Int32.to_int v))
  | PrimT (WordT Width32), PreLit s ->
    let v = check_word32 at s in
    rl := WordLit (Word32 v)
  | PrimT (WordT Width64), PreLit s ->
    let v = check_word64 at s in
    rl := WordLit (Word64 v)

  | PrimT _, _ ->
    let u = infer_lit context rl in
    if not (eq_typ context t u)
    then type_error at "expect literal of type %s, found literal of type %s" (string_of_typ t) (string_of_typ u)

  | _ -> type_error at "type %s has no literals" (string_of_typ t)


and infer_binop context at e1 bop e2 =
  let t1 = infer_exp context e1 in
  let t2 = infer_exp context e2 in
  match bop with
  | CatOp ->
    if eq_typ context t1 (PrimT TextT) && eq_typ context t1 t2
    then t1
    else type_error at "arguments to concatenation operator must have Text type"
  | AddOp | SubOp | MulOp | DivOp | ModOp ->
    if numeric_typ context t1 && eq_typ context t1 t2
    then t1
    else type_error at "arguments to numeric operator must have equivalent numeric types"
  | AndOp | OrOp | XorOp | ShiftLOp | ShiftROp | RotLOp | RotROp ->
    if logical_typ context t1 && t1 = t2
    then t1
    else type_error at "arguments to logical operator must have equivalent logical types"

and check_binop context at t e1 bop e2 =
  (match bop with
  | CatOp ->
    if not (eq_typ context t (PrimT TextT))
    then type_error at "expecting value of type %s, but concatenation returns a value of type Text" (string_of_typ t)
  | AddOp | SubOp | MulOp | DivOp | ModOp ->
    if not (numeric_typ context t)
    then type_error at "expecting value of type non-numeric type %s, operator returns a value of numeric type" (string_of_typ t)
  | AndOp | OrOp | XorOp | ShiftLOp | ShiftROp | RotLOp | RotROp ->
    if not (logical_typ context t)
    then type_error at "expecting value of type non-logical type %s, operator returns a value of logical type" (string_of_typ t)
  );
  check_exp context t e1;
  check_exp context t e2

and infer_relop context at e1 rop e2 =
  let t1 = infer_exp context e1 in
  let t2 = infer_exp context e2 in
  match rop with
  | EqOp 
  | NeqOp ->
    if equatable_typ context t1 && eq_typ context t1 t2
    then boolT
    else type_error at "arguments to an equality operator must have the same, equatable type"
  | _ ->
    if comparable_typ context t1 && eq_typ context t1 t2
    then boolT
    else type_error at "arguments to a relational operator must have the same, comparable type"

and check_relop context at t e1 rop e2 =
  if not (eq_typ context t boolT)
  then type_error at "expecting value of non-boolean type %s, relational operator returns a value of Bool type" (string_of_typ t);
  ignore (infer_relop context at e1 rop e2)

and infer_unop context at uop e =
  let t = infer_exp context e in
  match uop with
  | PosOp | NegOp ->
    if numeric_typ context t (* TBR: rule out Naturals *)
    then t
    else type_error at "argument to operator must have numeric type"
  | NotOp ->
    if logical_typ context t 
    then t
    else type_error at "arguments to a bitwise negation operator must have logical type"

and check_unop context at t uop e =
  match uop with
  | PosOp | NegOp ->
    if numeric_typ context t 
    then check_exp context t e
    else type_error at "argument to negation operator must have numeric type"
  | NotOp ->
    if logical_typ context t 
    then check_exp context t e
    else type_error at "arguments to a bitwise negation operator must have logical type"

and infer_exp context e =
  let t = infer_exp' context e in
  e.note <- norm_typ context t;
  t

and infer_exp' context e =
  match e.it with
  | VarE x ->
    (match lookup context.values x.it with
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
    TupT ts
  | ProjE (e, n) ->
    (match norm_typ context (infer_exp context e) with
    | TupT ts ->
      (try List.nth ts n
      with Failure _ -> type_error e.at "tuple projection %i >= %n is out-of-bounds" n (List.length ts))
    | t -> type_error e.at "expecting tuple type, found %s" (string_of_typ t)
    )
  | DotE (e, v) ->
    (match obj_typ context (infer_exp context e) with
    | ObjT (a, fts) as t ->
      (try
        let ft = List.find (fun (fts : typ_field) -> fts.var = v.it) fts in
  	    v.note <- ft.mut;
        ft.typ
      with Not_found -> type_error e.at "object of type %s has no field named %s" (string_of_typ t) v.it)
    | t -> type_error e.at "expecting object type, found %s" (string_of_typ t)
    )
  | AssignE (e1, e2) ->
    (*TBC: array and object update *)
    (match e1.it with
    | VarE v ->
      (match lookup context.values v.it with
      | Some (t1, VarMut) ->
        v.note <- VarMut;
        check_exp context t1 e2;
  	    unitT
      | Some (_, ConstMut) ->
        type_error e.at "cannot assign to immutable location"
      | None ->
     	  type_error e1.at "unbound mutable identifier %s" v.it
      )
    | DotE (o, v) ->
      (match obj_typ context (infer_exp context o) with
      | ObjT (a, fts) as t ->
  	    (try
          let ft = List.find (fun (fts : typ_field) -> fts.var = v.it) fts in
  	      v.note <- ft.mut;
          match ft.mut with 
          | VarMut ->
            check_exp context ft.typ e2;
  	        unitT
          | ConstMut ->
            type_error e.at "cannot assign to immutable field %s"  v.it
  	    with Not_found -> type_error e.at "object of type %s has no field named %s" (string_of_typ t) v.it)
      | t -> type_error e.at "expecting object type, found %s" (string_of_typ t)
      )
    | IdxE (ea, ei) ->
      (match norm_typ context (infer_exp context ea) with
      | ArrayT (VarMut, t) ->
        check_exp context intT ei;
        check_exp context t e2;
  	    unitT
      | ArrayT (ConstMut, _) as t1  -> 
        type_error e.at "cannot assign to immutable array of type %s" (string_of_typ t1) 
      | t -> type_error e.at "expecting array type, found %s" (string_of_typ t)
      )
    | _ ->
      type_error e.at "illegal assignment: expecting variable, mutable object field or mutable array element"
    )
  | ArrayE (_, []) ->
    type_error e.at "cannot infer type of empty array (use a type annotation)"
  | ArrayE (mut, es) ->
    let ts = List.map (infer_exp context) es in
    let t1 = List.hd ts in
    if List.for_all (eq_typ context t1) (List.tl ts)
    then ArrayT (mut.it, t1)
    else type_error e.at "array contains elements of distinct types"
  | IdxE (e1, e2) ->
    (match norm_typ context (infer_exp context e1) with
    | ArrayT (_, t1) -> 
      check_exp context intT e2;
      t1
    | t -> type_error e.at "illegal indexing: expected an array, found" (string_of_typ t)
    )
  | CallE (e1, ts, e2) ->
    let ts = List.map (check_typ context) ts in
    (match norm_typ context (infer_exp context e1) with
    | FuncT (tbs, t3, t4) -> (* TBC polymorphic instantiation, perhaps by matching? *)
      check_bounds e.at ts tbs;
      let sigma = substitute ts tbs in
      let t3' = subst sigma t3 in
      let t4' = subst sigma t4 in
      let t2 = infer_exp context e2 in
      if eq_typ context t2 t3'
      then t4'
      else type_error e.at "illegal function application: expecting argument of type %s found argument of type %s" (string_of_typ t3') (string_of_typ t2)
    | _ -> type_error e.at "illegal application: not a function"
    )
  | BlockE es ->
    check_block e.at context unitT es;
    unitT
  | NotE e ->
    check_exp context boolT e;
    boolT
  | AndE (e1, e2) ->
    check_exp context boolT e1;
    check_exp context boolT e2;
    boolT
  | OrE (e1, e2) ->
    check_exp context boolT e1;
    check_exp context boolT e2;
    boolT
  | IfE (e0, e1, e2) ->
    check_exp context boolT e0;
    let t1 = infer_exp context e1 in
    let t2 = infer_exp context e2 in
    if eq_typ context t1 t2 
    then t1
    else type_error e.at "branches of if have different types"
  | SwitchE (e, cs) ->
    let t = infer_exp context e in
    if not (switchable_typ context t) then type_error e.at "illegal type for switch";
    (match infer_cases context t cs None with
    | Some t -> t
    | None -> type_error e.at "couldn't infer type of case"
    ) 
  | WhileE (e0, e1) ->
    check_exp context boolT e0;
    check_exp context unitT e1;
    unitT
  | LoopE (e, None) ->
    check_exp context unitT e;
    unitT (* absurdTy? *)
  | LoopE (e0, Some e1) ->
    check_exp context unitT e0;
    (* TBR currently can't break or continue from guard *)
    check_exp context boolT e1;
    unitT
  | ForE (p, e0, e1)->
    let t = infer_exp context e0 in (*TBR is this for arrays only? If so, what about mutability*)
    if not (iterable_typ context t) then type_error e.at "cannot iterate over this type";
    let ve = check_pat context p (element_typ context t) in
    let context' = {context with values = union context.values ve} in
    check_exp context' unitT e1;
    unitT
  | LabelE (l, e) ->
    let context' = {context with labels = Env.add l.it unitT context.labels} in
    infer_exp context' e
  | BreakE (l, e) ->
    (match lookup context.labels l.it  with
    | Some t -> 
      (* todo: check type of e against ts! *)
      check_exp context t e ;
      unitT (*TBR actually, this could be polymorphic at least in a checking context*)
    | None ->
      match String.split_on_char ' ' l.it with
      | ["continue"; l'] -> type_error e.at "continue to unknown label %s" l'
      | _ -> type_error e.at "break to unknown label %s" l.it
    )
  | RetE e0 ->
    (match context.returns with
    | Some t ->
      check_exp context t e0;
      unitT (*TBR actually, this could be polymorphic at least in a checking context*)
    | None -> type_error e.at "illegal return"
    )
  | AsyncE e0 ->
    let context' =
      { context with
  		  labels = Env.empty;
  		  returns = Some unitT; (* TBR *)
  		  awaitable = true
      } in
    let t = infer_exp context' e0 in
    AsyncT t
  | AwaitE e0 ->
    if context.awaitable
    then
      match norm_typ context (infer_exp context e0) with
      | AsyncT t -> t
      | t -> type_error e0.at "expecting expression of async type, found expression of type %s" (string_of_typ t)
    else type_error e.at "illegal await in synchronous context"
  | AssertE e ->
    check_exp context boolT e;
    unitT
  | IsE (e, t) ->
    let _ = infer_exp context e in
    let _ = check_typ context t in (*TBR what if T has free type variables? How will we check this, sans type passing *) 
    boolT
  | AnnotE (e, t) ->
    let t = check_typ context t in 
    check_exp context t e;
    t
  | DecE ({it = FuncD (v, _, _, _, _)} as d) ->
    (* TODO: don't special-case functions *)
    let ve = check_decs context [d] in
    (match lookup context.values v.it with
    | Some (t, mut) -> t
    | None -> assert false
    )
  | DecE d ->
    let _ = check_decs context [d] in
    unitT
    
and infer_cases context pt cs t_opt  =
  match cs with
  | [] -> t_opt
  | {it = {pat; exp}; at}::cs ->
    let ve = check_pat context pat pt in
    let t = infer_exp (union_values context ve) exp in
    let t_opt' =
      match t_opt with
    	| None -> Some t
 	    | Some t' when eq_typ context t t' -> Some t
		  | _ -> type_error at "illegal case of different type from preceeding cases"
    in infer_cases context pt cs t_opt'

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
  | ArrayE (mut, es) ->
    (match norm_typ context t with
    | ArrayT (mut', t) when mut' = mut.it ->
      List.iter (check_exp context t) es
    | _ -> type_error e.at "array expression cannot produce expected type %s" (string_of_typ t)
    )
(* | IdxE(e1,e2) ->
   TBR, unfortunately, we can't easily do IdxE in checking mode because we don't know whether to expect
   a mutable or immutable array type (we could do better on assignment of course),
   so we rely on inference instead
*)
  | AsyncE e0 ->
    (match norm_typ context t with
    | AsyncT t ->
      let context =
        { context with
          labels = Env.empty;
		      returns = Some t; (* TBR *)
		      awaitable = true
        } in
      check_exp context t e0
    | _ -> type_error e.at "async expression cannot produce expected type %s" (string_of_typ t)
    )
  | LoopE (e, None) ->
    check_exp context unitT e (*TBR do we want to allow any type for the body? *)
  | BlockE es ->
    check_block e.at context t es
  | BreakE _ ->
    ignore (infer_exp context e)
  | RetE _ ->
    ignore (infer_exp context e)
  | LabelE (l, e) ->
    let context' = {context with labels = Env.add l.it t context.labels} in
    check_exp context' t e
  | _ ->
    let t' = infer_exp context e in
    if not (eq_typ context t t')
    then type_error e.at "expecting expression of type %s found expression of type %s" (string_of_typ t) (string_of_typ t')
  );
  e.note <- norm_typ context t    
    
and infer_block context es =
  match es with
  | [] -> unitT
  | {it = DecE d; at}::es ->
    let ve, ce, ke = check_decs context [d] in (* TBR: we currently check local decs sequentially, not recursively *)
    infer_block (union_kinds (union_constructors (union_values context ve) ce) ke) es
  | [e] -> infer_exp context e
  | e::es ->
    check_exp context unitT e; (* TBR: is this too strict? do we want to allow implicit discard? *)
    infer_block context es

and check_block r context t es =
  match es with
  | [] ->
    if not (eq_typ context t unitT)
    then type_error r "block  must end with expression of type" (string_of_typ t) 
  | {it = DecE d; at}::es ->
    let ve,ce,ke = check_decs context [d] in (* TBR: we currently check local decs sequentially, not recursively *)
    check_block r (union_kinds (union_constructors (union_values context ve) ce) ke) t es
  | [e] -> check_exp context t e
  | e::es ->
    check_exp context unitT e; (* TBR: is this too strict? do we want to allow implicit discard? *)
    check_block r context t es 


and infer_pats at context ve ts ps =
  match ps with
  | [] -> ve, TupT (List.rev ts)
  | p::ps ->
    let ve',t = infer_pat context p in
    infer_pats at context (disjoint_union at "duplicate binding for %s in pattern" ve ve') (t::ts) ps

and infer_pat context p =
  match p.it with
  | WildP ->  type_error p.at "can't infer type of pattern"
  | VarP v -> type_error p.at "can't infer type of pattern"
  | LitP l ->
    Env.empty, infer_lit context l
  | TupP ps ->
    infer_pats p.at context Env.empty [] ps
  | AnnotP (p, t) ->
    let t = check_typ context t in
    check_pat context p t, t

and check_pats at context ve ps ts =
  match ps,ts with
  | [], [] -> ve
  | p::ps, t::ts ->
    let ve' = check_pat context p t in
    check_pats at context (disjoint_union at "duplicate binding for %s in pattern" ve ve') ps ts  (*TBR reject shadowing *)
  | [], ts -> type_error at "tuple pattern has %i fewer components than expected type" (List.length ts)
  | ts, [] -> type_error at "tuple pattern has %i more components than expected type" (List.length ts)
         
and check_pat context p t =
  match p.it with
  | WildP -> Env.empty
  | VarP v -> Env.singleton v.it (t,ConstMut)
  | LitP rl ->
    check_lit p.at context t rl;
    Env.empty
  | TupP ps ->
    (match norm_typ context t with
    | TupT ts -> check_pats p.at context Env.empty ps ts 
    | _ -> type_error p.at "expected pattern of non-tuple type, found pattern of tuple type"
    )
  | AnnotP(p',t') ->
    let t' = check_typ context t' in
    if eq_typ context t t'
    then check_pat context p' t'
    else type_error p.at "expected pattern of one type, found pattern of unequal type"

      
and check_dec pass context d =
  let ve, ce, ke = check_dec' pass context d in    
  (* TBC store ve *)
  ve, ce, ke

and check_dec' pass context d =     
  match d.it with
  | LetD (p, e) ->
    if pass < TypDefPass then Env.empty, Env.empty, ConEnv.empty else      
    let t = infer_exp context e in
    let ve = check_pat context p t in 
    ve, Env.empty, ConEnv.empty
  | VarD (v, t, None) ->
    if pass < TypDefPass then Env.empty, Env.empty, ConEnv.empty else
    let t = check_typ context t in
    Env.singleton v.it (t, VarMut), Env.empty, ConEnv.empty
  | VarD (v, t, Some e) ->
    if pass < TypDefPass then Env.empty, Env.empty, ConEnv.empty else
    let t = check_typ context t in
    check_exp context t e;
    Env.singleton v.it (t, VarMut), Env.empty, ConEnv.empty
  | TypD (v, tps, t) ->
    let tbs, ce_tbs, ke_tbs = check_typ_binds context tps in
    let con =
      if pass = TypDecPass
      then Con.fresh v.it
	    else Lib.Option.value (lookup context.constructors v.it)
    in
    let kind0 = ParK (tbs, VarT (con, [])) in  (* dummy abstract type *)
    let ce0 = Env.singleton v.it con in
    let ke0 = ConEnv.singleton con kind0 in
    if pass = TypDecPass then Env.empty, ce0, ke0 else
    let context_tbs = union_kinds (union_constructors context ce_tbs) ke_tbs in
    let t = check_typ context_tbs t in
    let kind1 = DefK (tbs, t) in  (* dummy type *)
    let ce1 = Env.singleton v.it con in
    let ke1 = ConEnv.singleton con kind1 in
    Env.empty, ce1, ke1
  | FuncD (v, tps, p, t, e) ->
    if pass < TypDefPass then Env.empty, Env.empty, ConEnv.empty else
    let tbs, ce, ke = check_typ_binds context tps in
    let context_ce = union_kinds (union_constructors context ce) ke in
    let ve, t1 = infer_pat context_ce p in
    let t2 = check_typ context_ce t in
    let funcT = FuncT (tbs, t1, t2) in  (* TBR: we allow polymorphic recursion *)
    let context_ce_ve_v =
      { (add_value (union_values context_ce ve) v.it (funcT, ConstMut)) with
	      labels = Env.empty;
        returns = Some t2;
        awaitable = false
      } in
    check_exp context_ce_ve_v t2 e;
    Env.singleton v.it (funcT, ConstMut), Env.empty, ConEnv.empty
  | ClassD (a, v, tps, p, efs) ->
    let tbs, ce_ts, ke_ts = check_typ_binds context tps in
    let con =
      if pass = TypDecPass
      then Con.fresh v.it
	    else Lib.Option.value (lookup context.constructors v.it)
    in
    let kind0 = ObjK (tbs, a.it, []) in
    let ce0 = Env.singleton v.it con in
    let ke0 = ConEnv.singleton con kind0 in
    if pass = TypDecPass then Env.empty, ce0, ke0 else
    let context_ts = union_kinds (union_constructors context ce_ts) ke_ts in
    let context_ts_v = add_constructor context_ts v.it con kind0 in
    let ve, domT = infer_pat context_ts_v p in
    let classT = VarT (con, List.map (fun (t : Types.typ_bind) -> VarT (t.var, [])) tbs) in
    let consT = FuncT (tbs, domT, classT) (* TBR: we allow polymorphic recursion *) in
    let ve1ce1ke1 = Env.singleton v.it (consT, ConstMut), ce0, ke0 in
    if pass = ValDecPass then ve1ce1ke1 else
    let context_ts_v_dom =
      {context_ts_v with values = union context_ts_v.values ve} in
    let rec pre_members context field_env = function
      | [] -> field_env
      | {it = {var; mut; priv; exp = {it = AnnotE (e, t); at}}}::efs ->
        let t = check_typ context t in
        let field_env = disjoint_add_field var.at field_env var.it (mut.it,priv.it,t)  in
        pre_members context field_env efs
      | {it = {var; mut; priv; exp = {it = DecE {it = FuncD (v, us, p, t, e)}}}}::efs ->
        let us,ce_us,ke_us = check_typ_binds context us in
        let context_us = union_kinds (union_constructors context ce_us) ke_us in
        let _, domT = infer_pat context_us p in
        let rngT = check_typ context_us t in
        let funcT = FuncT (us, domT, rngT) in
        let field_env = disjoint_add_field var.at field_env var.it (mut.it, priv.it, funcT) in
  	    pre_members context field_env efs
      | {it = {var; mut; priv; exp = e}}::efs ->
        let t = infer_exp context e in (* TBR: this feels wrong as we don't know all field types yet, just the ones to the left *)
        let field_env = disjoint_add_field var.at field_env var.it (mut.it, priv.it, t) in
        pre_members context field_env efs
    in
    let pre_members = pre_members context_ts_v_dom Env.empty efs in
    let private_context = Env.map (fun (m, p, t) -> (t, m)) pre_members in
    let bindings = Env.bindings pre_members in
    let public_bindings = List.filter (fun (v, (m, p, t)) -> p = Public) bindings in
    let public_fields = List.map (fun (v, (m, p, t)) -> {var = v; typ = t; mut = m}) public_bindings in
    (* reject mutable or non-sharable public actor fields *)
    List.iter (check_typ_field context_ts_v_dom a.at a.it) public_fields;
    let kind2 = ObjK (tbs, a.it, public_fields) in
    let ve2, ce2, ke2 = Env.singleton v.it (consT,ConstMut),Env.singleton v.it con, ConEnv.singleton con kind2 in
    if pass = TypDefPass then ve2,ce2,ke2 else
    let _ = assert (pass = ValDefPass) in
    let all_fields = List.map (fun (v, (m, p, t)) -> {var = v; typ = t; mut = m}) bindings in
    let kind3 = ObjK (tbs, a.it, all_fields) in
    let field_context =
      add_constructor
        (add_value (union_values context_ts_v_dom private_context) v.it (consT, ConstMut))
      	v.it con kind3
    in
    (* infer the fields *)
    List.iter (fun {it = {var; mut; exp}} -> ignore (infer_exp field_context exp)) efs;
    ve2, ce2, ke2


and check_decs_aux pass context ve ce ke ds =
  match ds with
  | [] -> ve, ce, ke
  | d::ds ->
    let ve1, ce1, ke1 = check_dec pass context d in
    check_decs_aux pass (union_kinds (union_constructors (union_values context ve1) ce1) ke1) (union ve ve1) (union ce ce1) (union_conenv ke ke1) ds
      
and check_decs context ds =
  (* declare type constructors *)
  let ve0, ce0, ke0 = check_decs_aux TypDecPass context Env.empty Env.empty ConEnv.empty ds in
  (* declare instance constructors, given type constructors *)
  let ve1, ce1, ke1 = check_decs_aux ValDecPass (union_kinds (union_constructors (union_values context ve0) ce0) ke0) Env.empty Env.empty ConEnv.empty ds in
  (* define type constructors (declare public member types) *)
  let ve2, ce2, ke2 = check_decs_aux TypDefPass (union_kinds (union_constructors (union_values context ve1) ce1) ke1) Env.empty Env.empty ConEnv.empty ds in
  (* check class definitions (check public and private member expressions *)
  let ve3, ce3, ke3 = check_decs_aux ValDefPass (union_kinds (union_constructors (union_values context ve2) ce2) ke2) Env.empty Env.empty ConEnv.empty ds in
  ve3, ce3, ke3

let check_prog p =
  check_decs empty_context p.it
