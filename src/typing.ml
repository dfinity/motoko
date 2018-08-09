open Syntax
open Source
open Type
open Operators

exception KindError of Source.region * string
exception TypeError of Source.region * string

let kind_error region fmt =
  Printf.ksprintf (fun s -> raise (KindError (region, s))) fmt

let type_error region fmt =
  Printf.ksprintf (fun s -> raise (TypeError (region, s))) fmt

type val_env = (typ * mut) Env.t
type typ_env = con Env.t
type con_env = Type.con_env

type context =
  {
    vals : val_env;
    typs : typ_env;
    cons : con_env;
    labels : typ Env.t;
    returns : typ option;
    awaitable : bool
  }

let adjoin_vals c ve = {c with vals = Env.adjoin c.vals ve}
let adjoin_typs c te = {c with typs = Env.adjoin c.typs te}
let adjoin_cons c ke = {c with cons = Con.Env.adjoin c.cons ke}

let add_val c v tm = {c with vals = Env.add v tm c.vals}
let add_typ c v con kind =
  {c with typs = Env.add v con c.typs; cons = Con.Env.add con kind c.cons}

let disjoint_union at fmt env1 env2 =
  try Env.disjoint_union env1 env2 with Env.Overlap k -> type_error at fmt k
let disjoint_add at fmt k x env =
  try Env.disjoint_add k x env with Env.Overlap k -> type_error at fmt k

(* TBD
let disjoint_union_vals at c ve =   {c with vals = disjoint_union at "duplicate value %s"  c.vals ve}
let disjoint_union_tys at c te = {c with typs = disjoint_union at "duplicate type %s" c.typs te}
let disjoint_add_val at c v t = disjoint_union_vals at c (Env.singleton v t)
let disjoint_add_typ at c con = disjoint_union_typs at c (Env.singleton v con)
*)

let empty_context =
  {
    vals = Env.empty;
    typs = Env.empty;
    cons = Con.Env.empty;
    labels = Env.empty;
    returns = None;
    awaitable = false
  }


let rec structural context t =
  match normalize context.cons t with
  | VarT (con, ts) ->
    (match Con.Env.find_opt con context.cons with
    | Some (DefK (tbs, t) | AbsK (tbs, t)) ->
	    structural context (subst (make_subst ts tbs) t)
    | None -> assert false
    )
  | LikeT t -> structural context t (*TBR*)
  | _ -> t

(* types one can switch on - all primitives except floats *)

(* TBR - switch on option type? *)
let switchable_typ context t =
  match normalize context.cons t with
  | PrimT p -> p <> FloatT
  | _ -> false

(* types one can iterate over using `for`  *)
let iterable_typ context t =
  match normalize context.cons t with
  | ArrayT _ -> true
  | _ -> false

(* element type of iterable_type  *)
let element_typ context t =
  match normalize context.cons t with
  | ArrayT (_, t) -> t
  | _ -> assert false

let numeric_typ context t =
  match normalize context.cons t with
  | PrimT (NatT | IntT | WordT _ | FloatT) -> true
  | _ -> false

let logical_typ context t =
  match normalize context.cons t  with
  | PrimT (WordT _) -> true
  | _ -> false

let equatable_typ context t =
  match normalize context.cons t with
  | PrimT (BoolT | NatT | IntT | WordT _ | FloatT | TextT | CharT) -> true
    (* TBR do we really want = and != on floats ?*)
  | _ -> false

let comparable_typ context t =
  match normalize context.cons t with
  | PrimT (NatT | IntT | WordT _ | FloatT | TextT | CharT) -> true
  | _ -> false


(* TBR: the whole notion of sharable typ needs to be reviewed in the presence of subtyping *)
let rec sharable_typ (context : context) (t : typ) : bool =
  match normalize context.cons t with
  | VarT (c, ts) ->
    (match Con.Env.find_opt c context.cons with
    | Some (DefK (tbs, t) | AbsK (tbs, t)) ->
      sharable_typ context (subst (make_subst ts tbs) t) (* TBR *)
    | None -> assert false
    )
  | PrimT p -> true
  | ArrayT (m, t) ->
    m = ConstMut && sharable_typ context t
  | TupT ts ->
    List.for_all (sharable_typ context) ts 
  | FuncT (tbs, t1, t2) ->
    let te, ke = sharable_typ_binds context tbs in
    let context' = adjoin_cons (adjoin_typs context te) ke in
    List.for_all (fun tb -> sharable_typ context' tb.bound) tbs && (* TBR *)
    sharable_typ context' t1 &&
    (match normalize context'.cons t2 with
    | TupT [] | AsyncT _ -> true
    | _ -> false
    )
    (* TBR: a function type should be non-sharable if it closes over non-shareable locals *)
  | OptT t -> sharable_typ context t
  | AsyncT t -> sharable_typ context t
  | LikeT t -> sharable_typ context t
  | ObjT (Object, fs) ->
    (*TBR: this isn't stable with subtyping *)
    List.for_all (fun {lab; typ; mut}-> mut = ConstMut && sharable_typ context typ) fs
  | ObjT (Actor, fs) -> true
  | AnyT -> false (* TBR *)

and sharable_typ_binds context ts =
  let te = List.fold_left (fun c (bind : Type.typ_bind) -> Env.add (Con.name bind.con) bind.con c) context.typs ts in
  let ke = List.fold_left (fun c (bind : Type.typ_bind) -> Con.Env.add bind.con (AbsK ([], bind.bound)) c) context.cons ts in
  te, ke


(* let sprintf = Printf.sprintf *)

let check_bounds at ts bounds =
  if List.length bounds <> List.length ts then
    type_error at "expecting %i type arguments, found %i arguments" (List.length bounds) (List.length ts)

(* todo: compute refutability of pats and enforce accordingly (refutable in all cases but last, irrefutable elsewhere) *)
(* todo: type ObjE expressions (anonymous objects)*)


(* checking literal values are in bounds *)

let check_lit p f at s = 
    try f s with 
    |  _ -> type_error at "bad or out-of-range literal for type %s" (string_of_typ (PrimT p))

let check_nat = check_lit NatT Value.Nat.of_string_u
let check_int = check_lit IntT Value.Int.of_string_s
let check_word8 = check_lit (WordT Width8) Value.Word8.of_string_u
let check_word16 = check_lit (WordT Width16) Value.Word16.of_string_u
let check_word32 = check_lit (WordT Width32) Value.Word32.of_string_u
let check_word64 = check_lit (WordT Width64) Value.Word64.of_string_u

(* begin sanity test *)

let pow2 n = 1 lsl n
let lpow2 n = Int64.shift_left (Int64.of_int 1) n
let raises f r x = try f r x; false; with _ -> true

let _ =
  let module I32 = Wasm.I32 in
  (* text check_nat *)
  assert (check_nat no_region (string_of_int (pow2 Value.nat_width - 1)) = I32.of_int_u (pow2 Value.nat_width - 1));
  assert (check_nat no_region (string_of_int 0) = I32.of_int_u 0);
  assert (raises check_nat no_region (string_of_int (pow2 32)));
  assert (raises check_nat no_region (string_of_int (pow2 32 + 1)));

  (* test check_word16 *)
  assert (check_word16 no_region (string_of_int (pow2 16 - 1)) = Value.Word16.of_int_u (pow2 16 - 1));
  assert (check_word16 no_region (string_of_int 0) = Value.Word16.of_int_u 0);
  assert (raises check_word16 no_region (string_of_int (pow2 16)));
  assert (raises check_word16 no_region (string_of_int (pow2 16 + 1)));

  (* test check_int *)
  assert (check_int no_region (string_of_int (pow2 (Value.int_width - 1) - 1)) = I32.of_int_s (pow2 (Value.int_width - 1) - 1));
  assert (check_int no_region (string_of_int (- pow2 (Value.int_width - 1))) = I32.of_int_s (- pow2 (Value.int_width - 1)));
  assert (raises check_int no_region (Int64.to_string (lpow2 (Value.int_width - 1))));
  assert (raises check_int no_region (Int64.to_string (Int64.neg (Int64.add (lpow2 (Value.int_width - 1)) 1L))));
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
      {Type.con = con; bound = check_typ bind_context bind.it.Syntax.bound}
    ) ts in
  let te =
    List.fold_left (fun c (bind : Type.typ_bind) ->
      Env.add (Con.name bind.con) bind.con c
    ) context.typs ts in
  let ke =
    List.fold_left (fun c (bind : Type.typ_bind) ->
      Con.Env.add bind.con (AbsK ([], bind.bound)) c
    ) context.cons ts in
  ts, te, ke

(*TBD do we want F-bounded checking with mutually recursive bounds? *)

and check_typ context t =
  match t.it with
  | Syntax.VarT (c, tys) ->
    let ts = List.map (check_typ context) tys in
    (match Env.find_opt c.it context.typs with
    | Some con ->
      (match Con.Env.find_opt con context.cons with
      | Some (DefK (tbs, u) | AbsK (tbs, u)) ->
        check_bounds t.at ts tbs;
        (* subst ts bounds u ? *)
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
    let ts, te, ke = check_typ_binds context tbs in
    let context = adjoin_cons (adjoin_typs context te) ke in
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
    	  {lab = f.it.var.it; typ = check_typ context f.it.typ; mut = f.it.mut.it}
      ) fs in
    List.iter (check_typ_field context a.at a.it) fs;
    (* sort by name (for indexed access *)
    let fs' = List.sort (fun f1 f2 -> String.compare f1.lab f2.lab) fs in
    ObjT (a.it, fs')
  | Syntax.AnyT -> AnyT

and check_typ_field context at actor {lab; mut; typ} =
  match actor with
  | Object -> ()
  | Actor when mut = VarMut ->
    type_error at "public field %s of actor is mutable (an actor's public fields must be immutable)" lab
  | Actor ->
    if not (sharable_typ context typ)
    then type_error at "public field %s of actor has non-sharable type %s (the type of an actor's public field must be shareable)" lab (string_of_typ typ)


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
    rl := IntLit (Value.Int.of_string s); (* default *)
    PrimT IntT


and check_lit at context t rl =
  match normalize context.cons t, !rl with
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
    rl := NatLit v 
  | PrimT IntT, PreLit s ->
    let v = check_int at s in
    rl := IntLit v
  | PrimT (WordT Width8), PreLit s ->
    let v = check_word8 at s in
    rl := WordLit (Word8 v ) 
  | PrimT (WordT Width16), PreLit s ->
    let v = check_word16 at s in
    rl := WordLit (Word16 v)
  | PrimT (WordT Width32), PreLit s ->
    let v = check_word32 at s in
    rl := WordLit (Word32 v)
  | PrimT (WordT Width64), PreLit s ->
    let v = check_word64 at s in
    rl := WordLit (Word64 v)

  | PrimT _, _ ->
    let u = infer_lit context rl in
    if not (Type.eq context.cons t u)
    then type_error at "expect literal of type %s, found literal of type %s" (string_of_typ t) (string_of_typ u)

  | _ -> type_error at "type %s has no literals" (string_of_typ t)

and infer_binop context at e1 bop e2 =
  let t1 = infer_exp context e1 in
  let t2 = infer_exp context e2 in
  if Type.eq context.cons t1 t2
  then if has_binop e1.note bop
       then t1
       else type_error at "binary operator not available at argument type %s" (string_of_typ t1)
  else type_error at "arguments to binary operator must have equivalent types, found distinct types %s and %s" (string_of_typ t1) (string_of_typ t2)
 
and check_binop context at t e1 bop e2 =
  if has_binop t bop
  then (check_exp context t e1;
        check_exp context t e2)
  else type_error at "binary operator not available at expected type %s" (string_of_typ t)
 
and infer_relop context at e1 rop e2 =
  let t1 = infer_exp context e1 in
  let t2 = infer_exp context e2 in
  if Type.eq context.cons t1 t2
  then if has_relop e1.note rop
       then boolT
       else type_error at "relational operator not available at argument type %s" (string_of_typ t1)
  else type_error at "arguments to relational operator must have equivalent types, found arguments of distinct types %s and %s" (string_of_typ t1) (string_of_typ t2)

and check_relop context at t e1 rop e2 =
  if not (Type.eq context.cons t boolT)
  then type_error at "expecting value of non-boolean type %s, relational operator returns a value of Bool type" (string_of_typ t);
  ignore(infer_relop context at e1 rop e2)
 
and infer_unop context at uop e =
  let t = infer_exp context e in
  if has_unop e.note uop then
    t
  else type_error at "unary operator not available at argument type %s" (string_of_typ t)
  
and check_unop context at t uop e =
  check_exp context t e;
  if has_unop e.note uop
  then ()
  else type_error at "unary operator not available at expected type %s" (string_of_typ t);


and infer_exp context e =
  let t = infer_exp' context e in
  e.note <- normalize context.cons t;
  t

and infer_exp' context e =
  match e.it with
  | VarE x ->
    (match Env.find_opt x.it context.vals with
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
    (match normalize context.cons (infer_exp context e) with
    | TupT ts ->
      (try List.nth ts (Int32.to_int n)
      with Failure _ -> type_error e.at "tuple projection %li >= %n is out-of-bounds" n (List.length ts))
    | t -> type_error e.at "expecting tuple type, found %s" (string_of_typ t)
    )
  | DotE (e, v) ->
    (match structural context (infer_exp context e) with
    | ObjT (a, fts) as t ->
      (try
        let ft = List.find (fun (fts : typ_field) -> fts.lab = v.it) fts in
  	    v.note <- ft.mut;
        ft.typ
      with Not_found -> type_error e.at "object of type %s has no field named %s" (string_of_typ t) v.it)
    | t -> type_error e.at "expecting object type, found %s" (string_of_typ t)
    )
  | AssignE (e1, e2) ->
    (*TBC: array and object update *)
    (match e1.it with
    | VarE v ->
      (match Env.find_opt v.it context.vals with
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
      (match structural context (infer_exp context o) with
      | ObjT (a, fts) as t ->
  	    (try
          let ft = List.find (fun (fts : typ_field) -> fts.lab = v.it) fts in
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
      (match normalize context.cons (infer_exp context ea) with
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
    if List.for_all (Type.eq context.cons t1) (List.tl ts)
    then ArrayT (mut.it, t1)
    else type_error e.at "array contains elements of distinct types"
  | IdxE (e1, e2) ->
    (match normalize context.cons (infer_exp context e1) with
    | ArrayT (_, t1) -> 
      check_exp context intT e2;
      t1
    | t -> type_error e.at "illegal indexing: expected an array, found" (string_of_typ t)
    )
  | CallE (e1, ts, e2) ->
    let ts = List.map (check_typ context) ts in
    (match normalize context.cons (infer_exp context e1) with
    | FuncT (tbs, t3, t4) -> (* TBC polymorphic instantiation, perhaps by matching? *)
      check_bounds e.at ts tbs;
      let sigma = make_subst ts tbs in
      let t3' = subst sigma t3 in
      let t4' = subst sigma t4 in
      let t2 = infer_exp context e2 in
      if Type.eq context.cons t2 t3'
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
    if Type.eq context.cons t1 t2 
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
    let context' = {context with vals = Env.adjoin context.vals ve} in
    check_exp context' unitT e1;
    unitT
  | LabelE (l, e) ->
    let context' = {context with labels = Env.add l.it unitT context.labels} in
    infer_exp context' e
  | BreakE (l, e) ->
    (match Env.find_opt l.it context.labels  with
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
      match normalize context.cons (infer_exp context e0) with
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
    check_decs context [d];
    (match Env.find_opt v.it context.vals with
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
    let t =
    	match t_opt with
	| None -> infer_exp (adjoin_vals context ve) exp 
	| Some t ->
	  check_exp (adjoin_vals context ve) t exp;
	  t
    in
    infer_cases context pt cs (Some t)

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
    (match normalize context.cons t with
    | ArrayT (mut', t) when mut' = mut.it ->
      List.iter (check_exp context t) es
    | _ -> type_error e.at "array expression cannot produce expected type %s" (string_of_typ t)
    )
(* | IdxE(e1, e2) ->
   TBR, unfortunately, we can't easily do IdxE in checking mode because we don't know whether to expect
   a mutable or immutable array type (we could do better on assignment of course),
   so we rely on inference instead
*)
  | AsyncE e0 ->
    (match normalize context.cons t with
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
  | IfE (e0, e1, e2) ->
    check_exp context boolT e0;
    check_exp context t e1;
    check_exp context t e2
  | SwitchE (e1, cs) ->
    let t1 = infer_exp context e1 in
    if not (switchable_typ context t1) then type_error e1.at "illegal type for switch";
    (match infer_cases context t1 cs (Some t) with
    | Some t' -> assert (Type.eq context.cons t t')
    | None -> assert false
    )
  | BreakE _ ->
    ignore (infer_exp context e)
  | RetE _ ->
    ignore (infer_exp context e)
  | LabelE (l, e) ->
    let context' = {context with labels = Env.add l.it t context.labels} in
    check_exp context' t e
  | _ ->
    let t' = infer_exp context e in
    if not (Type.eq context.cons t t')
    then type_error e.at "expecting expression of type %s found expression of type %s" (string_of_typ t) (string_of_typ t')
  );
  e.note <- normalize context.cons t    
    
and infer_block context es =
  match es with
  | [] -> unitT
  | {it = DecE d; at}::es ->
    let ve, te, ke = check_decs context [d] in (* TBR: we currently check local decs sequentially, not recursively *)
    infer_block (adjoin_cons (adjoin_typs (adjoin_vals context ve) te) ke) es
  | [e] -> infer_exp context e
  | e::es ->
    check_exp context unitT e; (* TBR: is this too strict? do we want to allow implicit discard? *)
    infer_block context es

and check_block r context t es =
  match es with
  | [] ->
    if not (Type.eq context.cons t unitT)
    then type_error r "block  must end with expression of type" (string_of_typ t) 
  | {it = DecE d; at}::es ->
    let ve, te, ke = check_decs context [d] in (* TBR: we currently check local decs sequentially, not recursively *)
    check_block r (adjoin_cons (adjoin_typs (adjoin_vals context ve) te) ke) t es
  | [e] -> check_exp context t e
  | e::es ->
    check_exp context unitT e; (* TBR: is this too strict? do we want to allow implicit discard? *)
    check_block r context t es 


and infer_pats at context ve ts ps =
  match ps with
  | [] -> ve, TupT (List.rev ts)
  | p::ps ->
    let ve', t = infer_pat context p in
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
  match ps, ts with
  | [], [] -> ve
  | p::ps, t::ts ->
    let ve' = check_pat context p t in
    check_pats at context (disjoint_union at "duplicate binding for %s in pattern" ve ve') ps ts  (*TBR reject shadowing *)
  | [], ts -> type_error at "tuple pattern has %i fewer components than expected type" (List.length ts)
  | ts, [] -> type_error at "tuple pattern has %i more components than expected type" (List.length ts)
         
and check_pat context p t =
  match p.it with
  | WildP -> Env.empty
  | VarP v -> Env.singleton v.it (t, ConstMut)
  | LitP rl ->
    check_lit p.at context t rl;
    Env.empty
  | TupP ps ->
    (match normalize context.cons t with
    | TupT ts -> check_pats p.at context Env.empty ps ts 
    | _ -> type_error p.at "expected pattern of non-tuple type, found pattern of tuple type"
    )
  | AnnotP (p', t') ->
    let t' = check_typ context t' in
    if Type.eq context.cons t t'
    then check_pat context p' t'
    else type_error p.at "expected pattern of one type, found pattern of unequal type"

      
and check_dec pass context d =
  let ve, te, ke = check_dec' pass context d in    
  (* TBC store ve *)
  ve, te, ke

and check_dec' pass context d =     
  match d.it with
  | LetD (p, e) ->
    if pass < TypDefPass then Env.empty, Env.empty, Con.Env.empty else      
    let t = infer_exp context e in
    let ve = check_pat context p t in 
    ve, Env.empty, Con.Env.empty
  | VarD (v, t, None) ->
    if pass < TypDefPass then Env.empty, Env.empty, Con.Env.empty else
    let t = check_typ context t in
    Env.singleton v.it (t, VarMut), Env.empty, Con.Env.empty
  | VarD (v, t, Some e) ->
    if pass < TypDefPass then Env.empty, Env.empty, Con.Env.empty else
    let t = check_typ context t in
    check_exp context t e;
    Env.singleton v.it (t, VarMut), Env.empty, Con.Env.empty
  | TypD (v, tps, t) ->
    let tbs, te_tbs, ke_tbs = check_typ_binds context tps in
    let con =
      if pass = TypDecPass
      then Con.fresh v.it
	    else Lib.Option.value (Env.find_opt v.it context.typs)
    in
    let kind0 = AbsK (tbs, VarT (con, [])) in  (* dummy abstract type *)
    let te0 = Env.singleton v.it con in
    let ke0 = Con.Env.singleton con kind0 in
    if pass = TypDecPass then Env.empty, te0, ke0 else
    let context_tbs = adjoin_cons (adjoin_typs context te_tbs) ke_tbs in
    let t = check_typ context_tbs t in
    let kind1 = DefK (tbs, t) in  (* dummy type *)
    let te1 = Env.singleton v.it con in
    let ke1 = Con.Env.singleton con kind1 in
    Env.empty, te1, ke1
  | FuncD (v, tps, p, t, e) ->
    if pass < TypDefPass then Env.empty, Env.empty, Con.Env.empty else
    let tbs, te, ke = check_typ_binds context tps in
    let context_te = adjoin_cons (adjoin_typs context te) ke in
    let ve, t1 = infer_pat context_te p in
    let t2 = check_typ context_te t in
    let funcT = FuncT (tbs, t1, t2) in  (* TBR: we allow polymorphic recursion *)
    let context_te_ve_v =
      { (add_val (adjoin_vals context_te ve) v.it (funcT, ConstMut)) with
	      labels = Env.empty;
        returns = Some t2;
        awaitable = false
      } in
    check_exp context_te_ve_v t2 e;
    Env.singleton v.it (funcT, ConstMut), Env.empty, Con.Env.empty
  | ClassD (a, v, tps, p, efs) ->
    let tbs, te_ts, ke_ts = check_typ_binds context tps in
    let con =
      if pass = TypDecPass
      then Con.fresh v.it
	    else Lib.Option.value (Env.find_opt v.it context.typs)
    in
    let kind0 = AbsK (tbs, ObjT (a.it, [])) in
    let te0 = Env.singleton v.it con in
    let ke0 = Con.Env.singleton con kind0 in
    if pass = TypDecPass then Env.empty, te0, ke0 else
    let context_ts = adjoin_cons (adjoin_typs context te_ts) ke_ts in
    let context_ts_v = add_typ context_ts v.it con kind0 in
    let ve, domT = infer_pat context_ts_v p in
    let classT = VarT (con, List.map (fun (t : Type.typ_bind) -> VarT (t.con, [])) tbs) in
    let consT = FuncT (tbs, domT, classT) (* TBR: we allow polymorphic recursion *) in
    let ve1te1ke1 = Env.singleton v.it (consT, ConstMut), te0, ke0 in
    if pass = ValDecPass then ve1te1ke1 else
    let context_ts_v_dom =
      {context_ts_v with vals = Env.adjoin context_ts_v.vals ve} in
    let rec pre_members context field_env = function
      | [] -> field_env
      | {it = {var; mut; priv; exp = {it = AnnotE (e, t); at}}}::efs ->
        let t = check_typ context t in
        let field_env = disjoint_add var.at "duplicate field %s" var.it (mut.it, priv.it, t) field_env in
        pre_members context field_env efs
      | {it = {var; mut; priv; exp = {it = DecE {it = FuncD (v, us, p, t, e)}}}}::efs ->
        let us, te_us, ke_us = check_typ_binds context us in
        let context_us = adjoin_cons (adjoin_typs context te_us) ke_us in
        let _, domT = infer_pat context_us p in
        let rngT = check_typ context_us t in
        let funcT = FuncT (us, domT, rngT) in
        let field_env = disjoint_add var.at "duplicate field %s" var.it (mut.it, priv.it, funcT) field_env in
  	    pre_members context field_env efs
      | {it = {var; mut; priv; exp = e}}::efs ->
        let t = infer_exp context e in (* TBR: this feels wrong as we don't know all field types yet, just the ones to the left *)
        let field_env = disjoint_add var.at "duplicate field %s" var.it (mut.it, priv.it, t) field_env in
        pre_members context field_env efs
    in
    let pre_members = pre_members context_ts_v_dom Env.empty efs in
    let private_context = Env.map (fun (m, p, t) -> (t, m)) pre_members in
    let bindings = Env.bindings pre_members in
    let public_bindings = List.filter (fun (v, (m, p, t)) -> p = Public) bindings in
    let public_fields = List.map (fun (lab, (mut, _, typ)) -> {lab; typ; mut}) public_bindings in
    (* reject mutable or non-sharable public actor fields *)
    List.iter (check_typ_field context_ts_v_dom a.at a.it) public_fields;
    let kind2 = AbsK (tbs, ObjT (a.it, public_fields)) in
    let ve2, te2, ke2 = Env.singleton v.it (consT, ConstMut), Env.singleton v.it con, Con.Env.singleton con kind2 in
    if pass = TypDefPass then ve2, te2, ke2 else
    let _ = assert (pass = ValDefPass) in
    let all_fields = List.map (fun (lab, (mut, _, typ)) -> {lab; typ; mut}) bindings in
    let kind3 = AbsK (tbs, ObjT (a.it, all_fields)) in
    let field_context =
      add_typ
        (add_val (adjoin_vals context_ts_v_dom private_context) v.it (consT, ConstMut))
      	v.it con kind3
    in
    (* infer the fields *)
    List.iter (fun {it = {var; mut; exp}} -> ignore (infer_exp field_context exp)) efs;
    ve2, te2, ke2


and check_decs_aux pass context ve te ke = function
  | [] -> ve, te, ke
  | d::ds ->
    let ve1, te1, ke1 = check_dec pass context d in
    check_decs_aux pass (adjoin_cons (adjoin_typs (adjoin_vals context ve1) te1) ke1) (Env.adjoin ve ve1) (Env.adjoin te te1) (Con.Env.adjoin ke ke1) ds
      
and check_decs context ds =
  (* declare type constructors *)
  let ve0, te0, ke0 = check_decs_aux TypDecPass context Env.empty Env.empty Con.Env.empty ds in
  (* declare instance constructors, given type constructors *)
  let ve1, te1, ke1 = check_decs_aux ValDecPass (adjoin_cons (adjoin_typs (adjoin_vals context ve0) te0) ke0) Env.empty Env.empty Con.Env.empty ds in
  (* define type constructors (declare public member types) *)
  let ve2, te2, ke2 = check_decs_aux TypDefPass (adjoin_cons (adjoin_typs (adjoin_vals context ve1) te1) ke1) Env.empty Env.empty Con.Env.empty ds in
  (* check class definitions (check public and private member expressions *)
  let ve3, te3, ke3 = check_decs_aux ValDefPass (adjoin_cons (adjoin_typs (adjoin_vals context ve2) te2) ke2) Env.empty Env.empty Con.Env.empty ds in
  ve3, te3, ke3

let check_prog p =
  check_decs empty_context p.it
