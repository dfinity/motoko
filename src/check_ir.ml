open Source
module T = Type
module E = Effect

(* TODO: remove DecE from syntax, replace by BlockE [dec] *)
(* TODO: check constraint matching supports recursive bounds *)

(* TODO: make note immutable, perhaps just using type abstraction *)

(* TODO:
   dereferencing is still implicit in the IR (see immut_typ below) - consider making it explicit as   part of desugaring.
 *)

(* TODO: enforce second-class nature of T.Mut? in check_typ *)
(* TODO: check escape of free mutables via actors *)

(* helpers *)
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
    vals = scope.Typing.val_env;
    cons = scope.Typing.con_env;
    labs = T.Env.empty;
    rets = None;
    async = false;
  }


(* More error bookkeeping *)

exception CheckFailed of string

let type_error at text : Diag.message = Diag.{ sev = Diag.Error; at; cat = "IR type"; text }

let error env at fmt =
    Printf.ksprintf (fun s -> raise (CheckFailed (Diag.string_of_message (type_error at s)))) fmt


let add_lab c x t = {c with labs = T.Env.add x t c.labs}
let add_val c x t = {c with vals = T.Env.add x t c.vals}

let add_typs c cs =
  { c with
    cons = List.fold_right (fun c -> T.ConSet.disjoint_add c) cs c.cons;
  }

let adjoin c scope =
  { c with
    vals = T.Env.adjoin c.vals scope.val_env;
    cons = T.ConSet.disjoint_union c.cons scope.con_env;
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

let check_ids env ids = ignore
  (List.fold_left
    (fun dom id ->
      if List.mem id dom
      then error env no_region "duplicate field name %s in object type" id
      else id::dom
    ) [] ids
  )

let check env at p =
  if p then ignore
  else error env at

let check_sub env at t1 t2 =
  if T.sub t1 t2
  then ()
  else error env at "subtype violation:\n  %s\n  %s\n"
    (T.string_of_typ_expand t1) (T.string_of_typ_expand t2)

let make_mut mut : T.typ -> T.typ =
  match mut.it with
  | Syntax.Const -> fun t -> t
  | Syntax.Var -> fun t -> T.Mut t

let rec check_typ env typ : unit =
  match typ with
  | T.Pre ->
    error env no_region "illegal T.Pre type"
  | T.Var (s,i) ->
    error env no_region "free type variable %s, index %i" s  i
  | T.Con (c,typs) ->
    if not (T.ConSet.mem c env.cons) then
       error env no_region "free type constructor %s" (Con.name c);
    (match Con.kind c with | T.Def (tbs, t) | T.Abs (tbs, t)  ->
      check_typ_bounds env tbs typs no_region
    )
  | T.Any -> ()
  | T.Non -> ()
  | T.Shared -> ()
  | T.Prim _ -> ()
  | T.Array typ ->
    check_typ env typ
  | T.Tup typs ->
    List.iter (check_typ env) typs
  | T.Func (sort, control, binds, ts1, ts2) ->
    let cs, ce = check_typ_binds env binds in
    let env' = adjoin_cons env  ce in
    let ts = List.map (fun c -> T.Con(c,[])) cs in
    let ts1 = List.map (T.open_ ts) ts1 in
    let ts2 = List.map (T.open_ ts) ts2 in
    List.iter (check_typ env') ts1;
    List.iter (check_typ env') ts2;
    if control = T.Promises then begin
      match ts2 with
      | [T.Async _ ] -> ()
      | _ ->
        let t2 = T.seq ts2 in
        error env no_region "promising function with non-async result type \n  %s"
          (T.string_of_typ_expand t2)
    end;
    if sort = T.Sharable then begin
      let t1 = T.seq ts1 in
      check_sub env' no_region t1 T.Shared;
      match ts2 with
      | [] -> ()
      | [T.Async t2] ->
        check_sub env' no_region t2 T.Shared;
      | _ -> error env no_region "shared function has non-async result type\n  %s"
          (T.string_of_typ_expand (T.seq ts2))
    end
  | T.Opt typ ->
    check_typ env typ
  | T.Async typ ->
    check env no_region env.flavor.Ir.has_async_typ "async in non-async flavor";
    let t' = T.promote typ in
    check_sub env no_region t' T.Shared
  | T.Obj (sort, fields) ->
    let rec sorted fields =
      match fields with
      | []
      | [_] -> true
      | f1::((f2::_) as fields') ->
        T.compare_field f1 f2  < 0 && sorted fields'
    in
    check_ids env (List.map (fun (field : T.field) -> field.T.name) fields);
    List.iter (check_typ_field env sort) fields;
    check env no_region (sorted fields) "object type's fields are not sorted"
  | T.Mut typ ->
    check_typ env typ

and check_typ_field env s typ_field : unit =
  let {T.name; T.typ} = typ_field in
  check_typ env typ;
  check env no_region
     (s <> T.Actor || T.is_func (T.promote typ))
    "actor field has non-function type";
  check env no_region
     (s = T.Object T.Local || T.sub typ T.Shared)
    "shared object or actor field has non-shared type"


and check_typ_binds env typ_binds : T.con list * con_env =
  let ts = Type.open_binds typ_binds in
  let cs = List.map (function T.Con(c,[]) ->  c | _ -> assert false) ts in
  let env' = add_typs env cs in
  let _ = List.map
            (fun typ_bind ->
              let bd = T.open_ ts typ_bind.T.bound  in
              check_typ env' bd)
            typ_binds
  in
  cs, T.ConSet.of_list cs

and check_typ_bounds env (tbs : T.bind list) typs at : unit =
  match tbs, typs with
  | tb::tbs', typ::typs' ->
    check_typ env typ;
    check env at (T.sub typ tb.T.bound)
      "type argument does not match parameter bound";
    check_typ_bounds env tbs' typs' at
  | [], [] -> ()
  | [], _ -> error env at "too many type arguments"
  | _, [] -> error env at "too few type arguments"

and check_inst_bounds env tbs typs at =
  check_typ_bounds env tbs typs at

(* Literals *)

let type_lit env lit at : T.prim =
  let open Syntax in
  match lit with
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
  | PreLit (s,p) ->
    error env at "unresolved literal %s of type\n %s" s (T.string_of_prim p)

open Ir

(* Expressions *)

let isAsyncE exp =
  match exp.it with
  | AsyncE _ -> (* pre await transformation *)
    true
  | CallE(_,{it=PrimE("@async");_},_,cps) -> (* post await transformation *)
    true
  | _ ->
    false

let rec check_exp env (exp:Ir.exp) : unit =
  (* helpers *)
  let check p = check env exp.at p in
  let (<:) t1 t2 = check_sub env exp.at t1 t2 in
  let (<~) t1 t2 =
    (if T.is_mut t2 then t1 else T.as_immut t1) <: t2
  in
  (* check effect *)
  check (E.Ir.infer_effect_exp exp <= E.eff exp)
    "inferred effect not a subtype of expected effect";
  (* check typing *)
  let t = E.typ exp in
  match exp.it with
  | PrimE _ -> ()
  | VarE id ->
    let t0 = try T.Env.find id.it env.vals with
             |  Not_found -> error env id.at "unbound variable %s" id.it
    in
      t0 <~ t
  | LitE lit ->
    T.Prim (type_lit env lit exp.at) <: t
  | UnE (ot, op, exp1) ->
    check (Operator.has_unop ot op) "unary operator is not defined for operand type";
    check_exp env exp1;
    typ exp1 <: ot;
    ot <: t;
  | BinE (ot, exp1, op, exp2) ->
    check (Operator.has_binop ot op) "binary operator is not defined for operand type";
    check_exp env exp1;
    check_exp env exp2;
    typ exp1 <: ot;
    typ exp2 <: ot;
    ot <: t;
  | RelE (ot,exp1, op, exp2) ->
    check (Operator.has_relop ot op) "relational operator is not defined for operand type";
    check_exp env exp1;
    check_exp env exp2;
    typ exp1 <: ot;
    typ exp2 <: ot;
    T.bool <: t;
  | TupE exps ->
    List.iter (check_exp env) exps;
    T.Tup (List.map typ exps) <: t;
  | OptE exp1 ->
    check_exp env exp1;
    T.Opt (typ exp1) <: t;
  | ProjE (exp1, n) ->
    begin
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
    end
  | ActorE ( id, fields, t0) ->
    let env' = { env with async = false } in
    let t1 =  type_obj env' T.Actor id t fields in
    let t2 = T.promote t in
    check (T.is_obj t2) "bad annotation (object type expected)";
    t1 <: t2;
    t0 <: t;
  | ActorDotE(exp1,{it = Syntax.Name n;_})
  | DotE (exp1, {it = Syntax.Name n;_}) ->
    begin
      check_exp env exp1;
      let t1 = typ exp1 in
      let sort, tfs =
        try T.as_obj_sub n t1 with
        | Invalid_argument _ ->
          error env exp1.at "expected object type, but expression produces type\n  %s"
            (T.string_of_typ_expand t1)
      in
      check (match exp.it with
             | ActorDotE _ -> sort = T.Actor
             | DotE _ -> sort <> T.Actor
             | _ -> false) "sort mismatch";
      match List.find_opt (fun {T.name; _} -> name = n) tfs with
      | Some {T.typ = tn;_} ->
        tn <~ t
      | None ->
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
    T.unit <: t;
  | ArrayE (mut, t0, exps) ->
    List.iter (check_exp env) exps;
    List.iter (fun e -> typ e <: t0) exps;
    let t1 = T.Array (match mut.it with Syntax.Const -> t0 | Syntax.Var -> T.Mut t0) in
    t1 <: t;
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
    let tbs, t2, t3 =
      try T.as_func_sub (List.length insts) t1 with
      |  Invalid_argument _ ->
         error env exp1.at "expected function type, but expression produces type\n  %s"
           (T.string_of_typ_expand t1)
    in
    check_inst_bounds env tbs insts exp.at;
    check_exp env exp2;
    (typ exp2) <: T.open_ insts t2;
    T.open_ insts t3 <: t;
  | BlockE (decs, t0) ->
    let t1, scope = type_block env decs exp.at in
    check_typ env t0;
    check (T.eq t T.unit || T.eq t1 t0) "unexpected expected block type";
    t0 <: t;
  | IfE (exp1, exp2, exp3) ->
    check_exp env exp1;
    typ exp1 <: T.bool;
    check_exp env exp2;
    typ exp2 <: t;
    check_exp env exp3;
    typ exp3 <: t;
  | SwitchE (exp1, cases) ->
    check_exp env exp1;
    let t1 = T.promote (typ exp1) in
(*    if not env.pre then
      if not (Coverage.check_cases env.cons cases t1) then
        warn env exp.at "the cases in this switch do not cover all possible values";
 *)
    check_cases env t1 t cases;
  | WhileE (exp1, exp2) ->
    check_exp env exp1;
    typ exp1 <: T.bool;
    check_exp env exp2;
    typ exp2 <: T.unit;
    T.unit <: t;
  | LoopE (exp1, expo) ->
    check_exp env exp1;
    typ exp1 <: T.unit;
    begin match expo with
    | Some exp2 ->
      check_exp env exp2;
      (typ exp2) <: T.bool;
    | _ -> ()
    end;
    T.Non <: t; (* vacuously true *)
  | ForE (pat, exp1, exp2) ->
    begin
      check_exp env exp1;
      let t1 = T.promote (typ exp1) in
      try
        let _, tfs = T.as_obj_sub "next" t1 in
        let t0 = T.lookup_field "next" tfs in
        let t1, t2 = T.as_mono_func_sub t0 in
        T.unit <: t1;
        let t2' = T.as_opt_sub t2 in
        let ve = check_pat_exhaustive env pat in
        pat.note <: t2';
        check_exp (adjoin_vals env ve) exp2;
        typ exp2 <: T.unit;
        T.unit <: t
      with Invalid_argument _ ->
        error env exp1.at "expected iterable type, but expression has type\n  %s"
          (T.string_of_typ_expand t1)
    end;
  | LabelE (id, t0, exp1) ->
    assert (t0 <> T.Pre);
    check_typ env t0;
    check_exp (add_lab env id.it t0) exp1;
    typ exp1 <: t0;
    t0 <: t;
  | BreakE (id, exp1) ->
    begin
      match T.Env.find_opt id.it env.labs with
      | None ->
        error env id.at "unbound label %s" id.it
      | Some t1 ->
        check_exp env exp1;
        typ exp1 <: t1;
        T.Non <: t1; (* vacuously true *)
    end;
  | RetE exp1 ->
    begin
      match env.rets with
      | None ->
        error env exp.at "misplaced return"
      | Some t0 ->
        assert (t0 <> T.Pre);
        check_exp env exp1;
        typ exp1 <: t0;
        T.Non <: t; (* vacuously true *)
    end;
  | AsyncE exp1 ->
    check env.flavor.has_await "async expression in non-await flavor";
    let t1 = typ exp1 in
    let env' =
      {env with labs = T.Env.empty; rets = Some t1; async = true} in
    check_exp env' exp1;
    t1 <: T.Shared;
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
    t2 <: t;
  | AssertE exp1 ->
    check_exp env exp1;
    typ exp1 <: T.bool;
    T.unit <: t;
  | DeclareE (id, t0, exp1) ->
    check_typ env t0;
    let env' = adjoin_vals env (T.Env.singleton id.it t0) in
    check_exp env' exp1;
    (typ exp1) <: t;
  | DefineE (id, mut, exp1) ->
    check_exp env exp1;
    begin
      match T.Env.find_opt id.it env.vals with
      | None -> error env id.at "unbound variable %s" id.it
      | Some t0 ->
        match mut.it with
        | Syntax.Const ->
          typ exp1 <: t0
        | Syntax.Var ->
          let t0 = try T.as_mut t0 with
                   | Invalid_argument _ ->
                     error env exp.at "expected mutable %s" (T.string_of_typ t0)
          in
          typ exp1 <: t0
    end;
    T.unit <: t
  | NewObjE (sort, labids, t0) ->
    let t1 =
      T.Obj(sort.it,
            List.sort T.compare_field (List.map (fun (name,id) ->
                                           {T.name = Syntax.string_of_name name.it;
                                            T.typ = T.Env.find id.it env.vals}) labids))
    in
    let t2 = T.promote t in
    check (T.is_obj t2) "bad annotation (object type expected)";
    t1 <: t2;
    t0 <: t;

(* Cases *)

and check_cases env t_pat t cases =
  List.iter (check_case env t_pat t) cases

and check_case env t_pat t {it = {pat; exp}; _} =
  let ve = check_pat env pat in
  check_sub env pat.at pat.note t_pat;
  check_exp (adjoin_vals env ve) exp;
  if not (T.sub (typ exp)  t) then
    error env exp.at "bad case"

(* Patterns *)

and gather_pat env ve0 pat : val_env =
  let rec go ve pat =
    match pat.it with
    | WildP
    | LitP _ ->
      ve
    | VarP id ->
      if T.Env.mem id.it ve0 then
        error env pat.at "duplicate binding for %s in block" id.it;
      T.Env.add id.it pat.note ve (*TBR*)
    | TupP pats ->
      List.fold_left go ve pats
    | AltP (pat1, pat2) ->
      ve
    | OptP pat1 ->
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
  | VarP id -> T.Env.singleton id.it pat.note
  | LitP lit ->
    let t1 = T.Prim (type_lit env lit pat.at) in
    t1 <: t;
    T.Env.empty
  | TupP pats ->
    let ve = check_pats pat.at env pats T.Env.empty in
    let ts = List.map (fun pat -> pat.note) pats in
    T.Tup ts <: t;
    ve
  | OptP pat1 ->
    let ve = check_pat env pat1 in
    T.Opt pat1.note <: t;
    ve
  | AltP (pat1, pat2) ->
    let ve1 = check_pat env pat1 in
    let ve2 = check_pat env pat2 in
    pat1.note <: t;
    pat2.note <: t;
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

(* Objects *)

and type_obj env s id t fields : T.typ =
  let ve = gather_exp_fields env id.it t fields in
  let env' = adjoin_vals env ve in
  let tfs, _ve = type_exp_fields env' s id.it t fields in
  T.Obj(s,tfs)

and gather_exp_fields env id t fields : val_env =
  let ve0 = T.Env.singleton id t in
  List.fold_left (gather_exp_field env) ve0 fields

and gather_exp_field env ve field : val_env =
  let {id; exp; mut; priv;_} : exp_field' = field.it in
  if T.Env.mem id.it ve then
    error env id.at "duplicate field name %s in object" id.it;
  T.Env.add id.it ( make_mut mut (typ exp)) ve

and type_exp_fields env s id t fields : T.field list * val_env =
  let env' = add_val env id t in
  let tfs, ve =
    List.fold_left (type_exp_field env' s) ([], T.Env.empty) fields in
  List.sort T.compare_field tfs, ve

and is_func_exp exp =
  match exp.it with
  | BlockE ([dec],_)-> is_func_dec dec
  | _ -> false

and is_func_dec dec =
  match dec.it with
  | FuncD _ -> true
  | _ -> false

and type_exp_field env s (tfs, ve) field : T.field list * val_env =
  let {id; name; exp; mut; priv} = field.it in
  let t = try T.Env.find id.it env.vals with
          | Not_found -> error env field.at "field typing not found"
  in
  assert (t <> T.Pre);
  check_exp (adjoin_vals env ve)  exp;
  check_sub env field.at (typ exp) (T.as_immut t);
  check env field.at ((mut.it = Syntax.Var) = T.is_mut t)
    "inconsistent mutability of field and field type";
  check env field.at
    ((s = T.Actor && priv.it = Syntax.Public) ==>
       is_func_exp exp)
    "public actor field is not a function";
  check env field.at
    (if (s <> T.Object T.Local && priv.it = Syntax.Public)
     then T.sub t T.Shared
     else true)
    "public shared object or actor field has non-shared type";
  let ve' = T.Env.add id.it t ve in
  let tfs' =
    if priv.it = Syntax.Private
    then tfs
    else {T.name = Syntax.string_of_name name.it; typ = t} :: tfs
  in tfs', ve'


(* Blocks and Declarations *)

and type_block env decs at : T.typ * scope =
  let scope = gather_block_decs env decs in
  let t = type_block_exps (adjoin env scope) decs in
  t, scope

and type_block_exps env decs : T.typ =
  match decs with
  | [] -> T.unit
  | [dec] ->
    check_dec env dec;
    typ dec;
  | dec::decs' ->
    check_dec env dec;
    type_block_exps env decs'

and check_open_typ_binds env typ_binds =
  let cs = List.map (fun tp -> tp.it.con) typ_binds in
  let ce = List.fold_right (fun c ce -> T.ConSet.disjoint_add c ce) cs T.ConSet.empty in
  let binds = close_typ_binds cs (List.map (fun tb -> tb.it) typ_binds) in
  let _,_ = check_typ_binds env binds in
  cs,ce

and close_typ_binds cs tbs =
  List.map (fun {con; bound} -> {Type.var = Con.name con; bound = Type.close cs bound}) tbs

and check_dec env dec  =
  (* helpers *)
  let check p = check env dec.at p in
  let (<:) t1 t2 = check_sub env dec.at t1 t2 in
  (* check effect *)
  check (E.Ir.infer_effect_dec dec <= E.eff dec)
    "inferred effect not a subtype of expected effect";
  (* check typing *)
  let t = typ dec in
  match dec.it with
  | ExpD exp ->
    check_exp env exp;
    (typ exp) <: t
  | LetD (_, exp) | VarD (_, exp) ->
    check_exp env exp;
    T.unit <: t
  | FuncD (cc, id, typ_binds, pat, t2, exp) ->
    let t0 = T.Env.find id.it env.vals in
    let _cs,ce = check_open_typ_binds env typ_binds in
    let env' = adjoin_cons env ce in
    let ve = check_pat_exhaustive env' pat in
    check_typ env' t2;
    check ((cc.Value.sort = T.Sharable && Type.is_async t2)
           ==> isAsyncE exp)
      "shared function with async type has non-async body";
    let env'' =
      {env' with labs = T.Env.empty; rets = Some t2; async = false} in
    check_exp (adjoin_vals env'' ve) exp;
    check_sub env' dec.at (typ exp) t2;
    t0 <: t;
  | TypD c ->
    check (T.ConSet.mem c env.cons) "free type constructor";
    let (binds,typ) =
      match Con.kind c with
      | T.Abs(binds,typ)
      | T.Def(binds,typ) -> (binds,typ)
    in
    let cs, ce = check_typ_binds env binds in
    let ts = List.map (fun c -> T.Con(c,[])) cs in
    let env' = adjoin_cons env ce in
    check_typ env' (T.open_ ts  typ);
    T.unit <: t;

and check_block env t decs at : scope =
  let scope = gather_block_decs env decs in
  check_block_exps (adjoin env scope) t decs at;
  scope

and check_block_exps env t decs at =
  match decs with
  | [] ->
    check_sub env at T.unit t
  | [dec] ->
    check_dec env dec;
    check env at (T.is_unit t || T.sub (typ dec) t)
      "declaration does not produce expect type"
  | dec::decs' ->
    check_dec env dec;
    check_block_exps env t decs' at

and gather_block_decs env decs =
  List.fold_left (gather_dec env) empty_scope decs

and gather_dec env scope dec : scope =
  match dec.it with
  | ExpD _ ->
    scope
  | LetD (pat, _) ->
    let ve = gather_pat env scope.val_env pat in
    { scope with val_env = ve}
  | VarD (id, exp) ->
    check env dec.at
      (not (T.Env.mem id.it scope.val_env))
      "duplicate variable definition in block";
    let ve =  T.Env.add id.it (T.Mut (typ exp)) scope.val_env in
    { scope with val_env = ve}
  | FuncD (call_conv, id, typ_binds, pat, typ, exp) ->
    let func_sort = call_conv.Value.sort in
    let cs = List.map (fun tb -> tb.it.con) typ_binds in
    let t1 = pat.note in
    let t2 = typ in
    let ts1 = match call_conv.Value.n_args with
      | 1 -> [t1]
      | _ -> T.as_seq t1
    in
    let ts2 = match call_conv.Value.n_res  with
      | 1 -> [t2]
      | _ -> T.as_seq t2
    in
    let c = match func_sort, t2 with
      | T.Sharable, (T.Async _) -> T.Promises  (* TBR: do we want this for T.Local too? *)
      | _ -> T.Returns
    in
    let ts = List.map (fun typbind -> typbind.it.bound) typ_binds in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close cs t}) cs ts in
    let t = T.Func (func_sort, c, tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2) in
    let ve' =  T.Env.add id.it t scope.val_env in
    { scope with val_env = ve' }
  | TypD c ->
    check env dec.at
      (not (T.ConSet.mem c scope.con_env))
      "duplicate definition of type in block";
    let ce' = T.ConSet.disjoint_add c scope.con_env in
    { scope with con_env = ce' }

(* Programs *)

let check_prog scope phase ((decs, flavor) as prog) : unit =
  let env = env_of_scope scope flavor in
  try
   ignore (check_block env T.unit decs no_region)
  with CheckFailed s ->
    let bt = Printexc.get_backtrace () in
    if !Flags.verbose
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

