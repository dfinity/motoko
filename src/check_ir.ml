open Source
module T = Type
module E = Effect

(* TODO: remove DecE from syntax, replace by BlockE [dec] *)
(* TODO: check constraint matching supports recursive bounds *)

(* TODO: make note immutable, perhaps just using type abstraction *)

(* TODO:
   open code review issues
   place where we access Syntax.note_typ or pat.note are good places to considering
   add type info to IR.exp' constructors
   (e.g. identifier bindings, PrimE, branches) so that we can remove the type notes altogether.
   add type and term predicate to rule out constructs after passes, We could even compose these I guess....
   restore effect inference
*)

(* Scope (the external interface) *)

type val_env = T.typ T.Env.t
type con_env = T.con_env

type scope =
  { val_env : val_env;
    con_env : con_env;
  }

let empty_scope : scope =
  { val_env = T.Env.empty;
    con_env = Con.Env.empty
  }

let adjoin_scope scope1 scope2 =
  { val_env = T.Env.adjoin scope1.val_env scope2.val_env;
    con_env = Con.Env.adjoin scope1.con_env scope2.con_env;
  }

(* Contexts (internal) *)

type lab_env = T.typ T.Env.t
type ret_env = T.typ option

type env =
  { vals : val_env;
    cons : con_env;
    labs : lab_env;
    rets : ret_env;
    async : bool;
  }

let env_of_scope scope =
  { vals = scope.Typing.val_env;
    cons = scope.Typing.con_env;
    labs = T.Env.empty;
    rets = None;
    async = false;
  }

(* More error bookkeeping *)

let type_error at text : Diag.message = Diag.{ sev = Diag.Error; at; cat = "IR type"; text }

let error env at fmt =
  Printf.ksprintf (fun s -> failwith (Diag.string_of_message (type_error at s))) fmt


let add_lab c x t = {c with labs = T.Env.add x t c.labs}
let add_val c x t = {c with vals = T.Env.add x t c.vals}

let add_typs c cs ks =
  { c with
    cons = List.fold_right2 Con.Env.add cs ks c.cons;
  }

let adjoin c scope =
  { c with
    vals = T.Env.adjoin c.vals scope.val_env;
    cons = Con.Env.adjoin c.cons scope.con_env;
  }

let adjoin_vals c ve = {c with vals = T.Env.adjoin c.vals ve}
let adjoin_cons c ce = {c with cons = Con.Env.adjoin c.cons ce}
let adjoin_typs c ce =
  { c with
    cons = Con.Env.adjoin c.cons ce;
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

let infer_mut mut : T.typ -> T.typ =
  match mut.it with
  | Syntax.Const -> fun t -> t
  | Syntax.Var -> fun t -> T.Mut t

let rec check_typ env typ : unit =
  match typ with
  | T.Pre ->
    error env no_region "illegal T.Pre type"
  | T.Var (s,i) ->
    error env no_region "free type variable %s, index %i" s  i
  | T.Con(c,typs) ->
    (match Con.Env.find_opt c env.cons with
    | Some (T.Def (tbs, t) | T.Abs (tbs, t))  ->
      check_typ_bounds env tbs typs no_region
    | None -> error env no_region "unbound type constructor %s" (Con.to_string c)
    )
  | T.Any -> ()
  | T.Non -> ()
  | T.Shared -> ()
  | T.Class -> ()
  | T.Prim _  -> ()
  | T.Array typ ->
    check_typ env typ
  | T.Tup typs ->
    List.iter (check_typ env) typs
  | T.Func (sort, control, binds, ts1, ts2) ->
    let cs, ce = check_typ_binds env binds in
    let env' = adjoin_typs env  ce in
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
          (T.string_of_typ_expand env'.cons t2)
    end;
    if sort = T.Call T.Sharable then begin
      let t1 = T.seq ts1 in
      if not (T.sub env'.cons t1 T.Shared) then
        error env no_region "shared function has non-shared parameter type\n  %s"
          (T.string_of_typ_expand env'.cons t1);
      begin match ts2 with
      | [] -> ()
      | [T.Async t2] ->
        if not (T.sub env'.cons t2 T.Shared) then
          error env no_region "shared function has non-shared result type\n  %s"
            (T.string_of_typ_expand env'.cons t2);
      | _ -> error env no_region "shared function has non-async result type\n  %s"
          (T.string_of_typ_expand env'.cons (T.seq ts2))
      end
    end
  | T.Opt typ ->
    check_typ env typ
  | T.Async typ ->
    let t' = T.promote env.cons typ in
    if not (T.sub env.cons t' T.Shared) then
      error env no_region "async type has non-shared parameter type\n  %s"
        (T.string_of_typ_expand env.cons t')
  | T.Like typ ->
    check_typ env typ
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
    if not (sorted fields) then
      error env no_region "object type's fields are not sorted\n  %s"
        (T.string_of_typ_expand env.cons typ);
  | T.Mut typ ->
    check_typ env typ

and check_typ_field env s typ_field : unit =
  let {T.name; T.typ} = typ_field in
  check_typ env typ;
  if s = T.Actor && not (T.is_func (T.promote env.cons typ)) then
    error env no_region "actor field %s has non-function type\n  %s"
      name  (T.string_of_typ_expand env.cons typ);
  if s <> T.Object T.Local && not (T.sub env.cons typ T.Shared) then
    error env no_region "shared object or actor field %s has non-shared type\n  %s"
      name (T.string_of_typ_expand env.cons typ)

and check_typ_binds env typ_binds : T.con list * con_env =
  let ts,ce = Type.open_binds env.cons typ_binds in
  let cs = List.map (function T.Con(c,[]) ->  c | _ -> assert false) ts in
  let ks = List.map2 (fun c t -> T.Abs ([], t)) cs ts in
  let env' = add_typs env cs ks in
  let _ = List.map
            (fun typ_bind ->
              let bd = T.open_ ts typ_bind.T.bound  in
              check_typ env' bd)
            typ_binds
  in
  cs, Con.Env.from_list2 cs ks

and check_typ_bounds env (tbs : T.bind list) typs at : unit =
  match tbs, typs with
  | tb::tbs', typ::typs' ->
    check_typ env typ;
    if not (T.sub env.cons typ tb.T.bound) then
        error env no_region "type argument\n  %s\ndoes not match parameter bound\n  %s"
          (T.string_of_typ_expand env.cons typ)
          (T.string_of_typ_expand env.cons tb.T.bound);
    check_typ_bounds env tbs' typs' at
  | [], [] -> ()
  | [], _ -> error env at "too many type arguments"
  | _, [] -> error env at "too few type arguments"

and check_inst_bounds env tbs typs at =
  check_typ_bounds env tbs typs at


(* Literals *)

let infer_lit env lit at : T.prim =
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
  | AsyncE _ -> true
  | _ -> false

let rec infer_exp env exp : T.typ =
  T.as_immut (infer_exp_mut env exp)

and infer_exp_promote env exp : T.typ =
  let t = infer_exp env exp in
  T.promote env.cons t

and infer_exp_mut env exp : T.typ =
  let t = infer_exp' env exp in
  begin
      (* TODO: enable me one infer_effect works on Ir nodes...
      let e = E.infer_effect_exp exp in
      assert (T.Triv < T.Await);
      if not (e <= E.eff exp) then begin
      error env exp.at "inferred effect not a subtype of expected effect"
      end;
       *)
    (*TBR: it's weird that we need to mask mutability, but I think there's an inconsistency
           between the way the type checker annotates l-expressions in checking (never immutable)
           vs. inference mode (maybe mutable) *)
    if not (Type.sub env.cons (if T.is_mut (E.typ exp) then t else T.as_immut t) (E.typ exp)) then
      begin
        error env exp.at "inferred type %s not a subtype of expected type %s in \n %s"
          (T.string_of_typ_expand env.cons t)
          (T.string_of_typ_expand env.cons (E.typ exp))
          (Wasm.Sexpr.to_string 80 (Arrange_ir.exp exp))
      end
  end;
  E.typ exp;

and infer_exp' env (exp:Ir.exp) : T.typ =
  let t = E.typ exp in
  match exp.it with
  | PrimE _ ->
    exp.note.Syntax.note_typ (* error env exp.at "cannot infer type of primitive"  *)
  | VarE id ->
    (match T.Env.find_opt id.it env.vals with
    | Some T.Pre ->
      assert false (* error env id.at "cannot infer type of forward variable %s" id.it; *)
    | Some t -> t
    | None -> error env id.at "unbound variable %s" id.it
    )
  | LitE lit ->
    T.Prim (infer_lit env lit exp.at)
  | UnE (ot, op, exp1) ->
    if not (Operator.has_unop ot op) then
      error env exp.at "operator is not defined for operand type\n  %s"
        (T.string_of_typ_expand env.cons ot);
    check_exp env ot exp1;
    ot
  | BinE (ot, exp1, op, exp2) ->
    if not (Operator.has_binop ot op) then
      error env exp.at "operator not defined for operand types\n  %s and\n  %s"
        (T.string_of_typ_expand env.cons ot)
        (T.string_of_typ_expand env.cons ot);
    check_exp env ot exp1;
    check_exp env ot exp2;
    ot
  | RelE (ot,exp1, op, exp2) ->
    if not (Operator.has_relop ot op) then
      error env exp.at "operator not defined for operand types\n  %s and\n  %s"
        (T.string_of_typ_expand env.cons ot)
        (T.string_of_typ_expand env.cons ot);
    check_exp env ot exp1;
    check_exp env ot exp2;
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
      let ts = T.as_tup_sub n env.cons t1 in
      match List.nth_opt ts n with
      | Some t -> t
      | None ->
        error env exp.at "tuple projection %n is out of bounds for type\n  %s"
          n (T.string_of_typ_expand env.cons t1)
    with Invalid_argument _ ->
      error env exp1.at "expected tuple type, but expression produces type\n  %s"
        (T.string_of_typ_expand env.cons t1)
    )
  | ActorE ( id, fields, t) ->
    let env' = { env with async = false } in
    let t1 = infer_obj env' T.Actor id t fields in
    let t2 = T.promote env.cons t in
    if not (T.is_obj t2) then
      error env exp.at "bad annotation %s (object type expected)" (T.string_of_typ t);
    if T.sub env.cons t1 t2 then
      t
    else
      error env no_region "expecting actor of type %s, but expression produces %s"
        (T.string_of_typ_expand env.cons t2)
        (T.string_of_typ_expand env.cons t1)
  | ActorDotE(exp1,{it = Syntax.Name n;_})
  | DotE (exp1, {it = Syntax.Name n;_}) ->
    let t1 = infer_exp_promote env exp1 in
    (try
       let sort, tfs = T.as_obj_sub n env.cons t1 in
       begin
         match exp.it with
         | ActorDotE _ ->
           if (sort <> T.Actor) then
          error env exp.at "expected actor found object"
         | DotE _ ->
           if (sort == T.Actor) then
          error env exp.at "expected object found actor"
         | _ -> assert false
       end;
       match List.find_opt (fun {T.name; _} -> name = n) tfs with
       | Some {T.typ = t; _} -> t
       | None ->
         error env exp1.at "field name %s does not exist in type\n  %s"
           n (T.string_of_typ_expand env.cons t1)
     with Invalid_argument _ ->
       error env exp1.at "expected object type, but expression produces type\n  %s"
         (T.string_of_typ_expand env.cons t1)
    )
  | AssignE (exp1, exp2) ->
     begin
      let t1 = infer_exp_mut env exp1 in
      try
        let t2 = T.as_mut t1 in
        check_exp env t2 exp2
      with Invalid_argument _ ->
        error env exp.at "expected mutable assignment target";
    end;
    T.unit
  | ArrayE (mut, t, exps) ->
    let ts = List.map (infer_exp env) exps in
    let t1 = List.fold_left (T.lub env.cons) t ts in
    T.Array (match mut.it with Syntax.Const -> t1 | Syntax.Var -> T.Mut t1)
  | IdxE (exp1, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let t = T.as_array_sub env.cons t1 in
      check_exp env T.nat exp2;
      t
    with Invalid_argument _ ->
      error env exp1.at "expected array type, but expression produces type\n  %s"
        (T.string_of_typ_expand env.cons t1)
    )
  | CallE (call_conv, exp1, insts, exp2) ->
    (* TODO: check call_conv (assuming there's something to check) *)
    let t1 = infer_exp_promote env exp1 in
    (try
      let tbs, t2, t = T.as_func_sub (List.length insts) env.cons t1 in
      check_inst_bounds env tbs insts exp.at;
      check_exp env (T.open_ insts t2) exp2;
      T.open_ insts t
    with Invalid_argument _ ->
      error env exp1.at "expected function type, but expression produces type\n  %s"
        (T.string_of_typ_expand env.cons t1)
    )
  | BlockE (decs, t) ->
    let t1, scope = infer_block env decs exp.at in
    (*  let _t2 = try T.avoid env.cons scope.con_env t1 with T.Unavoidable c -> assert false in *)
    let env' = adjoin env scope in
    check_typ env t;
    if not (T.eq env.cons t T.unit || T.eq env'.cons t1 t) then
      error env exp.at "expected block type\n  %s, found declaration with inequivalent type\n  %s"
        (T.string_of_typ t)
        (T.string_of_typ t1);
    t
  | IfE (exp1, exp2, exp3) ->
    check_exp env T.bool exp1;
    check_exp env t exp2;
    check_exp env t exp3;
    t
  | SwitchE (exp1, cases) ->
    let t1 = infer_exp_promote env exp1 in
(*    if not env.pre then
      if not (Coverage.check_cases env.cons cases t1) then
        warn env exp.at "the cases in this switch do not cover all possible values";
 *)
    check_cases env t1 t cases;
    t
  | WhileE (exp1, exp2) ->
    check_exp env T.bool exp1;
    check_exp env T.unit exp2;
    T.unit
  | LoopE (exp1, expo) ->
    check_exp env T.unit exp1;
    Lib.Option.app (check_exp env T.bool) expo;
    T.Non
  | ForE (pat, exp1, exp2) ->
    begin
      let t1 = infer_exp_promote env exp1 in
      (try
        let _, tfs = T.as_obj_sub "next" env.cons t1 in
        let t = T.lookup_field "next" tfs in
        let t1, t2 = T.as_mono_func_sub env.cons t in
        if not (T.sub env.cons T.unit t1) then raise (Invalid_argument "");
        let t2' = T.as_opt_sub env.cons t2 in
        let ve = check_pat_exhaustive env t2' pat in
        check_exp (adjoin_vals env ve) T.unit exp2
      with Invalid_argument _ ->
        error env exp1.at "expected iterable type, but expression has type\n  %s"
          (T.string_of_typ_expand env.cons t1)
      );
    end;
    T.unit
  | LabelE (id, typ, exp1) ->
    let t = check_typ env typ;typ in
    check_exp (add_lab env id.it typ) t exp1;
    t
  | BreakE (id, exp1) ->
    begin
      match T.Env.find_opt id.it env.labs with
      | Some t ->
        check_exp env t exp1
      | None ->
        in error env id.at "unbound label %s" name
    end;
    T.Non
  | RetE exp1 ->
    begin
      match env.rets with
      | Some T.Pre ->
        assert false; (* error env exp.at "cannot infer return type" *)
      | Some t ->
        check_exp env t exp1
      | None ->
        error env exp.at "misplaced return"
    end;
    T.Non
  | AsyncE exp1 ->
    let env' =
      {env with labs = T.Env.empty; rets = Some (* T.Pre *) exp1.note.Syntax.note_typ; async = true} in
    let t = infer_exp env' exp1 in
    if not (T.sub env.cons t T.Shared) then
      error env exp1.at "async type has non-shared parameter type\n  %s"
        (T.string_of_typ_expand env.cons t);
    T.Async t
  | AwaitE exp1 ->
    if not env.async then
      error env exp.at "misplaced await";
    let t1 = infer_exp_promote env exp1 in
    (try
      T.as_async_sub env.cons t1
    with Invalid_argument _ ->
      error env exp1.at "expected async type, but expression has type\n  %s"
        (T.string_of_typ_expand env.cons t1)
    )
  | AssertE exp1 ->
    check_exp env T.bool exp1;
    T.unit
  | IsE (exp1, exp2) ->
    (* TBR: restrict t1 to objects? *)
    begin
      let _t1 = infer_exp env exp1 in
      check_exp env T.Class exp2
    end;
    T.bool
  | DeclareE (id, typ, exp1) ->
    let env' = adjoin_vals env (T.Env.singleton id.it typ) in
    infer_exp env' exp1
  | DefineE (id, mut, exp1) ->
     begin
       match T.Env.find_opt id.it env.vals with
       | Some t1 ->
         begin
           try
             let t2 = match mut.it with | Syntax.Var -> T.as_mut t1 | Syntax.Const -> t1 in
             check_exp env t2 exp1
           with Invalid_argument _ ->
             error env exp.at "expected mutable assignment target";
         end;
       | None -> error env id.at "unbound variable %s" id.it
    end;
    T.unit
  | NewObjE (sort, labids, t) ->
    let t1 =
      T.Obj(sort.it,
            List.sort T.compare_field (List.map (fun (name,id) ->
                                           {T.name = Syntax.string_of_name name.it;
                                            T.typ = T.Env.find id.it env.vals}) labids))
    in
    let t2 = T.promote env.cons t in
    if not (T.is_obj t2) then
      error env  exp.at "bad annotation %s (object type expected)" (T.string_of_typ t);
    if T.sub env.cons t1 t2 then
      t
    else
      error env no_region "expecting object of type %s, but expression produces %s"
        (T.string_of_typ_expand env.cons t2)
        (T.string_of_typ_expand env.cons t1)

and check_exp env t exp =
  let t' = infer_exp env exp in
  if not (T.sub env.cons t' t) then
    error env exp.at "expression\n  %s\n of type\n  %s\ncannot produce expected type\n  %s"
      (Wasm.Sexpr.to_string 80 (Arrange_ir.exp exp))
      (T.string_of_typ_expand env.cons t')
      (T.string_of_typ_expand env.cons t)


(* Cases *)

and check_cases env t_pat t cases =
  List.iter (check_case env t_pat t) cases

and check_case env t_pat t {it = {pat; exp}; _} =
  let ve = check_pat env t_pat pat in
  check_exp (adjoin_vals env ve) t exp

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
      go ve pat1
    | OptP pat1 ->
      go ve pat1
  in T.Env.adjoin ve0 (go T.Env.empty pat)

and infer_pat_exhaustive env pat : T.typ * val_env =
  let t, ve = infer_pat env pat in
  (* TODO: actually check exhaustiveness *)
  t, ve

and infer_pat env pat : T.typ * val_env =
  assert (pat.note <> T.Pre);
  let t, ve = infer_pat' env pat in
  if not (T.sub env.cons pat.note t) then (* TBR: should we allow contra-variance ?*)
    error env pat.at "pattern of type \n  %s\n cannot consume expected type \n  %s"
      (T.string_of_typ_expand env.cons t)
      (T.string_of_typ_expand env.cons pat.note);
  t, ve

and infer_pat' env pat : T.typ * val_env =
  match pat.it with
  | WildP ->
    (pat.note, T.Env.empty)
  | VarP id ->
    (pat.note, T.Env.singleton id.it pat.note)
  | LitP lit ->
    let t = T.Prim (infer_lit env lit pat.at) in
    if not (T.sub env.cons t pat.note) then (* TBR isn't this test the wrong way around? *)
    error env pat.at "type of literal pattern \n  %s\n cannot produce expected type \n  %s"
      (T.string_of_typ_expand env.cons t)
      (T.string_of_typ_expand env.cons pat.note);
    (pat.note, T.Env.empty)
  | TupP pats ->
    let ts, ve = infer_pats pat.at env pats [] T.Env.empty in
    T.Tup ts, ve
  | OptP pat1 ->
    let t1, ve = infer_pat env pat1 in
    T.Opt t1, ve
  | AltP (pat1, pat2) ->
    let t1, ve1 = infer_pat env pat1 in
    let t2, ve2 = infer_pat env pat2 in
    let t = T.lub env.cons t1 t2 in
    if ve1 <> T.Env.empty || ve2 <> T.Env.empty then
      error env pat.at "variables are not allowed in pattern alternatives";
    t, T.Env.empty

and infer_pats at env pats ts ve : T.typ list * val_env =
  match pats with
  | [] -> List.rev ts, ve
  | pat::pats' ->
    let t, ve1 = infer_pat env pat in
    let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
    infer_pats at env pats' (t::ts) ve'


and check_pat_exhaustive env t pat : val_env =
  (* TODO: check exhaustiveness? *)
  check_pat env t pat

and check_pat env t pat : val_env =
  assert (pat.note <> T.Pre);
  let (t,ve) = infer_pat env pat in
  let t' = T.normalize env.cons t in
  if not (T.sub env.cons t t') then
    error env pat.at "type of pattern \n  %s\n cannot consume expected type \n  %s"
      (T.string_of_typ_expand env.cons t')
      (T.string_of_typ_expand env.cons pat.note);
  ve

(* Objects *)

and infer_obj env s id t fields : T.typ =
  let ve = gather_exp_fields env id.it t fields in
  let env' = adjoin_vals env ve in
  let tfs, _ve = infer_exp_fields env' s id.it t fields in
  T.Obj(s,tfs)

and gather_exp_fields env id t fields : val_env =
  let ve0 = T.Env.singleton id t in
  List.fold_left (gather_exp_field env) ve0 fields

and gather_exp_field env ve field : val_env =
  let {id; exp; mut; priv;_} : exp_field' = field.it in
  if T.Env.mem id.it ve then
    error env id.at "duplicate field name %s in object" id.it;
  T.Env.add id.it (infer_mut mut exp.note.Syntax.note_typ) ve

and infer_exp_fields env s id t fields : T.field list * val_env =
  let env' = add_val env id t in
  let tfs, ve =
    List.fold_left (infer_exp_field env' s) ([], T.Env.empty) fields in
  List.sort T.compare_field tfs, ve

and is_func_exp exp =
  match exp.it with
  | BlockE ([dec],_)-> is_func_dec dec
  | _ -> Printf.printf "[1]%!"; false

and is_func_dec dec =
  match dec.it with
  | FuncD _ -> true
  | _ -> Printf.printf "[2]%!"; false

and infer_exp_field env s (tfs, ve) field : T.field list * val_env =
  let {id; name; exp; mut; priv} = field.it in
  let t =
    match T.Env.find id.it env.vals with
    | T.Pre ->
      assert false
    | t ->
      begin
        check_exp (adjoin_vals env ve) (T.as_immut t) exp;
        if (mut.it = Syntax.Var) <> T.is_mut t then
          error env field.at
            "%smutable field %s cannot produce expected %smutable field of type\n  %s"
            (if mut.it = Syntax.Var then "" else "im") id.it
            (if T.is_mut t then "" else "im")
            (T.string_of_typ_expand env.cons (T.as_immut t))
      end;
      t
  in
  begin
    if s = T.Actor && priv.it = Syntax.Public && not (is_func_exp exp) then
      error env field.at "public actor field is not a function";
    if s <> T.Object T.Local && priv.it = Syntax.Public && not (T.sub env.cons t T.Shared) then
      error env field.at "public shared object or actor field %s has non-shared type\n  %s"
        (Syntax.string_of_name name.it) (T.string_of_typ_expand env.cons t)
  end;
  let ve' = T.Env.add id.it t ve in
  let tfs' =
    if priv.it = Syntax.Private
    then tfs
    else {T.name = Syntax.string_of_name name.it; typ = t} :: tfs
  in tfs', ve'


(* Blocks and Declarations *)

and infer_block env decs at : T.typ * scope =
  let scope = gather_block_decs env decs in
  let t = infer_block_exps (adjoin env scope) decs in
  t, scope

and infer_block_exps env decs : T.typ =
  match decs with
  | [] -> T.unit
  | [dec] -> infer_dec env dec
  | dec::decs' ->
    check_dec env T.unit dec;
    infer_block_exps env decs'

and cons_of_typ_binds typ_binds =
  let con_of_typ_bind tp =
      match tp.note with
      | T.Con(c,[]) -> c
      | _ -> assert false (* TODO: remove me by tightening note to Con.t *)
  in
  List.map con_of_typ_bind typ_binds

and check_open_typ_binds env typ_binds =
  let cs = cons_of_typ_binds typ_binds in
  let ks = List.map (fun tp -> T.Abs([],tp.it.T.bound)) typ_binds in
  let ce = List.fold_right2 Con.Env.add cs ks Con.Env.empty in
  let binds = T.close_binds cs (List.map (fun tb -> tb.it) typ_binds) in
  let _,_ = check_typ_binds env binds in
  cs,ce

and infer_dec env dec : T.typ =
  let t =
  match dec.it with
  | ExpD exp ->
    infer_exp env exp
  | LetD (_, exp) | VarD (_, exp) ->
    ignore (infer_exp env exp);
    T.unit
  | FuncD (sort, id, typ_binds, pat, typ, exp) ->
    let t = T.Env.find id.it env.vals in
    let _cs,ce = check_open_typ_binds env typ_binds in
    let env' = adjoin_typs env ce in
    let t1, ve = infer_pat_exhaustive env' pat in
    check_typ env' typ;
    let env'' =
      {env' with labs = T.Env.empty; rets = Some typ; async = false} in
    check_exp (adjoin_vals env'' ve) typ exp;
    t
  | TypD (c, k) ->
    let (binds,typ) =
      match k with
      | T.Abs(binds,typ)
      | T.Def(binds,typ) -> (binds,typ)
    in
    let cs,ce = check_typ_binds env binds in
    let ts = List.map (fun c -> T.Con(c,[])) cs in
    let env' = adjoin_typs env ce in
    check_typ env' (T.open_ ts  typ);
    T.unit
  in
  if not (Type.sub env.cons t (E.typ dec)) then begin
    error env dec.at "inferred dec type %s not a subtype of expected type %s"
      (T.string_of_typ_expand env.cons t)
      (T.string_of_typ_expand env.cons (E.typ dec));
  end;
  (* TODO: enable me one infer_effect works on Ir nodes...
  let e = E.infer_effect_dec dec in
  assert (T.Triv < T.Await);
  if not (e <= E.eff dec) then begin
    error env dec.at "inferred effect not a subtype of expected effect"
  end;
   *)
  E.typ dec

and check_block env t decs at : scope =
  let scope = gather_block_decs env decs in
  check_block_exps (adjoin env scope) t decs at;
  scope

and check_block_exps env t decs at =
  match decs with
  | [] ->
    if not (T.sub env.cons T.unit t) then
      error env at "empty block cannot produce expected type\n  %s"
        (T.string_of_typ_expand env.cons t)
  | [dec] ->
    check_dec env t dec
  | dec::decs' ->
    check_dec env T.unit dec;
    check_block_exps env t decs' at

and check_dec env t dec =
  let t' = infer_dec env dec in
  (* TBR: special-case unit? *)
  if not (T.eq env.cons t T.unit || T.sub env.cons t' t) then
    error env dec.at "expression of type\n  %s\ncannot produce expected type\n  %s"
      (T.string_of_typ_expand env.cons t)
      (T.string_of_typ_expand env.cons t')

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
    if T.Env.mem id.it scope.val_env then
      error env dec.at "duplicate definition for %s in block" id.it;
    let ve =  T.Env.add id.it (T.Mut exp.note.Syntax.note_typ) scope.val_env in
    { scope with val_env = ve}
  | FuncD (call_conv, id, typ_binds, pat, typ, exp) ->
    let func_sort = call_conv.Value.sort in
    let cs = cons_of_typ_binds typ_binds in
    let t1 = pat.note in
    let t2 = typ in
    if Type.is_async t2 && not (isAsyncE exp) then
       error env dec.at "shared function with async type has non-async body"
    let ts1 = match call_conv.Value.n_args with
      | 1 -> [t1]
      | _ -> T.as_seq t1
    in
    let ts2 = match call_conv.Value.n_res  with
      | 1 -> [t2]
      | _ -> T.as_seq t2
    in
    let c = match func_sort, t2 with
      | T.Call T.Sharable, (T.Async _) -> T.Promises  (* TBR: do we want this for T.Local too? *)
      | _ -> T.Returns
    in
    let ts = List.map (fun typbind -> typbind.it.T.bound) typ_binds in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close cs t}) cs ts in
    let t = T.Func (func_sort, c, tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2) in
    let ve' =  T.Env.add id.it t scope.val_env in
    {scope with val_env = ve'}
  | TypD (c, k) ->
    if Con.Env.mem c scope.con_env then
      error env dec.at "duplicate definition for type %s in block" (Con.to_string c);
    let ce' = Con.Env.add c k scope.con_env in
    {scope with con_env = ce'}

(* Programs *)

let check_prog scope prog : scope =
  let env = env_of_scope scope in
  check_block env T.unit prog.it prog.at

let infer_prog scope prog : (T.typ * scope) =
  let env = env_of_scope scope in
  infer_block env prog.it prog.at
