open Source
module T = Type
module E = Effect

(* TODO: fix uses of List.sort compare etc on fields rather than field names *)
(* TODO: annote DotE in checker for desugaring sans environments *)
(* TODO: remove DecE from syntax, replace by BlockE [dec] *)         
(* TODO: check constraint matching supports recursive bounds *)           

(* TODO: remove T.pre, simplify env, desugar ClassD to TypD + FuncD, make note immutable and remove remaining updates *)         
(* Error bookkeeping *)

(* Recovering from errors *)

exception Recover

let recover_with (x : 'a) (f : 'b -> 'a) (y : 'b) = try f y with Recover -> x
let recover_opt f y = recover_with None (fun y -> Some (f y)) y
let recover f y = recover_with () f y

(* Scope (the external interface) *)

type val_env = T.typ T.Env.t
type typ_env = T.con T.Env.t
type con_env = T.con_env

type scope = Typing.scope =
  { val_env : val_env;
    typ_env : typ_env; (* TODO: delete me *)
    con_env : con_env;
  }

let empty_scope : scope =
  { val_env = T.Env.empty;
    typ_env = T.Env.empty;
    con_env = Con.Env.empty
  }

let adjoin_scope scope1 scope2 =
  { val_env = T.Env.adjoin scope1.val_env scope2.val_env;
    typ_env = T.Env.adjoin scope1.typ_env scope2.typ_env; (* TODO: delete me *)
    con_env = Con.Env.adjoin scope1.con_env scope2.con_env;
  }

(* Contexts (internal) *)

type lab_env = T.typ T.Env.t
type ret_env = T.typ option

type env =
  { vals : val_env;
    typs : typ_env; (* TODO: remove me *)
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

(* More error bookkeeping *)

let type_error at text : Diag.message = Diag.{ sev = Diag.Error; at; cat = "type"; text }
let type_warning at text : Diag.message = Diag.{ sev = Diag.Warning; at; cat = "type"; text }

let local_error env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_error at s)) fmt
let error env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_error at s); raise Recover) fmt
let warn env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_warning at s)) fmt


let add_lab c x t = {c with labs = T.Env.add x t c.labs}
let add_val c x t = {c with vals = T.Env.add x t c.vals}
(*
let add_con c con k = {c with cons = Con.Env.add con k c.cons}
let add_typ c x con k =
  { c with
    typs = T.Env.add x con c.typs;
    cons = Con.Env.add con k c.cons;
  }
*)

let add_typs c xs cs ks =
  { c with
    (* typs = List.fold_right2 T.Env.add xs cs c.typs; *)
    cons = List.fold_right2 Con.Env.add cs ks c.cons;
  }

let adjoin c scope =
  { c with
    vals = T.Env.adjoin c.vals scope.val_env;
    (*    typs = T.Env.adjoin c.typs scope.typ_env; *)
    cons = Con.Env.adjoin c.cons scope.con_env;
  }

let adjoin_vals c ve = {c with vals = T.Env.adjoin c.vals ve}
let adjoin_cons c ce = {c with cons = Con.Env.adjoin c.cons ce}
let adjoin_typs c ce =
  { c with
    (*   typs = T.Env.adjoin c.typs te; *)
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
    if (control = T.Promises) then begin
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
    check_ids env (List.map (fun (field : T.field) -> field.T.name) fields);
    List.iter (check_typ_field env sort) fields
    (* TODO: check fields are sorted, c.f. typecheck:ml: *)
    (* T.Obj (sort.it, List.sort compare fs) *) (* IS THAT EVEN CORRECT? *)
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
  let xs = List.map (fun typ_bind -> typ_bind.T.var) typ_binds in
  let cs = List.map (fun x -> Con.fresh x) xs in
  let _ (* te *) = List.fold_left2 (fun te typ_bind c ->
      let id = typ_bind.T.var in
      if T.Env.mem id te then
        error env no_region "duplicate type name %s in type parameter list" id;
      T.Env.add id c te
    ) T.Env.empty typ_binds cs in
  let pre_ks = List.map (fun c -> T.Abs ([], T.Pre)) cs in
  let pre_env' = add_typs {env with pre = true} xs cs pre_ks in
  let ts = List.map (fun typ_bind -> let t = typ_bind.T.bound in
                                     check_typ pre_env' t;
                                     t) typ_binds in
  let ks = List.map2 (fun c t -> T.Abs ([], t)) cs ts in
  let env' = add_typs env xs cs ks in
  let _ = List.map (fun typ_bind -> check_typ env' typ_bind.T.bound) typ_binds in
  cs, Con.Env.from_list2 cs ks

and check_typ_bounds env (tbs : T.bind list) typs at : unit =
  match tbs, typs with
  | tb::tbs', typ::typs' ->
    check_typ env typ;
    if not env.pre then begin
      if not (T.sub env.cons typ tb.T.bound) then
        local_error env no_region "type argument\n  %s\ndoes not match parameter bound\n  %s"
          (T.string_of_typ_expand env.cons typ)
          (T.string_of_typ_expand env.cons tb.T.bound)
    end;
    check_typ_bounds env tbs' typs' at 
  | [], [] -> ()
  | [], _ -> local_error env at "too many type arguments"
  | _, [] -> error env at "too few type arguments"

and check_inst_bounds env tbs typs at =
  check_typ_bounds env tbs typs at


(* Literals *)

let infer_lit env lit at : T.prim =
  Syntax.( (* yuck *)
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
  )

let check_lit env t lit at =
  Syntax.(
  match T.normalize env.cons t, lit with
  | T.Opt _, NullLit -> ()
  | t, _ ->
    let t' = T.Prim (infer_lit env lit at) in
    if not (T.sub env.cons t' t) then
      error env at "literal of type\n  %s\ndoes not have expected type\n  %s"
        (T.string_of_typ t') (T.string_of_typ_expand env.cons t)
  )


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
  (* TODO: enable me one infer_effect works on Ir nodes...
  let e = E.infer_effect_exp exp in 
  assert (T.Triv < T.Await);
  if not (e <= E.eff exp) then begin
    error env exp.at "inferred effect not a subtype of expected effect" 
  end;
   *)
  if not (Type.sub env.cons t (E.typ exp)) then begin
    error env exp.at "inferred type %s not a subtype of expected type %s"
      (T.string_of_typ_expand env.cons t)
      (T.string_of_typ_expand env.cons (E.typ exp));
  end;
  E.typ exp;

and infer_exp' env (exp:Ir.exp) : T.typ =
  match exp.it with
  | PrimE _ ->
    exp.note.Syntax.note_typ (* error env exp.at "cannot infer type of primitive"  *)
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
    begin
    if not (Operator.has_unop t op) then
      error env exp.at "operator is not defined for operand type\n  %s"
        (T.string_of_typ_expand env.cons t);
    if not (T.eq env.cons ot t) then
      error env exp.at "bad operator annotation, expecting  %s, found %s"
        (T.string_of_typ_expand env.cons t)
        (T.string_of_typ_expand env.cons ot);
    end;
    t
  | BinE (ot, exp1, op, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    let t2 = infer_exp_promote env exp2 in
    let t = T.lub env.cons t1 t2 in
    begin
      if not (Operator.has_binop t op) then
        error env exp.at "operator not defined for operand types\n  %s and\n  %s"
          (T.string_of_typ_expand env.cons t1)
          (T.string_of_typ_expand env.cons t2);
      if not (T.eq env.cons ot t) then
        error env exp.at "bad operator annotation, expecting  %s, found %s"
          (T.string_of_typ_expand env.cons t)
          (T.string_of_typ_expand env.cons ot);
    end;
    t
  | RelE (ot,exp1, op, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    let t2 = infer_exp_promote env exp2 in
    let t = T.lub env.cons t1 t2 in
    if not env.pre then begin
      if not (Operator.has_relop t op) then
        error env exp.at "operator not defined for operand types\n  %s and\n  %s"
          (T.string_of_typ_expand env.cons t1)
          (T.string_of_typ_expand env.cons t2);
      if not (T.eq env.cons ot t) then
        error env exp.at "bad operator annotation, expecting  %s, found %s"
          (T.string_of_typ_expand env.cons t)
          (T.string_of_typ_expand env.cons ot);
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

  | ActorE ( id, fields) ->
    let env' = { env with async = false } in
    infer_obj env' T.Actor id fields
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
  | ArrayE (mut, exps) ->
    let ts = List.map (infer_exp env) exps in
    let t1 = List.fold_left (T.lub env.cons) T.Non ts in
    T.Array (match mut.it with Syntax.Const -> t1 | Syntax.Var -> T.Mut t1)
  | IdxE (exp1, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let t = T.as_array_sub env.cons t1 in
      if not env.pre then check_exp env T.nat exp2;
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
      if not env.pre then check_exp env (T.open_ insts t2) exp2;
      T.open_ insts t
    with Invalid_argument _ ->
      error env exp1.at "expected function type, but expression produces type\n  %s"
        (T.string_of_typ_expand env.cons t1)
    )
  | BlockE decs ->
    let t, scope = infer_block env decs exp.at in
    (try T.avoid env.cons scope.con_env t with T.Unavoidable c ->
      error env exp.at "local class type %s is contained in inferred block type\n  %s"
        (Con.to_string c)
        (T.string_of_typ_expand (Con.Env.adjoin env.cons scope.con_env) t)
    )
  | IfE (exp1, exp2, exp3) ->
    if not env.pre then check_exp env T.bool exp1;
    let t2 = infer_exp env exp2 in
    let t3 = infer_exp env exp3 in
    let t = T.lub env.cons t2 t3 in
    t
  | SwitchE (exp1, cases) ->
    let t1 = infer_exp_promote env exp1 in
    let t = infer_cases env t1 T.Non cases in
(*    if not env.pre then
      if not (Coverage.check_cases env.cons cases t1) then
        warn env exp.at "the cases in this switch do not cover all possible values";
 *)
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
        let _, tfs = T.as_obj_sub "next" env.cons t1 in
        let t = T.lookup_field "next" tfs in
        let t1, t2 = T.as_mono_func_sub env.cons t in
        if not (T.sub env.cons T.unit t1) then raise (Invalid_argument "");
        let t2' = T.as_opt_sub env.cons t2 in
        let ve = check_pat_exhaustive env t2' pat in
        check_exp (adjoin_vals env ve) T.unit exp2
      with Invalid_argument _ ->
        local_error env exp1.at "expected iterable type, but expression has type\n  %s"
          (T.string_of_typ_expand env.cons t1)
      );
    end;
    T.unit
  | LabelE (id, typ, exp1) ->
    let t = check_typ env typ;typ in
    check_exp (add_lab env id.it typ) t exp1;
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
    if not env.pre then check_exp env T.bool exp1;
    T.unit
  | IsE (exp1, exp2) ->
    (* TBR: restrict t1 to objects? *)
    if not env.pre then begin
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
  | NewObjE (sort, labids) ->
     T.Obj(sort.it, List.map (fun (name,id) ->
                        {T.name = Syntax.string_of_name name.it; T.typ = T.Env.find id.it env.vals}) labids)
       
and check_exp env t exp =
  let t' = T.normalize env.cons t in
  check_exp' env t' exp;

and check_exp' env t exp =
(*  
  match exp.it, t with
  | PrimE s, T.Func _ ->
    ()
  | LitE lit, _ ->
    check_lit env t lit exp.at
  | UnE (ot, op, exp1), t' ->
    if not (Operator.has_unop ot op) then
      error env exp.at "no such unary operator for type";
       (T.string_of_typ_expand env.cons t');
    if not (Type.eq env.cons ot t') then
      error env exp.at "bad unary operator annotation, expecting %s found %s"
        (T.string_of_typ_expand env.cons t')
        (T.string_of_typ_expand env.cons (E.typ exp));
    check_exp env t' exp1
  | BinE (ot, exp1, op, exp2), t' ->
    if not (Operator.has_binop ot op) then
      error env exp.at "no such binary operator for type";
       (T.string_of_typ_expand env.cons t');
    if not (Type.eq env.cons ot t') then
      error env exp.at "bad binary operator annotation, expecting %s found %s"
        (T.string_of_typ_expand env.cons t')
        (T.string_of_typ_expand env.cons (E.typ exp));
    check_exp env t' exp1;
    check_exp env t' exp2
  | TupE exps, T.Tup ts when List.length exps = List.length ts ->
    List.iter2 (check_exp env) ts exps
  | OptE exp1, _ when T.is_opt t ->
    check_exp env (T.as_opt t) exp1
(*    
  | ObjE (sort, id, fields), T.Obj (s, tfs) when s = sort.it ->
    let env' = if sort.it = T.Actor then { env with async = false } else env in
    ignore (check_obj env' s tfs id fields exp.at)
 *)
  | ArrayE (mut, exps), T.Array t' ->
    if (mut.it = Var) <> T.is_mut t' then
      local_error env exp.at "%smutable array expression cannot produce expected type\n  %s"
        (if mut.it = Const then "im" else "")
        (T.string_of_typ_expand env.cons (T.Array t'));
    List.iter (check_exp env (T.as_immut t')) exps
  | AsyncE exp1, T.Async t' ->
    let env' = {env with labs = T.Env.empty; rets = Some t'; async = true} in
    check_exp env' t' exp1
  | BlockE decs, _ ->
    ignore (check_block env t decs exp.at)
  | IfE (exp1, exp2, exp3), _ ->
    check_exp env T.bool exp1;
    check_exp env t exp2;
    check_exp env t exp3
  | SwitchE (exp1, cases), _ ->
    let t1 = infer_exp_promote env exp1 in
    check_cases env t1 t cases;
    if not (Coverage.check_cases env.cons cases t1) then
      warn env exp.at "the cases in this switch do not cover all possible values";
  | _ ->
 *)
    let t' = infer_exp env exp in
    if not (T.sub env.cons t' t) then
      local_error env exp.at "expression of type\n  %s\ncannot produce expected type\n  %s"
        (T.string_of_typ_expand env.cons t')
        (T.string_of_typ_expand env.cons t)


(* Cases *)

and infer_cases env t_pat t cases : T.typ =
  List.fold_left (infer_case env t_pat) t cases

and infer_case env t_pat t {it = {pat; exp}; at; _} =
  let ve = check_pat env t_pat pat in
  let t' = recover_with T.Non (infer_exp (adjoin_vals env ve)) exp in
  let t'' = T.lub env.cons t t' in
  if
    t'' = T.Any &&
    T.promote env.cons t <> T.Any && T.promote env.cons t' <> T.Any
  then
    warn env at "the switch has type %s because branches have inconsistent types,\nthis case produces type\n  %s\nthe previous produce type\n  %s"
      (T.string_of_typ t'')
      (T.string_of_typ_expand env.cons t)
      (T.string_of_typ_expand env.cons t');
  t''

and check_cases env t_pat t cases =
  List.iter (check_case env t_pat t) cases

and check_case env t_pat t {it = {pat; exp}; _} =
  let ve = check_pat env t_pat pat in
  recover (check_exp (adjoin_vals env ve) t) exp


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
      T.Env.add id.it T.Pre ve
    | TupP pats ->
      List.fold_left go ve pats
    | AltP (pat1, pat2) ->
      go ve pat1
    | OptP pat1 ->
      go ve pat1
  in T.Env.adjoin ve0 (go T.Env.empty pat)



and infer_pat_exhaustive env pat : T.typ * val_env =
  let t, ve = infer_pat env pat in
  t, ve

and infer_pat env pat : T.typ * val_env =
  assert (pat.note <> T.Pre);
  let t, ve = infer_pat' env pat in
  if not env.pre then
    pat.note <- T.normalize env.cons t;
  t, ve

and infer_pat' env pat : T.typ * val_env =
  match pat.it with
  | WildP ->
    error env pat.at "cannot infer type of wildcard"
  | VarP _ ->
    error env pat.at "cannot infer type of variable"
  | LitP lit ->
    T.Prim (infer_lit env lit pat.at), T.Env.empty
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
  check_pat env t pat

and check_pat env t pat : val_env =
  assert (pat.note <> T.Pre);
  if t = T.Pre then snd (infer_pat env pat) else
  let t' = T.normalize env.cons t in
  let ve = check_pat' env t pat in
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
  | TupP pats ->
    (try
      let ts = T.as_tup_sub (List.length pats) env.cons t in
      check_pats env ts pats T.Env.empty pat.at
    with Invalid_argument _ ->
      error env pat.at "tuple pattern cannot consume expected type\n  %s"
        (T.string_of_typ_expand env.cons t)
    )
  | OptP pat1 ->
    (try
      let t1 = T.as_opt t in
      check_pat env t1 pat1
    with Invalid_argument _ ->
      error env pat.at "option pattern cannot consume expected type\n  %s"
        (T.string_of_typ_expand env.cons t)
    )
  | AltP (pat1, pat2) ->
    let ve1 = check_pat env t pat1 in
    let ve2 = check_pat env t pat2 in
    if ve1 <> T.Env.empty || ve2 <> T.Env.empty then
      error env pat.at "variables are not allowed in pattern alternatives";
    T.Env.empty
(*  TBD  
  | _ ->
    let t', ve = infer_pat env pat in
    if not (T.sub env.cons t t') then
      error env pat.at "pattern of type\n  %s\ncannot consume expected type\n  %s"
        (T.string_of_typ_expand env.cons t')
        (T.string_of_typ_expand env.cons t);
    ve
 *)
    
and check_pats env ts pats ve at : val_env =
  match pats, ts with
  | [], [] -> ve
  | pat::pats', t::ts ->
    let ve1 = check_pat env t pat in
    let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
    check_pats env ts pats' ve' at
  | [], ts ->
    local_error env at "tuple pattern has %i fewer components than expected type"
      (List.length ts); ve
  | ts, [] ->
    error env at "tuple pattern has %i more components than expected type"
      (List.length ts)


(* Objects *)

and infer_obj env s id fields : T.typ =
  let pre_ve = gather_exp_fields env id.it fields in
  let pre_env = adjoin_vals {env with pre = true} pre_ve in
  let tfs, ve = infer_exp_fields pre_env s id.it T.Pre fields in
  let t = T.Obj (s, tfs) in
  if not env.pre then begin
    let env' = adjoin_vals (add_val env id.it t) ve in
    ignore (infer_exp_fields env' s id.it t fields)
  end;
  t


and check_obj env s tfs id fields at : T.typ =
  let pre_ve = gather_exp_fields env id.it fields in
  let pre_ve' = List.fold_left
    (fun ve {T.name; typ = t} ->
      if not (T.Env.mem name ve) then
        error env at "%s expression without field %s cannot produce expected type\n  %s"
          (if s = T.Actor then "actor" else "object") name
          (T.string_of_typ_expand env.cons t);
      T.Env.add name t ve
    ) pre_ve tfs
  in
  let pre_env = adjoin_vals {env with pre = true} pre_ve' in
  let tfs', ve = infer_exp_fields pre_env s id.it T.Pre fields in
  let t = T.Obj (s, tfs') in
  let env' = adjoin_vals (add_val env id.it t) ve in
  ignore (infer_exp_fields env' s id.it t fields);
  t


and gather_exp_fields env id fields : val_env =
  let ve0 = T.Env.singleton id T.Pre in
  List.fold_left (gather_exp_field env) ve0 fields

and gather_exp_field env ve field : val_env =
  let {id; _} : exp_field' = field.it in
  if T.Env.mem id.it ve then
    error env id.at "duplicate field name %s in object" id.it;
  T.Env.add id.it T.Pre ve


and infer_exp_fields env s id t fields : T.field list * val_env =
  let env' = add_val env id t in
  let tfs, ve =
    List.fold_left (infer_exp_field env' s) ([], T.Env.empty) fields in
  List.sort compare tfs, ve

and is_func_exp exp =
  match exp.it with
  (*   | DecE dec -> is_func_dec dec *)
  | BlockE [dec] -> is_func_dec dec   
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
      infer_mut mut (infer_exp (adjoin_vals env ve) exp)
    | t ->
      (* When checking object in analysis mode *)
      if not env.pre then begin
        check_exp (adjoin_vals env ve) (T.as_immut t) exp;
        if (mut.it = Syntax.Var) <> T.is_mut t then
          local_error env field.at
            "%smutable field %s cannot produce expected %smutable field of type\n  %s"
            (if mut .it = Syntax.Var then "" else "im") id.it
            (if T.is_mut t then "" else "im")
            (T.string_of_typ_expand env.cons (T.as_immut t))
      end;
      t
  in
  if not env.pre then begin
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
  let scope = infer_block_decs env decs in
  let t = infer_block_exps (adjoin env scope) decs in
  t, scope

and infer_block_exps env decs : T.typ =
  match decs with
  | [] -> T.unit
  | [dec] -> infer_dec env dec
  | dec::decs' ->
    if not env.pre then recover (check_dec env T.unit) dec;
    recover_with T.Non (infer_block_exps env) decs'

and check_open_typ_binds env typ_binds =
  let cs = List.map (fun tp -> match tp.note with
                               | T.Con(c,[]) -> c
                               | _ -> assert false (* TODO: remove me by tightening note to Con.t *)

             ) typ_binds in
  let ks = List.map (fun tp -> T.Abs([],tp.it.T.bound)) typ_binds in
  let ce = List.fold_right2 Con.Env.add  cs ks Con.Env.empty in
  let binds = T.close_binds cs (List.map (fun tb -> tb.it) typ_binds) in
  let _,_ = check_typ_binds env binds in
  cs,ce

  
  
and infer_dec env dec : T.typ =
  let t =
  match dec.it with
  | ExpD exp ->
    infer_exp env exp
  | LetD (_, exp) | VarD (_, exp) ->
    if not env.pre then ignore (infer_exp env exp);
    T.unit
  | FuncD (sort, id, typbinds, pat, typ, exp) ->
    let t = T.Env.find id.it env.vals in
    if not env.pre then begin
      let _cs,ce = check_open_typ_binds env typbinds in
      let env' = adjoin_typs env ce in
      let _, ve = infer_pat_exhaustive env' pat in
      check_typ env' typ;
      let env'' =
        {env' with labs = T.Env.empty; rets = Some typ; async = false} in
      check_exp (adjoin_vals env'' ve) typ exp
    end;
    t
(*    
  | ClassD (id, tid, typbinds, sort, pat, id', fields) ->
    let t = T.Env.find id.it env.vals in
    if not env.pre then begin
      let _cs, _ts, te, ce = check_typ_binds env typbinds in
      let env' = adjoin_typs env te ce in
      let _, ve = infer_pat_exhaustive env' pat in
      let env'' =
        {env' with labs = T.Env.empty; rets = None; async = false} in
      ignore (infer_obj (adjoin_vals env'' ve) sort.it id' fields)
    end;
    t
*)
  | TypD _ ->
     T.unit
  in
  if not (Type.sub env.cons t (E.typ dec)) then begin
    error env dec.at "inferred type %s not a subtype of expected type %s"
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
  let scope = infer_block_decs env decs in
  check_block_exps (adjoin env scope) t decs at;
  scope

and check_block_exps env t decs at =
  match decs with
  | [] ->
    if not (T.sub env.cons T.unit t) then
      local_error env at "empty block cannot produce expected type\n  %s"
        (T.string_of_typ_expand env.cons t)
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
     if not (T.eq env.cons t T.unit || T.sub env.cons t' t) then
       local_error env dec.at "expression of type\n  %s\ncannot produce expected type\n  %s"
         (T.string_of_typ_expand env.cons t)
         (T.string_of_typ_expand env.cons t')
  
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


and infer_block_decs env decs : scope =
  let scope = gather_block_typdecs env decs in
  let env' = adjoin {env with pre = true} scope in
  let ce = infer_block_typdecs env' decs in
  let env'' = adjoin env { scope with con_env = ce } in
  let _ce' = infer_block_typdecs env'' decs in
  (* TBR: assertion does not work for types with binders, due to stamping *)
  (* assert (ce = ce'); *)
  let pre_ve' = gather_block_valdecs env decs in
  let ve = infer_block_valdecs (adjoin_vals env'' pre_ve') decs in
  { scope with  val_env = ve; con_env = ce }


(* Pass 1: collect type identifiers and their arity *)
and gather_block_typdecs env decs : scope =
  List.fold_left (gather_dec_typdecs env) empty_scope decs

and gather_dec_typdecs env scope dec : scope =
  match dec.it with
  | ExpD _ | LetD _ | VarD _ | FuncD _ -> scope
  | TypD (c, k) ->
    if Con.Env.mem c scope.con_env then
      error env dec.at "duplicate definition for type %s in block" (Con.to_string c);
    let ce' = Con.Env.add c k scope.con_env in
    {scope with con_env = ce'}



(* Pass 2 and 3: infer type definitions *)
and infer_block_typdecs env decs : con_env =
  let _env', ce =
    List.fold_left (fun (env, ce) dec ->
      let ce' = infer_dec_typdecs env dec in
      adjoin_cons env ce', Con.Env.adjoin ce ce'
    ) (env, Con.Env.empty) decs
  in ce

and infer_dec_typdecs env dec : con_env =
  match dec.it with
  | ExpD _ | LetD _ | VarD _ | FuncD _ ->
    Con.Env.empty
  | TypD (c, k) ->
    let (binds,typ) =
      match k with
      | T.Abs(binds,typ)
      | T.Def(binds,typ) -> (binds,typ)
    in
    let cs,ce = check_typ_binds env binds in
    let env' = adjoin_typs env ce in
    check_typ env' typ;
    Con.Env.singleton c k

(* Pass 4: collect value identifiers *)
and gather_block_valdecs env decs : val_env =
  List.fold_left (gather_dec_valdecs env) T.Env.empty decs

and gather_dec_valdecs env ve dec : val_env =
  match dec.it with
  | ExpD _ | TypD _ ->
    ve
  | LetD (pat, _) ->
    gather_pat env ve pat
  | VarD (id, _) | FuncD (_, id, _, _, _, _) ->
    if T.Env.mem id.it ve then
      error env dec.at "duplicate definition for %s in block" id.it;
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
  | FuncD (call_conv, id, typbinds, pat, typ, exp) ->
    let func_sort = call_conv.Value.sort in
    let cs, ce = check_open_typ_binds env typbinds in
    let env' = adjoin_typs env ce in
    let t1, _ = infer_pat {env' with pre = true} pat in
    check_typ env' typ;
    let t2 = typ in
    if not env.pre && func_sort = T.Call T.Sharable then begin
      if not (T.sub env'.cons t1 T.Shared) then
        error env pat.at "shared function has non-shared parameter type\n  %s"
          (T.string_of_typ_expand env'.cons t1);
      begin match t2 with
      | T.Tup [] -> ()
      | T.Async t2 ->
        if not (T.sub env'.cons t2 T.Shared) then
          error env no_region "shared function has non-shared result type\n  %s"
            (T.string_of_typ_expand env'.cons t2);
        if not (isAsyncE exp) then
          error env dec.at "shared function with async type has non-async body"
      | _ -> error env no_region "shared function has non-async result type\n  %s"
          (T.string_of_typ_expand env'.cons t2)
      end;
    end;
    let ts1 = match pat.it with
      | TupP ps -> T.as_seq t1
      | _ -> [t1]
    in
    let ts2 = match t2 with
      | T.Tup _ -> T.as_seq t2
      | _ -> [t2]
    in

    let c = match func_sort, t2 with
      | T.Call T.Sharable, (T.Async _) -> T.Promises  (* TBR: do we want this for T.Local too? *)
      | _ -> T.Returns
    in
    let ts = List.map (fun typbind -> typbind.it.T.bound) typbinds in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close cs t}) cs ts in
    T.Env.singleton id.it
      (T.Func (func_sort, c, tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2))
  | TypD _ ->
    T.Env.empty
(*    
  | ClassD (conid, id, typbinds, sort, pat, id', fields) ->
    let cs, ts, te, ce = check_typ_binds env typbinds in
    let env' = adjoin_typs env te ce in
    let c = T.Env.find id.it env.typs in
    let t1, _ = infer_pat {env' with pre = true} pat in
    let ts1 = match pat.it with
      | TupP _ -> T.as_seq t1
      | _ -> [t1] in
    let t2 = T.Con (c, List.map (fun c -> T.Con (c, [])) cs) in
    let tbs = List.map2 (fun c t -> {T.var = Con.name c; bound = T.close cs t}) cs ts in
    T.Env.singleton conid.it (T.Func (T.Construct, T.Returns, tbs, List.map (T.close cs) ts1, [T.close cs t2]))
 *)

(* Programs *)

let check_prog scope prog : scope Diag.result =
  Diag.with_message_store (fun msgs ->
    let env = env_of_scope msgs scope in
    recover_opt (check_block env T.unit prog.it) prog.at)

let infer_prog scope prog : (T.typ * scope) Diag.result =
  Diag.with_message_store (fun msgs ->
    let env = env_of_scope msgs scope in
    recover_opt (infer_block env prog.it) prog.at)
