open Source

open Common
open Syntax

module T = Mo_types.Type
module M = Mo_def.Syntax
module Arrange = Mo_def.Arrange

module Stamps = Env.Make(String)

(* symbol generation *)

let stamps : int Stamps.t ref = ref Stamps.empty

let reset_stamps () = stamps := Stamps.empty

let fresh_stamp name =
  let n = Lib.Option.get (Stamps.find_opt name !stamps) 0 in
  stamps := Stamps.add name (n + 1) !stamps;
  n

let fresh_id name =
  let n = fresh_stamp name in
  if n = 0 then
    name
  else Printf.sprintf "%s_%i" name (fresh_stamp name)

(* helpers for constructing annotated syntax *)

let (^^^) at it note = { it; at; note}

let (!!!) at it = (^^^) at it NoInfo


let intLitE at i =
  !!! at (IntLitE (Mo_values.Numerics.Int.of_int i))

let accE at ?(perm=FullP) fldacc =
  !!! at
    (AccE(
         fldacc,
         !!! at (PermE (!!! at perm))))

let rec conjoin es at =
  match es with
  | [] -> !!! at (BoolLitE true)
  | e::[] -> e
  | e::es' -> !!! at (AndE(e, conjoin es' at))

let rec adjoin ctxt e = function
  | [] -> e
  | f :: fs -> f ctxt (adjoin ctxt e fs)

let locE at arr_e ix_e field_name =
  !!! at (CallE ("$loc", [arr_e; ix_e])), !!! at field_name

let sizeE at lhs =
  !!! at (CallE ("$size", [lhs]))

let arrayAccE at lhs field_name perm =
  let (!!) p = !!! at p in
  !! (CallE ("$array_acc", [lhs; !!(FldE field_name); !!(PermE(!! perm))]))

let prjE at tuple_e ix_e field_name =
  !!! at (CallE ("$prj", [tuple_e; ix_e])), !!! at field_name

let (|:) (x_opt : 'a option) (xs : 'a list) : 'a list =
  match x_opt with
  | None -> xs
  | Some(x) -> x :: xs

type sort = Field | Local | Method

(* the LHS of an assignment *)
type lvalue =
  | LValueVar of id         (* a variable *)
  | LValueUninitVar of id   (* an uninitialized variable *)
  | LValueFld of fldacc     (* field access *)

let lvalue_str (lval : lvalue) : string =
  match lval with
  | LValueVar x -> x.it
  | LValueUninitVar x -> x.it
  | LValueFld fld -> (snd fld).it

module Env = T.Env

type ctxt =
  { self : string option;
    ids : (sort * T.t) T.Env.t;
    ghost_items : (ctxt -> item) list ref;
    ghost_inits : (ctxt -> seqn') list ref;
    ghost_perms : (ctxt -> Source.region -> exp) list ref;
    ghost_conc : (ctxt -> exp -> exp) list ref;
  }

let self ctxt at =
  match ctxt.self with
  | Some id -> !!! at (LocalVar (!!! at id,!!! at RefT))
  | _ -> failwith "no self"

let rec extract_invariants : item list -> (par -> invariants -> invariants) = function
  | [] -> fun _ x -> x
  | { it = InvariantI (s, e); at; _ } :: p ->
      fun self es ->
        !!! at (CallE(s, [ !!! at (LocalVar (fst self, snd self)) ]))
        :: extract_invariants p self es
  | _ :: p -> extract_invariants p

let rec extract_loop_invariants (e : M.exp) : (M.exp list * M.exp) =
  match e with
  | { it = M.BlockE ds; _ } ->
    let (invs, ds') = extract_loop_invariants' ds [] in
    (invs, { e with it = M.BlockE ds' })
  | _ -> ([], e)
and extract_loop_invariants' (ds : M.dec list) (acc : M.exp list) : (M.exp list * M.dec list) =
  match ds with
  | M.({ it = ExpD ({ it = AssertE (Loop_invariant, inv); _ }); _ }) :: ds ->
      extract_loop_invariants' ds (inv :: acc)
  | _ -> (List.rev acc, ds)

let rec extract_concurrency (seq : seqn) : stmt' list * seqn =
  let open List in
  let extr (concs, stmts) s : stmt' list * stmt list =
    match s.it with
    | ConcurrencyS _ -> s.it :: concs, stmts
    | SeqnS seq ->
      let concs', seq = extract_concurrency seq in
      rev_append concs' concs, { s with it = SeqnS seq } :: stmts
    | WhileS (e, inv, seq) ->
      let concs', seq = extract_concurrency seq in
      rev_append concs' concs, { s with it = WhileS (e, inv, seq) } :: stmts
    | IfS (e, the, els) ->
      let the_concs, the = extract_concurrency the in
      let els_concs, els = extract_concurrency els in
      rev_append els_concs (rev_append the_concs concs), { s with it = IfS (e, the, els) } :: stmts
    | _ -> concs, s :: stmts in

  let stmts = snd seq.it in
  let conc, stmts = List.fold_left extr ([], []) stmts in
  rev conc, { seq with it = fst seq.it, rev stmts }

let concat_map_seqn' : ('a -> seqn') -> 'a list -> seqn' =
  fun f xs ->
    let ds, stmts = List.split (List.map f xs) in
    List.concat ds, List.concat stmts

let rec unit (u : M.comp_unit) : prog Diag.result =
  Diag.(
    reset_stamps();
    try return (unit' u) with
    | Unsupported (at, desc) -> error at "0" "viper" ("translation to viper failed:\n"^desc)
    | _ -> error u.it.M.body.at "1" "viper" "translation to viper failed"
  )

and unit' (u : M.comp_unit) : prog =
  let { M.imports; M.body } = u.it in
  match body.it with
  | M.ActorU(id_opt, decs) ->
    let ctxt = { self = None; ids = Env.empty; ghost_items = ref []; ghost_inits = ref []; ghost_perms = ref []; ghost_conc = ref [] } in
    let ctxt', perms, inits, mk_is = dec_fields ctxt decs in
    let is' = List.map (fun mk_i -> mk_i ctxt') mk_is in
    (* given is', compute ghost_is *)
    let ghost_is = List.map (fun mk_i -> mk_i ctxt') !(ctxt.ghost_items) in
    let init_id = !!! (Source.no_region) "__init__" in
    let self_id = !!! (Source.no_region) "$Self" in
    let self_typ = !!! (self_id.at) RefT in
    let ctxt'' = { ctxt' with self = Some self_id.it } in
    let perm =
      fun (at : region) ->
       List.fold_left
         (fun pexp -> fun p_fn ->
           !!! at (AndE(pexp, p_fn ctxt'' at)))
         (!!! at (BoolLitE true))
         (perms @ !(ctxt.ghost_perms))
    in
    (* Add initializer *)
    let init_list = concat_map_seqn' (fun mk_s -> mk_s ctxt'') (inits @ !(ctxt.ghost_inits)) in
    let init_body =
      !!! (body.at) init_list (* ATG: Is this the correct position? *)
    in
    let init_m =
      (^^^) (body.at) (MethodI(init_id, [self_id, self_typ], [], [], [], Some init_body)) ActorInit
    in
    let is'' = init_m :: is' in
    (* Add permissions *)
    let is''' = List.map (function
    | {it = MethodI (id, ins, outs, pres, posts, body); at; note: info} ->
      (^^^)
        at
        (MethodI (id, ins, outs,
          !!! at (CallE("$Perm", [self ctxt'' at]))::pres,
          !!! at (CallE("$Perm", [self ctxt'' at]))::posts,
          body))
        note
      | x -> x) is'' in
    (* Add functional invariants to public functions *)
    let invs = extract_invariants is''' (self_id, self_typ) [] in
    let is4 = List.map (function
      | {
        it = MethodI (id, ins, outs, pres, posts, body);
        at;
        note = ActorInit
      } -> ((^^^)
        at
        (MethodI(id, ins, outs,
          pres,
          posts @ [!!! at (CallE("$Inv", [self ctxt'' at]))],
          body))
        ActorInit
      )
      | {
        it = MethodI (id, ins, outs, pres, posts, body);
        at;
        note = PublicFunction x
      } -> ((^^^)
        at
        (MethodI(id, ins, outs,
          pres @ [!!! at (CallE("$Inv", [self ctxt'' at]))],
          posts @ [!!! at (CallE("$Inv", [self ctxt'' at]))],
          body))
        (PublicFunction x)
      )
      | x -> x
    ) is''' in
    let perm_def = !!! (body.at) (InvariantI("$Perm", perm body.at)) in
    let inv_def = !!! (body.at) (InvariantI("$Inv", adjoin ctxt'' (conjoin invs body.at) !(ctxt.ghost_conc))) in
    let is = ghost_is @ (perm_def :: inv_def :: is4) in
    !!! (body.at) is
  | _ -> assert false

and dec_fields (ctxt : ctxt) (ds : M.dec_field list) =
  match ds with
  | [] ->
    (ctxt, [], [], [])
  | d :: ds ->
    let ctxt, perm, init, mk_i = dec_field ctxt d in
    let ctxt, perms, inits, mk_is = dec_fields ctxt ds in
    (ctxt, perm |: perms, init |: inits, mk_i::mk_is)

and dec_field ctxt d =
  let ctxt, perm, init, mk_i = dec_field' ctxt d.it in
   (ctxt,
    perm,
    init,
    fun ctxt' ->
      let (i, info) = mk_i ctxt' in
      (^^^) (d.at) i info)

and static_invariants at lhs e =
  match e.it with
 | M.AnnotE (e, _) -> static_invariants at lhs e
 | M.ArrayE (_, es) -> [array_size_inv at lhs (List.length es)]
 | _ -> []

and dec_field' ctxt d =
  match d.M.dec.it with
  (* async functions *)
  | M.(LetD ({it=VarP f;note;_},
             {it=FuncE(x, sp, tp, p, t_opt, sugar,
             {it = AsyncE (T.Fut, _, e); _} );_}, None)) -> (* ignore async *)
      { ctxt with ids = Env.add f.it (Method, note) ctxt.ids },
      None, (* no perm *)
      None, (* no init *)
      fun ctxt' ->
        let open Either in
        let self_id = !!! (Source.no_region) "$Self" in
        let method_args = args p in
        let method_args' = List.map (fun (id, t) -> id, tr_typ t) method_args in
        let ctxt'' = { ctxt'
          with self = Some self_id.it;
               ids = List.fold_left (fun env (id, t) -> Env.add id.it (Local, t) env) ctxt.ids method_args }
        in
        let stmts = stmt ctxt'' e in
        let _, stmts = extract_concurrency stmts in
        let pres, stmts' = List.partition_map (function { it = PreconditionS exp; _ } -> Left exp | s -> Right s) (snd stmts.it) in
        let posts, stmts' = List.partition_map (function { it = PostconditionS exp; _ } -> Left exp | s -> Right s) stmts' in
        let arg_preds = local_access_preds ctxt'' in
        let ret_preds, ret = rets t_opt in
        let pres = arg_preds @ pres in
        let posts = arg_preds @ posts @ ret_preds in
        let stmts'' = stmts' @ [!!! Source.no_region (LabelS(!!! (Source.no_region) "$Ret"))] in
        (MethodI(id f, (self_id, !!! Source.no_region RefT)::method_args', ret, pres, posts, Some { stmts with it = fst stmts.it, stmts'' } ),
        PublicFunction f.it)
  (* private sync functions *)
  | M.(LetD ({it=VarP f; note;_},
             {it=FuncE(x, sp, tp, p, t_opt, sugar, e );_},
             None)) ->
      { ctxt with ids = Env.add f.it (Method, note) ctxt.ids },
      None, (* no perm *)
      None, (* no init *)
      fun ctxt' ->
        let open Either in
        let self_id = !!! (Source.no_region) "$Self" in
        let method_args = args p in
        let method_args' = List.map (fun (id, t) -> id, tr_typ t) method_args in
        let ctxt'' = { ctxt'
          with self = Some self_id.it;
               ids = List.fold_left (fun env (id, t) -> Env.add id.it (Local, t) env) ctxt.ids method_args }
        in
        let stmts = stmt ctxt'' e in
        let _, stmts = extract_concurrency stmts in
        let pres, stmts' = List.partition_map (function { it = PreconditionS exp; _ } -> Left exp | s -> Right s) (snd stmts.it) in
        let posts, stmts' = List.partition_map (function { it = PostconditionS exp; _ } -> Left exp | s -> Right s) stmts' in
        let arg_preds = local_access_preds ctxt'' in
        let ret_preds, ret = rets t_opt in
        let pres = arg_preds @ pres in
        let posts = arg_preds @ posts @ ret_preds in
        let stmts'' = stmts' @ [!!! Source.no_region (LabelS(!!! (Source.no_region) "$Ret"))] in
        (MethodI(id f, (self_id, !!! Source.no_region RefT)::method_args', ret, pres, posts, Some { stmts with it = fst stmts.it, stmts'' } ),
        PrivateFunction f.it)
  (* fields *)
  | M.LetD ({it=M.VarP x;_}, e, None)
  | M.VarD (x, e) ->
      let t = e.note.M.note_typ in
      let fldacc = fun ctxt' -> (self ctxt' e.at, id x) in
      let lhs = fun ctxt' -> !!! Source.no_region (FldAcc (fldacc ctxt')) in
      let perms ctxt' at =
            conjoin ([ accE at (self ctxt' at, id x) ]
                     @ (access_pred (lhs ctxt') t |: [])
                     @ static_invariants at (lhs ctxt') e) at in
      { ctxt with ids = Env.add x.it (Field, t) ctxt.ids },
      Some perms, (* perm *)
      Some (fun ctxt' -> (* init *)
          let at = span x.at e.at in
          assign_stmts ctxt' at (LValueFld (fldacc ctxt')) e),
      (fun ctxt' ->
        (FieldI(id x, tr_typ t),
        NoInfo))
  (* invariants *)
  | M.(ExpD { it = AssertE (Invariant, e); at; _ }) ->
      ctxt,
      None, (* no perm *)
      None, (* no init *)
      fun ctxt' ->
        (InvariantI (Printf.sprintf "invariant_%d" at.left.line, exp { ctxt' with self = Some "$Self" }  e), NoInfo)
  | _ ->
     unsupported d.M.dec.at (Arrange.dec d.M.dec)

and args p = match p.it with
  | M.TupP ps -> List.map arg ps
  | M.ParP p  -> [arg p]
  | _ -> unsupported p.at (Arrange.pat p)
and arg p = match p.it with
  | M.AnnotP (p, t) ->
      (match p.it with
        | M.VarP x -> (id x, t.note)
        | _ -> unsupported p.at (Arrange.pat p))
  | _ -> unsupported p.at (Arrange.pat p)

and access_pred lhs t =
  match T.normalize t with
  | T.Array elem_t -> Some (array_acc Source.no_region lhs elem_t)
  | T.Tup   ts     -> Some (tuple_acc Source.no_region lhs ts WildcardP) (* tuples are immutable *)
  | _ -> None

(* Get access predicates for all local variables in current scope *)
and local_access_preds ctxt =
  let (!!) p = !!! Source.no_region p in
  let preds = Env.fold (fun id info preds ->
      match info with
      | (Local, t) ->
        let pred = access_pred !!(LocalVar (!!id, tr_typ t)) t in
        pred |: preds
      | _ -> preds)
    ctxt.ids []
  in preds

and block ctxt at ds =
  let ctxt, mk_ss = decs ctxt ds in
  !!! at (mk_ss ctxt)

and decs ctxt ds =
  match ds with
  | [] -> (ctxt, fun ctxt' -> ([],[]))
  | d::ds' ->
    let (ctxt1, mk_s) = dec ctxt d in
    let (ctxt2, mk_ss) = decs ctxt1 ds' in
    (ctxt2,
     fun ctxt' ->
       let (l, s) = mk_s ctxt' in
       let (ls, ss) = mk_ss ctxt' in
       (l @ ls, s @ ss))

and dec ctxt d =
  let (!!) p = !!! (d.at) p in
  match d.it with
  | M.LetD ({it=M.VarP x;_}, e, None)
  | M.VarD (x, e) ->
    { ctxt with ids = Env.add x.it (Local, e.note.M.note_typ) ctxt.ids },
    fun ctxt' ->
      let lval = LValueUninitVar (id x) in
      let d = !!(id x, tr_typ e.note.M.note_typ) in
      let ds, stmts = assign_stmts ctxt' d.at lval e in
      (d :: ds, stmts)
  | M.(ExpD e) -> (* TODO: restrict to e of unit type? *)
     (ctxt,
      fun ctxt' ->
        let s = stmt ctxt' e in
        s.it)
  | _ ->
     unsupported d.at (Arrange.dec d)

and stmt ctxt (s : M.exp) : seqn =
  let (!!) p = !!! (s.at) p in
  match s.it with
  | M.TupE [] ->
     block ctxt s.at []
  | M.BlockE ds ->
     block ctxt s.at ds
  | M.IfE(e, s1, s2) ->
    !!([],
       [ !!(IfS(exp ctxt e, stmt ctxt s1, stmt ctxt s2))])
  | M.(AwaitE(T.Fut, { it = AsyncE (T.Fut, _, e); at; _ })) -> (* gross hack *)
     let id = fresh_id "$message_async" in
     let (!!) p = !!! (s.at) p in
     let (!@) p = !!! at p in
     ctxt.ghost_items :=
       (fun ctxt ->
         !!(FieldI (!!id, !!IntT))) ::
       !(ctxt.ghost_items);
     let mk_s = fun ctxt ->
       [],
       [!@ (FieldAssignS ((self  ctxt s.at, !!id),
            intLitE (s.at) 0))]
     in
     ctxt.ghost_inits := mk_s :: !(ctxt.ghost_inits);
     let mk_p = fun ctxt at ->
       accE at (self ctxt at, !!! at id)
     in
     ctxt.ghost_perms := mk_p :: !(ctxt.ghost_perms);
     let stmts = stmt ctxt e in
     (* assume that each `async {...}` has an assertion *)
     let conc, _ = extract_concurrency stmts in
     let mk_c = match conc with
       | [] ->
         fun _ x -> x
       | ConcurrencyS ("1", _, cond) :: _ ->
         let (!?) p = !!! (cond.at) p in
         let zero, one = intLitE Source.no_region 0, intLitE Source.no_region 1 in
         fun ctxt x ->
           let ghost_fld () = !?(FldAcc (self ctxt cond.at, !?id)) in
           let between = !?(AndE (!?(LeCmpE (zero, ghost_fld ())), !?(LeCmpE (ghost_fld (), one)))) in
           let is_one = !?(EqCmpE (ghost_fld (), one)) in
           !?(AndE (x, !?(AndE (between, !?(Implies (is_one, cond.it (exp ctxt)))))))
       | _ -> unsupported e.at (Arrange.exp e) in
     ctxt.ghost_conc := mk_c :: !(ctxt.ghost_conc);
     !!([],
        [ !!(FieldAssignS(
            (self ctxt Source.no_region, !!id),
            (!!(AddE(!!(FldAcc (self ctxt (s.at), !!id)),
                     intLitE Source.no_region 1)))));
          !@(ExhaleS (!@(AndE(!@(CallE("$Perm", [self ctxt at])),
                              !@(CallE("$Inv", [self ctxt at]))))));
          !@(SeqnS (
              !@([],
                 [
                   !@(InhaleS (!@(AndE(!@(CallE("$Perm", [self ctxt at])),
                                  !@(AndE(!@(CallE("$Inv", [self ctxt at])),
                                          !@(GtCmpE(!@(FldAcc (self ctxt at, !@id)),
                                               intLitE Source.no_region 0))))))));
                   !@(FieldAssignS(
                          (self ctxt at, !@id),
                          (!@(SubE(!@(FldAcc (self ctxt at, !@id)),
                                   intLitE at 1)))));
                   !!! (e.at) (SeqnS stmts);
                   !@(ExhaleS (!@(AndE(!@(CallE("$Perm", [self ctxt at])),
                                       !@(CallE("$Inv", [self ctxt at])))))) ])));
          !!(InhaleS (!!(AndE(!!(CallE("$Perm", [self ctxt at])),
                              !!(CallE("$Inv", [self ctxt at]))))));
        ])
  | M.WhileE(e, s1) ->
     let (invs, s1') = extract_loop_invariants s1 in
     let invs' = List.map (fun inv -> exp ctxt inv) invs in
     let invs'' = invs' @ local_access_preds ctxt in
     let invs''' = invs'' @ [!!(AndE(!!(CallE("$Perm", [self ctxt s.at])),
                                     !!(CallE("$Inv",  [self ctxt s.at]))))] in
     !!([],
        [ !!(WhileS(exp ctxt e, invs''', stmt ctxt s1')) ])

  | M.(AssignE({it = VarE x; _}, e2)) ->
      let lval = (match fst (Env.find x.it ctxt.ids) with
                 | Local -> LValueVar (!!! (x.at) (x.it))
                 | Field -> LValueFld (self ctxt x.at, id x)
                 | _ -> unsupported s.at (Arrange.exp s)
                 ) in
      !!(assign_stmts ctxt s.at lval e2)
  | M.(AssignE({it = IdxE (e1, e2);_}, e3)) ->
      let lval = LValueFld (array_loc ctxt s.at e1 e2 e3.note.M.note_typ) in
      !!(assign_stmts ctxt s.at lval e3)
  | M.AssertE (M.Precondition, e) ->
    !!( [],
        [ !!(PreconditionS (exp ctxt e)) ])
  | M.AssertE (M.Postcondition, e) ->
    !!([],
       [ !!(PostconditionS (exp ctxt e)) ])
  | M.AssertE (M.Concurrency n, e) ->
    !!([],
       [ !!(ConcurrencyS (n, exp ctxt e, !! ((|>) e))) ])
  | M.AssertE (M.Static, e) ->
    !!([],
       [ !!(AssertS (exp ctxt e)) ])
  | M.AssertE (M.Runtime, e) ->
    !!([],
       [ !!(AssumeS (exp ctxt e)) ])
  | M.(CallE({it = VarE m; _}, inst, args)) ->
    !!([],
       [ !!(MethodCallS ([], id m,
       let self_var = self ctxt m.at in
       self_var :: call_args ctxt args))])
  | M.RetE e ->
      let lval = (LValueUninitVar (!!! (Source.no_region) "$Res")) in
      let ds, stmts = assign_stmts ctxt s.at lval e in
      let stmt = !!(GotoS(!!! (Source.no_region) "$Ret")) in
      !!(ds, stmts @ [stmt])
  | _ ->
     unsupported s.at (Arrange.exp s)

(* Translate assignment a:=b or initialization. May create temporary variables
   if the LHS is a field or if the RHS is an array. *)
and assign_stmts ctxt at (lval : lvalue) (e : M.exp) : seqn' =
  let (!!) p = !!! at p in
  let t = e.note.M.note_typ in
  match e with
  | M.({it=TupE [];_}) -> [], []
  | M.({it=AnnotE (e, _);_}) -> assign_stmts ctxt at lval e
  | M.({it = CallE({it = VarE m; _}, inst, args); _}) ->
    fld_via_tmp_var lval t (fun x ->
      let self_var = self ctxt m.at in
      [], [ !!(MethodCallS ([x], id m, self_var :: call_args ctxt args)) ])
  | M.({it=ArrayE(mut, es); _}) ->
    via_tmp_var lval t (fun x ->
      let lhs = !!(LocalVar (x, tr_typ t)) in
      [], array_alloc at ctxt lhs (array_elem_t t) es)
  | M.({it=TupE(es); _}) ->
    via_tmp_var lval t (fun x ->
      let lhs = !!(LocalVar (x, tr_typ t)) in
      [], tuple_alloc at ctxt lhs (tuple_elem_ts t) es)
  | _ ->
    [], [!!(assign_stmt lval (exp ctxt e))]

and assign_stmt (lval : lvalue) (e : exp) : stmt' =
  match lval with
    | LValueVar x       -> VarAssignS(x, e)
    | LValueUninitVar x -> VarAssignS(x, e)
    | LValueFld fld     -> FieldAssignS(fld, e)

and fld_via_tmp_var (lval : lvalue) (t : T.typ) (f : id -> seqn') : seqn' =
  match lval with
  | LValueVar x       -> f x
  | LValueUninitVar x -> f x
  | LValueFld _       -> via_tmp_var lval t f

and via_tmp_var (lval : lvalue) (t : T.typ) (f : id -> seqn') : seqn' =
  match lval with
  | LValueUninitVar x -> f x  (* initialization never needs a tmp variable *)
  | _ ->
    let (!!) p = !!! Source.no_region p in
    let tmp_id = !! (fresh_id ("$t_" ^ lvalue_str lval)) in
    let tmp_typ = tr_typ t in
    let tmp_e = !! (LocalVar (tmp_id, tmp_typ)) in
    let d = !! (tmp_id, tmp_typ) in
    let ds, stmts = f tmp_id in
    let stmt = !!(assign_stmt lval tmp_e) in
    d :: ds, stmts @ [stmt]

and call_args ctxt e =
  match e with
  | {it = M.TupE args; _} -> List.map (fun arg -> exp ctxt arg) args
  | arg -> [exp ctxt arg]

and exp ctxt e =
  let open Mo_values.Operator in
  let (!!) p = !!! (e.at) p in
  let t = e.note.M.note_typ in
  match e.it with
  | M.VarE x ->
    begin
     match fst (Env.find x.it ctxt.ids) with
     | Local ->
        !!(LocalVar (id x, tr_typ t))
     | Field ->
        !!(FldAcc (self ctxt x.at, id x))
     | _ ->
        unsupported e.at (Arrange.exp e)
    end
  | M.ResVarE -> !!(FldE "$Res")
  | M.AnnotE(a, b) ->
    exp ctxt a
  | M.CallE ({it=M.DotE (e1, {it="size";_});_}, _inst, {it=M.TupE ([]);at;_})
      -> sizeE at (exp ctxt e1)
  | M.LitE r ->
    begin match !r with
    | M.BoolLit b ->
       !!(BoolLitE b)
    | M.IntLit i ->
       !!(IntLitE i)
    | M.NatLit i ->
       !!(IntLitE i)
    | _ ->
       unsupported e.at (Arrange.exp e)
    end
  | M.NotE e ->
     !!(NotE (exp ctxt e))
  | M.RelE (ot, e1, op, e2) ->
     let e1, e2 = exp ctxt e1, exp ctxt e2 in
     !!(match op with
      | EqOp -> EqCmpE (e1, e2)
      | NeqOp -> NeCmpE (e1, e2)
      | GtOp -> GtCmpE (e1, e2)
      | GeOp -> GeCmpE (e1, e2)
      | LtOp -> LtCmpE (e1, e2)
      | LeOp -> LeCmpE (e1, e2))
  | M.BinE (ot, e1, op, e2) ->
     let e1, e2 = exp ctxt e1, exp ctxt e2 in
     !!(match op with
      | AddOp -> AddE (e1, e2)
      | SubOp -> SubE (e1, e2)
      | MulOp -> MulE (e1, e2)
      | DivOp -> DivE (e1, e2)
      | ModOp -> ModE (e1, e2)
      | _ -> unsupported e.at (Arrange.exp e))
  | M.OrE (e1, e2) ->
     !!(OrE (exp ctxt e1, exp ctxt e2))
  | M.AndE (e1, e2) ->
     !!(AndE (exp ctxt e1, exp ctxt e2))
  | M.ImpliesE (e1, e2) ->
     !!(Implies (exp ctxt e1, exp ctxt e2))
  | M.OldE e ->
    !!(Old (exp ctxt e))
  | M.IdxE (e1, e2) ->
     !!(FldAcc (array_loc ctxt e.at e1 e2 t))
  | M.ProjE (e, i) ->
     !!(FldAcc (prjE e.at (exp ctxt e) (intLitE e.at i) (typed_field t)))
  | _ ->
     unsupported e.at (Arrange.exp e)

and rets t_opt =
  let (!!) p = !!! Source.no_region p in
  match t_opt with
  | None -> [], []
  | Some t ->
     (match t.note with
     | T.Tup [] -> [], []
     | T.Async (T.Fut, _, _) -> [], []
     | typ ->
        let pred = access_pred !!(LocalVar (!!"$Res", tr_typ typ)) typ in
        pred |: [], [(!!"$Res", tr_typ  typ)]
    )

and id id = { it = id.it; at = id.at; note = NoInfo }

and tr_typ typ =
  { it = tr_typ' typ;
    at = Source.no_region;
    note = NoInfo }
and tr_typ' typ =
  match T.normalize typ with
  | T.Prim T.Int -> IntT
  | T.Prim T.Nat -> IntT    (* Viper has no native support for Nat, so translate to Int *)
  | T.Prim T.Bool -> BoolT
  | T.Array _ -> ArrayT     (* Viper arrays are not parameterised by element type *)
  | T.Tup   _ -> TupleT     (* Viper tuples are not parameterised by element type *)
  | t -> unsupported Source.no_region (Mo_types.Arrange_type.typ t)

and is_mut t =
  match T.normalize t with
  | T.Mut _ -> true
  | _       -> false

and array_elem_t t =
  match T.normalize t with
  | T.Array elem_t -> elem_t
  | t -> failwith "array_elem_t: expected array type"

and tuple_elem_ts t =
  match T.normalize t with
  | T.Tup ts -> ts
  | t -> failwith "tuple_elem_ts: expected tuple type"

(* name of field of typed reference *)
and typed_field t =
  match T.normalize t  with
  | T.Mut elem_t -> typed_field elem_t
  | T.Prim T.Int  -> "$int"
  | T.Prim T.Nat  -> "$int"  (* Viper has no native support for Nat, so translate to Int *)
  | T.Prim T.Bool -> "$bool"
  | _ -> unsupported Source.no_region (Mo_types.Arrange_type.typ t)

and array_size_inv at lhs n =
  !!! at (EqCmpE (sizeE at lhs, intLitE at n))

and array_acc at lhs t =
  match T.normalize t with
  | T.Mut _-> arrayAccE at lhs (typed_field t) FullP
  | _      -> arrayAccE at lhs (typed_field t) WildcardP

(* Allocate array on the LHS expression.
   Note: array_alloc assumes that the array is uninitialized. Assignment to
   existing arrays must be done via a temporary variable. *)
and array_alloc at ctxt lhs t es : stmt list =
  let (!!) p = !!! at p in
  let init_array = List.mapi (fun i e ->
    FieldAssignS (locE at lhs (intLitE at i) (typed_field t), exp ctxt e)) es in
  (* InhaleS (!! (FldAcc (locE at lhs (intLitE at i) (typed_field t))) === e)) es in *)
  let reset_perm =
    (match T.normalize t with
     | T.Mut _ -> []
     | _       -> [ExhaleS (array_acc at lhs t); InhaleS (array_acc at lhs t)])in
  let stmts = [ InhaleS (array_acc at lhs (T.Mut t))
              ; InhaleS (array_size_inv at lhs (List.length es))]
              @ init_array
              @ reset_perm
  in List.map (!!) stmts

and array_loc ctxt at e1 e2 t =
  locE at (exp ctxt e1) (exp ctxt e2) (typed_field t)

and tuple_acc at lhs ts perm =
  (* TODO tuple: think about general recursive scheme *)
  let ps = List.mapi (fun i t -> accE at ~perm (prjE at lhs (intLitE at i) (typed_field t))) ts in
  conjoin ps at

and tuple_alloc at ctxt lhs ts es : stmt list =
  let tsi = List.mapi (fun i t -> i, t) ts in
  let init_tuple =
    List.map2 (fun e (i, t) ->
      FieldAssignS (prjE at lhs (intLitE at i) (typed_field t), exp ctxt e)
    ) es tsi in
  let reset_perms =
    List.concat_map (fun (i, t) ->
      let r = prjE at lhs (intLitE at i) (typed_field t) in
      if is_mut t then [] else [ ExhaleS (accE at r ~perm:WildcardP)
                               ; InhaleS (accE at r ~perm:WildcardP)]
    ) tsi in
  let stmts = [ InhaleS (tuple_acc at lhs ts FullP)]
              @ init_tuple
              @ reset_perms
  in List.map (!!! at) stmts

and tuple_prj ctxt at e1 e2 t =
  prjE at (exp ctxt e1) (exp ctxt e2) (typed_field t)

