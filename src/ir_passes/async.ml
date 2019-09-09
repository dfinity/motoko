open As_types
open Ir_def

module E = Env
open Source
module Ir = Ir
open Ir
open Ir_effect
module T = Type
open T
open Construct

(* lower the async type itself
   - adds a final reply argument to every awaitable shared function, replace
     the result by unit
   - declares a trapping reject callback in every awaitable shared function
   - transforms types, introductions and eliminations of awaitable shared
     functions only, leaving non-awaitable shared functions unchanged.
   - ensures every call to an awaitable shared function that takes a tuple has
     a manifest tuple argument extended with a final reply continuation.
 *)

(* written as a functor so we can allocate some temporary shared state without making it global *)

module Transform() = struct

  module ConRenaming = E.Make(struct type t = T.con let compare = Con.compare end)

  (* the state *)

  (* maps constructors to new constructors (name name, new stamp, new kind)
     it is initialized with the type constructors defined outside here, which are
     not rewritten.

     If we run this translation on two program fragments (e.g. prelude and program)
     we would have to pass down the `con_renaming`. But this is simply the right thing
     to do for a pass that changes the context.
  *)

  let con_renaming = ref ConRenaming.empty

  (* Configuring the translation for target platform V1 or V2 *)

  type platform =
    V1  (* legacy, Haskell *)
  | V2  (* new, Rust *)

  let platform = V1

  let _ = V2 (* suppress warning on unused V2 *)

  (* Lowering options, specific to V1 or V2 *)

  let add_reply_parameter, add_reject_parameter, add_reply_argument, add_reject_argument =
    match platform with
    | V1 -> (true, false, true, false)
    | V2 -> (false, false, true, true)

  (* Helper for selective code generation based on predicated lazy expressions *)
  let rec select bls =
    match bls with
    | [] -> []
    | (true, l)::bls' ->
      (Lazy.force l)::select bls'
    | (false, _)::bls' ->
      select bls'

  (* Explicit invocation of reply, reject and call System API functions;
     implemented (as far as possible) for V1;
     TBC for V2 *)

  let sys_replyE vs =
    match platform with
    | V1 -> assert false (* never required in V1, `reply` is by calling continuation*)
    | V2 -> failwith "NYI" (* TODO: call dedicated prim *)

  let sys_rejectE e =
    match platform with
    |  V1 -> assertE (boolE false) (* used in V1 to cause traps on non-local throws *)
    |  V2 -> failwith "NYI" (* TODO: call dedicated prim *)

  let sys_callE v1 typs vs reply reject =
    match platform with
    | V1 ->
          assert (add_reply_argument && not add_reject_argument);
          callE v1 typs (seqE (vs @ [reply]))
    | V2 -> failwith "NYI" (* TODO: call dedicated prim, separating args vs from reply & reject *)

  let sys_error_codeE () =
    match platform with
    | V1 -> { it = TagE ("error", tupE []);
              at = no_region;
              note = {
                note_typ = T.Variant (T.catchErrorCodes);
                note_eff = T.Triv }
            };
    | V2 -> failwith "NYI" (* TODO: call dedicated prim *)

  let errorMessageE e =
  { it = PrimE (OtherPrim "errorMessage", [e]);
    at = no_region;
    note = { note_typ = T.text; note_eff = eff e }
  }

  let make_errorE e_code e_msg =
  { it = PrimE (OtherPrim "make_error", [e_code; e_msg]);
    at = no_region;
    note = { note_typ = T.Prim T.Error; note_eff = max (eff e_code) (eff e_msg) }
  }

  (* End of configuration *)

  let unary typ = [typ]

  let nary typ = T.as_seq typ

  let replyT as_seq typ = T.Func(T.Shared, T.Returns, [], as_seq typ, [])

  let rejectT = T.Func(T.Shared, T.Returns, [], [T.text], [])

  let fulfillT as_seq typ = T.Func(T.Local, T.Returns, [], as_seq typ, [])

  let failT = T.Func(T.Local, T.Returns, [], [T.catch], [])

  let t_async as_seq t =
    T.Func (T.Local, T.Returns, [], [fulfillT as_seq t; failT], [])

  let new_async_ret as_seq t = [t_async as_seq t; fulfillT as_seq t; failT]

  let new_asyncT =
    T.Func (
        T.Local,
        T.Returns,
        [ { var = "T"; bound = T.Any } ],
        [],
        new_async_ret unary (T.Var ("T", 0))
      )

  let new_asyncE =
    idE "@new_async" new_asyncT

  let new_async t1 =
    let call_new_async = callE new_asyncE [t1] (tupE []) in
    let async = fresh_var "async" (typ (projE call_new_async 0)) in
    let fulfill = fresh_var "fulfill" (typ (projE call_new_async 1)) in
    let fail = fresh_var "fail" (typ (projE call_new_async 2)) in
    (async, fulfill, fail), call_new_async

  let new_nary_async_reply t1 =
    let (unary_async, unary_fulfill, fail), call_new_async = new_async t1 in
    let v' = fresh_var "v" t1 in
    let ts1 = T.as_seq t1 in
    (* construct the n-ary async value, coercing the continuation, if necessary *)
    let nary_async =
      let k' = fresh_var "k" (contT t1) in
      let r' = fresh_var "r" err_contT in
      match ts1 with
      | [t] ->
        unary_async
      | ts ->
        let seq_of_v' = tupE (List.mapi (fun i _ -> projE v' i) ts) in
        [k';r'] -->*  (unary_async -*- (tupE[([v'] -->* (k' -*- seq_of_v'));r']))
    in
    (* construct the n-ary reply message that sends a sequence of values to fulfill the async *)
    let nary_reply =
      let vs,seq_of_vs =
        match ts1 with
        | [t] ->
          let v = fresh_var "rep" t in
          [v],v
        | ts ->
          let vs = fresh_vars "rep" ts in
          vs, tupE vs
      in
      vs -@>* (unary_fulfill -*- seq_of_vs)
    in
    let nary_reject =
      let v = fresh_var "msg" T.text in
      [v] -@>* (fail -*- (make_errorE (sys_error_codeE()) v))
    in
    let async,reply,reject =
      fresh_var "async" (typ nary_async),
      fresh_var "reply" (typ nary_reply),
      fresh_var "reject" (typ nary_reject)
    in
      (async, reply, reject),
        blockE [letP (tupP [varP unary_async; varP unary_fulfill; varP fail])  call_new_async]
          (tupE [nary_async; nary_reply; nary_reject])


  let letEta e scope =
    match e.it with
    | VarE _ -> scope e (* pure, so reduce *)
    | _  -> let f = fresh_var "x" (typ e) in
            letD f e :: (scope f) (* maybe impure; sequence *)

  let isAwaitableFunc exp =
    match typ exp with
    | T.Func (T.Shared,T.Promises,_,_,[T.Async _]) -> true
    | _ -> false

  let extendTup ts ts' = ts @ ts'

  (* Given sequence type ts, bind e of type (seq ts) to a
   sequence of expressions supplied to decs d_of_es,
   preserving effects of e when the sequence type is empty.
   d_of_es must not duplicate or discard the evaluation of es.
   *)
  let letSeq ts e d_of_vs =
    match ts with
    | [] ->
      (expD e)::d_of_vs []
    | [t] ->
      let x = fresh_var "x" t in
      let p = varP x in
      (letP p e)::d_of_vs [x]
    | ts ->
      let xs = fresh_vars "x" ts in
      let p = tupP (List.map varP xs) in
      (letP p e)::d_of_vs (xs)

  let rec t_typ (t:T.typ) =
    match t with
    | T.Prim _
      | Var _ -> t
    | Con (c, ts) ->
      Con (t_con c, List.map t_typ ts)
    | Array t -> Array (t_typ t)
    | Tup ts -> Tup (List.map t_typ ts)
    | Func (s, c, tbs, t1, t2) ->
      begin
        match s with
        | T.Shared ->
           begin
             match t2 with
             | [] ->
               assert (c = T.Returns);
               Func(s, c, List.map t_bind tbs, List.map t_typ t1, List.map t_typ t2)
             | [Async t2] ->
               assert (c = T.Promises);
               Func (s, T.Returns, List.map t_bind tbs,
                     extendTup (List.map t_typ t1)
                       (select
                          [ add_reply_parameter, lazy (replyT nary (t_typ t2));
                            add_reject_parameter, lazy rejectT ]),
                     [])
             | _ -> assert false
           end
        | T.Local ->
          Func (s, c, List.map t_bind tbs, List.map t_typ t1, List.map t_typ t2)
      end
    | Opt t -> Opt (t_typ t)
    | Variant fs -> Variant (List.map t_field fs)
    | Async t -> t_async nary (t_typ t)
    | Obj (s, fs) -> Obj (s, List.map t_field fs)
    | Mut t -> Mut (t_typ t)
    | Serialized t -> Serialized (t_typ t)
    | Any -> Any
    | Non -> Non
    | Pre -> Pre
    | Typ c -> Typ (t_con c)

  and t_bind {var; bound} =
    {var; bound = t_typ bound}

  and t_binds typbinds = List.map t_bind typbinds

  and t_kind k =
    match k with
    | T.Abs(typ_binds,typ) ->
      T.Abs(t_binds typ_binds, t_typ typ)
    | T.Def(typ_binds,typ) ->
      T.Def(t_binds typ_binds, t_typ typ)

  and t_con c =
    match  ConRenaming.find_opt c (!con_renaming) with
    | Some c' -> c'
    | None ->
      let clone = Con.clone c (Abs ([], Pre)) in
      con_renaming := ConRenaming.add c clone (!con_renaming);
      (* Need to extend con_renaming before traversing the kind *)
      Type.set_kind clone (t_kind (Con.kind c));
      clone

  and prim = function
    | UnPrim (ot, op) -> UnPrim (t_typ ot, op)
    | BinPrim (ot, op) -> BinPrim (t_typ ot, op)
    | RelPrim (ot, op) -> RelPrim (t_typ ot, op)
    | ShowPrim ot -> ShowPrim (t_typ ot)
    | SerializePrim ot -> SerializePrim (t_typ ot)
    | DeserializePrim ot -> DeserializePrim (t_typ ot)
    | NumConvPrim (t1,t2) -> NumConvPrim (t1,t2)
    | OtherPrim s -> OtherPrim s

  and t_field {lab; typ} =
    { lab; typ = t_typ typ }

  let rec t_exp (exp: exp) =
    { it = t_exp' exp;
      note = { note_typ = t_typ exp.note.note_typ;
               note_eff = exp.note.note_eff};
      at = exp.at;
    }
  and t_exp' (exp:exp) =
    let exp' = exp.it in
    match exp' with
    | LitE _ -> exp'
    | VarE id -> exp'
    | TupE exps ->
      TupE (List.map t_exp exps)
    | OptE exp1 ->
      OptE (t_exp exp1)
    | TagE (i, exp1) ->
      TagE (i, t_exp exp1)
    | ProjE (exp1, n) ->
      ProjE (t_exp exp1, n)
    | DotE (exp1, id) ->
      DotE (t_exp exp1, id)
    | ActorDotE (exp1, id) ->
      ActorDotE (t_exp exp1, id)
    | AssignE (exp1, exp2) ->
      AssignE (t_exp exp1, t_exp exp2)
    | ArrayE (mut, t, exps) ->
      ArrayE (mut, t_typ t, List.map t_exp exps)
    | IdxE (exp1, exp2) ->
      IdxE (t_exp exp1, t_exp exp2)
    | PrimE (OtherPrim "@await", [a;kr]) ->
      ((t_exp a) -*- (t_exp kr)).it
    | PrimE (OtherPrim "@async", [exp2]) ->
      let t1, contT = match typ exp2 with
        | Func(_,_,
               [],
               [Func(_, _, [], ts1, []) as contT; _],
               []) -> (* TBR, why isn't this []? *)
          (t_typ (T.seq ts1),t_typ contT)
        | t -> assert false in
      let k = fresh_var "k" contT in
      let v1 = fresh_var "v" t1 in
      let r = fresh_var "r" err_contT in
      let e = fresh_var "e" T.catch in
      let post = fresh_var "post" (T.Func(T.Shared, T.Returns, [], [], [])) in
      let u = fresh_var "u" T.unit in
      let ((nary_async, nary_reply, reject), def) = new_nary_async_reply t1 in
      (blockE [letP (tupP [varP nary_async; varP nary_reply; varP reject]) def;
               funcD k v1 (nary_reply -*- v1);
               nary_funcD r [e] (reject -*- (errorMessageE e));
               funcD post u (t_exp exp2 -*- (tupE [k;r]));
               expD (post -*- tupE[])]
               nary_async
      ).it
    | CallE (cc, exp1, typs, exp2) when isAwaitableFunc exp1 ->
      let ts1,t2 =
        match typ exp1 with
        | T.Func (T.Shared,T.Promises,tbs,ts1,[T.Async t2]) ->
          List.map t_typ ts1, t_typ t2
        | _ -> assert(false)
      in
      let exp1' = t_exp exp1 in
      let exp2' = t_exp exp2 in
      let typs = List.map t_typ typs in
      let ((nary_async, nary_reply, reject), def) = new_nary_async_reply t2 in
      let _ = letEta in
      (blockE ( letP (tupP [varP nary_async; varP nary_reply; varP reject]) def ::
                letEta exp1' (fun v1 ->
                  letSeq ts1 exp2' (fun vs ->
                      [ expD (sys_callE v1 typs vs nary_reply reject) ]
                    )
                  )
         )
         nary_async)
        .it
    | PrimE (p, exps) ->
      PrimE (prim p, List.map t_exp exps)
    | CallE (cc, exp1, typs, exp2)  ->
      assert (not (isAwaitableFunc exp1));
      CallE (cc, t_exp exp1, List.map t_typ typs, t_exp exp2)
    | BlockE b ->
      BlockE (t_block b)
    | IfE (exp1, exp2, exp3) ->
      IfE (t_exp exp1, t_exp exp2, t_exp exp3)
    | SwitchE (exp1, cases) ->
      let cases' = List.map
                     (fun {it = {pat; exp}; at; note} ->
                       {it = {pat = t_pat pat ;exp = t_exp exp}; at; note})
                     cases
      in
      SwitchE (t_exp exp1, cases')
    | LoopE exp1 ->
      LoopE (t_exp exp1)
    | LabelE (id, typ, exp1) ->
      LabelE (id, t_typ typ, t_exp exp1)
    | BreakE (id, exp1) ->
      BreakE (id, t_exp exp1)
    | RetE exp1 ->
      RetE (t_exp exp1)
    | AsyncE _
    | AwaitE _
    | TryE _
    | ThrowE _ -> assert false
    | AssertE exp1 ->
      AssertE (t_exp exp1)
    | DeclareE (id, typ, exp1) ->
      DeclareE (id, t_typ typ, t_exp exp1)
    | DefineE (id, mut ,exp1) ->
      DefineE (id, mut, t_exp exp1)
    | FuncE (x, cc, typbinds, args, typT, exp) ->
      let s = cc.Call_conv.sort in
      begin
        match s with
        | T.Local  ->
          FuncE (x, cc, t_typ_binds typbinds, t_args args, List.map t_typ typT, t_exp exp)
        | T.Shared ->
          begin
            match typ exp with
            | T.Tup [] ->
              FuncE (x, cc, t_typ_binds typbinds, t_args args, List.map t_typ typT, t_exp exp)
            | T.Async res_typ ->
              let cc' = Call_conv.message_cc (cc.Call_conv.n_args + 1) in
              let res_typ = t_typ res_typ in
              let reply_typ = replyT nary res_typ in
              let k = fresh_var "k" reply_typ in
              let r = fresh_var "r" rejectT in
              let args' = t_args args @
                            (select [ add_reply_parameter, lazy (arg_of_exp k);
                                      add_reject_parameter, lazy (arg_of_exp r)])
              in
              let typbinds' = t_typ_binds typbinds in
              let y = fresh_var "y" res_typ in
              let e = fresh_var "e" T.catch in
              let exp' =
                match exp.it with
                | PrimE (OtherPrim "@async", [cps]) ->
                  blockE
                    (select
                       [ (not add_reply_parameter,
                          lazy (
                              let vs = List.map (fresh_var "v") (nary res_typ) in
                              nary_funcD k vs (sys_replyE vs)))
                       ;
                         (not add_reject_parameter,
                          lazy (
                              let msg = fresh_var "msg" T.text in
                              funcD r msg (sys_rejectE msg)))])
                    ((t_exp cps) -*-
                       tupE [(y --> (k -*- y));
                             ([e] -->* (r -*- (errorMessageE e)))]
                    )
                | _ -> assert false
              in
              FuncE (x, cc', typbinds', args', [], exp')
            | _ -> assert false
          end
      end
    | ActorE (id, ds, fs, typ) ->
      ActorE (id, t_decs ds, t_fields fs, t_typ typ)
    | NewObjE (sort, ids, t) ->
      NewObjE (sort, t_fields ids, t_typ t)

  and t_dec dec = { dec with it = t_dec' dec.it }

  and t_dec' dec' =
    match dec' with
    | TypD con_id -> TypD (t_con con_id)
    | LetD (pat,exp) -> LetD (t_pat pat,t_exp exp)
    | VarD (id,exp) -> VarD (id,t_exp exp)

  and t_decs decs = List.map t_dec decs

  and t_block (ds, exp) = (t_decs ds, t_exp exp)

  and t_fields fs =
    List.map (fun f -> { f with note = t_typ f.note }) fs

  and t_args as_ = List.map t_arg as_

  and t_arg a = { a with note = t_typ a.note }

  and t_pat pat =
    { pat with
      it = t_pat' pat.it;
      note = t_typ pat.note }

  and t_pat' pat =
    match pat with
    | WildP
      | LitP _
      | VarP _ ->
      pat
    | TupP pats ->
      TupP (List.map t_pat pats)
    | ObjP pfs ->
      ObjP (map_obj_pat t_pat pfs)
    | OptP pat1 ->
      OptP (t_pat pat1)
    | TagP (i, pat1) ->
      TagP (i, t_pat pat1)
    | AltP (pat1, pat2) ->
      AltP (t_pat pat1, t_pat pat2)

  and t_typ_bind' {con; bound} =
    {con = t_con con; bound = t_typ bound}

  and t_typ_bind typ_bind =
    { typ_bind with it = t_typ_bind' typ_bind.it }

  and t_typ_binds typbinds = List.map t_typ_bind typbinds

  and t_prog (prog, flavor) = (t_block prog, { flavor with has_async_typ = false } )

end

let transform env prog =
  let module T = Transform() in
  (*
  Initialized the con_renaming with those type constructors already in scope.
  Eventually, pipeline will allow us to pass the con_renaming to downstream program
  fragments, then we would simply start with an empty con_renaming and the prelude.
  *)
  Type.ConSet.iter (fun c -> T.con_renaming := T.ConRenaming.add c c (!T.con_renaming)) env.Scope.con_env;
  T.t_prog prog
