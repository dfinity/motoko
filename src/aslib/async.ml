module E = Env
open Source
module Ir = Ir
open Ir
open Effect
module T = Type
open T
open Construct

(* lower the async type itself
   - adds a final callback argument to every awaitable shared function, replace
     the result by unit
   - transforms types, introductions and eliminations awaitable shared
     functions only, leaving non-awaitable shared functions unchanged.
   - ensures every call to an awaitable shared function that takes a tuple has
     a manifest tuple argument.

   (for debugging, the `flattening` function can be used to disable argument flattening and use uniform pairing instead)
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

  let unary typ = [typ]

  let nary typ = T.as_seq typ

  let replyT as_seq typ = T.Func(T.Sharable, T.Returns, [], as_seq typ, [])

  let fulfillT as_seq typ = T.Func(T.Local, T.Returns, [], as_seq typ, [])

  let t_async as_seq t =
    T.Func (T.Local, T.Returns, [], [T.Func(T.Local, T.Returns, [],as_seq t,[])], [])

  let new_async_ret as_seq t = [t_async as_seq t;fulfillT as_seq t]

  let new_asyncT =
    T.Func (
        T.Local,
        T.Returns,
        [ { var = "T"; bound = T.Shared } ],
        [],
        new_async_ret unary (T.Var ("T", 0))
      )

  let new_asyncE =
    idE ("@new_async"@@no_region) new_asyncT

  let new_async t1 =
    let call_new_async = callE new_asyncE [t1] (tupE []) in
    let async = fresh_var "async" (typ (projE call_new_async 0)) in
    let fulfill = fresh_var "fulfill" (typ (projE call_new_async 1)) in
    (async,fulfill),call_new_async

  let new_nary_async_reply t1 =
    let (unary_async,unary_fulfill),call_new_async = new_async t1 in
    let v' = fresh_var "v" t1 in
    let ts1 = T.as_seq t1 in
    (* construct the n-ary async value, coercing the continuation, if necessary *)
    let nary_async =
      let k' = fresh_var "k" (contT t1) in
      match ts1 with
      | [t] ->
        unary_async
      | ts ->
        let seq_of_v' = tupE (List.mapi (fun i _ -> projE v' i) ts) in
        k' --> (unary_async -*- ([v'] -->* (k' -*- seq_of_v')))
    in
    (* construct the n-ary reply message that sends a sequence of value to fulfill the async *)
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
      vs -@>* (unary_fulfill -*-  seq_of_vs)
    in
    let async,reply = fresh_var "async" (typ nary_async), fresh_var "fulfill" (typ nary_reply) in
    (async,reply),blockE [letP (tupP [varP unary_async; varP unary_fulfill])  call_new_async]
                         (tupE [nary_async; nary_reply])


  let letEta e scope =
    match e.it with
    | VarE _ -> scope e (* pure, so reduce *)
    | _  -> let f = fresh_var "x" (typ e) in
            letD f e :: (scope f) (* maybe impure; sequence *)

  let isAwaitableFunc exp =
    match typ exp with
    | T.Func (T.Sharable,T.Promises,_,_,[T.Async _]) -> true
    | _ -> false

  let extendTup ts t2 = ts @ [t2]

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
    | Free c -> Free (t_con c)
    | Con (t, ts) ->
      Con (t_typ t, List.map t_typ ts)
    | Array t -> Array (t_typ t)
    | Tup ts -> Tup (List.map t_typ ts)
    | Func (s, c, tbs, t1, t2) ->
      begin
        match s with
        |  T.Sharable ->
           begin
             match t2 with
             | [] ->
               assert (c = T.Returns);
               Func(s, c, List.map t_bind tbs, List.map t_typ t1, List.map t_typ t2)
             | [Async t2] ->
               assert (c = T.Promises);
               Func (s, T.Returns, List.map t_bind tbs,
                     extendTup (List.map t_typ t1) (replyT nary (t_typ t2)), [])
             | _ -> assert false
           end
        | _ ->
          Func (s, c, List.map t_bind tbs, List.map t_typ t1, List.map t_typ t2)
      end
    | Opt t -> Opt (t_typ t)
    | Variant fs -> Variant (List.map t_field fs)
    | Async t -> t_async nary (t_typ t)
    | Obj (s, fs) -> Obj (s, List.map t_field fs)
    | Mut t -> Mut (t_typ t)
    | Shared -> Shared
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

  and t_operator_type ot =
    (* We recreate the reference here. That is ok, because it
     we run after type inference. Once we move async past desugaring,
     it will be a pure value anyways. *)
    t_typ ot

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
    | PrimE _
      | LitE _ -> exp'
    | VarE id -> exp'
    | UnE (ot, op, exp1) ->
      UnE (t_operator_type ot, op, t_exp exp1)
    | ShowE (ot, exp1) ->
      ShowE (t_operator_type ot, t_exp exp1)
    | BinE (ot, exp1, op, exp2) ->
      BinE (t_operator_type ot, t_exp exp1, op, t_exp exp2)
    | RelE (ot, exp1, op, exp2) ->
      RelE (t_operator_type ot, t_exp exp1, op, t_exp exp2)
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
    | CallE (cc,{it=PrimE "@await";_}, typs, exp2) ->
      begin
        match exp2.it with
        | TupE [a;k] -> ((t_exp a) -*- (t_exp k)).it
        | _ -> assert false
      end
    | CallE (cc,{it=PrimE "@async";_}, typs, exp2) ->
      let t1, contT = match typ exp2 with
        | Func(_,_,
               [],
               [Func(_,_,[],ts1,[]) as contT],
               []) -> (* TBR, why isn't this []? *)
          (t_typ (T.seq ts1),t_typ contT)
        | t -> assert false in
      let k = fresh_var "k" contT in
      let v1 = fresh_var "v" t1 in
      let post = fresh_var "post" (T.Func(T.Sharable,T.Returns,[],[],[])) in
      let u = fresh_var "u" T.unit in
      let ((nary_async,nary_reply),def) = new_nary_async_reply t1 in
      (blockE [letP (tupP [varP nary_async; varP nary_reply]) def;
               funcD k v1 (nary_reply -*- v1);
               funcD post u (t_exp exp2 -*- k);
               expD (post -*- tupE[])]
               nary_async
      ).it
    | CallE (cc,exp1, typs, exp2) when isAwaitableFunc exp1 ->
      let ts1,t2 =
        match typ exp1 with
        | T.Func (T.Sharable,T.Promises,tbs,ts1,[T.Async t2]) ->
          List.map t_typ ts1, t_typ t2
        | _ -> assert(false)
      in
      let exp1' = t_exp exp1 in
      let exp2' = t_exp exp2 in
      let typs = List.map t_typ typs in
      let ((nary_async,nary_reply),def) = new_nary_async_reply t2 in
      let _ = letEta in
      (blockE ( letP (tupP [varP nary_async; varP nary_reply]) def ::
                letEta exp1' (fun v1 ->
                  letSeq ts1 exp2' (fun vs ->
                    [ expD (callE v1 typs (seqE (vs@[nary_reply]))) ]
                  )
                 )
               )
               nary_async)
        .it
    | CallE (cc, exp1, typs, exp2)  ->
      CallE(cc, t_exp exp1, List.map t_typ typs, t_exp exp2)
    | BlockE b ->
      BlockE (t_block b)
    | IfE (exp1, exp2, exp3) ->
      IfE (t_exp exp1, t_exp exp2, t_exp exp3)
    | SwitchE (exp1, cases) ->
      let cases' = List.map
                     (fun {it = {pat;exp}; at; note} ->
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
    | AsyncE _ -> assert false
    | AwaitE _ -> assert false
    | AssertE exp1 ->
      AssertE (t_exp exp1)
    | DeclareE (id, typ, exp1) ->
      DeclareE (id, t_typ typ, t_exp exp1)
    | DefineE (id, mut ,exp1) ->
      DefineE (id, mut, t_exp exp1)
    | FuncE (x, cc, typbinds, args, typT, exp) ->
      let s = cc.Value.sort in
      begin
        match s with
        | T.Local  ->
          FuncE (x, cc, t_typ_binds typbinds, t_args args, List.map t_typ typT, t_exp exp)
        | T.Sharable ->
          begin
            match typ exp with
            | T.Tup [] ->
              FuncE (x, cc, t_typ_binds typbinds, t_args args, List.map t_typ typT, t_exp exp)
            | T.Async res_typ ->
              let cc' = Value.message_cc (cc.Value.n_args + 1) in
              let res_typ = t_typ res_typ in
              let reply_typ = replyT nary res_typ in
              let k = fresh_var "k" reply_typ in
              let args' = t_args args @ [ arg_of_exp k ] in
              let typbinds' = t_typ_binds typbinds in
              let y = fresh_var "y" res_typ in
              let exp' =
                match exp.it with
                | CallE(_, async,_,cps) ->
                  begin
                    match async.it with
                    | PrimE("@async") -> ((t_exp cps) -*- (y --> (k -*- y)))
                    | _ -> assert false
                  end
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
  Type.ConSet.iter (fun c -> T.con_renaming := T.ConRenaming.add c c (!T.con_renaming)) env.Typing.con_env;
  T.t_prog prog
