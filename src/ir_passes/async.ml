open Mo_types
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

module ConRenaming = E.Make(struct type t = T.con let compare = Cons.compare end)

(* Helpers *)

let selfcallE ts e1 e2 e3 =
  { it = SelfCallE (ts, e1, e2, e3);
    at = no_region;
    note = Note.{ def with typ = T.unit } }

let error_ty =
  T.(Tup [ Variant [
    {lab = "error"; typ = unit; depr = None};
    {lab = "system"; typ = unit; depr = None}
  ]; text])

let errorMessageE e =
  projE (primE (CastPrim (T.error, error_ty)) [e]) 1

let unary typ = [typ]

let nary typ = T.as_seq typ

let fulfillT as_seq typ = T.Func(T.Local, T.Returns, [], as_seq typ, [])

let failT = T.Func(T.Local, T.Returns, [], [T.catch], [])

let t_async as_seq t =
  T.Func (T.Local, T.Returns, [], [fulfillT as_seq t; failT],
     [T.Opt (T.Func(T.Local, T.Returns, [], [], []))])

let new_async_ret as_seq t = [t_async as_seq t; fulfillT as_seq t; failT]

let new_asyncT =
  T.Func (
    T.Local,
    T.Returns,
    [ { var = "T"; sort = T.Type; bound = T.Any } ],
    [],
    new_async_ret unary (T.Var ("T", 0)))

let new_asyncE () =
  varE (var "@new_async" new_asyncT)

let new_async t1 =
  let call_new_async = callE (new_asyncE ()) [t1] (tupE []) in
  let async = fresh_var "async" (typ (projE call_new_async 0)) in
  let fulfill = fresh_var "fulfill" (typ (projE call_new_async 1)) in
  let fail = fresh_var "fail" (typ (projE call_new_async 2)) in
  (async, fulfill, fail), call_new_async

let new_nary_async_reply mode ts1 =
  (* The async implementation isn't n-ary *)
  let t1 = T.seq ts1 in
  let (unary_async, unary_fulfill, fail), call_new_async = new_async t1 in
  let v' = fresh_var "v" t1 in
  (* construct the n-ary async value, coercing the continuation, if necessary *)
  let nary_async =
    match ts1 with
    | [t] ->
      varE unary_async
    | ts ->
      let k' = fresh_var "k" (contT t1) in
      let r' = fresh_var "r" err_contT in
      let seq_of_v' = tupE (List.mapi (fun i _ -> projE (varE v') i) ts) in
      [k';r'] -->* (
        varE unary_async -*- (tupE[([v'] -->* (varE k' -*- seq_of_v')); varE r'])
      )
  in
  (* construct the n-ary reply callback that sends a sequence of values to fulfill the async *)
  let nary_reply =
    let vs,seq_of_vs =
      match ts1 with
      | [t] ->
        let v = fresh_var "rep" t in
        [v], varE v
      | ts ->
        let vs = fresh_vars "rep" ts in
        vs, tupE (List.map varE vs)
    in
    vs -->* (varE unary_fulfill -*- seq_of_vs)
  in
  let async,reply,reject =
    fresh_var "async" (typ nary_async),
    fresh_var "reply" (typ nary_reply),
    fresh_var "reject" (typ_of_var fail)
  in
    (async, reply, reject),
      blockE [letP (tupP [varP unary_async; varP unary_fulfill; varP fail])  call_new_async]
        (tupE [nary_async; nary_reply; varE fail])

let letEta e scope =
  match e.it with
  | VarE _ -> scope e (* pure, so reduce *)
  | _  -> let f = fresh_var "x" (typ e) in
          letD f e :: (scope (varE f)) (* maybe impure; sequence *)

let isAwaitableFunc exp =
  match typ exp with
  | T.Func (T.Shared _, T.Promises, _, _, _) -> true
  | _ -> false

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

(* name e in f unless named already *)
let ensureNamed e f =
  match e.it with
  | VarE v -> f (var v (typ e))
  | _ ->
    let v = fresh_var "v" (typ e) in
    blockE [letD v e] (f v)

(* The actual transformation *)

let transform mode prog =

  (* the state *)
  let con_renaming = ref ConRenaming.empty

  (* maps constructors to new constructors (name name, new stamp, new kind)
     it is initialized with the type constructors defined outside here, which are
     not rewritten.

     If we run this translation on two program fragments (e.g. prelude and program)
     we would have to pass down the `con_renaming`. But this is simply the right thing
     to do for a pass that changes the context.

     Eventually, pipeline will allow us to pass the con_renaming to downstream program
     fragments, then we would simply start with an empty con_renaming and the prelude.
  *)
  in

  let rec t_typ (t:T.typ) =
    match t with
    | T.Prim _
      | Var _ -> t
    | Con (c, ts) ->
      Con (t_con c, List.map t_typ ts)
    | Array t -> Array (t_typ t)
    | Tup ts -> Tup (List.map t_typ ts)
    | Func (s, c, tbs, ts1, ts2) ->
      let c' = match c with T.Promises -> T.Replies | _ -> c in
      Func (s, c', List.map t_bind tbs, List.map t_typ ts1, List.map t_typ ts2)
    | Opt t -> Opt (t_typ t)
    | Variant fs -> Variant (List.map t_field fs)
    | Async (_, t) -> t_async nary (t_typ t) (* TBR exploit the index _ *)
    | Obj (s, fs) -> Obj (s, List.map t_field fs)
    | Mut t -> Mut (t_typ t)
    | Any -> Any
    | Non -> Non
    | Pre -> Pre
    | Typ c -> Typ (t_con c)

  and t_bind tb =
    { tb with bound = t_typ tb.bound }

  and t_binds typbinds = List.map t_bind typbinds

  and t_kind k =
    match k with
    | T.Abs (typ_binds,typ) ->
      T.Abs (t_binds typ_binds, t_typ typ)
    | T.Def (typ_binds,typ) ->
      T.Def (t_binds typ_binds, t_typ typ)

  and t_con c =
    match Cons.kind c with
    | T.Def ([], T.Prim _) -> c
    | _ ->
      match  ConRenaming.find_opt c (!con_renaming) with
      | Some c' -> c'
      | None ->
        let clone = Cons.clone c (Abs ([], Pre)) in
        con_renaming := ConRenaming.add c clone (!con_renaming);
        (* Need to extend con_renaming before traversing the kind *)
        Type.set_kind clone (t_kind (Cons.kind c));
        clone

  and t_prim p = Ir.map_prim t_typ (fun id -> id) p

  and t_field {lab; typ; depr} =
    { lab; typ = t_typ typ; depr }
  in

  let rec t_exp (exp: exp) =
    { it = t_exp' exp;
      note = Note.{ def with
        typ = t_typ exp.note.typ;
        eff = exp.note.eff
      };
      at = exp.at;
    }
  and t_exp' (exp:exp) =
    let exp' = exp.it in
    match exp' with
    | LitE _ -> exp'
    | VarE id -> exp'
    | AssignE (exp1, exp2) ->
      AssignE (t_lexp exp1, t_exp exp2)
    | PrimE (CPSAwait _, [a; kr]) ->
      (ensureNamed (t_exp kr) (fun vkr ->
         let resume = fresh_var "resume" (T.Func(T.Local, T.Returns, [], [], [])) in
         (switch_optE ((t_exp a) -*- varE vkr)
            (unitE()) (* suspend *)
            (varP resume) (* yield and resume *)
              (* try await async (); resume() catch e -> r(e) *)
              (selfcallE [] (ic_replyE [] (unitE())) (varE resume) (projE (varE vkr) 1))
         T.unit
         ))).it
    | PrimE (CPSAsync t0, [exp1]) ->
      let t0 = t_typ t0 in
      let tb, ts1 = match typ exp1 with
        | Func(_,_, [tb], [Func(_, _, [], ts1, []); _], []) ->
          tb, List.map t_typ (List.map (T.open_ [t0]) ts1)
        | t -> assert false in
      let ((nary_async, nary_reply, reject), def) = new_nary_async_reply mode ts1 in
      (blockE [
        letP (tupP [varP nary_async; varP nary_reply; varP reject]) def;
        let ic_reply = (* flatten v, here and below? *)
          let v = fresh_var "v" (T.seq ts1) in
          v --> (ic_replyE ts1 (varE v)) in
        let ic_reject =
          let e = fresh_var "e" T.catch in
          [e] -->* (ic_rejectE (errorMessageE (varE e))) in
        let exp' = callE (t_exp exp1) [t0] (tupE [ic_reply; ic_reject]) in
        expD (selfcallE ts1 exp' (varE nary_reply) (varE reject))
        ]
        (varE nary_async)
      ).it
    | PrimE (CallPrim typs, [exp1; exp2]) when isAwaitableFunc exp1 ->
      let ts1,ts2 =
        match typ exp1 with
        | T.Func (T.Shared _, T.Promises, tbs, ts1, ts2) ->
          List.map (fun t -> t_typ (T.open_ typs t)) ts1,
          List.map (fun t -> t_typ (T.open_ typs t)) ts2
        | _ -> assert(false)
      in
      let exp1' = t_exp exp1 in
      let exp2' = t_exp exp2 in
      let ((nary_async, nary_reply, reject), def) = new_nary_async_reply mode ts2 in
      let _ = letEta in
      (blockE (
        letP (tupP [varP nary_async; varP nary_reply; varP reject]) def ::
        letEta exp1' (fun v1 ->
          letSeq ts1 exp2' (fun vs ->
            [ expD (ic_callE v1 (seqE (List.map varE vs)) (varE nary_reply) (varE reject)) ]
            )
          )
         )
         (varE nary_async))
        .it
    | PrimE (p, exps) ->
      PrimE (t_prim p, List.map t_exp exps)
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
    | AsyncE _
    | TryE _ -> assert false
    | DeclareE (id, typ, exp1) ->
      DeclareE (id, t_typ typ, t_exp exp1)
    | DefineE (id, mut ,exp1) ->
      DefineE (id, mut, t_exp exp1)
    | FuncE (x, s, c, typbinds, args, ret_tys, exp) ->
      begin
        match s with
        | T.Local  ->
          FuncE (x, s, c, t_typ_binds typbinds, t_args args, List.map t_typ ret_tys, t_exp exp)
        | T.Shared s' ->
          begin
            match c, exp with
            | Promises, exp ->
              let ret_tys = List.map t_typ ret_tys in
              let args' = t_args args in
              let typbinds' = t_typ_binds typbinds in
              let t0, cps = match exp.it with
                | PrimE (CPSAsync t0, [cps]) -> t_typ t0, cps
                | _ -> assert false in
              let t1, contT = match typ cps with
                | Func (_,_,
                    [tb],
                    [Func(_, _, [], ts1, []) as contT; _],
                    []) ->
                  (t_typ (T.seq (List.map (T.open_ [t0]) ts1)),t_typ (T.open_ [t0] contT))
                | t -> assert false in
              let k =
                let v = fresh_var "v" t1 in
                v --> (ic_replyE ret_tys (varE v)) in
              let r =
                let e = fresh_var "e" T.catch in
                [e] -->* (ic_rejectE (errorMessageE (varE e))) in
              let exp' = callE (t_exp cps) [t0] (tupE [k;r]) in
              FuncE (x, T.Shared s', Replies, typbinds', args', ret_tys, exp')
            (* oneway, always with `ignore(async _)` body *)
            | Returns,
              { it = BlockE (
                  [ { it = LetD (
                      { it = WildP; _},
                      ({ it = PrimE (CPSAsync _, _); _} as exp)); _ }],
                  { it = PrimE (TupPrim, []); _ } );
                _ } ->
              let ret_tys = List.map t_typ ret_tys in
              let args' = t_args args in
              let typbinds' = t_typ_binds typbinds in
              let t0, cps = match exp.it with
                | PrimE (CPSAsync t0, [cps]) -> t_typ t0, cps
                | _ -> assert false in
              let t1, contT = match typ cps with
                | Func (_, _,
                    [tb],
                    [Func(_, _, [], ts1, []) as contT; _],
                    []) ->
                  (t_typ (T.seq (List.map (T.open_ [t0]) ts1)),t_typ (T.open_ [t0] contT))
                | t -> assert false in
              let k =
                let v = fresh_var "v" t1 in
                v --> tupE [] in (* discard return *)
              let r =
                let e = fresh_var "e" T.catch in
                [e] -->* tupE [] in (* discard error *)
              let exp' = callE (t_exp cps) [t0] (tupE [k;r]) in
              FuncE (x, T.Shared s', Returns, typbinds', args', ret_tys, exp')
            | Returns, _ ->
              assert false
            | Replies,_ -> assert false
          end
      end
    | ActorE (ds, fs, {meta; preupgrade; postupgrade; heartbeat}, typ) ->
      ActorE (t_decs ds, t_fields fs, {meta; preupgrade = t_exp preupgrade; postupgrade = t_exp postupgrade; heartbeat = t_exp heartbeat}, t_typ typ)
    | NewObjE (sort, ids, t) ->
      NewObjE (sort, t_fields ids, t_typ t)
    | SelfCallE _ -> assert false

  and t_lexp lexp =
    { it = t_lexp' lexp.it;
      note = t_typ lexp.note;
      at = lexp.at;
    }
  and t_lexp' (lexp':lexp') =
    match lexp' with
    | VarLE _ -> lexp'
    | DotLE (exp1, id) ->
      DotLE (t_exp exp1, id)
    | IdxLE (exp1, exp2) ->
      IdxLE (t_exp exp1, t_exp exp2)

  and t_dec dec = { dec with it = t_dec' dec.it }

  and t_dec' dec' =
    match dec' with
    | LetD (pat,exp) -> LetD (t_pat pat,t_exp exp)
    | VarD (id, t, exp) -> VarD (id, t_typ t, t_exp exp)

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

  and t_typ_bind' tb =
    { tb with con = t_con tb.con; bound = t_typ tb.bound }

  and t_typ_bind typ_bind =
    { typ_bind with it = t_typ_bind' typ_bind.it }

  and t_typ_binds typbinds = List.map t_typ_bind typbinds

  and t_comp_unit = function
    | LibU _ -> raise (Invalid_argument "cannot compile library")
    | ProgU ds -> ProgU (t_decs ds)
    | ActorU (args_opt, ds, fs, {meta; preupgrade; postupgrade; heartbeat}, t) ->
      ActorU (Option.map t_args args_opt, t_decs ds, t_fields fs,
        { meta; preupgrade = t_exp preupgrade; postupgrade = t_exp postupgrade; heartbeat = t_exp heartbeat }, t_typ t)

  and t_prog (cu, flavor) = (t_comp_unit cu, { flavor with has_async_typ = false } )
in
  t_prog prog
