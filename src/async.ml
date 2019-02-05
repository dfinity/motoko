open Syntax
open Source
open Effect
module T = Type
open T
open Syntaxops

(* lower the async type itself
   - adds a final callback argument to every awaitable shared function, replace the result by unit
   - transforms types, introductions and eliminations awaitable shared functions only, leaving non-awaitable shared functions unchanged.
   - ensures every call to an awaitable shared function that takes a tuple has a manifest tuple argument.

   (for debugging, the `flattening` function can be used to disable argument flattening and use uniform pairing instead)
 *)

let localS =
  {it=T.Call T.Local;
   at=no_region;
   note=()}

let sharableS =
  {it=T.Call T.Sharable;
   at=no_region;
   note=()}

let unary typ = [typ]

let nary typ = T.as_seq typ

let replyT as_seq typ = T.Func(T.Call T.Sharable, T.Returns, [], as_seq typ, [])

let fullfillT as_seq typ = T.Func(T.Call T.Local, T.Returns, [], as_seq typ, [])

let tupT ts = {it = TupT ts;
               at = no_region;
               note = T.Tup (List.map (fun t -> t.note) ts)}

let unitT = tupT []

let funcT(s,bds,t1,t2) =
  {it = FuncT (s, bds, t1, t2);
   at = no_region;
   note = T.Pre} (* TBR: try harder *)

let t_async as_seq t =
  T.Func (T.Call T.Local, T.Returns, [], [T.Func(T.Call T.Local, T.Returns, [],as_seq t,[])], [])

let new_async_ret as_seq t = [t_async as_seq t;fullfillT as_seq t]
let new_asyncT =
   T.Func(T.Call T.Local,T.Returns,
                          [{var = "T";
                            bound = T.Shared}],
                          [],
                          new_async_ret unary (T.Var ("T", 0)))

let new_asyncE =
  idE ("@new_async"@@no_region) new_asyncT

let bogusT t=
  { it = PrimT "BogusT" (* bogus,  but we shouln't use it anymore *);
    at = no_region;
    note = t;
  }

let new_async t1 =
  let call_new_async =
    callE new_asyncE
      [bogusT t1]
      (tupE[])
      (T.seq (new_async_ret unary t1)) in
  let async  = fresh_id (typ (projE call_new_async 0)) in
  let fullfill = fresh_id (typ (projE call_new_async 1)) in
  (async,fullfill),call_new_async

let letP p e =  {it = LetD(p,e);
                 at = no_region;
                 note = {e.note with note_typ = T.unit}}

let new_nary_async_reply t1 =
  let (unary_async,unary_fullfill),call_new_async = new_async t1 in
  let v' = fresh_id t1 in
  let ts1 = T.as_seq t1 in
  (* construct the n-ary async value, coercing the continuation, if necessary *)
  let nary_async =
    let k' = fresh_id (contT t1) in
    match ts1 with
    | [t] ->
      unary_async
    | ts ->
      let seq_of_v' = tupE (List.mapi (fun i _ -> projE v' i) ts) in
      k' --> (unary_async -*- ([v'] -->* (k' -*- seq_of_v')))
  in
  (* construct the n-ary reply message that sends a sequence of value to fullfill the async *)
  let nary_reply =
    let vs,seq_of_vs =
      match ts1 with
      | [t] ->
        let v = fresh_id t in
        [v],v
      | ts ->
        let vs = List.map fresh_id ts in
        vs, tupE vs
    in
    vs -@>* (unary_fullfill -*-  seq_of_vs)
  in
  let async,reply = fresh_id (typ nary_async), fresh_id (typ nary_reply) in
  (async,reply),blockE [letP (tupP [varP unary_async;varP unary_fullfill])  call_new_async;
                    expD (tupE [nary_async;nary_reply])]


let replyTT t = funcT(sharableS,[],t,unitT)

let letEta e scope =
  match e.it with
  | VarE _ -> scope e (* pure, so reduce *)
  | _  -> let f = fresh_id (typ e) in
          letD f e :: (scope f) (* maybe impure; sequence *)


let isAwaitableFunc exp =
  match typ exp with
  | T.Func (T.Call T.Sharable,T.Promises,_,_,[T.Async _]) -> true
  | _ -> false

let extendTup ts t2 = ts @ [t2]


let extendTupP p1 p2 =
  match p1.it with
  | TupP ps ->
    begin
      match ps with
      | [] -> p2, fun d -> (letP p1 (tupE [])::d)
      | ps ->
           tupP (ps@[p2]), fun d -> d
    end
  | _ -> tupP [p1;p2], fun d -> d

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
      let x = fresh_id t in
      let p = varP x in
      (letP p e)::d_of_vs [x]
  | ts ->
      let xs = List.map fresh_id ts in
      let p = tupP (List.map varP xs) in
      (letP p e)::d_of_vs (xs)

let rec t_typ (t:T.typ) =
  match t with
  | T.Prim _
  | Var _ -> t
  | Con (c, ts) ->
    Con (c, List.map t_typ ts)
  | Array t -> Array (t_typ t)
  | Tup ts -> Tup (List.map t_typ ts)
  | Func (s, c, tbs, t1, t2) ->
     begin
       match s with
       |  T.Call T.Sharable ->
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
  | Async t -> t_async nary (t_typ t)
  | Obj (s, fs) -> Obj (s, List.map t_field  fs)
  | Mut t -> Mut (t_typ t)
  | Class -> Class
  | Shared -> Shared
  | Any -> Any
  | Non -> Non
  | Pre -> Pre

and t_bind {var; bound} =
  {var; bound = t_typ bound}

and t_kind k = match k with
  | T.Abs(tbs,t) ->
    T.Abs(List.map t_bind tbs, t_typ t)
  | T.Def(tbs,t) ->
    T.Def(List.map t_bind tbs, t_typ t)

and t_operator_type ot =
  (* We recreate the reference here. That is ok, because it
     we run after type inference. Once we move async past desugaring,
     it will be a pure value anyways. *)
  ref (t_typ !ot)

and t_con_id con_id =
  { it = con_id.it;
    at = con_id.at;
    note = Lib.Option.map (fun (c,k) -> (c,t_kind k)) con_id.note
  }

and t_field {name; typ} =
  {name; typ = t_typ typ}
let rec t_exp (exp:Syntax.exp) =
  { it = t_exp' exp;
    note = { note_typ = t_typ exp.note.note_typ;
             note_eff = exp.note.note_eff};
    at = exp.at;
  }
and t_exp' (exp:Syntax.exp) =
  let exp' = exp.it in
  match exp' with
  | PrimE _
  | LitE _ -> exp'
  | VarE id -> exp'
  | UnE (ot, op, exp1) ->
    UnE (t_operator_type ot, op, t_exp exp1)
  | BinE (ot, exp1, op, exp2) ->
    BinE (t_operator_type ot, t_exp exp1, op, t_exp exp2)
  | RelE (ot, exp1, op, exp2) ->
    RelE (t_operator_type ot, t_exp exp1, op, t_exp exp2)
  | TupE exps ->
    TupE (List.map t_exp exps)
  | OptE exp1 ->
    OptE (t_exp exp1)
  | ProjE (exp1, n) ->
    ProjE (t_exp exp1, n)
  | ObjE (sort, id, fields) ->
    let fields' = t_fields fields in
    ObjE (sort, id, fields')
  | DotE (exp1, sr, id) ->
    DotE (t_exp exp1, ref !sr, id)
  | AssignE (exp1, exp2) ->
    AssignE (t_exp exp1, t_exp exp2)
  | ArrayE (mut, exps) ->
    ArrayE (mut, List.map t_exp exps)
  | IdxE (exp1, exp2) ->
    IdxE (t_exp exp1, t_exp exp2)
  | CallE ({it=PrimE "@await";_}, typs, exp2) ->
    begin
     match exp2.it with
     | TupE [a;k] -> ((t_exp a) -*- (t_exp k)).it
     | _ -> assert false
    end
  | CallE ({it=PrimE "@async";_}, typs, exp2) ->
    let t1, contT = match typ exp2 with
      | Func(_,_,
             [],
             [Func(_,_,[],ts1,[]) as contT],
             []) -> (* TBR, why isn't this []? *)
        (t_typ (T.seq ts1),t_typ contT)
      | t -> assert false in
    let k = fresh_id contT in
    let v1 = fresh_id t1 in
    let post = fresh_id (T.Func(T.Call T.Sharable,T.Returns,[],[],[])) in
    let u = fresh_id T.unit in
    let ((nary_async,nary_reply),def) = new_nary_async_reply t1 in
    (blockE [letP (tupP [varP nary_async; varP nary_reply]) def;
             funcD k v1 (nary_reply -*- v1);
             funcD post u (t_exp exp2 -*- k);
             expD (post -*- tupE[]);
             expD nary_async])
      .it
  | CallE (exp1, typs, exp2) when isAwaitableFunc exp1 ->
    let ts1,t2 =
      match typ exp1 with
      | T.Func (T.Call T.Sharable,T.Promises,tbs,ts1,[T.Async t2]) ->
           List.map t_typ ts1, t_typ t2
      | _ -> assert(false)
    in
    let exp1' = t_exp exp1 in
    let exp2' = t_exp exp2 in
    let typs = List.map t_typT typs in
    let ((nary_async,nary_reply),def) = new_nary_async_reply t2 in
    let _ = letEta in
    (blockE (letP (tupP [varP nary_async; varP nary_reply]) def::
               letEta exp1' (fun v1 ->
                   letSeq ts1 exp2' (fun vs ->
                       [expD (callE v1 typs (seqE (vs@[nary_reply])) T.unit);
                        expD nary_async]))))
      .it
  | CallE (exp1, typs, exp2)  ->
    CallE(t_exp exp1, List.map t_typT typs, t_exp exp2)
  | BlockE (decs, ot) ->
    BlockE (t_decs decs, ref (t_typ (!ot)))
  | NotE exp1 ->
    NotE (t_exp exp1)
  | AndE (exp1, exp2) ->
    AndE (t_exp exp1, t_exp exp2)
  | OrE (exp1, exp2) ->
    OrE (t_exp exp1, t_exp exp2)
  | IfE (exp1, exp2, exp3) ->
    IfE (t_exp exp1, t_exp exp2, t_exp exp3)
  | SwitchE (exp1, cases) ->
    let cases' = List.map
                  (fun {it = {pat;exp}; at; note} ->
                    {it = {pat = t_pat pat ;exp = t_exp exp}; at; note})
                  cases
    in
    SwitchE (t_exp exp1, cases')
  | WhileE (exp1, exp2) ->
    WhileE (t_exp exp1, t_exp exp2)
  | LoopE (exp1, exp2_opt) ->
    LoopE (t_exp exp1, Lib.Option.map t_exp exp2_opt)
  | ForE (pat, exp1, exp2) ->
    ForE (t_pat pat, t_exp exp1, t_exp exp2)
  | LabelE (id, _typ, exp1) ->
    LabelE (id, t_typT _typ, t_exp exp1)
  | BreakE (id, exp1) ->
    BreakE (id, t_exp exp1)
  | RetE exp1 ->
    RetE (t_exp exp1)
  | AsyncE _ -> assert false
  | AwaitE _ -> assert false
  | AssertE exp1 ->
    AssertE (t_exp exp1)
  | AnnotE (exp1, typ) ->
    AnnotE (t_exp exp1, t_typT typ)
  | DecE (dec, ot) ->
    DecE (t_dec dec, ref (t_typ (!ot)))
  | DeclareE (id, typ, exp1) ->
    DeclareE (id, t_typ typ, t_exp exp1)
  | DefineE (id, mut ,exp1) ->
    DefineE (id, mut, t_exp exp1)
  | NewObjE (sort, ids) -> exp'

and t_dec dec =
  { it = t_dec' dec.it;
    note = { note_typ = t_typ dec.note.note_typ;
             note_eff = dec.note.note_eff};
    at = dec.at
  }

and t_dec' dec' =
  match dec' with
  | ExpD exp -> ExpD (t_exp exp)
  | TypD (con_id, typbinds, typ) ->
    TypD (t_con_id con_id, t_typbinds typbinds, t_typT typ)
  | LetD (pat,exp) -> LetD (t_pat pat,t_exp exp)
  | VarD (id,exp) -> VarD (id,t_exp exp)
  | FuncD (s, id, typbinds, pat, typT, exp) ->
    begin
      match s.it with
      | T.Local ->
         FuncD (s, id, t_typbinds typbinds, t_pat pat, t_typT typT, t_exp exp)
      | T.Sharable ->
         begin
           match typ exp with
           | T.Tup [] ->
              FuncD (s, id, t_typbinds typbinds, t_pat pat, t_typT typT, t_exp exp)
           | T.Async res_typ ->
              let res_typ = t_typ res_typ in
              let pat = t_pat pat in
              let reply_typ = replyT nary res_typ in
              let typT' = tupT []  in
              let k = fresh_id reply_typ in
              let pat',d = extendTupP pat (varP k) in
              let typbinds' = t_typbinds typbinds in
              let x = fresh_id res_typ in
              let exp' =
                match exp.it with
                | CallE(async,_,cps) ->
                   begin
                     match async.it with
                     | PrimE("@async") ->
                        blockE
                          (d [expD ((t_exp cps) -*- (x --> (k -*- x)))])
                     | _ -> assert false
                   end
                | _ -> assert false
              in
              FuncD (s, id, typbinds', pat', typT', exp')
          | _ -> assert false
         end
    end
  | ClassD (id, con_id, typbinds, sort, pat, id', fields) ->
    let fields' = t_fields fields in
    ClassD (id, t_con_id con_id, t_typbinds typbinds, sort, t_pat pat, id', fields')

and t_decs decs = List.map t_dec decs

and t_fields fields =
  List.map (fun (field:exp_field) ->
      { field with it = { field.it with exp = t_exp field.it.exp }})
    fields

and t_pat pat =
  { pat with it = t_pat' pat.it;
             note = t_typ pat.note}

and t_pat' pat =
  match pat with
  | WildP
  | LitP _
  | SignP _
  | VarP _ ->
     pat
  | TupP pats ->
    TupP (List.map t_pat pats)
  | OptP pat1 ->
    OptP (t_pat pat1)
  | AltP (pat1, pat2) ->
    AltP (t_pat pat1, t_pat pat2)
  | AnnotP (pat1, _typ) ->
    AnnotP (t_pat pat1, t_typT _typ)

(* translate syntactic types *)

and t_asyncT t =
  FuncT (localS,
         [],
         funcT(localS,[],t,unitT),
         unitT)

and t_typT t =
  { it = t_typT' t.it;
    at  = t.at;
    note = t_typ t.note;
  }

and t_typT' t =
  match t with
  | VarT (s, ts) ->
     VarT (s,List.map t_typT ts)
  | PrimT p ->
     PrimT p
  | ObjT (s, ts) ->
     ObjT (s, List.map t_typ_fieldT ts)
  | ArrayT (m, t) ->
     ArrayT(m, t_typT t)
  | OptT t ->
     OptT (t_typT t)
  | TupT ts ->
     TupT (List.map t_typT ts)
  | FuncT (s, tbs, t1, t2) ->
     begin
       match s.it with
       |  T.Call T.Sharable ->
         begin
           match t2.it with
           | TupT [] ->
              FuncT (s, t_typbinds tbs, t_typT t1, t_typT t2)
           | AsyncT t2 ->
              FuncT (localS, t_typbinds tbs,
                     tupT [t_typT t1; replyTT (t_typT t2)], unitT)
           | _ -> assert false
         end
       | _ ->
          FuncT (s, t_typbinds tbs, t_typT t1, t_typT t2)
     end
  | AsyncT t ->
     t_asyncT (t_typT t)
  | ParT t ->
     ParT (t_typT t)
and t_typ_fieldT fld =
   { fld with it = {fld.it with typ = t_typT fld.it.typ}}
and t_typ_bindT bnd =
   { bnd with it = {bnd.it with Syntax.bound = t_typT bnd.it.Syntax.bound}}

and t_typbinds typbinds = List.map t_typ_bindT typbinds
and t_prog prog:prog = {prog with it = t_decs prog.it}
