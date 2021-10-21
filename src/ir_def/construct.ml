
(* WIP translation of syntaxops to use IR in place of Source *)
open Source
open Ir
open Ir_effect
open Mo_values
module Con = Mo_types.Con
module T = Mo_types.Type

(* Field names *)

let nameN s = s

let nextN = "next"

(* Identifiers *)

type var = (string * T.typ)

let var id typ = (id, typ)

let id_of_var (id, _) = id
let typ_of_var (_, typ) = typ

(* Fresh id generation *)

module Stamps = Map.Make(String)
let id_stamps = ref Stamps.empty

let fresh name_base () : string =
  let n = Lib.Option.get (Stamps.find_opt name_base !id_stamps) 0 in
  id_stamps := Stamps.add name_base (n + 1) !id_stamps;
  Printf.sprintf "$%s/%i" name_base n

let fresh_id name_base () : id =
  fresh name_base ()

let fresh_var name_base typ : var =
  let name = fresh name_base () in
  var name typ

let fresh_vars name_base ts =
  List.mapi (fun i t -> fresh_var (Printf.sprintf "%s%i" name_base i) t) ts

(* type arguments *)

let typ_arg c sort typ =
  { it = { Ir.con = c; Ir.sort = sort; Ir.bound = typ };
    at = no_region;
    note = () }

(* Patterns *)

let varP (n, typ) =
  { it = VarP n;
    at = no_region;
    note = typ
  }

let tupP pats =
  { it = TupP pats;
    note = T.Tup (List.map (fun p -> p.note) pats);
    at = no_region }

let seqP ps =
  match ps with
  | [p] -> p
  | ps -> tupP ps

let wildP =
  { it = WildP;
    at = no_region;
    note = T.Any
  }

(* Primitives *)

let varE (id, typ) =
  { it = VarE id; at = no_region; note = Note.{ def with typ = T.as_immut typ } }

let varLE (id, typ) =
  { it = VarLE id; at = no_region; note = typ }

let primE prim es =
  let typ = match prim with
    | ShowPrim _ -> T.text
    | ICReplyPrim _ -> T.Non
    | ICRejectPrim -> T.Non
    | ICCallerPrim -> T.caller
    | ICStableRead t -> t
    | ICStableWrite _ -> T.unit
    | IdxPrim -> T.(as_immut (as_array (List.hd es).note.Note.typ))
    | IcUrlOfBlob -> T.text
    | ActorOfIdBlob t -> t
    | BinPrim (t, _) -> t
    | CastPrim (t1, t2) -> t2
    | RelPrim _ -> T.bool
    | SerializePrim _ -> T.blob
    | SystemCyclesAvailablePrim -> T.nat64
    | SystemCyclesAcceptPrim -> T.nat64
    | _ -> assert false (* implement more as needed *)
  in
  let effs = List.map eff es in
  let eff = List.fold_left max_eff T.Triv effs in
  { it = PrimE (prim, es);
    at = no_region;
    note = Note.{ def with typ; eff }
  }

let selfRefE typ =
  { it = PrimE (SelfRef typ, []);
    at = no_region;
    note = Note.{ def with typ }
  }


let assertE e =
  { it = PrimE (AssertPrim, [e]);
    at = no_region;
    note = Note.{ def with typ = T.unit; eff = eff e }
  }


let asyncE typ_bind e typ1 =
  { it = AsyncE (typ_bind, e, typ1);
    at = no_region;
    note = Note.{ def with typ = T.Async (typ1, typ e); eff = T.Triv }
  }

let awaitE e =
  { it = PrimE (AwaitPrim, [e]);
    at = no_region;
    note = Note.{ def with typ = snd (T.as_async (T.normalize (typ e))) ; eff = T.Await }
  }

let cps_asyncE typ1 typ2 e =
  { it = PrimE (CPSAsync typ1, [e]);
    at = no_region;
    note = Note.{ def with typ = T.Async (typ1, typ2); eff = eff e }
  }

let cps_awaitE cont_typ e1 e2 =
  match cont_typ with
  | T.Func(T.Local, T.Returns, [], _, ts2) ->
    { it = PrimE (CPSAwait cont_typ, [e1; e2]);
      at = no_region;
      note = Note.{ def with typ = T.seq ts2; eff = max_eff (eff e1) (eff e2) }
    }
  | _ -> assert false

let ic_replyE ts e =
  (match ts with
  | [t] -> assert (T.sub (e.note.Note.typ) t)
  | _ -> assert (T.sub (T.Tup ts) (e.note.Note.typ)));
  { it = PrimE (ICReplyPrim ts, [e]);
    at = no_region;
    note = Note.{ def with typ = T.unit; eff = eff e }
  }

let ic_rejectE e =
  { it = PrimE (ICRejectPrim, [e]);
    at = no_region;
    note = Note.{ def with typ = T.unit; eff = eff e }
  }

let ic_callE f e k r =
  let es = [f; e; k; r] in
  let effs = List.map eff es in
  let eff = List.fold_left max_eff T.Triv effs in
  { it = PrimE (ICCallPrim, es);
    at = no_region;
    note = Note.{ def with typ = T.unit; eff = eff }
  }


(* tuples *)

let projE e n =
  match typ e with
  | T.Tup ts ->
     { it = PrimE (ProjPrim n, [e]);
       note = Note.{ def with typ = List.nth ts n; eff = eff e };
       at = no_region;
     }
  | _ -> failwith "projE"

let optE e =
 { it = PrimE (OptPrim, [e]);
   note = Note.{ def with typ = T.Opt (typ e); eff = eff e };
   at = no_region;
 }

let tagE i e =
 { it = PrimE (TagPrim i, [e]);
   note = Note.{ def with typ = T.Variant [{T.lab = i; typ = typ e; depr = None}]; eff = eff e };
   at = no_region;
 }

let dec_eff dec = match dec.it with
  | LetD (_,e) | VarD (_, _, e) -> eff e

let rec simpl_decs decs = List.concat_map simpl_dec decs
and simpl_dec dec = match dec.it with
  | LetD ({it = WildP;_}, {it = PrimE (TupPrim, []);_}) ->
    []
  | LetD ({it = TupP ps;_}, {it = PrimE (TupPrim, es);_}) when List.length ps = List.length es ->
    simpl_decs (List.map2 (fun p e -> LetD (p, e) @@ p.at) ps es)
  | _ ->
    [ dec ]

let blockE decs exp =
  let decs' = simpl_decs decs in
  match decs' with
  | [] -> exp
  | _ ->
    let es = List.map dec_eff decs' in
    let typ = typ exp in
    let eff =  List.fold_left max_eff (eff exp) es in
    { it = BlockE (decs', exp);
      at = no_region;
      note = Note.{ def with typ; eff }
    }

let natE n =
  { it = LitE (NatLit n);
    at = no_region;
    note = Note.{ def with typ = T.nat }
  }

let textE s =
  { it = LitE (TextLit s);
    at = no_region;
    note = Note.{ def with typ = T.text }
  }

let blobE s =
  { it = LitE (BlobLit s);
    at = no_region;
    note = Note.{ def with typ = T.blob }
  }

let boolE b =
  { it = LitE (BoolLit b);
    at = no_region;
    note = Note.{ def with typ = T.bool }
  }

let nullE () =
  { it = LitE NullLit;
    at = no_region;
    note = Note.{ def with typ = T.Prim T.Null }
  }


(* Functions *)

let funcE name sort ctrl typ_binds args typs exp =
  let cs = List.map (function { it = {con;_ }; _ } -> con) typ_binds in
  let tbs = List.map (function { it = { sort; bound; con}; _ } ->
    {T.var = Con.name con; T.sort; T.bound = T.close cs bound})
    typ_binds
  in
  let ts1 = List.map (function arg -> T.close cs arg.note) args in
  let ts2 = List.map (T.close cs) typs in
  let typ = T.Func(sort, ctrl, tbs, ts1, ts2) in
  { it = FuncE(name, sort, ctrl, typ_binds, args, typs, exp);
    at = no_region;
    note = Note.{ def with typ; eff = T.Triv };
  }

let callE exp1 typs exp2 =
  let typ =  match T.promote (typ exp1) with
    | T.Func (_sort, _control, _, _, ret_tys) ->
      T.open_ typs (T.seq ret_tys)
    | T.Non -> T.Non
    | _ -> raise (Invalid_argument "callE expect a function")
  in
  { it = PrimE (CallPrim typs, [exp1; exp2]);
    at = no_region;
    note = Note.{ def with
     typ;
     eff = max_eff (eff exp1) (eff exp2)
    }
  }

let ifE exp1 exp2 exp3 typ =
  { it = IfE (exp1, exp2, exp3);
    at = no_region;
    note = Note.{ def with
      typ = typ;
      eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
    }
  }

let falseE () = boolE false
let trueE () = boolE true
let notE : Ir.exp -> Ir.exp = fun e ->
  primE (RelPrim (T.bool, Operator.EqOp)) [e; falseE ()]
let andE : Ir.exp -> Ir.exp -> Ir.exp = fun e1 e2 -> ifE e1 e2 (falseE ()) T.bool
let orE : Ir.exp -> Ir.exp -> Ir.exp = fun e1 e2 -> ifE e1 (trueE ()) e2 T.bool
let rec conjE : Ir.exp list -> Ir.exp = function
  | [] -> trueE ()
  | [x] -> x
  | (x::xs) -> andE x (conjE xs)

let dotE exp name typ =
  { it = PrimE (DotPrim name, [exp]);
    at = no_region;
    note = Note.{ def with
      typ = typ;
      eff = eff exp
    }
  }

let switch_optE exp1 exp2 pat exp3 typ1  =
  { it =
      SwitchE
        (exp1,
         [{ it = {pat = {it = LitP NullLit;
                         at = no_region;
                         note = typ exp1};
                  exp = exp2};
            at = no_region;
           note = () };
          { it = {pat = {it = OptP pat;
                        at = no_region;
                        note = typ exp1};
                  exp = exp3};
            at = no_region;
            note = () }]
        );
    at = no_region;
    note = Note.{ def with
      typ = typ1;
      eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
    }
  }

let switch_variantE exp1 cases typ1 =
  { it =
      SwitchE (exp1,
        List.map (fun (l,p,e) ->
          { it = {pat = {it = TagP (l, p);
                         at = no_region;
                         note = typ exp1};
                  exp = e};
            at = no_region;
            note = ()
          })
          cases
      );
    at = no_region;
    note = Note.{ def with
      typ = typ1;
      eff = List.fold_left max_eff (eff exp1) (List.map (fun (l,p,e) -> eff e) cases)
    }
  }

let tupE exps =
  let effs = List.map eff exps in
  let eff = List.fold_left max_eff T.Triv effs in
  { it = PrimE (TupPrim, exps);
    at = no_region;
    note = Note.{ def with typ = T.Tup (List.map typ exps); eff };
  }

let unitE () = tupE []

let breakE l exp =
  { it = PrimE (BreakPrim l, [exp]);
    at = no_region;
    note = Note.{ def with typ = T.Non; eff = eff exp };
  }

let retE exp =
  { it = PrimE (RetPrim, [exp]);
    at = no_region;
    note = Note.{ def with typ = T.Non; eff = eff exp };
  }

let immuteE e =
  { e with
    note = Note.{ def with typ = T.as_immut (typ e); eff = eff e };
  }

let assignE v exp2 =
  assert (T.is_mut (typ_of_var v));
  { it = AssignE (varLE v, exp2);
    at = no_region;
    note = Note.{ def with typ = T.unit; eff = eff exp2 };
  }

let labelE l typ exp =
  { it = LabelE (l, typ, exp);
    at = no_region;
    note = Note.{ def with typ; eff = eff exp }
  }

(* Used to desugar for loops, while loops and loop-while loops. *)
let loopE exp =
  { it = LoopE exp;
    at = no_region;
    note = Note.{ def with typ = T.Non; eff = eff exp }
  }

let declare_idE x typ exp1 =
  { it = DeclareE (x, typ, exp1);
    at = no_region;
    note = exp1.note;
  }

let define_idE x mut exp1 =
  { it = DefineE (x, mut, exp1);
    at = no_region;
    note = Note.{ def with typ = T.unit }
  }

let newObjE sort ids typ =
  { it = NewObjE (sort, ids, typ);
    at = no_region;
    note = Note.{ def with typ }
  }


(* Declarations *)

let letP pat exp = LetD (pat, exp) @@ no_region

let letD x exp = letP (varP x) exp

let varD x exp =
  let t = typ_of_var x in
  assert (T.is_mut t);
  VarD (id_of_var x, T.as_immut t, exp) @@ no_region

let expD exp =
  let pat = { it = WildP; at = exp.at; note = exp.note.Note.typ } in
  LetD (pat, exp) @@ exp.at

let let_no_shadow (id, typ) exp decs =
  (* could be replaced by a more simple “defined by this decs” function *)
  let (_,f) = Freevars.decs decs in
  if Freevars.M.mem id f
  then decs
  else [ letD (id, typ) exp ] @ decs

(* Derived expressions *)

let letE x exp1 exp2 = blockE [letD x exp1] exp2

let thenE exp1 exp2 = blockE [expD exp1] exp2

let ignoreE exp =
  if typ exp = T.unit
  then exp
  else thenE exp (tupE [])


(* Mono-morphic function expression *)

let arg_of_var (id, typ) =
  { it = id; at = no_region; note = typ }

let var_of_arg { it = id; note = typ; _} = (id, typ)

let unary_funcE name typ x exp =
  let sort, control, arg_tys, ret_tys = match typ with
    | T.Func(s, c, _, ts1, ts2) -> s, c, ts1, ts2
    | _ -> assert false in
  let args, exp' =
    if List.length arg_tys = 1;
    then
      [ arg_of_var x ], exp
    else
      let vs = fresh_vars "param" arg_tys in
      List.map arg_of_var vs,
      blockE [letD x (tupE (List.map varE vs))] exp
  in
  ({it = FuncE
     ( name,
       sort,
       control,
       [],
       args,
       (* TODO: Assert invariant: retty has no free (unbound) DeBruijn indices -- Claudio *)
       ret_tys,
       exp'
     );
    at = no_region;
    note = Note.{ def with typ }
   })

let nary_funcE name typ xs exp =
  let sort, control, arg_tys, ret_tys = match typ with
    | T.Func(s, c, _, ts1, ts2) -> s, c, ts1, ts2
    | _ -> assert false in
  assert (List.length arg_tys = List.length xs);
  ({it = FuncE
      ( name,
        sort,
        control,
        [],
        List.map arg_of_var xs,
        ret_tys,
        exp
      );
    at = no_region;
    note = Note.{ def with typ }
  })

(* Mono-morphic function declaration, sharing inferred from f's type *)
let funcD ((id, typ) as f) x exp =
  letD f (unary_funcE id typ x exp)

(* Mono-morphic, n-ary function declaration *)
let nary_funcD ((id, typ) as f) xs exp =
  letD f (nary_funcE id typ xs exp)

(* Continuation types *)

let answerT = T.unit

let contT typ = T.Func (T.Local, T.Returns, [], T.as_seq typ, [])

let err_contT =  T.Func (T.Local, T.Returns, [], [T.catch], [])

let cpsT typ = T.Func (T.Local, T.Returns, [], [contT typ; err_contT], [])

(* Sequence expressions *)

let seqE es =
  match es with
  | [e] -> e
  | es -> tupE es

(* Lambdas & continuations *)

(* Lambda abstraction *)

(* local lambda *)
let (-->) x exp =
  let fun_ty = T.Func (T.Local, T.Returns, [], T.as_seq (typ_of_var x), T.as_seq (typ exp)) in
  unary_funcE "$lambda" fun_ty x exp

(* n-ary local lambda *)
let (-->*) xs exp =
  let fun_ty = T.Func (T.Local, T.Returns, [], List.map typ_of_var xs, T.as_seq (typ exp)) in
  nary_funcE "$lambda" fun_ty xs exp

let close_typ_binds cs tbs =
  List.map (fun {it = {con; sort; bound}; _} -> {T.var = Con.name con; sort=sort; bound = T.close cs bound}) tbs

(* polymorphic, n-ary local lambda *)
let forall tbs e =
 let cs = List.map (fun tb -> tb.it.con) tbs in
 match e.it, e.note.Note.typ with
 | FuncE (n, s, c1, [], xs, ts, exp),
   T.Func ( _, c2, [], ts1, ts2) ->
   { e with
     it = FuncE(n, s, c1, tbs, xs, ts, exp);
     note = Note.{ e.note with
       typ = T.Func(s, c2, close_typ_binds cs tbs,
         List.map (T.close cs) ts1,
         List.map (T.close cs) ts2)
     }
   }
 | _ -> assert false

(* Lambda application (monomorphic) *)

let ( -*- ) exp1 exp2 = callE exp1 [] exp2


(* derived loop forms; each can be expressed as an unconditional loop *)

let whileE exp1 exp2 =
  (* while e1 e2
     ~~> label l loop {
           if e1 then { e2 } else { break l }
         }
  *)
  let lab = fresh_id "done" () in
  labelE lab T.unit (
      loopE (
          ifE exp1
            exp2
            (breakE lab (tupE []))
            T.unit
        )
    )

let loopWhileE exp1 exp2 =
  (* loop e1 while e2
    ~~> label l loop {
          let () = e1 ;
          if e2 { } else { break l }
        }
   *)
  let lab = fresh_id "done" () in
  labelE lab T.unit (
      loopE (
          thenE exp1
            ( ifE exp2
               (tupE [])
               (breakE lab (tupE []))
               T.unit
            )
        )
    )

let forE pat exp1 exp2 =
  (* for (p in e1) e2
     ~~>
     let nxt = e1.next ;
     label l loop {
       switch nxt () {
         case null { break l };
         case ?p    { e2 };
       }
     } *)
  let lab = fresh_id "done" () in
  let ty1 = exp1.note.Note.typ in
  let _, tfs = T.as_obj_sub [nextN] ty1 in
  let tnxt = T.lookup_val_field nextN tfs in
  let nxt = fresh_var "nxt" tnxt in
  letE nxt (dotE exp1 nextN tnxt) (
    labelE lab T.unit (
      loopE (
        switch_optE (callE (varE nxt) [] (tupE []))
          (breakE lab (tupE []))
          pat exp2 T.unit
      )
    )
  )

let unreachableE () =
  (* Do we want a dedicated UnreachableE in the AST? *)
  loopE (unitE ())

