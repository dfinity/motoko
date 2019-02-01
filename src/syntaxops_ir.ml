(* WIP translation of syntaxops to use IR in place of Source *)

open Source
open Ir
open Effect

module S = Syntax
module T = Type

type var = exp

(* Mutabilities *)

let varM = S.Var@@no_region
let constM = S.Const@@no_region

(* Field names *)

let nameN s = (S.Name s)@@no_region

let nextN = nameN "next"

(* Identifiers *)

let idE id typ =
  {it = VarE id;
   at = no_region;
   note = {S.note_typ = typ;
           S.note_eff = T.Triv}
  }

let id_of_exp x =
  match x.it with
  | VarE x -> x
  | _ -> failwith "Impossible: id_of_exp"

(* Fresh id generation *)

let id_stamp = ref 0

let fresh () =
  let name = Printf.sprintf "$%i" (!id_stamp) in
  id_stamp := !id_stamp + 1;
  name

let fresh_lab () =
  let name = fresh () in
  name@@no_region

let fresh_id typ =
  let name = fresh () in
  idE (name@@no_region) typ


(* Patterns *)

let varP x =
  { it=VarP (id_of_exp x);
    at = x.at;
    note = x.note.S.note_typ
  }

let tupP pats =
  {it = TupP pats;
   note = T.Tup (List.map (fun p -> p.note) pats);
   at = no_region}

let seqP ps =
  match ps with
  | [p] -> p
  | ps -> tupP ps

let as_seqP p =
  match p.it with
  | TupP ps -> ps
  | _ -> [p]

(* Primitives *)

let primE name typ =
  {it = PrimE name;
   at = no_region;
   note = {S.note_typ = typ;
           S.note_eff = T.Triv}
  }

(* tuples *)

let projE e n =
  match typ e with
  | T.Tup ts ->
     {it = ProjE(e,n);
      note = {S.note_typ = List.nth ts n;
              S.note_eff = eff e};
      at = no_region;
     }
  | _ -> failwith "projE"

let decE dec = {dec with it = BlockE ([dec], dec.note.note_typ)}

let blockE decs =
  let rec typ_decs decs =
    match decs with
    | [] -> T.unit
    | [dec] -> typ dec
    | _::decs -> typ_decs decs
  in
  let es = List.map eff decs in
  let typ = typ_decs decs in
  let e =  List.fold_left max_eff Type.Triv es in
  { it = BlockE (decs, typ);
    at = no_region;
    note = {S.note_typ = typ;
            S.note_eff = e}
  }

let textE s =
  { it = LitE (S.TextLit s);
    at = no_region;
    note = {S.note_typ = T.Prim T.Text;
            S.note_eff = T.Triv;}
  }


let unitE =
  { it = TupE [];
    at = no_region;
    note = {S.note_typ = T.Tup [];
            S.note_eff = T.Triv}
  }

let boolE b =
  { it = LitE (S.BoolLit b);
    at = no_region;
    note = {S.note_typ = T.bool;
            S.note_eff = T.Triv}
  }

(* Take `cc` as a param. *)
let callE cc e1 ts e2 t =
  { it = CallE(cc,e1,ts,e2);
    at = no_region;
    note = {S.note_typ = t;
            S.note_eff = T.Triv}
  }



let ifE exp1 exp2 exp3 typ =
  { it = IfE (exp1, exp2, exp3);
    at = no_region;
    note = {S.note_typ = typ;
            S.note_eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
           }
  }

let dotE exp name typ =
  { it = DotE (exp, name);
    at = no_region;
    note = {S.note_typ = typ;
            S.note_eff = eff exp}
  }

let switch_optE exp1 exp2 pat exp3 typ =
  { it = 
      SwitchE 
        (exp1,
         [{it = {pat = {it = LitP S.NullLit;
                        at = no_region;
                        note = exp1.note.S.note_typ};
                 exp = exp2};
           at = no_region;
           note = ()};
          {it = {pat = {it = OptP pat;
                        at = no_region;
                        note = exp1.note.S.note_typ};
                 exp = exp3};
           at = no_region;
           note = ()}]
        );
    at = no_region;
    note = {S.note_typ = typ;
            S.note_eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
           }
  }

let tupE exps =
  let effs = List.map eff exps in
  let eff = List.fold_left max_eff Type.Triv effs in
  {it = TupE exps;
   at = no_region;
   note = {S.note_typ = T.Tup (List.map typ exps);
           S.note_eff = eff}
  }

let breakE l exp typ =
  { it = BreakE (l, exp);
    at = no_region;
    note = {S.note_eff = eff exp;
            S.note_typ = typ}
  }

let retE exp typ =
  { it = RetE exp;
    at = no_region;
    note = {S.note_eff = eff exp;
            S.note_typ = typ}
  }

let assignE exp1 exp2 =
  { it = AssignE (exp1,exp2);
    at = no_region;
    note = {S.note_eff = Effect.max_eff (eff exp1) (eff exp2);
            S.note_typ = Type.unit}
  }

let labelE l typT exp =
  { exp with it = LabelE(l,typT,exp) }

let loopE exp1 exp2Opt =
  { it = LoopE(exp1,exp2Opt);
    at = no_region;
    note = {S.note_eff = Effect.max_eff (eff exp1)
                         (match exp2Opt with
                          | Some exp2 -> eff exp2
                          | None -> Type.Triv);
            S.note_typ = Type.Non}
  }


let declare_idE x typ exp1 =
  { it = DeclareE (x, typ, exp1);
    at = no_region;
    note = exp1.note;
  }

let define_idE x mut exp1 =
  { it = DefineE (x, mut, exp1);
    at = no_region;
    note = { S.note_typ = T.unit;
             S.note_eff =T.Triv}
  }

let newObjE typ sort ids =
  { it = NewObjE (sort, ids, typ); 
    at = no_region;
    note = { S.note_typ = typ;
             S.note_eff = T.Triv}
  }


(* Declarations *)

let letP p e =
  {it = LetD(p,e);
   at = no_region;
   note = { S.note_typ = T.unit; (* ! *)
            S.note_eff = e.note.S.note_eff; }
  }

let letD x exp = { it = LetD (varP x,exp);
                   at = no_region;
                   note = { S.note_eff = eff exp;
                            S.note_typ = T.unit;} (* ! *)
                 }

let varD x exp = { it = VarD (x,exp);
                   at = no_region;
                   note = { S.note_eff = eff exp;
                            S.note_typ = T.unit;} (* ! *)
                 }
let expD exp =  { exp with it = ExpD exp}


(* let expressions (derived) *)

let letE x exp1 exp2 = blockE [letD x exp1; expD exp2]

(* Mono-morphic function declaration, sharing inferred from f's type *)
let funcD f x e =
  match f.it,x.it with
  | VarE _, VarE _ ->
    let sharing,t1,t2 = match typ f with
      | T.Func(T.Call sharing, _, _, ts1, ts2) -> sharing,T.seq ts1, T.seq ts2
      | _ -> assert false in
    let cc = Value.call_conv_of_typ (typ f) in
     {it=FuncD( cc, (id_of_exp f),
               [],
               {it = VarP (id_of_exp x); at = no_region; note = t1},
               (* TODO: Assert invariant: t2 has no free (unbound) DeBruijn indices -- Claudio *)
               t2,
               e);
            at = no_region;
            note = f.note}
  | _ -> failwith "Impossible: funcD"


(* Mono-morphic, n-ary function declaration *)
let nary_funcD f xs e =
  match f.it,f.note.S.note_typ with
  | VarE _,
    T.Func(T.Call sharing,_,_,_,ts2) ->
    let cc = Value.call_conv_of_typ (typ f) in
    let t2 = T.seq ts2 in
      {it=FuncD( 
               cc,
               id_of_exp f,
               [],
               seqP (List.map varP xs),
               t2,
               e);
      at = no_region;
      note = f.note}
  | _,_ -> failwith "Impossible: funcD"


(* Continuation types *)

let answerT = T.unit

let contT typ = T.Func(T.Call T.Local, T.Returns, [], T.as_seq typ, [])
let cpsT typ = T.Func(T.Call T.Local, T.Returns, [], [contT typ], [])

let fresh_cont typ = fresh_id (contT typ)

(* Sequence expressions *)

let seqE es =
  match es with
  | [e] -> e
  | es -> tupE es

let as_seqE e =
  match e.it with
  | TupE es -> es
  | _ -> [e]

(* Lambdas & continuations *)

(* Lambda abstraction *)

(* local lambda *)
let  (-->) x e =
  match x.it with
  | VarE _ ->
     let f = idE ("$lambda"@@no_region) (T.Func(T.Call T.Local, T.Returns, [], T.as_seq (typ x), T.as_seq (typ e))) in
     decE  (funcD f x e)
  | _ -> failwith "Impossible: -->"

(* n-ary local lambda *)
let (-->*) xs e  =
  let f = idE ("$lambda"@@no_region)
            (T.Func(T.Call T.Local, T.Returns, [],
                    List.map typ xs, T.as_seq (typ e))) in
  decE (nary_funcD f xs e)


(* n-ary shared lambda *)
let (-@>*) xs e  =
  let f = idE ("$lambda"@@no_region)
            (T.Func(T.Call T.Sharable, T.Returns, [],
                    List.map typ xs, T.as_seq (typ e))) in
  decE (nary_funcD f xs e)


(* Lambda application (monomorphic) *)

let ( -*- ) exp1 exp2 =
  match exp1.note.S.note_typ with
  | T.Func(_, _, [], ts1, ts2) ->
(* for debugging bad applications, imprecisely
    (if not ((T.seq ts1) = (typ exp2))
     then
       begin
         (Printf.printf "\nBad -*- application: func:\n  %s \n arg:  %s\n, expected type: \n  %s: received type: \n  %s"
            (Wasm.Sexpr.to_string 80 (Arrange.exp exp1))
            (Wasm.Sexpr.to_string 80 (Arrange.exp exp2))
            (T.string_of_typ (T.seq ts1))
            (T.string_of_typ (typ exp2)));

       end
     else ());
 *)
    let cc = 
      let t  = exp1.note.S.note_typ in
      Value.call_conv_of_typ t in

    {it = CallE(cc, exp1, [], exp2);
     at = no_region;
     note = {S.note_typ = T.seq ts2;
              S.note_eff = max_eff (eff exp1) (eff exp2)}
    }
  | typ1 -> failwith
           (Printf.sprintf "Impossible: \n func: %s \n : %s arg: \n %s"
              (Wasm.Sexpr.to_string 80 (Arrange_ir.exp exp1))
              (Type.string_of_typ typ1)
              (Wasm.Sexpr.to_string 80 (Arrange_ir.exp exp2)))


(* Intermediate, cps-based @async and @await primitives,
   introduced by await(opt).ml, removed by async.ml
*)

let prim_async typ =
  primE "@async" (T.Func(T.Call T.Local, T.Returns, [], [cpsT typ], [T.Async typ]))

let prim_await typ =
  primE "@await" (T.Func(T.Call T.Local, T.Returns, [], [T.Async typ; contT typ], []))

