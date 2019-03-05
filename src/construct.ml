(* WIP translation of syntaxops to use IR in place of Source *)
open Source
open Ir
open Effect

module S = Syntax
module T = Type

type var = exp

(* Mutabilities *)

let varM = S.Var @@ no_region
let constM = S.Const @@ no_region

(* Field names *)

let nameN s = Name s @@ no_region

let nextN = nameN "next"

(* Identifiers *)

let idE id typ =
  { it = VarE id;
    at = no_region;
    note = { S.note_typ = typ;
             S.note_eff = T.Triv }
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

let fresh_id () =
  let name = fresh () in
  name @@ no_region

let fresh_var typ =
  let name = fresh () in
  idE (name @@ no_region) typ


(* Patterns *)

let varP x =
  { it = VarP (id_of_exp x);
    at = x.at;
    note = x.note.S.note_typ
  }

let tupP pats =
  { it = TupP pats;
    note = T.Tup (List.map (fun p -> p.note) pats);
    at = no_region }

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
  { it = PrimE name;
    at = no_region;
    note = { S.note_typ = typ;
             S.note_eff = T.Triv }
  }

(* tuples *)

let projE e n =
  match typ e with
  | T.Tup ts ->
     { it = ProjE (e, n);
       note = { S.note_typ = List.nth ts n;
                S.note_eff = eff e };
       at = no_region;
     }
  | _ -> failwith "projE"

let dec_eff dec = match dec.it with
  | TypD _ -> T.Triv
  | LetD (_,e) | VarD (_,e) -> eff e

let blockE decs exp =
  match decs with
  | [] -> exp
  | _ ->
  let es = List.map dec_eff decs in
  let typ = typ exp in
  let e =  List.fold_left max_eff (eff exp) es in
  { it = BlockE (decs, exp);
    at = no_region;
    note = {S.note_typ = typ;
            S.note_eff = e }
  }

let textE s =
  { it = LitE (S.TextLit s);
    at = no_region;
    note = { S.note_typ = T.Prim T.Text;
             S.note_eff = T.Triv }
  }


let unitE =
  { it = TupE [];
    at = no_region;
    note = { S.note_typ = T.Tup [];
             S.note_eff = T.Triv }
  }

let boolE b =
  { it = LitE (S.BoolLit b);
    at = no_region;
    note = { S.note_typ = T.bool;
             S.note_eff = T.Triv}
  }

let callE exp1 ts exp2 t =
  { it = CallE (Value.call_conv_of_typ (typ exp1), exp1, ts, exp2);
    at = no_region;
    note = { S.note_typ = t;
             S.note_eff = max_eff (eff exp1) (eff exp2) }
  }



let ifE exp1 exp2 exp3 typ =
  { it = IfE (exp1, exp2, exp3);
    at = no_region;
    note = { S.note_typ = typ;
             S.note_eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
           }
  }

let dotE exp name typ =
  { it = DotE (exp, name);
    at = no_region;
    note = { S.note_typ = typ;
             S.note_eff = eff exp }
  }

let switch_optE exp1 exp2 pat exp3 typ1  =
  { it =
      SwitchE
        (exp1,
         [{ it = {pat = {it = LitP S.NullLit;
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
    note = { S.note_typ = typ1;
             S.note_eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
           }
  }

let tupE exps =
  let effs = List.map eff exps in
  let eff = List.fold_left max_eff Type.Triv effs in
  { it = TupE exps;
    at = no_region;
    note = { S.note_typ = T.Tup (List.map typ exps);
             S.note_eff = eff }
  }

let breakE l exp =
  { it = BreakE (l, exp);
    at = no_region;
    note = { S.note_eff = eff exp;
             S.note_typ = Type.Non }
  }

let retE exp =
  { it = RetE exp;
    at = no_region;
    note = { S.note_eff = eff exp;
             S.note_typ = Type.Non }
  }

let immuteE e =
  { e with
    note = { S.note_eff = eff e;
             S.note_typ = T.as_immut (typ e) }
  }


let assignE exp1 exp2 =
  assert (T.is_mut (typ exp1));
  { it = AssignE (exp1, exp2);
    at = no_region;
    note = { S.note_eff = Effect.max_eff (eff exp1) (eff exp2);
             S.note_typ = Type.unit }
  }

let labelE l typ exp =
  { it = LabelE (l, typ, exp);
    at = no_region;
    note = { S.note_eff = eff exp;
             S.note_typ = typ }
  }

let loopE exp1 exp2Opt =
  { it = LoopE (exp1, exp2Opt);
    at = no_region;
    note = { S.note_eff = Effect.max_eff (eff exp1)
                            (match exp2Opt with
                             | Some exp2 -> eff exp2
                             | None -> Type.Triv);
             S.note_typ = Type.Non }
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
             S.note_eff = T.Triv}
  }

let newObjE sort ids typ =
  { it = NewObjE (sort, ids, typ);
    at = no_region;
    note = { S.note_typ = typ;
             S.note_eff = T.Triv }
  }


(* Declarations *)

let letP pat exp = LetD (pat, exp) @@ no_region

let letD x exp = letP (varP x) exp

let varD x exp =
  VarD (x, exp) @@ no_region

let expD exp =
  let pat = { it = WildP; at = exp.at; note = exp.note.note_typ } in
  LetD (pat, exp) @@ exp.at

let ignoreE exp =
  if typ exp = T.unit
  then exp
  else blockE [expD exp] (tupE [])


(* let expressions (derived) *)

let letE x exp1 exp2 = blockE [letD x exp1] exp2

(* Mono-morphic function expression *)
let funcE name t x exp =
  let retty = match t with
    | T.Func(_, _, _, _, ts2) -> T.seq ts2
    | _ -> assert false in
  let cc = Value.call_conv_of_typ t in
  ({it = FuncE
     ( name,
       cc,
       [],
       varP x,
       (* TODO: Assert invariant: retty has no free (unbound) DeBruijn indices -- Claudio *)
       retty,
       exp
     );
    at = no_region;
    note = { S.note_eff = T.Triv; S.note_typ = t }
   })

let nary_funcE name t xs exp =
  let retty = match t with
    | T.Func(_, _, _, _, ts2) -> T.seq ts2
    | _ -> assert false in
  let cc = Value.call_conv_of_typ t in
  ({it = FuncE
      ( name,
        cc,
        [],
        seqP (List.map varP xs),
        retty,
        exp
      );
    at = no_region;
    note = { S.note_eff = T.Triv; S.note_typ = t }
  })

(* Mono-morphic function declaration, sharing inferred from f's type *)
let funcD f x exp =
  match f.it, x.it with
  | VarE _, VarE _ ->
    letD f (funcE (id_of_exp f).it (typ f) x exp)
  | _ -> failwith "Impossible: funcD"

(* Mono-morphic, n-ary function declaration *)
let nary_funcD f xs exp =
  match f.it with
  | VarE _ ->
    letD f (nary_funcE (id_of_exp f).it (typ f) xs exp)
  | _ -> failwith "Impossible: funcD"


(* Continuation types *)

let answerT = T.unit

let contT typ = T.Func (T.Local, T.Returns, [], T.as_seq typ, [])
let cpsT typ = T.Func (T.Local, T.Returns, [], [contT typ], [])

let fresh_cont typ = fresh_var (contT typ)

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
let (-->) x exp =
  let fun_ty = T.Func (T.Local, T.Returns, [], T.as_seq (typ x), T.as_seq (typ exp)) in
  funcE "$lambda" fun_ty x exp

(* n-ary local lambda *)
let (-->*) xs exp =
  let fun_ty = T.Func (T.Local, T.Returns, [], List.map typ xs, T.as_seq (typ exp)) in
  nary_funcE "$lambda" fun_ty xs exp


(* n-ary shared lambda *)
let (-@>*) xs exp  =
  let fun_ty = T.Func (T.Sharable, T.Returns, [], List.map typ xs, T.as_seq (typ exp)) in
  nary_funcE "$lambda" fun_ty xs exp


(* Lambda application (monomorphic) *)

let ( -*- ) exp1 exp2 =
  match typ exp1 with
  | T.Func (_, _, [], ts1, ts2) ->
    let cc = Value.call_conv_of_typ (typ exp1) in
    { it = CallE (cc, exp1, [], exp2);
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
  primE "@async" (T.Func (T.Local, T.Returns, [], [cpsT typ], [T.Async typ]))

let prim_await typ =
  primE "@await" (T.Func (T.Local, T.Returns, [], [T.Async typ; contT typ], []))
