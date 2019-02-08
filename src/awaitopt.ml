open Source
open Ir
open Effect
module R = Rename
module T = Type
open Construct

(* continuations, syntactic and meta-level *)

type kont = ContVar of exp
          | MetaCont of T.typ * (exp -> exp)

let meta typ exp =
  let expanded = ref false in
  let exp v = assert (not(!expanded));
              expanded := true;
              exp v
  in
  MetaCont (typ, exp)

(* reify a continuation as syntax *)
let letcont k scope =
  match k with
  | ContVar k' -> scope k' (* letcont eta-contraction *)
  | MetaCont (typ, cont) ->
    let k' = fresh_cont typ in
    let v = fresh_id typ in
    blockE [funcD k' v (cont v); (* at this point, I'm really worried about variable capture *)
            expD (scope k')]

(* The empty identifier names the implicit return label *)
let id_ret = ""

let ( -@- ) k exp2 =
  match k with
  | ContVar exp1 ->
     exp1 -*- exp2
  | MetaCont (typ,k) ->
     match exp2.it with
     | VarE _ -> k exp2
     | _ ->
        let u = fresh_id typ in
        letE u exp2
          (k  u)

(* Label environments *)

module LabelEnv = Env.Make(String)

module PatEnv = Env.Make(String)

type label_sort = Cont of kont | Label


(* Trivial translation of pure terms (eff = T.Triv) *)

let rec t_exp context exp =
  assert (eff exp = T.Triv);
  { exp with it = t_exp' context exp.it }
and t_exp' context exp' =
  match exp' with
  | PrimE _
  | VarE _
  | LitE _ -> exp'
  | UnE (ot, op, exp1) ->
    UnE (ot, op, t_exp context exp1)
  | BinE (ot, exp1, op, exp2) ->
    BinE (ot, t_exp context exp1, op, t_exp context exp2)
  | RelE (ot, exp1, op, exp2) ->
    RelE (ot, t_exp context exp1, op, t_exp context exp2)
  | TupE exps ->
    TupE (List.map (t_exp context) exps)
  | OptE exp1 ->
    OptE (t_exp context exp1)
  | ProjE (exp1, n) ->
    ProjE (t_exp context exp1, n)
  | ActorE (id, fields, typ) ->
    let fields' = t_fields context fields in
    ActorE (id, fields', typ)
  | DotE (exp1, id) ->
    DotE (t_exp context exp1, id)
  | ActorDotE (exp1, id) ->
    ActorDotE (t_exp context exp1, id)
  | AssignE (exp1, exp2) ->
    AssignE (t_exp context exp1, t_exp context exp2)
  | ArrayE (mut, typ, exps) ->
    ArrayE (mut, typ, List.map (t_exp context) exps)
  | IdxE (exp1, exp2) ->
    IdxE (t_exp context exp1, t_exp context exp2)
  | CallE (cc, exp1, typs, exp2) ->
    CallE (cc, t_exp context exp1, typs, t_exp context exp2)
  | BlockE (decs, typ) ->
    BlockE (t_decs context decs, typ)
  | IfE (exp1, exp2, exp3) ->
    IfE (t_exp context exp1, t_exp context exp2, t_exp context exp3)
  | SwitchE (exp1, cases) ->
    let cases' = List.map
                  (fun {it = {pat;exp}; at; note} ->
                     {it = {pat;exp = t_exp context exp}; at; note})
                  cases
    in
    SwitchE (t_exp context exp1, cases')
  | WhileE (exp1, exp2) ->
    WhileE (t_exp context exp1, t_exp context exp2)
  | LoopE (exp1, exp2_opt) ->
    LoopE (t_exp context exp1, Lib.Option.map (t_exp context) exp2_opt)
  | ForE (pat, exp1, exp2) ->
    ForE (pat, t_exp context exp1, t_exp context exp2)
  | LabelE (id, _typ, exp1) ->
    let context' = LabelEnv.add id.it Label context in
    LabelE (id, _typ, t_exp context' exp1)
  | BreakE (id, exp1) ->
    begin
      match LabelEnv.find_opt id.it context with
      | Some (Cont k) -> RetE (k -@- (t_exp context exp1))
      | Some Label -> BreakE (id, t_exp context exp1)
      | None -> assert false
    end
  | RetE exp1 ->
    begin
      match LabelEnv.find_opt id_ret context with
      | Some (Cont k) -> RetE (k -@- (t_exp context exp1))
      | Some Label -> RetE (t_exp context exp1)
      | None -> assert false
    end
  | AsyncE exp1 ->
     let exp1 = R.exp R.Renaming.empty exp1 in (* rename all bound vars apart *)
     (* add the implicit return label *)
     let k_ret = fresh_cont (typ exp1) in
     let context' = LabelEnv.add id_ret (Cont (ContVar k_ret)) LabelEnv.empty in
     (prim_async (typ exp1) -*- (k_ret --> (c_exp context' exp1 (ContVar k_ret))))
     .it
  | AwaitE _ -> assert false (* an await never has effect T.Triv *)
  | AssertE exp1 ->
    AssertE (t_exp context exp1)
  | DeclareE (id, typ, exp1) ->
    DeclareE (id, typ, t_exp context exp1)
  | DefineE (id, mut ,exp1) ->
    DefineE (id, mut, t_exp context exp1)
  | NewObjE (sort, ids, typ) -> exp'

and t_dec context dec =
  {dec with it = t_dec' context dec.it}
and t_dec' context dec' =
  match dec' with
  | ExpD exp -> ExpD (t_exp context exp)
  | TypD _ -> dec'
  | LetD (pat, exp) -> LetD (pat, t_exp context exp)
  | VarD (id, exp) -> VarD (id, t_exp context exp)
  | FuncD (s, id, typbinds, pat, typ, exp) ->
    let context' = LabelEnv.add id_ret Label LabelEnv.empty in
    FuncD (s, id, typbinds, pat, typ,t_exp context' exp)

and t_decs context decs = List.map (t_dec context) decs

and t_fields context fields =
  List.map (fun (field:exp_field) ->
      { field with it = { field.it with exp = t_exp context field.it.exp }})
    fields

(* non-trivial translation of possibly impure terms (eff = T.Await) *)

and unary context k unE e1 =
  match eff e1 with
  | T.Await ->
    c_exp context e1 (meta (typ e1) (fun v1 -> k -@- unE v1))
  | T.Triv ->
    assert false

and binary context k binE e1 e2 =
  match eff e1, eff e2 with
  | T.Triv, T.Await ->
    let v1 = fresh_id (typ e1) in (* TBR *)
    letE v1 (t_exp context e1)
      (c_exp context e2 (meta (typ e2) (fun v2 -> k -@- binE v1 v2)))
  | T.Await, T.Await ->
    c_exp context e1
      (meta (typ e1) (fun v1 ->
           c_exp context e2
             (meta (typ e2) (fun v2 ->
                  k -@- binE v1 v2))))
  | T.Await, T.Triv ->
    c_exp context e1 (meta (typ e1) (fun v1 -> k -@- binE v1 (t_exp context e2)))
  | T.Triv, T.Triv ->
    assert false

and nary context k naryE es =
  let rec nary_aux vs es  =
    match es with
    | [] -> k -@- naryE (List.rev vs)
    | [e1] when eff e1 = T.Triv ->
       (* TBR: optimization - no need to name the last trivial argument *)
       k -@- naryE (List.rev (e1 :: vs))
    | e1 :: es ->
       match eff e1 with
       | T.Triv ->
          let v1 = fresh_id (typ e1) in
          letE v1 (t_exp context e1)
            (nary_aux (v1 :: vs) es)
       | T.Await ->
          c_exp context e1
            (meta (typ e1) (fun v1 -> nary_aux (v1 :: vs) es))
  in
  nary_aux [] es


and c_if context k e1 e2 e3 =
  letcont k (fun k ->
  let trans_branch exp = match eff exp with
    | T.Triv -> k -*- t_exp context exp
    | T.Await -> c_exp context exp (ContVar k)
  in
  let e2 = trans_branch e2 in
  let e3 = trans_branch e3 in
  match eff e1 with
  | T.Triv ->
    ifE (t_exp context e1) e2 e3 answerT
  | T.Await ->
     c_exp context e1 (meta (typ e1) (fun v1 -> ifE v1 e2 e3 answerT))
  )

and c_while context k e1 e2 =
  let loop = fresh_id (contT T.unit) in
  let v2 = fresh_id T.unit in
  let e2 = match eff e2 with
    | T.Triv -> loop -*- t_exp context e2
    | T.Await -> c_exp context e2 (ContVar loop)
  in
  match eff e1 with
  | T.Triv ->
    blockE [funcD loop v2
              (ifE (t_exp context e1)
                 e2
                 (k -@- unitE)
                 answerT);
            expD (loop -*- unitE)]
  | T.Await ->
    blockE [funcD loop v2
              (c_exp context e1 (meta (T.bool)
                                   (fun v1 ->
                                     ifE v1
                                       e2
                                       (k -@- unitE)
                                       answerT)));
            expD (loop -*- unitE)]

and c_loop_none context k e1 =
  let loop = fresh_id (contT T.unit) in
  match eff e1 with
  | T.Triv ->
    assert false
  | T.Await ->
    let v1 = fresh_id T.unit in
    blockE [funcD loop v1
              (c_exp context e1 (ContVar loop));
            expD(loop -*- unitE)]

and c_loop_some context k e1 e2 =
  let loop = fresh_id (contT T.unit) in
  let u = fresh_id T.unit in
  let v1 = fresh_id T.unit in
  let e2 = match eff e2 with
    | T.Triv -> ifE (t_exp context e2)
                  (loop -*- unitE)
                  (k -@- unitE)
                  answerT
   | T.Await ->
     c_exp context e2
       (meta (typ e2)
          (fun v2 -> ifE v2
                       (loop -*- unitE)
                       (k -@- unitE)
                       answerT))
  in
  match eff e1 with
  | T.Triv ->
    blockE [funcD loop u
              (letE v1 (t_exp context e1) e2);
            expD (loop -*- unitE)]
  | T.Await ->
    blockE [funcD loop u
              (c_exp context e1 (meta (typ e1) (fun v1 -> e2)));
            expD (loop -*- unitE)]

and c_for context k pat e1 e2 =
  let v1 = fresh_id (typ e1) in
  let next_typ = (T.Func(T.Local, T.Returns, [], [], [T.Opt pat.note])) in
  let dotnext v = dotE v nextN next_typ -*- unitE in
  let loop = fresh_id (contT T.unit) in
  let v2 = fresh_id T.unit in
  let e2 = match eff e2 with
    | T.Triv -> loop -*- t_exp context e2
    | T.Await -> c_exp context e2 (ContVar loop) in
  let body v1 =
    blockE
      [funcD loop v2
         (switch_optE (dotnext v1)
            (k -@- unitE)
            pat e2
            T.unit);
       expD (loop -*- unitE)]
  in
  match eff e1 with
  | T.Triv ->
    letE v1 (t_exp context e1)
      (body v1)
  | T.Await ->
    c_exp context e1 (meta (typ e1) (fun v1 -> body v1))

and c_exp context exp =
  c_exp' context exp

and c_exp' context exp k =
  let e exp' = {it=exp'; at = exp.at; note = exp.note} in
  match exp.it with
  | _ when is_triv exp ->
    k -@- (t_exp context exp)
  | PrimE _
  | VarE _
  | LitE _ ->
    assert false
  | UnE (ot, op, exp1) ->
    unary context k (fun v1 -> e (UnE (ot, op, v1))) exp1
  | BinE (ot, exp1, op, exp2) ->
    binary context k (fun v1 v2 -> e (BinE (ot, v1, op, v2))) exp1 exp2
  | RelE (ot, exp1, op, exp2) ->
    binary context k (fun v1 v2 -> e (RelE (ot, v1, op, v2))) exp1 exp2
  | TupE exps ->
    nary context k (fun vs -> e (TupE vs)) exps
  | OptE exp1 ->
    unary context k (fun v1 -> e (OptE v1)) exp1
  | ProjE (exp1, n) ->
    unary context k (fun v1 -> e (ProjE (v1, n))) exp1
  | ActorE (id, fields, t) ->
    assert false; (* ActorE fields cannot await *)
  | DotE (exp1, id) ->
    unary context k (fun v1 -> e (DotE (v1, id))) exp1
  | ActorDotE (exp1, id) ->
    unary context k (fun v1 -> e (DotE (v1, id))) exp1
  | AssignE (exp1, exp2) ->
    binary context k (fun v1 v2 -> e (AssignE (v1, v2))) exp1 exp2
  | ArrayE (mut, typ, exps) ->
    nary context k (fun vs -> e (ArrayE (mut, typ, vs))) exps
  | IdxE (exp1, exp2) ->
    binary context k (fun v1 v2 -> e (IdxE (v1, v2))) exp1 exp2
  | CallE (cc, exp1, typs, exp2) ->
    binary context k (fun v1 v2 -> e (CallE (cc, v1, typs, v2))) exp1 exp2
  | BlockE (decs,t) ->
    c_block context decs k
  | IfE (exp1, exp2, exp3) ->
    c_if context k exp1 exp2 exp3
  | SwitchE (exp1, cases) ->
    letcont k (fun k ->
    let cases' = List.map
                   (fun {it = {pat;exp}; at; note} ->
                     let exp' = match eff exp with
                       | T.Triv -> k -*- (t_exp context exp)
                       | T.Await -> c_exp context exp (ContVar k)
                     in
                     {it = {pat;exp = exp' }; at; note})
                  cases
    in
    begin
    match eff exp1 with
    | T.Triv ->
       {exp with it = SwitchE(t_exp context exp1, cases')}
    | T.Await ->
       c_exp context exp1
         (meta (typ exp1)
            (fun v1 -> {exp with it = SwitchE(v1,cases')}))
    end)
  | WhileE (exp1, exp2) ->
    c_while context k exp1 exp2
  | LoopE (exp1, None) ->
    c_loop_none context k exp1
  | LoopE (exp1, Some exp2) ->
    c_loop_some context k exp1 exp2
  | ForE (pat, exp1, exp2) ->
    c_for context k pat exp1 exp2
  | LabelE (id, _typ, exp1) ->
     letcont k
       (fun k ->
         let context' = LabelEnv.add id.it (Cont (ContVar k)) context in
         c_exp context' exp1 (ContVar k)) (* TODO optimize me, if possible *)
  | BreakE (id, exp1) ->
    begin
      match LabelEnv.find_opt id.it context with
      | Some (Cont k') ->
         c_exp context exp1 k'
      | Some Label -> assert false
      | None -> assert false
    end
  | RetE exp1 ->
    begin
      match LabelEnv.find_opt id_ret context with
      | Some (Cont k') ->
         c_exp context exp1 k'
      | Some Label -> assert false
      | None -> assert false
    end
  | AsyncE exp1 ->
     (* add the implicit return label *)
     let k_ret = fresh_cont (typ exp1) in
     let context' = LabelEnv.add id_ret (Cont (ContVar k_ret)) LabelEnv.empty in
     k -@- (prim_async (typ exp1) -*- (k_ret --> (c_exp context' exp1 (ContVar k_ret))))
  | AwaitE exp1 ->
     letcont k
       (fun k ->
         match eff exp1 with
         | T.Triv ->
            prim_await (typ exp) -*- (tupE [t_exp context exp1;k])
         | T.Await ->
            c_exp context  exp1
              (meta (typ exp1) (fun v1 -> (prim_await (typ exp) -*- (tupE [v1;k]))))
       )
  | AssertE exp1 ->
    unary context k (fun v1 -> e (AssertE v1)) exp1
  | DeclareE (id, typ, exp1) ->
    unary context k (fun v1 -> e (DeclareE (id, typ, v1))) exp1
  | DefineE (id, mut, exp1) ->
    unary context k (fun v1 -> e (DefineE (id, mut, v1))) exp1
  | NewObjE _ -> exp

and c_block context decs k =
  declare_decs decs (c_decs context decs k)

and c_dec context dec (k:kont) =
  match dec.it with
  | ExpD exp ->
    begin
      match eff exp with
      | T.Triv -> k -@- (t_exp context exp)
      | T.Await -> c_exp context exp k
    end
  | TypD _ ->
    k -@- unitE
  | LetD (pat,exp) ->
    let patenv,pat' = rename_pat pat in
    let block exp =
      let dec_pat' = {dec with it = LetD(pat',exp)} in
      blockE ( dec_pat' :: 
              (define_pat patenv pat)
              @[expD (k -@- (tupE[]))])
    in
     begin
       match eff exp with
       | T.Triv ->
         block (t_exp context exp)
       | T.Await ->
         c_exp context exp (meta (typ exp)
                              (fun v -> block v))
     end
  | VarD (id,exp) ->
    begin
      match eff exp with
      | T.Triv ->
        k -@- define_idE id varM (t_exp context exp)
      | T.Await ->
        c_exp context exp
          (meta (typ exp)
             (fun v -> k -@- define_idE id varM v))
    end
  | FuncD  (_, id, _ (* typbinds *), _ (* pat *), _ (* typ *), _ (* exp *) ) ->
    let func_typ = typ dec in
    let v = fresh_id func_typ in
    let u = fresh_id T.unit in
    blockE [letD v (decE (t_dec context dec));
            letD u (define_idE id constM v);
            expD (k -@- v)]


and c_decs context decs k =
  match decs with
  | [] ->
    k -@- unitE
  | [dec] ->  c_dec context dec k
  | (dec :: decs) ->
     c_dec context dec (meta (typ dec) (fun v-> c_decs context decs k))

(* Blocks and Declarations *)

and declare_dec dec exp : exp =
  match dec.it with
  | ExpD _
  | TypD _ -> exp
  | LetD (pat, _) -> declare_pat pat exp
  | VarD (id, exp1) -> declare_id id (T.Mut (typ exp1)) exp
  | FuncD (_, id, _, _, _, _) -> declare_id id (typ dec) exp

and declare_decs decs exp : exp =
  match decs with
  | [] -> exp
  | dec :: decs' ->
    declare_dec dec (declare_decs decs' exp)

(* Patterns *)

and declare_id id typ exp =
  declare_idE id typ exp

and declare_pat pat exp : exp =
  match pat.it with
  | WildP | LitP  _ ->  exp
  | VarP id -> declare_id id pat.note exp
  | TupP pats -> declare_pats pats exp
  | OptP pat1 -> declare_pat pat1 exp
  | AltP (pat1, pat2) -> declare_pat pat1 exp

and declare_pats pats exp : exp =
  match pats with
  | [] -> exp
  | pat :: pats' ->
    declare_pat pat (declare_pats pats' exp)

and rename_pat pat =
  let (patenv,pat') = rename_pat' pat in
  (patenv, { pat with it = pat' })

and rename_pat' pat =
  match pat.it with
  | WildP
  | LitP _ -> (PatEnv.empty, pat.it)
  | VarP id ->
    let v = fresh_id pat.note in
    (PatEnv.singleton id.it v,
     VarP (id_of_exp v))
  | TupP pats ->
    let (patenv,pats') = rename_pats pats in
    (patenv,TupP pats')
  | OptP pat1 ->
    let (patenv,pat1) = rename_pat pat1 in
    (patenv, OptP pat1)
  | AltP (pat1,pat2) ->
    assert(Freevars.S.is_empty (snd (Freevars.pat pat1)));
    assert(Freevars.S.is_empty (snd (Freevars.pat pat2)));
    (PatEnv.empty,pat.it)

and rename_pats pats =
  match pats with
  | [] -> (PatEnv.empty,[])
  | (pat :: pats) ->
    let (patenv1, pat') = rename_pat pat in
    let (patenv2, pats') = rename_pats pats in
    (PatEnv.disjoint_union patenv1 patenv2, pat' :: pats')

and define_pat patenv pat : dec list =
  match pat.it with
  | WildP
  | LitP _ ->
    []
  | VarP id ->
    [ expD (define_idE id constM (PatEnv.find id.it patenv)) ]
  | TupP pats -> define_pats patenv pats
  | OptP pat1 -> define_pat patenv pat1
  | AltP (pat1, pat2) ->
    assert(Freevars.S.is_empty (snd (Freevars.pat pat1)));
    assert(Freevars.S.is_empty (snd (Freevars.pat pat2)));
    []

and define_pats patenv (pats : pat list) : dec list =
  List.concat (List.map (define_pat patenv) pats)

and t_prog prog:prog = { prog with it = t_decs LabelEnv.empty prog.it }

let check_exp env exp =
  match exp.it with
  | AwaitE _ -> Check_ir.error env exp.at "invalid await"
  | AsyncE _ -> Check_ir.error env exp.at "invalid async"
  | _ -> ()

let check_prog scope prog =
  let env = { (Check_ir.env_of_scope scope) with
              Check_ir.check_exp = check_exp }
  in
  Check_ir.check_prog env prog

let transform scope prog =
  let prog = t_prog prog in
  check_prog scope prog;
  prog;

