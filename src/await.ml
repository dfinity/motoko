open Syntax
open Source
module T = Type


(* a simple effect analysis to annote expressions as Triv(ial) (await-free) or Await (containing unprotected awaits) *)

(* in future we could merge this with the type-checker
   but I prefer to keep it mostly separate for now *)

let max_eff e1 e2 =
  match e1,e2 with
  | T.Triv,T.Triv -> T.Triv
  | _ , T.Await -> T.Await
  | T.Await,_ -> T.Await

let effect_exp (exp:Syntax.exp) : T.eff =
   exp.note.note_eff

     
(* infer the effect of an expression, assuming all sub-expressions are correctly effect-annotated *)
let rec infer_effect_exp (exp:Syntax.exp) : T.eff =
  match exp.it with
  | PrimE _
  | VarE _ 
  | LitE _ ->
    T.Triv
  | UnE (_, exp1)
  | ProjE (exp1, _)
  | OptE exp1
  | DotE (exp1, _)
  | NotE exp1
  | AssertE exp1 
  | LabelE (_, _, exp1) 
  | BreakE (_, exp1) 
  | RetE exp1   
  | IsE (exp1, _) 
  | AnnotE (exp1, _) 
  | LoopE (exp1, None) -> 
    effect_exp exp1
  | BinE (exp1, _, exp2)
  | IdxE (exp1, exp2) 
  | RelE (exp1, _, exp2) 
  | AssignE (exp1, exp2) 
  | CallE (exp1, _, exp2) 
  | AndE (exp1, exp2)
  | OrE (exp1, exp2) 
  | WhileE (exp1, exp2) 
  | LoopE (exp1, Some exp2) 
  | ForE (_, exp1, exp2)-> 
    let t1 = effect_exp exp1 in
    let t2 = effect_exp exp2 in
    max_eff t1 t2
  | TupE exps 
  | ArrayE exps ->
    let es = List.map effect_exp exps in
    List.fold_left max_eff Type.Triv es
  | BlockE decs ->
    let es = List.map effect_dec decs in
    List.fold_left max_eff Type.Triv es 
  | ObjE (_, _, efs) ->
    effect_field_exps efs 
  | IfE (exp1, exp2, exp3) ->
    let e1 = effect_exp exp1 in
    let e2 = effect_exp exp2 in
    let e3 = effect_exp exp3 in
    max_eff e1 (max_eff e2 e3)
  | SwitchE (exp1, cases) ->
    let e1 = effect_exp exp1 in
    let e2 = effect_cases cases in
    max_eff e1 e2
  | AsyncE exp1 ->
    let _ = effect_exp exp1 in
    T.Triv
  | AwaitE exp1 ->
    let _ = effect_exp exp1 in
    T.Await 
  | DecE d ->
    effect_dec d
    
and effect_cases cases =
  match cases with
  | [] ->
    T.Triv
  | {it = {pat; exp}; _}::cases' ->
    let e = effect_exp exp in
    max_eff e (effect_cases cases')

(*  TBD  
and effect_block es =
  List.fold_left max_eff T.Triv es
 *)
            
and effect_field_exps efs =
  List.fold_left (fun e (fld:exp_field) -> max_eff e (effect_exp fld.it.exp)) T.Triv efs

and effect_dec d =
  match d.it with
  | ExpD e
  | LetD (_,e) 
  | VarD (_, e) ->
    effect_exp e
  | TypD (v, tps, t) ->
    T.Triv
  | FuncD (v, tps, p, t, e) ->
    let _ = effect_exp e in
    T.Triv
  | ClassD (a, v, tps, p, efs) ->
    effect_field_exps efs 

(* sugar *)                      
let typ e = e.note.note_typ                   
let eff e = e.note.note_eff
                      
(* the translation *)
let is_triv (exp:exp)  =
    eff exp = T.Triv
              
let answerT = Type.unit
let contT typ = T.Func([],typ,answerT)

(* primitives *)
let exp_of_id name typ =
  {it = VarE (name@@no_region);
   at = no_region;
   note = {note_typ = typ;
           note_eff = T.Triv}
  } 
                
              
let id_stamp = ref 0

let fresh_id typ =
  let name = Printf.sprintf "$%i" (!id_stamp) in
  let k = exp_of_id name typ in
  (id_stamp := !id_stamp + 1;
   k)

let fresh_cont typ = fresh_id (contT typ)

(* the empty identifier names the implicit return label *)
let id_ret = "" 
    
(* Lambda and continuation abstraction *)
                          
let  (-->) k e =
  match k.it with
  | VarE v ->
     {it=DecE(FuncD("" @@ no_region, (* no recursion *)
                [],
                {it=VarP v;at=no_region;note=k.note},
                AnyT@@no_region, (* bogus,  but we shouln't use it anymore *)
                e)@@no_region
            );
     at = e.at;
     note = {note_typ = Type.Func([],typ k,typ e);
             note_eff = T.Triv}
    }
  | _ -> failwith "Impossible"

let id_of_exp x =
  match x.it with
  | VarE x -> x
  | _ -> failwith "Impossible"

(*                  
let prim_make_async = fresh_id (T.Func([{T.var="t";T.bound=T.Any}],
                                  T.unit,
                                  T.Async (T.Var ("t",0))))
let prim_set_async = fresh_id (T.Func([{T.var="t";T.bound=T.Any}],
                                  T.Tup [T.Async (T.Var ("t",0)); T.Var("t",0)],
                                  T.unit))
let prim_get_async = fresh_id (T.Func([{T.var="t";T.bound=T.Any}],
                                  T.Tup [T.Async (T.Var ("t",0)); contT (T.Var("t",0))],
                                  T.unit))
let prim_scheduler_queue = fresh_id (T.Func([],
                                  T.Func([],T.unit,T.unit),
                                  T.unit))
let prim_sheduler_yield = fresh_id (T.Func([], T.unit, T.unit))
*)                         

let prim_of_id name typ = exp_of_id ("@" ^ name) typ
let prim_make_async typ =
  prim_of_id "make_async" (T.Func([], T.unit,T.Async typ))
let prim_set_async typ =
  prim_of_id "set_async" (T.Func([],T.Tup [T.Async typ; typ], T.unit))
let prim_get_async typ = 
  prim_of_id "get_async" (T.Func([], T.Tup [T.Async typ; contT typ], T.unit))
let prim_scheduler_queue  =
  prim_of_id "scheduler_queue" (T.Func([],T.Func([],T.unit,T.unit), T.unit))
let prim_sheduler_yield = 
  prim_of_id "scheduler_yield" (T.Func([],T.unit, T.unit))


let decE dec =
  let note =
      match dec.it with
      | LetD (_,exp) ->
        exp.note
      | ExpD exp ->
        exp.note 
      | VarD (id,exp) ->
        {note_typ = T.Mut (typ exp);
         note_eff = eff exp}
      | _ -> failwith "NYI"
   in
   { it = DecE dec;
     at = no_region;
     note = note;
   }

        
(* let varP x = {it=VarP (id x); at = x.at; note = x.note} *)
let varP x = {x with it=VarP (id_of_exp x)}
let letD x exp = LetD (varP x,exp) @@ no_region
let letE x exp1 exp2 =
  { it = BlockE [LetD (varP x,exp1) @@ no_region;
                 ExpD exp2 @@ no_region];
    at = no_region;
    note = {note_typ = typ exp1;
            note_eff = max_eff (eff exp1) (eff exp2)}           
  }

let unitE =
  { it = TupE [];
    at = no_region;
    note = {note_typ = T.Tup [];
            note_eff = T.Triv}
  }
  
let boolE b =
  { it = LitE (ref (BoolLit true));
    at = no_region;
    note = {note_typ = T.bool;
            note_eff = T.Triv}
  }
let ifE exp1 exp2 exp3 typ =
  { it = IfE (exp1,exp2, exp3);
    at = no_region;
    note = {note_typ = typ;
            note_eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
           }           
  }
let expD exp = ExpD exp @@ no_region                                  
let tupE exps =
   let effs = List.map effect_exp exps in
   let eff = List.fold_left max_eff Type.Triv effs in
   {it = TupE exps;
    at = no_region;
    note = {note_typ = T.Tup (List.map typ exps);
            note_eff = eff}
   }
                             
            
(* Lambda and continuation application *)
    
let ( -@- ) e e =
  match e.note.note_typ with
  | Type.Func([],_,t) ->
     {it = CallE(e,[],e);
      at = no_region;
      note = {note_typ = t;
              note_eff = Type.Triv}
     }
  | _ -> failwith "Impossible"
                           
(* Label environments *)

module LabelEnv = Env.Make(String) 
             
type label_sort = Cont of exp | Label


(* trivial translation of pure terms (eff = T.Triv) *)                                  

let rec t_exp context exp =
  assert (eff exp = T.Triv);
  { exp with it = t_exp' context exp.it }
and t_exp' context exp' =
  match exp' with
  | PrimE _
  | VarE _ 
  | LitE _ -> exp'
  | UnE (op, exp1) ->
    UnE (op, t_exp context exp1)
  | BinE (exp1, op, exp2) ->
    BinE (t_exp context exp1, op, t_exp context exp2)
  | RelE (exp1, op, exp2) ->
    RelE (t_exp context exp1, op, t_exp context exp2)
  | TupE exps ->
    TupE (List.map (t_exp context) exps)
  | OptE exp1 ->
    OptE (t_exp context exp1)
  | ProjE (exp1, n) ->
    ProjE (t_exp context exp1, n)
  | ObjE (sort, id, fields) ->
    let fields' = t_fields context fields in                    
    ObjE (sort, id, fields')
  | DotE (exp1, id) ->
    DotE (t_exp context exp1, id)
  | AssignE (exp1, exp2) ->
    AssignE (t_exp context exp1, t_exp context exp2)
  | ArrayE exps ->
    ArrayE (List.map (t_exp context) exps)
  | IdxE (exp1, exp2) ->
     IdxE (t_exp context exp1, t_exp context exp2)
  | CallE (exp1, typs, exp2) ->
    CallE (t_exp context exp1, typs, t_exp context exp2)
  | BlockE decs ->
     BlockE (t_decs context decs)
  | NotE exp1 ->
    NotE (t_exp context exp1)     
  | AndE (exp1, exp2) ->
    AndE (t_exp context exp1, t_exp context exp2)
  | OrE (exp1, exp2) ->
    OrE (t_exp context exp1, t_exp context exp2)
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
      | None -> failwith "t_exp: Impossible"
    end
  | RetE exp1 ->
    begin
      match LabelEnv.find_opt id_ret context with
      | Some (Cont k) -> RetE (k -@- (t_exp context exp1))
      | Some Label -> RetE (t_exp context exp1)
      | None -> failwith "t_exp: Impossible"
    end
  | AsyncE exp1 ->
     let async = fresh_id (T.Async (typ exp1)) in
     let typ1 = typ exp1 in
     let v1 = fresh_id (typ exp1) in
     let k' = fresh_cont (typ exp1) in
     let u1 = fresh_id T.unit in
     let u2 = fresh_id T.unit in
     let context' = LabelEnv.add id_ret (Cont k') LabelEnv.empty in
     (letE async (prim_make_async typ1 -@- tupE [])
      (letE k' (v1 --> prim_set_async typ1 -@- (tupE [async;v1]))
         (letE u1 (prim_scheduler_queue -@- (u2 --> c_exp context' exp1 -@- k'))
            async)))
       .it
  | AwaitE _ -> failwith "Impossible" (* an await never has effect T.Triv *)
  | AssertE exp1 ->
    AssertE (t_exp context exp1)
  | IsE (exp1, typ) ->
    IsE (t_exp context exp1,typ) 
  | AnnotE (exp1, typ) ->
    AnnotE (t_exp context exp1,typ)
  | DecE dec ->
    DecE (t_dec context dec) 

and t_block context decs : dec list= 
  List.map (t_dec context) decs

and t_dec context dec =
  {dec with it = t_dec' context dec.it}
and t_dec' context dec' =
  match dec' with
  | ExpD exp -> ExpD (t_exp context exp)
  | TypD _ -> dec'
  | LetD (pat,exp) -> LetD (pat,t_exp context exp)
  | VarD (id,exp) -> VarD (id,t_exp context exp)
  | FuncD (id,typbinds, pat, typ, exp) ->
    let context' = LabelEnv.add id_ret Label LabelEnv.empty in
    FuncD (id, typbinds, pat, typ,t_exp context' exp)
  | ClassD (id, typbinds, sort, pat, fields) ->
    let context' = LabelEnv.add id_ret Label LabelEnv.empty in     
    let fields' = t_fields context' fields in             
    ClassD (id, typbinds, sort, pat, fields')
and t_decs context decs = List.map (t_dec context) decs           
and t_fields context fields = 
  List.map (fun (field:exp_field) ->
      { field with it = { field.it with exp = t_exp context field.it.exp }})
    fields

(* non-trivial translation of possibly impure terms (eff = T.Await) *)

and unary context k unE e1 =
  match eff e1 with
  | T.Await ->
    let v1 = fresh_id (typ e1) in
    k -->  (c_exp context e1) -@-
             (v1 --> k -@- unE v1)
  | T.Triv ->
    failwith "Impossible:unary"
    
and binary context k binE e1 e2 =
  match eff e1, eff e2 with
  | T.Triv, T.Await ->
    let v1 = fresh_id (typ e1) in
    let v2 = fresh_id (typ e2) in     
    k -->  letE v1 (t_exp context e1)
                   ((c_exp context e2) -@-
                    (v2 --> k -@- binE v1 v2))
  | T.Await, T.Await ->
    let v1 = fresh_id (typ e1) in
    let v2 = fresh_id (typ e2) in     
    k -->  (c_exp context e1) -@-
             (v1 --> (c_exp context e2) -@-
                      (v2 --> k -@- binE v1 v2))
  | T.Await, T.Triv ->
    let v1 = fresh_id (typ e1) in
    k -->  (c_exp context e1) -@-
             (v1 --> k -@- binE v1 (t_exp context e2))
  | T.Triv, T.Triv ->
    failwith "Impossible:binary";  

and nary context k naryE es =
  let rec nary_aux vs es  =
    match es with
    | [] -> k -@- naryE (List.rev vs)
    | [e1] when eff e1 = T.Triv ->
       (* TBR: optimization - no need to name the last trivial argument *)
       k -@- naryE (List.rev (e1::vs))
    | e1::es ->
       match eff e1 with
       | T.Triv ->
          let v1 = fresh_id (typ e1) in
          letE v1 (t_exp context e1)
            (nary_aux (v1::vs) es)
       | T.Await ->
          let v1 = fresh_id (typ e1) in
          (c_exp context e1) -@-
            (v1 --> nary_aux (v1::vs) es)
  in
  k --> nary_aux [] es
                 
and c_and context k e1 e2 =
 let e2 = match eff e2 with
    | T.Triv -> k -@- t_exp context e2
    | T.Await -> c_exp context e2 -@- k
 in
 match eff e1 with
  | T.Triv ->
    k -->  ifE (t_exp context e1)
               e2
               (k -@- boolE false)
               answerT
  | T.Await ->
    let v1 = fresh_id (typ e1) in
    k -->  (c_exp context e1) -@-
            (v1 -->
               ifE v1
                 e2
                 (k -@- boolE false)
                 answerT)

and c_or context k e1 e2 =
  let e2 = match eff e2 with
    | T.Triv -> k -@- t_exp context e2
    | T.Await -> c_exp context e2 -@- k
  in
  match eff e1 with
  | T.Triv ->
    k -->  ifE (t_exp context e1)
               (k -@- boolE true)
               e2
               answerT
  | T.Await ->
    let v1 = fresh_id (typ e1) in
    k -->  (c_exp context e1) -@-
            (v1 -->
               ifE v1
                 (k -@- boolE true)
                 e2
                 answerT)

and c_if context k e1 e2 e3 =
  let trans_branch exp = match eff exp with
    | T.Triv -> k -@- t_exp context exp
    | T.Await -> c_exp context exp -@- k
  in
  let e2 = trans_branch e2 in
  let e3 = trans_branch e3 in               
  match eff e1 with
  | T.Triv ->
    k -->  ifE (t_exp context e1) e2 e3 answerT
  | T.Await ->
    let v1 = fresh_id (typ e1) in
    k -->  (c_exp context e1) -@-
      (v1 --> ifE v1 e2 e3 answerT)

and c_while context k e1 e2 =
 let loop = fresh_id (contT T.unit) in
 let v2 = fresh_id T.unit in                    
 let e2 = match eff e2 with
    | T.Triv -> loop -@- t_exp context e2
    | T.Await -> c_exp context e2 -@- loop
 in
 match eff e1 with
 | T.Triv ->
    k --> letE loop (v2 -->
                       ifE (t_exp context e1)
                         e2
                         (k -@- unitE)
                          answerT)
               (loop -@- unitE)
 | T.Await ->
    let v1 = fresh_id T.bool in                      
    k --> letE loop (v2 -->
                       (c_exp context e1) -@-
                        (v1 --> 
                           ifE v1
                             e2
                             (k -@- unitE)
                             answerT))
               (loop -@- unitE)

and c_loop_none context k e1 =
 let loop = fresh_id (contT T.unit) in
 match eff e1 with
 | T.Triv ->
    failwith "Impossible"
 | T.Await ->
    let v1 = fresh_id T.unit in                      
    k --> letE loop (v1 -->
                       (c_exp context e1) -@- loop)
               (loop -@- unitE)

and c_loop_some context k e1 e2 =
 let loop = fresh_id (contT T.unit) in
 let u = fresh_id T.unit in
 let v1 = fresh_id T.unit in
 let e2 = match eff e2 with
   | T.Triv -> ifE (t_exp context e2)
                 (loop -@- unitE)
                 (k -@- unitE)
                 answerT
   | T.Await ->
       let v2 = fresh_id T.bool in                    
       c_exp context e2 -@-
         (v2 --> ifE v2
                   (loop -@- unitE)
                   (k -@- unitE)
                   answerT)
 in
 match eff e1 with
 | T.Triv ->
     k --> letE loop (u -->
                        letE v1 (t_exp context e1)
                          e2)
             (loop -@- unitE)
 | T.Await ->
     k --> letE loop (u -->
                        (c_exp context e1) -@-
                        (v1 --> e2))
             (loop -@- unitE)
               
and c_exp context exp =
  { exp with it = (c_exp' context exp).it }
and c_exp' context exp =
  let e exp' = {it=exp'; at = exp.at; note = exp.note} in
  let k = fresh_cont (typ exp) in
  match exp.it with
  | _ when is_triv exp ->
    k --> k -@- (t_exp context exp)
  | PrimE _
  | VarE _ 
  | LitE _ ->
    assert false
  | UnE (op, exp1) ->
    unary context k (fun v1 -> e (UnE(op, v1))) exp1
  | BinE (exp1, op, exp2) ->
    binary context k (fun v1 v2 -> e (BinE (v1, op, v2))) exp1 exp2
  | RelE (exp1, op, exp2) ->
    binary context k (fun v1 v2 -> e (RelE (v1, op, v2))) exp1 exp2
  | TupE exps ->
    nary context k (fun vs -> e (TupE vs)) exps
  | OptE exp1 ->
    unary context k (fun v1 -> e (OptE v1)) exp1 
  | ProjE (exp1, n) ->
    unary context k (fun v1 -> e (ProjE (v1, n))) exp1 
  | ObjE (sort, id, fields) ->
    failwith "NYI"      
(*    let fields' = c_fields context fields in                    
    ObjE (sort, id, fields') *)
  | DotE (exp1, id) ->
    unary context k (fun v1 -> e (DotE (v1, id))) exp1 
  | AssignE (exp1, exp2) ->
    binary context k (fun v1 v2 -> e (AssignE (v1, v2))) exp1 exp2
  | ArrayE exps ->
    nary context k (fun vs -> e (ArrayE vs)) exps
  | IdxE (exp1, exp2) ->
    binary context k (fun v1 v2 -> e (IdxE (v1, v2))) exp1 exp2
  | CallE (exp1, typs, exp2) ->
    binary context k (fun v1 v2 -> e (CallE (v1, typs, v2))) exp1 exp2 
  | BlockE decs ->
    failwith "NYI"
    (* BlockE (c_decs context decs) *)
  | NotE exp1 ->
    unary context k (fun v1 -> e (NotE v1)) exp1 
  | AndE (exp1, exp2) ->
    c_and context k exp1 exp2
  | OrE (exp1, exp2) ->
    c_or context k exp1 exp2
  | IfE (exp1, exp2, exp3) ->
    c_if context k exp1 exp2 exp3 
  | SwitchE (exp1, cases) ->
    let cases' = List.map
                   (fun {it = {pat;exp}; at; note} ->
                     let exp' = match eff exp with
                       | T.Triv -> k -@- (t_exp context exp)
                       | T.Await -> (c_exp context exp) -@- k
                     in
                     {it = {pat;exp = exp' }; at; note})
                  cases
    in
    begin
    match eff exp1 with
    | T.Triv ->
       k --> {exp with it = SwitchE(t_exp context exp1, cases')}
    | T.Await ->
       let v1 = fresh_id (typ exp1) in
       c_exp context exp1 -@-
         (v1 --> {exp with it = SwitchE(v1,cases')})
    end
  | WhileE (exp1, exp2) ->
    c_while context k exp1 exp2
  | LoopE (exp1, None) ->
    c_loop_none context k exp1 
  | LoopE (exp1, Some exp2) ->
    c_loop_some context k exp1 exp2                 
  | ForE (pat, exp1, exp2) ->
    failwith "NYI" 
  (*    ForE (pat, c_exp context exp1, c_exp context exp2) *)
  | LabelE (id, _typ, exp1) ->
    failwith "NYI"
(*    let context' = LabelEnv.add id.it Label context in
    LabelE (id, _typ, c_exp context' exp1) *)
  | BreakE (id, exp1) ->
    failwith "NYI" 
(*    begin
      match LabelEnv.find_opt id.it context with
      | Some (Cont k) -> RetE (k -@- (c_exp context exp1))
      | Some Label -> BreakE (id, c_exp context exp1)
      | None -> failwith "c_exp: Impossible"
    end
*)
  | RetE exp1 ->
    failwith "NYI"
(*    begin
      match LabelEnv.find_opt id_ret context with
      | Some (Cont k) -> RetE (k -@- (c_exp context exp1))
      | Some Label -> RetE (c_exp context exp1)
      | None -> failwith "c_exp: Impossible"
    end
 *)
  | AsyncE exp1 ->
     failwith "NYI"
(*             
     let async = fresh_id (T.Async (typ exp1)) in
     let typ1 = typ exp1 in
     let v1 = fresh_id (typ exp1) in
     let k' = fresh_cont (typ exp1) in
     let u1 = fresh_id T.unit in
     let u2 = fresh_id T.unit in
     let context' = LabelEnv.add id_ret (Cont k') context in
     (letE async (prim_make_async typ1 -@- tupE [])
      (letE k' (v1 --> prim_sec_async typ1 -@- (tupE [async;v1]))
         (letE u1 (prim_scheduler_queue -@- (u2 --> c_exp context' exp1 -@- k'))
            async)))
       .it
 *)
  | AwaitE _ ->
    failwith "NYI" (* an await never has effect T.Triv *)
  | AssertE exp1 ->
    unary context k (fun v1 -> e (AssertE exp1)) exp1  
  | IsE (exp1, typ) ->
    unary context k (fun v1 -> e (IsE (v1,typ))) exp1  
  | AnnotE (exp1, typ) ->
    (* TBR - just erase the annotation instead? *)
    unary context k (fun v1 -> e (AnnotE (v1,typ))) exp1  
  | DecE dec ->
     failwith "NYI"
(*       
    DecE (c_dec context dec)  *)

and c_block context decs : dec list= 
  List.map (c_dec context) decs

and c_dec context dec =
  {dec with it = c_dec' context dec.it}
and c_dec' context dec' =
  match dec' with
  | ExpD exp -> ExpD (c_exp context exp)
  | TypD _ -> dec'
  | LetD (pat,exp) -> LetD (pat,c_exp context exp)
  | VarD (id,exp) -> VarD (id,c_exp context exp)
  | FuncD (id,typbinds, pat, typ, exp) ->
    let context' = LabelEnv.add id_ret Label context in
    FuncD (id, typbinds, pat, typ,c_exp context' exp)
  | ClassD (id, typbinds, sort, pat, fields) ->
    let fields' = c_fields context fields in             
    ClassD (id, typbinds, sort, pat, fields')
and c_decs context decs = List.map (c_dec context) decs           
and c_fields context fields = 
  List.map (fun (field:exp_field) ->
      { field with it = { field.it with exp = c_exp context field.it.exp }})
    fields
