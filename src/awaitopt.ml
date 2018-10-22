open Syntax
open Source
module T = Type
module R = Rename             

(* TODO: 
- avoid admin. reductions, perhaps by optimizing c_* in tail positions
- is async<T> shareable or not?
- consider introducing async function type, removing AsyncE and then allocating async<T> values on caller side, not callee side.
- consider using labels for (any) additional continuation arguments.
*)
             

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
  | AnnotE (exp1, _) 
  | LoopE (exp1, None) ->  
    effect_exp exp1 
  | BinE (exp1, _, exp2)
  | IdxE (exp1, exp2)
  | IsE (exp1, exp2) 
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
    T.Triv
  | AwaitE exp1 ->
    T.Await 
  | DecE d ->
     effect_dec d
  | DeclareE (_, _, exp1) ->
     effect_exp exp1
  | DefineE (_, _, exp1) ->
     effect_exp exp1
  | NewObjE _ ->
     T.Triv
    
and effect_cases cases =
  match cases with
  | [] ->
    T.Triv
  | {it = {pat; exp}; _}::cases' ->
    let e = effect_exp exp in
    max_eff e (effect_cases cases')

and effect_field_exps efs =
  List.fold_left (fun e (fld:exp_field) -> max_eff e (effect_exp fld.it.exp)) T.Triv efs
                 
and effect_dec dec =
  dec.note.note_eff

and infer_effect_dec dec =    
  match dec.it with
  | ExpD e
  | LetD (_,e) 
  | VarD (_, e) ->
    effect_exp e
  | TypD (v, tps, t) ->
    T.Triv
  | FuncD (v, tps, p, t, e) ->
    T.Triv
  | ClassD (a, v, tps, p, efs) ->
    T.Triv

(* sugar *)                      
let typ e = e.note.note_typ                   

let eff e = e.note.note_eff

let typ_dec dec = dec.note.note_typ
                          
(* the translation *)
let is_triv (exp:exp)  =
    eff exp = T.Triv
              
let answerT = Type.unit
let contT typ = T.Func(T.Call, [], typ, answerT)
let cpsT typ = T.Func(T.Call, [], contT typ, answerT)

(* identifiers *)
let exp_of_id name typ =
  {it = VarE (name@@no_region);
   at = no_region;
   note = {note_typ = typ;
           note_eff = T.Triv}
  } 
                
(* primitives *)
let primE name typ =
  {it = PrimE name;
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


let id_of_exp x =
  match x.it with
  | VarE x -> x
  | _ -> failwith "Impossible: id_of_exp"

let idE id typ =
  {it = VarE id;
   at = no_region;
   note = {note_typ = typ;
           note_eff = T.Triv}
  }
   

(* TBR: require shareable typ? *)                                  
let prim_async typ = primE "@async" (T.Func(T.Call,[], cpsT typ, T.Async typ))
let prim_await typ = 
  primE "@await" (T.Func(T.Call, [], T.Tup [T.Async typ; contT typ], T.unit))
let prim_actor_field_unit typ = 
  primE "@actor_field_unit" (T.Func(T.Call, [], T.Tup [T.Prim T.Text; typ], typ))
let prim_actor_field_async typ = 
  primE "@actor_field_async" (T.Func(T.Call, [], T.Tup [T.Prim T.Text; typ], typ))
        
let actor_field typ =
  match typ with
  | T.Func (_, _, _, T.Tup []) ->
     prim_actor_field_unit typ
  | T.Func (_, _, _, T.Async _) ->
     prim_actor_field_async typ
  | _ -> assert false
  
let varP x = {x with it=VarP (id_of_exp x)}
let letD x exp = { exp with it = LetD (varP x,exp) }
let varD x exp = { exp with it = VarD (x,exp) }                   
let textE s =
  {
    it = LitE (ref (TextLit s));
    at = no_region;
    note = {note_typ = T.Prim T.Text;
            note_eff = T.Triv;}
  }
    
let letE x exp1 exp2 = 
  { it = BlockE [letD x exp1;
                 {exp2 with it = ExpD exp2}];
    at = no_region;
    note = {note_typ = typ exp2;
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
  { it = IfE (exp1, exp2, exp3);
    at = no_region;
    note = {note_typ = typ;
            note_eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
           }           
  }
let dotE exp1 id typ =
  { it = DotE (exp1,{it=id;at=no_region;note=()});
    at = no_region;
    note = {note_typ = typ;
            note_eff = eff exp1}
  }
let switch_optE exp1 exp2 pat exp3 typ =
  { it = SwitchE (exp1,
                  [{it = {pat = {it = LitP (ref NullLit); 
                                 at = no_region;
                                 note = {note_typ = exp1.note.note_typ;
                                         note_eff = T.Triv}};
                          exp = exp2};
                    at = no_region;
                    note = ()};
                   {it = {pat = {it = OptP pat; 
                                 at = no_region;
                                 note = {note_typ = exp1.note.note_typ;
                                         note_eff = T.Triv}};
                          exp = exp3};
                    at = no_region;
                    note = ()}]
           );
    at = no_region;
    note = {note_typ = typ;
            note_eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
           }
  }
    
let expD exp =  {exp with it = ExpD exp}
let tupE exps =
   let effs = List.map effect_exp exps in
   let eff = List.fold_left max_eff Type.Triv effs in
   {it = TupE exps;
    at = no_region;
    note = {note_typ = T.Tup (List.map typ exps);
            note_eff = eff}
   }

let declare_idE x typ exp1 =
  { it = DeclareE (x, typ, exp1);
    at = no_region;
    note = exp1.note;
   }

let define_idE x mut exp1 =
  { it = DefineE (x, mut @@ no_region, exp1);
    at = no_region;
    note = { note_typ = T.unit;
             note_eff =T.Triv}
  }

let newObjE  typ sort ids =
  { it = NewObjE (sort, ids);
    at = no_region;
    note = { note_typ = typ;
             note_eff = T.Triv}
  }

let fresh_cont typ = fresh_id (contT typ)

(* Lambda abstraction *)
                          
let  (-->) k e =
  match k.it with
  | VarE v ->
     let note = {note_typ = T.Func(T.Call, [], typ k, typ e);
                 note_eff = T.Triv} in
     {it=DecE({it=FuncD("" @@ no_region, (* no recursion *)
                        [],
                        {it=VarP v;at=no_region;note=k.note},
                        PrimT "Any"@@no_region, (* bogus,  but we shouln't use it anymore *)
                        e);
               at = no_region;
               note;}
            );
     at = e.at;
     note;
    }
  | _ -> failwith "Impossible: -->"
            
(* Lambda application *)
    
let ( -*- ) exp1 exp2 =
  match exp1.note.note_typ with
  | Type.Func(_, [], _, t) ->
     {it = CallE(exp1, [], exp2);
      at = no_region;
      note = {note_typ = t;
              note_eff = max_eff (eff exp1) (eff exp2)}
     }
  | typ1 -> failwith
           (Printf.sprintf "Impossible: \n func: %s \n : %s arg: \n %s"
              (Wasm.Sexpr.to_string 80 (Arrange.exp exp1))
              (Type.string_of_typ typ1)
              (Wasm.Sexpr.to_string 80 (Arrange.exp exp2)))


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
       letE k' (v --> cont v) (* at this point, I'm really worried about variable capture *)
       (scope k')                          

(* the empty identifier names the implicit return label *)
let id_ret = "" 
    

let ( -@- ) k exp2 =
  match k with
  | ContVar exp1 ->
     exp1 -*- exp2
  | MetaCont (typ,k) ->
     let u = fresh_id typ in
     letE u exp2
       (k  u)
     (* k exp2 *)                         
           
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
     let exp1 = R.exp R.Renaming.empty exp1 in (* rename all bound vars apart *)
     (* add the implicit return label *)
     let k_ret = fresh_cont (typ exp1) in
     let context' = LabelEnv.add id_ret (Cont (ContVar k_ret)) LabelEnv.empty in
     (prim_async (typ exp1) -*- (k_ret --> (c_exp context' exp1 (ContVar k_ret))))
     .it                            
  | AwaitE _ -> failwith "Impossible: await" (* an await never has effect T.Triv *)
  | AssertE exp1 ->
    AssertE (t_exp context exp1)
  | IsE (exp1, exp2) ->
    IsE (t_exp context exp1, t_exp context exp2) 
  | AnnotE (exp1, typ) ->
    AnnotE (t_exp context exp1,typ)
  | DecE dec ->
    DecE (t_dec context dec)
  | DeclareE (id, typ, exp1) ->
    DeclareE (id, typ, t_exp context exp1)
  | DefineE (id, mut ,exp1) ->
    DefineE (id, mut, t_exp context exp1)
  | NewObjE (sort, ids) -> exp' 

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
    c_exp context e1 (meta (typ e1) (fun v1 -> k -@- unE v1))
  | T.Triv ->
    failwith "Impossible:unary"
    
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
          c_exp context e1 
            (meta (typ e1) (fun v1 -> nary_aux (v1::vs) es))
  in
  nary_aux [] es
                 
and c_and context k e1 e2 =
 let e2 = match eff e2 with
    | T.Triv -> k -@- t_exp context e2
    | T.Await -> c_exp context e2  k
 in
 match eff e1 with
  | T.Triv ->
    ifE (t_exp context e1)
         e2
         (k -@- boolE false)
         answerT
  | T.Await ->
    c_exp context e1 (meta (typ e1) (fun v1 ->
                                      ifE v1
                                          e2
                                          (k -@- boolE false)
                                          answerT))

and c_or context k e1 e2 =
  let e2 = match eff e2 with
    | T.Triv -> k -@- t_exp context e2
    | T.Await -> c_exp context e2 k
  in
  match eff e1 with
  | T.Triv ->
    ifE (t_exp context e1)
          (k -@- boolE true)
          e2
          answerT
  | T.Await ->
    c_exp context e1 (meta (typ e1)
                        (fun v1 ->
                          ifE v1
                            (k -@- boolE true)
                            e2
                            answerT))

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
    letE loop (v2 -->
                 ifE (t_exp context e1)
                   e2
                   (k -@- unitE)
                   answerT)
    (loop -*- unitE)
 | T.Await ->
    letE loop (v2 -->
                 c_exp context e1 (meta (T.bool)
                                     (fun v1 -> 
                                       ifE v1
                                         e2
                                         (k -@- unitE)
                                         answerT)))
    (loop -*- unitE)

and c_loop_none context k e1 =
 let loop = fresh_id (contT T.unit) in
 match eff e1 with
 | T.Triv ->
    failwith "Impossible: c_loop_none"
 | T.Await ->
    let v1 = fresh_id T.unit in                      
    letE loop (v1 -->
                c_exp context e1 (ContVar loop))
    (loop -*- unitE)

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
     letE loop (u --> letE v1 (t_exp context e1) e2)
     (loop -*- unitE)
 | T.Await ->
     letE loop (u -->
                  c_exp context e1 (meta (typ e1) (fun v1 -> e2)))
     (loop -*- unitE)

and c_for context k pat e1 e2 =
 let v1 = fresh_id (typ e1) in
 let next_typ = (T.Func(T.Call, [], T.unit, T.Opt (typ pat))) in
 let dotnext v = dotE v "next" next_typ -*- unitE in
 let loop = fresh_id (contT T.unit) in 
 let v2 = fresh_id T.unit in                    
 let e2 = match eff e2 with
    | T.Triv -> loop -*- t_exp context e2
    | T.Await -> c_exp context e2 (ContVar loop) in
 let body v1 =
   letE loop (v2 -->
                (switch_optE (dotnext v1)
                   (k -@- unitE)
                   pat e2
                   T.unit))
     (loop -*- unitE)                                          
 in
 match eff e1 with
 | T.Triv ->
    letE v1 (t_exp context e1)
      (body v1)
 | T.Await ->
    c_exp context e1 (meta (typ e1) (fun v1 -> body v1))
             
(* for object expression, we expand to a block that defines all recursive (actor) fields as locals and returns a constructed object, 
   and continue as c_exp *)             
and c_obj context exp sort id fields =
  let rec c_fields fields decs ids =
    match fields with
      | [] ->
         let decs = letD (idE id (typ exp)) (newObjE (typ exp) sort (List.rev ids)) :: decs in
         {exp with it = BlockE (List.rev decs)}
      | {it = {id; mut; priv; exp}; at; note}::fields ->
         let exp = if sort.it = T.Actor && priv.it = Public
                   then actor_field (typ exp) -*- tupE([textE id.it;exp])
                   else exp
         in
         let ids =
           match priv.it with
           | Public -> id::ids
           | Private -> ids                
         in
         match mut.it with 
         | Const -> c_fields fields ((letD (idE id (typ exp)) exp)::decs) ids
         | Var -> c_fields fields (varD id exp::decs) ids
  in
  c_exp context (c_fields fields [] [])
        

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
    c_obj context exp sort id fields k
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
    c_block context decs k
  | NotE exp1 ->
    unary context k (fun v1 -> e (NotE v1)) exp1 
  | AndE (exp1, exp2) ->
    c_and context k exp1 exp2
  | OrE (exp1, exp2) ->
    c_or context k exp1 exp2
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
         (c_exp context' exp1 (ContVar k)) (* TODO optimize me, if possible *))
  | BreakE (id, exp1) ->
    begin
      match LabelEnv.find_opt id.it context with
      | Some (Cont k') ->
         c_exp context exp1 k'
      | Some Label -> failwith "c_exp: Impossible"
      | None -> failwith "c_exp: Impossible"
    end
  | RetE exp1 ->
    begin
      match LabelEnv.find_opt id_ret context with
      | Some (Cont k') ->
         c_exp context exp1 k'                   
      | Some Label -> failwith "c_exp: Impossible"
      | None -> failwith "c_exp: Impossible"
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
            prim_await (typ exp1) -*- (tupE [t_exp context exp1;k])
         | T.Await ->
            c_exp context  exp1
              (meta (typ exp1) (fun v1 -> (prim_await (typ exp1) -*- (tupE [v1;k]))))
       )
  | AssertE exp1 ->
    unary context k (fun v1 -> e (AssertE v1)) exp1  
  | IsE (exp1, exp2) ->
    binary context k (fun v1 v2 -> e (IsE (v1,v2))) exp1 exp2
  | AnnotE (exp1, typ) ->
    (* TBR just erase the annotation instead? *)
    unary context k (fun v1 -> e (AnnotE (v1,typ))) exp1  
  | DecE dec ->
    c_dec context dec k  
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
     | T.Triv -> (k -@- (t_exp context exp))
     | T.Await -> c_exp context exp k 
     end                                       
  | TypD _ ->
     k -@- unitE
  | LetD (pat,exp) ->
     let patenv,pat' = rename_pat pat in
     let dec' v = {dec with it = LetD(pat',v)} in
     (* TBR should we worry about duplicating v? *)
     let block v =
       { it = BlockE ((dec' v::define_pat patenv pat)@[{v with it = ExpD v}]);
         at = no_region;
         note = {note_typ = typ exp;
                 note_eff = eff exp}
       }
     in
     begin
     match eff exp with
     | T.Triv ->
        let v = fresh_id (typ exp) in
        letE v (t_exp context exp)
          (k -@- block v)
     | T.Await ->
        let v = fresh_id (typ exp) in
        c_exp context exp (meta (typ exp)
                             (fun w -> k -@- letE v w (block v)))
     end                                       
  | VarD (id,exp) ->
     begin
     match eff exp with
     | T.Triv ->
        k -@- define_idE id Var (t_exp context exp)
     | T.Await ->
        c_exp context exp
          (meta (typ exp)
             (fun v -> k -@- define_idE id Var v))
     end                                       
  | FuncD  (id, _ (* typbinds *), _ (* pat *), _ (* typ *), _ (* exp *) ) 
  | ClassD (id, _ (* typbinds *), _ (* sort *), _ (* pat *), _ (* fields *) ) ->     
     (* todo: use a block not lets as in LetD *)
    let func_typ = typ_dec dec in
    let v = fresh_id func_typ in 
    let u = fresh_id T.unit in
    letE v ({it = DecE (t_dec context dec);
             at = no_region;
             note = {note_typ = func_typ;
                     note_eff = T.Triv}})
      (letE u (define_idE id Const v)
         (k -@- v))


and c_decs context decs k =
  match decs with
  |  [] ->
     k -@- unitE
  | [dec] ->  c_dec context dec k
  | (dec::decs) ->
     c_dec context dec (meta (typ dec) (fun v-> c_decs context decs k))
  
(* and c_fields context fields = 
  List.map (fun (field:exp_field) ->
      { field with it = { field.it with exp = c_exp context field.it.exp }})
    fields *)
           
(* Blocks and Declarations *)

and declare_dec dec exp : exp =     
  match dec.it with
  | ExpD _
  | TypD _ -> exp
  | LetD (pat, _) -> declare_pat pat exp
  | VarD (id, exp1) -> declare_id id (T.Mut (typ exp1)) exp
  | FuncD (id, _, _, _, _)
  | ClassD (id, _, _, _, _) -> declare_id id (typ_dec dec) exp

and declare_decs decs exp : exp =
  match decs with
  | [] -> exp
  | dec::decs' ->
    declare_dec dec (declare_decs decs' exp)

(* Patterns *)

and declare_id id typ exp =
  declare_idE id typ exp
              
and declare_pat pat exp : exp =
  match pat.it with
  | WildP | LitP _ | SignP _ ->  exp
  | VarP id -> declare_id id (pat.note.note_typ) exp
  | TupP pats -> declare_pats pats exp
  | OptP pat1 -> declare_pat pat1 exp
  | AltP (pat1, pat2) -> declare_pat pat1 exp
  | AnnotP (pat1, _typ) -> declare_pat pat1 exp

and declare_pats pats exp : exp =
  match pats with
  | [] -> exp
  | pat::pats' ->
    declare_pat pat (declare_pats pats' exp)

and rename_pat pat =
  let (patenv,pat') = rename_pat' pat in
  (patenv,{pat with it = pat'})

and rename_pat' pat =
  match pat.it with
  | WildP -> (PatEnv.empty, pat.it)
  | LitP _ | SignP _ -> (PatEnv.empty, pat.it)
  | VarP id ->
     let v = fresh_id pat.note.note_typ in
     (PatEnv.singleton id.it v,
      VarP (id_of_exp v))
  | TupP pats -> let (patenv,pats') = rename_pats pats in
                 (patenv,TupP pats')
  | OptP pat1 ->
     let (patenv,pat1) = rename_pat pat1 in
     (patenv, OptP pat1) 
  | AltP (pat1,pat2) ->
    (* TBR this assumes pat1 and pat2 bind no variables; add an assert to check?*)
    (PatEnv.empty,pat.it) 
  | AnnotP (pat1, _typ) ->
     let (patenv,pat1) = rename_pat pat1 in
     (patenv, AnnotP( pat1, _typ))

and rename_pats pats =
    match pats with
    | [] -> (PatEnv.empty,[])
    | (pat::pats) ->
       let (patenv1,pat') = rename_pat pat in
       let (patenv2,pats') = rename_pats pats in
       (PatEnv.disjoint_union patenv1 patenv2, pat'::pats')
     
and define_pat patenv pat : dec list =
  match pat.it with
  | WildP -> []
  | LitP _ | SignP _ ->
    []
  | VarP id ->
     [ let d = define_idE id Const (PatEnv.find id.it patenv) in
       {d with it = ExpD d}  
     ]
  | TupP pats -> define_pats patenv pats  
  | OptP pat1 -> define_pat patenv pat1
  | AltP _ -> []
  | AnnotP (pat1, _typ) -> define_pat patenv pat1 

and define_pats patenv (pats : pat list) : dec list =
  List.concat (List.map (define_pat patenv) pats)

and t_prog prog:prog = {prog with it = t_decs LabelEnv.empty prog.it}
