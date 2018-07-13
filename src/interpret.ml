open Syntax
open Source
open Types
open Typing

module I32 = Wasm.I32
module I64 = Wasm.I64

module Env = Map.Make(String)

module Values =
struct

  type value =
      | NullV 
      | BoolV of bool
      | NatV of nat
      | IntV of int
      | WordV of word
      | FloatV of float
      | CharV of unicode
      | TextV of string
      | TupV of value list
      | ObjV of value Env.t
      | ArrV of value array
      | FuncV of (value -> value)
      | VarV of value ref
      | AsyncV of async
  and async = {mutable result: value option; mutable waiters : cont list}
  and cont = value -> value

  let nullV = NullV
  let boolV b = BoolV b
  let natV n = NatV n
  let intV n = IntV n
  let wordV w = WordV w
  let floatV f = FloatV f
  let charV c = CharV c
  let textV s = TextV s
  let tupV vs = TupV vs
  let objV ve = ObjV ve
  let funcV f = FuncV f
  let unitV = TupV([])
  let asyncV async = AsyncV async
  
  let projV (TupV vs) n = List.nth vs n
  let dotV (ObjV ve) v = Env.find v ve
  let assignV (VarV r) v  = r := v;unitV
  let updateV (ArrV a) (IntV i) v  = a.(i) <- v;unitV
  let indexV (ArrV a) (IntV i) = a.(i)
  let applyV (FuncV f) v = f v
  let derefV v =
      match v with
      | VarV r -> !r
      | v -> v
  let notV (BoolV b) = BoolV (not b)
  let async_of_V(AsyncV async) = async
  let bool_of_V (BoolV b) = b
end

open Values

exception Trap of Source.region * string


let lookup map k = try Some (Env.find k map)  with _ -> None (* TODO: use find_opt in 4.05 *)



type context = {values: value Env.t; constructors: con Env.t; kinds: kind ConEnv.t; label: string option;  breaks: cont Env.t; continues: cont Env.t ; returns: cont option; awaitable: bool}

let prelude = {values = Env.empty;
               constructors = Env.empty;
               kinds = ConEnv.empty;
               label = None;
               breaks = Env.empty;
               continues = Env.empty;
               returns = None;
               awaitable = false}

let addBreak context labelOpt k_break =
    match labelOpt with
    | None -> context
    | Some label -> { context with breaks = Env.add label k_break context.breaks }

let addBreakAndContinue context labelOpt k_break k_continue =
    match labelOpt with
    | None -> context
    | Some label -> {{ context with breaks = Env.add label k_break context.breaks } with continues = Env.add label k_continue context.continues }

let sprintf = Printf.sprintf

let rec inf_lit context rl =
  match !rl with
    | NullLit -> nullV
    | BoolLit b -> boolV b
    | NatLit n -> natV n
    | IntLit i -> intV i
    | WordLit w -> wordV w
    | FloatLit f -> floatV f
    | CharLit c -> charV c
    | TextLit s -> textV s
    | PreLit s -> failwith "inf_lit"

and inf_binop context e1 bop e2 = unitV
(*
    let t1 = inf_exp context e1 in
    let t2 = inf_exp context e2 in
    match bop with
    | CatOp ->
      if eq_typ context t1 (PrimT TextT) && eq_typ context t1 t2 then
         t1
      else typeError at "arguments to concatenation operator must have Text type"
    | AddOp | SubOp | MulOp | DivOp | ModOp ->
      if numeric_typ context t1 && eq_typ context t1 t2 then
         t1
      else typeError at "arguments to numeric operator must have equivalent numeric types"
    | AndOp | OrOp | XorOp | ShiftLOp | ShiftROp | RotLOp | RotROp ->
      if logical_typ context t1 && t1 = t2 then
         t1
      else typeError at "arguments to logical operator must have equivalent logical types"
    | _ -> typeError at "operator doesn't take operands of types %s and %s" (typ_to_string t1) (typ_to_string t2)
*)
and inf_relop context e1 rop e2 = unitV
(*
    let t1 = inf_exp context e1 in
    let t2 = inf_exp context e2 in
    match rop with
    | EqOp 
    | NeqOp ->
      if equatable_typ context t1 && eq_typ context t1 t2
      then boolT
      else typeError at "arguments to an equality operator must have the same, equatable type"
    | _ ->
      if comparable_typ context t1 && eq_typ context t1 t2
      then boolT
      else typeError at "arguments to a relational operator must have the same, comparable type"
*)

and inf_uop context uop e = unitV
(*
    let t = inf_exp context e in
    match uop with
    | PosOp 
    | NegOp ->
      if numeric_typ context t 
      then t
      else typeError at "argument to negation operator must have numeric type"
    | NotOp ->
      if logical_typ context t 
      then t
      else typeError at "arguments to a bitwise negation operator must have logical type"
*)
and inf_exps context vs es k =
    match es with
    | [] -> k (List.rev vs)
    | (e::es) -> inf_exp context e (fun v -> inf_exps context (v::vs) es k)
and inf_exp context e k  =
    inf_exp' context e k 
and inf_exp' context e k =
let labelOpt = context.label in
let context = {context with label = None} in
match e.it with
| VarE x ->
    k (derefV (Env.find x.it context.values))
| LitE rl ->
    k (inf_lit context rl)
| UnE(uop,e1) ->
    inf_exp context e1 (fun v1 -> k (inf_uop context uop v1))
| BinE (e1,bop,e2) ->
   inf_exp context e1 (fun v1 -> inf_exp context e2 (fun v2 -> k (inf_binop context v1 bop v2 )))
| RelE (e1,rop,e2) ->
   inf_exp context e1 (fun v1 -> inf_exp context e2 (fun v2 -> k (inf_relop context v1 rop v2 )))
| TupE es ->
    inf_exps context [] es (fun vs -> k (tupV (List.rev vs)))
| ProjE(e1,n) ->
    inf_exp context e1 (fun v1 -> k (projV v1 n))
| DotE(e1,v) ->
    inf_exp context e1 (fun v1 -> k (derefV (dotV v1 v.it)))
| AssignE(e1,e2) ->
    begin
    match e1.it with
    (*TBC: array and object update *)
    | VarE v ->
      let v1 = Env.find v.it context.values in
      inf_exp context e2 (fun v2 ->
      k (assignV v1 v2))
    | DotE(e,v) ->
      inf_exp context e1 (fun v1 ->
      inf_exp context e2 (fun v2 ->
      let loc = dotV v1 v.it in
      k(assignV loc v2)))
    | IdxE(ea,ei) ->
      inf_exp context ea (fun va ->
      inf_exp context ei (fun vi ->
      inf_exp context e2 (fun v2 -> k(updateV va vi v2))))
    end
| ArrayE es ->
    inf_exps context [] es (fun vs ->
    k (ArrV (Array.of_list (List.rev vs))))
| IdxE(e1,e2) ->
    inf_exp context e1 (fun v1 ->
    inf_exp context e2 (fun v2 ->
    k (indexV v1 v2)))
| CallE(e1,e2) ->
    inf_exp context e1 (fun v1 ->
    inf_exp context e2 (fun v2 ->
    k (applyV v1 v2)))
| BlockE es ->
    let k_break = k in
    let context' = addBreak context labelOpt k_break in
    inf_block context' es k
| NotE(e1) ->
    inf_exp context e1 (fun v -> k (notV v))
| AndE(e1,e2) ->
    inf_exp context e1
    (fun v -> if (bool_of_V v)
              then inf_exp context e2 k
              else k (boolV false))
| OrE(e1,e2) ->
    inf_exp context e1
    (fun v -> if (bool_of_V v)
              then k (boolV true)
              else inf_exp context e2 k)
| IfE(e0,e1,e2) ->
     inf_exp context e0
     (fun v -> if (bool_of_V v)
               then inf_exp context e1 k
               else inf_exp context e2 k)
| SwitchE(e,cs) ->
  failwith "NYI:SwitchE"
| WhileE(e0,e1) ->
  let e_while = e in
  inf_exp context e0
  (fun v -> let k_continue = fun v -> inf_exp context e_while k in
            let context' = addBreakAndContinue context labelOpt k k_continue in
            if (bool_of_V v)
            then inf_exp context e1 k_continue
            else k unitV)
| LoopE(e0,None) ->
  let e_loop = e in
  inf_exp context e0
  (fun v -> let k_continue = fun v -> inf_exp context e_loop k in
            let context' = addBreakAndContinue context labelOpt k k_continue in
            inf_exp context' e0 k_continue)
| LoopE(e0,Some e1) ->
  let e_loop = e in
  inf_exp context e0
  (fun v -> let k_continue =
                fun v -> inf_exp context e1
                         (fun v1 -> if (bool_of_V v1)
                                    then k(unitV)
                                    else inf_exp context e_loop k)
            in
            let context' = addBreakAndContinue context labelOpt k k_continue in
            inf_exp context' e0 k_continue)
| ForE(p,e0,e1)->
  failwith "NYI:ForE"
(* labels *)
| LabelE(l,e) ->
  let context = {context with label = Some l.it} in
  inf_exp context e k
| BreakE(l,e) ->
  let k_break = Env.find l.it context.breaks in
  inf_exp context e k_break
| ContE l ->
  let k_continue = Env.find l.it context.continues in
  k unitV
| RetE e0 ->
  let (Some k_return) = context.returns in
  inf_exp context e k_return
| AsyncE e0 ->
  let async = {result=None;waiters=[]} in
  let k_return = fun v -> async.result <- Some v;
                          let ws = async.waiters in
                          async.waiters <- [];
                          List.map (fun k -> k v) ws; 
                          unitV
  in      
  let context = {values = context.values;
                 constructors = context.constructors;
                 kinds = context.kinds;
                 breaks = Env.empty;
                 label = None;
                 continues = Env.empty;
                 returns = Some k_return; 
                 awaitable = true}
  in
  inf_exp context e0 k_return
| AwaitE e0 ->
  inf_exp context e0 (fun v ->
  let async = async_of_V v in
  match async.result with
  | Some v -> k v
  | None -> async.waiters = k::async.waiters;
  unitV) 
| AssertE e ->
  inf_exp context e (fun  v ->
  if bool_of_V v
  then k(unitV)
  else failwith  "Assert failure %" (string_of_region e.at))
| IsE(e,t) ->
  failwith "NYI:IsE"
| AnnotE(e,t) ->
  inf_exp context e k
| DecE d ->
  let _ = check_decs context [d] in
  k unitV
    
and inf_cases context pt cs t_opt  =
  match cs with
  | [] -> t_opt
  | {it={pat=p;exp=e};at}::cs ->
    let ve = check_pat context p pt in
    let t = inf_exp (union_values context ve) e in
    let t_opt' = match t_opt with
              | None -> Some t
              | Some t' ->
                 if eq_typ context t t'
                 then Some t'
                 else typeError at "illegal case of different type from preceeding cases" in
    inf_cases context pt cs t_opt'
    
and inf_block context es k =
  match es with
  | [] -> k(unitT)
  | {it = DecE d;at}::es ->
    check_decs context [d] (fun ve ->
    (* TBR: we currently evaluation decs sequentially, not recursively *)
    inf_block  (union_values context ve) es k) 
  | [e] -> inf_exp context e k
  | e::es ->
     inf_exp context e (fun v ->
     inf_block context es k)
     
and check_dec pass context d =
    let ve,ce,ke = check_dec' pass context d in    
    (* TBC store ve *)
    ve,ce,ke

and check_dec' pass context d =     
    match d.it with
    | LetD (p,e) ->
      if pass < 3 then
         Env.empty, Env.empty, ConEnv.empty
      else      
         let t = inf_exp context e in
         let ve = check_pat context p t in 
         ve, Env.empty, ConEnv.empty
    | VarD (v,t,None) ->
      if pass < 3 then
         Env.empty, Env.empty, ConEnv.empty
      else
      let t = check_typ context t in
      Env.singleton v.it (t,VarMut), Env.empty, ConEnv.empty
    | VarD (v,t,Some e) ->
      if pass < 3 then
         Env.empty, Env.empty, ConEnv.empty
      else
        let t = check_typ context t in
        check_exp context t e;
        Env.singleton v.it (t,VarMut),
        Env.empty,
        ConEnv.empty
    | TypD(v,ts,t) ->
      let ts,ce_ts,ke_ts = check_typ_binds context ts in
      let con = if pass = 0
                then Con.fresh v.it
                else
                match lookup context.constructors v.it with
                | Some con -> con
                | None -> assert(false);failwith "Impossible"
      in
      let kind0 = ParK(ts,VarT(con,[])) (* dummy abstract type *) in 
      let ce0 = Env.singleton v.it con in
      let ke0 = ConEnv.singleton con kind0 in
      if pass = 0 then
         Env.empty, ce0, ke0
      else
      let context_ts = union_kinds (union_constructors context ce_ts) ke_ts in
      let t = check_typ context_ts t in
      let kind1 = DefK(ts,t) (* dummy type *) in 
      let ce1 = Env.singleton v.it con in
      let ke1 = ConEnv.singleton con kind1 in
         Env.empty, ce1, ke1
    | FuncD(v,ts,p,t,e) ->
      if pass < 3 then
         Env.empty, Env.empty, ConEnv.empty
      else
      let ts,ce,ke = check_typ_binds context ts in
      let context_ce = union_kinds (union_constructors context ce) ke in
      let ve,dom = inf_pat context_ce p in
      let rng = check_typ context_ce t in
      let funcT= FuncT(ts,dom,rng) (* TBR: we allow polymorphic recursion *) in
      let context_ce_ve_v = 
           let {values;constructors;kinds} = add_value (union_values context_ce ve) v.it (funcT,ConstMut) in
            {values;
             constructors;
             kinds;
             label = None;
             breaks = Env.empty;
             continues = Env.empty;
             returns = Some rng;
             awaitable = false}
      in
      check_exp context_ce_ve_v rng e;
      Env.singleton v.it (funcT,ConstMut), Env.empty, ConEnv.empty
    | ClassD(a,v,ts,p,efs) ->
      let ts,ce_ts,ke_ts = check_typ_binds context ts in
      let con = if pass = 0
                then Con.fresh v.it
                else
                match lookup context.constructors v.it with
                | Some con -> con
                | None -> assert(false);failwith "Impossible"
      in
      let kind0 = ObjK(ts,a.it,[]) in
      let ce0 = Env.singleton v.it con in
      let ke0 = ConEnv.singleton con kind0 in
      if pass = 0 then
         Env.empty, ce0, ke0
      else
      let context_ts = union_kinds (union_constructors context ce_ts) ke_ts in
      let context_ts_v = add_constructor context_ts v.it con kind0 in
      let ve,dom = inf_pat context_ts_v p in
      let classT = VarT(con,List.map (fun t -> VarT(t.var,[])) ts) in
      let consT = FuncT(ts,dom,classT) (* TBR: we allow polymorphic recursion *) in
      let ve1ce1ke1 = Env.singleton v.it (consT,ConstMut),ce0,ke0 in
      if pass = 1 then
          ve1ce1ke1
      else        
      let context_ts_v_dom =
          {context_ts_v with values = union context_ts_v.values ve} in
      let rec pre_members context field_env efs =
          match efs with
          | [] -> field_env
          | {it={var;mut;priv;exp={it=AnnotE(e,t);at}}}::efs ->
            let t = check_typ context t in
            let field_env = disjoint_add_field var.at field_env var.it (mut.it,priv.it,t)  in
            pre_members context field_env efs
          | {it={var;mut;priv;exp={it=DecE({it=FuncD(v,us,p,t,e);at=_});at=_}}}::efs ->
            let us,ce_us,ke_us = check_typ_binds context us in
            let context_us = union_kinds (union_constructors context ce_us) ke_us in
            let _,dom = inf_pat context_us p in
            let rng = check_typ context_us t in
            let funcT = FuncT(us,dom,rng) in
            let field_env = disjoint_add_field var.at field_env var.it (mut.it,priv.it,funcT) in
            pre_members context field_env efs
          | {it={var;mut;priv;exp=e}}::efs ->
            let t = inf_exp context e in (* TBR: this feels wrong as we don't know all field types yet, just the ones to the left *)
            let field_env = disjoint_add_field var.at field_env var.it (mut.it,priv.it,t) in
            pre_members context field_env efs
      in
      let pre_members = pre_members context_ts_v_dom Env.empty efs in
      let private_context = Env.map (fun (m,p,t) -> (t,m)) pre_members in
      let bindings = Env.bindings pre_members in
      let public_fields =
          let public_bindings = List.filter (fun (v,(m,p,t)) -> p = Public) bindings  in
          List.map (fun (v,(m,p,t)) -> {var=v;typ=t;mut=m}) public_bindings
      in
      let kind2 = ObjK(ts,a.it,public_fields) in
      let ve2,ce2,ke2 = Env.singleton v.it (consT,ConstMut),Env.singleton v.it con, ConEnv.singleton con kind2 in
      if pass = 2 then
          ve2,ce2,ke2
      else (* pass = 3 *)
          let _ = assert(pass = 3) in
          let all_fields = List.map (fun (v,(m,p,t)) -> {var=v;typ=t;mut=m}) bindings in
          let kind3 = ObjK(ts,a.it,all_fields) in
          let field_context = add_constructor (add_value (union_values context_ts_v_dom private_context) v.it (consT,ConstMut))
                                                         v.it con kind3
          in
          (* infer the fields *)
          let _ = List.map (fun {it={var;mut;exp}} -> inf_exp field_context exp) efs in
          ve2,ce2,ke2


and inf_pats at context ve ts ps =
   match ps with
   | [] -> ve, TupT(List.rev ts)
   | p::ps ->
     let ve',t = inf_pat context p in
     inf_pats at context (disjoint_union at "duplicate binding for %s in pattern" ve ve') (t::ts) ps

and inf_pat context p =
   match p.it with
   | WildP ->  typeError p.at "can't infer type of pattern"
   | VarP v -> typeError p.at "can't infer type of pattern"
   | LitP l ->
     Env.empty,inf_lit context l
   | TupP ps ->
     inf_pats p.at context Env.empty [] ps
   | AnnotP(p,t) ->
     let t = check_typ context t in
     check_pat context p t,t
     
and check_pat context p t =
   match p.it with
   | WildP -> Env.empty
   | VarP v -> Env.singleton v.it (t,ConstMut)
   | LitP rl ->
      check_lit p.at context t rl;
      Env.empty
   | TupP ps ->
     (match norm_typ context t with
      | TupT ts ->
        check_pats p.at context Env.empty ps ts 
      | _ -> typeError p.at "expected pattern of non-tuple type, found pattern of tuple type")
   | AnnotP(p',t') ->
     let t' = check_typ context t' in
     if eq_typ context t t'
     then check_pat context p' t'
     else typeError p.at "expected pattern of one type, found pattern of unequal type"
and check_pats at context ve ps ts =
   match ps,ts with
   | [],[] -> ve
   | p::ps,t::ts ->
     let ve' = check_pat context p t in
     check_pats at context (disjoint_union at "duplicate binding for %s in pattern" ve ve') ps ts  (*TBR reject shadowing *)
   | [],ts -> typeError at "tuple pattern has %i fewer components than expected type" (List.length ts)
   | ts,[] -> typeError at "tuple pattern has %i more components than expected type" (List.length ts)
         
      
and check_decs_aux pass context ve ce ke ds  = match ds with
   |  [] ->  ve,ce,ke
   |  d::ds ->
      let ve1,ce1,ke1 = check_dec pass context d in
      check_decs_aux pass (union_kinds (union_constructors (union_values context ve1) ce1) ke1) (union ve ve1) (union ce ce1) (union_conenv ke ke1) ds
      

and check_decs context ds =
      (* declare type constructors *)
      let ve0,ce0,ke0 = check_decs_aux 0 context Env.empty Env.empty ConEnv.empty ds in
      (* declare instance constructors, given type constructors *)
      let ve1,ce1,ke1 = check_decs_aux 1 (union_kinds (union_constructors (union_values context ve0) ce0) ke0) Env.empty Env.empty ConEnv.empty ds in
      (* define type constructors (declare public member types) *)
      let ve2,ce2,ke2 = check_decs_aux 2 (union_kinds (union_constructors (union_values context ve1) ce1) ke1) Env.empty Env.empty ConEnv.empty ds in
      (* check classes definitions (check public and private member expressions *)
      let ve3,ce3,ke3 = check_decs_aux 3 (union_kinds (union_constructors (union_values context ve2) ce2) ke2) Env.empty Env.empty ConEnv.empty ds in
      ve3,ce3,ke3


let check_prog p =
    check_decs prelude p.it
     

    





