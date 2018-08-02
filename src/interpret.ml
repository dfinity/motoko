open Syntax
open Source
open Types
open Typing
open Printf
open Values
open Operators

module Scheduler =
  struct
    let q : (unit -> value) Queue.t = Queue.create ()
    let queue work  = Queue.add work q
    let yield () =
      (* printf "\n YIELD "; *)
    	let work = Queue.take q in
      work ()

    let rec run () =
      (* printf "\n RUN "; *)
      if not (Queue.is_empty q)
      then (ignore (yield ()); run ())
      else unitV
  end

open Scheduler
       
let debug = ref true

exception Trap of Source.region * string

type context = {values: recbinding Env.t; constructors: con Env.t; kinds: kind ConEnv.t; labels: cont Env.t; returns: cont option; awaitable: bool}

let union_values c ve = {c with values = union c.values ve}

let prelude = {values = Env.empty;
               constructors = Env.empty;
               kinds = ConEnv.empty;
               labels = Env.empty;
               returns = None;
               awaitable = false}

let last_context = ref prelude
let last_region = ref Source.no_region

let callee_context context ve k_return =
    {values = union context.values ve;
     constructors = Env.empty; (*TBR*)
     kinds = ConEnv.empty;    (*TBR*)
     labels = Env.empty; 
     returns =  Some k_return;
     awaitable = false}


let sprintf = Printf.sprintf

let rec interpret_lit context rl =
  match !rl with
    | NullLit -> nullV
    | BoolLit b -> boolV b
    | NatLit n -> natV n
    | IntLit i -> intV i
    | WordLit (Word8 w) -> word8V w
    | WordLit (Word16 w) -> word16V w
    | WordLit (Word32 w) -> word32V  w
    | WordLit (Word64 w) -> word64V w
    | FloatLit f -> floatV f
    | CharLit c -> charV c
    | TextLit s -> textV s
    | PreLit s -> failwith "interpret_lit"

and interpret_exps context vs es k =
    match es with
    | [] -> k (List.rev vs)
    | (e::es) -> interpret_exp context e (fun v -> interpret_exps context (v::vs) es k)
and interpret_exp context e k  =
    last_region := e.at; (* debugging *)
    last_context := context;
    interpret_exp' context e k 

and interpret_exp' context e k =
match e.it with
| VarE x ->
    (match x.note with
     | VarMut -> k (derefV (checkV (Env.find x.it context.values)))
     | ConstMut -> k (val_of_B (checkV (Env.find x.it context.values))))
| LitE rl ->
    k (interpret_lit context rl)
| UnE(uop,e1) ->
   let t1 = e1.note in
   interpret_exp context e1 (fun v1 -> k (Operators.find_unop t1 uop v1))
| BinE (e1,bop,e2) ->
   let t1 = e1.note in
   interpret_exp context e1 (fun v1 -> interpret_exp context e2 (fun v2 -> k (Operators.find_binop t1 bop v1 v2)))
| RelE (e1,rop,e2) ->
   let t1 = e1.note in
   interpret_exp context e1 (fun v1 -> interpret_exp context e2 (fun v2 -> k (Operators.find_relop t1 rop v1 v2)))
| TupE es ->
    interpret_exps context [] es (fun vs -> k (tupV vs))
| ProjE(e1,n) ->
    interpret_exp context e1 (fun v1 -> k (projV v1 n))
| DotE(e1,v) ->
    let mut = v.note in
    let it = v.it in
    interpret_exp context e1 (fun v1 ->
    k (match mut with
       | VarMut -> (derefV (checkV (dotV v1 it)))
       | ConstMut -> (val_of_B (checkV (dotV v1 it))))
    )
| AssignE(e1,e2) ->
    begin
    match e1.it with
    | VarE v ->
      let v1 = checkV (Env.find v.it context.values) in
      interpret_exp context e2 (fun v2 ->
      k (assignV v1 v2))
    | DotE(e,v) ->
      interpret_exp context e1 (fun v1 ->
      interpret_exp context e2 (fun v2 ->
      let loc = checkV (dotV v1 v.it) in
      k(assignV loc v2)))
    | IdxE(ea,ei) ->
      interpret_exp context ea (fun va ->
      interpret_exp context ei (fun vi ->
      interpret_exp context e2 (fun v2 -> k(updateV va vi v2))))
    end
| ArrayE (m, es) ->
    interpret_exps context [] es (fun vs ->
    k (arrV (Array.of_list vs)))
| IdxE(e1,e2) ->
    interpret_exp context e1 (fun v1 ->
    interpret_exp context e2 (fun v2 ->
    k (indexV v1 v2)))
| CallE(e1,ts,e2) ->
    interpret_exp context e1 (fun v1 ->
    interpret_exp context e2 (fun v2 ->
    applyV v1 v2 k))
| BlockE es ->
    interpret_block context es k
| NotE(e1) ->
    interpret_exp context e1 (fun v -> k (notV v))
| AndE(e1,e2) ->
    interpret_exp context e1
    (fun v -> if (bool_of_V v)
              then interpret_exp context e2 k
              else k (boolV false))
| OrE(e1,e2) ->
    interpret_exp context e1
    (fun v -> if (bool_of_V v)
              then k (boolV true)
              else interpret_exp context e2 k)
| IfE(e0,e1,e2) ->
     interpret_exp context e0
     (fun v -> if (bool_of_V v)
               then interpret_exp context e1 k
               else interpret_exp context e2 k)
| SwitchE(e,cs) ->
  interpret_exp context e (fun v ->
    interpret_cases context cs v k)
| WhileE(e0,e1) ->
  let e_while = e in
  let k_continue = fun v -> interpret_exp context e_while k in
  interpret_exp context e0 (fun v0 ->
  if (bool_of_V v0)
  then interpret_exp context e1 k_continue
  else k unitV)
| LoopE(e0,None) ->
  let e_loop = e in
  let k_continue = fun v -> interpret_exp context e_loop k in
  interpret_exp context e0 k_continue
| LoopE(e0,Some e1) ->
  let e_loop = e in
  let k_continue = fun v ->
      interpret_exp context e1 (fun v1 ->
      if (bool_of_V v1)
      then interpret_exp context e_loop k
      else k unitV)
  in
  interpret_exp context e0 k_continue
| ForE(p,e0,e1)->
  failwith "NYI:ForE"
(* labels *)
| LabelE(l,e) ->
  let context' = {context with labels = Env.add l.it k context.labels} in
  interpret_exp context' e k
| BreakE(l,e) ->
  let k_break = Env.find l.it context.labels in
  interpret_exp context e k_break
| RetE e0 ->
  let (Some k_return) = context.returns in 
  interpret_exp context e0 k_return
| AsyncE e0 ->
  let async = {result=None;waiters=[]} in
  let k_return = fun v -> let runnables = set_result async v in
                          List.iter queue runnables; 
                          unitV
  in      
  let context = {values = context.values;
                 constructors = context.constructors;
                 kinds = context.kinds;
                 labels = Env.empty;
                 returns = Some k_return; 
                 awaitable = true}
  in
  queue (fun()->interpret_exp context e0 k_return);
  k (asyncV async)
| AwaitE e0 ->
  interpret_exp context e0 (fun v ->
  let async = async_of_V v in
  match async.result with
  | Some v -> k v
  | None -> (async.waiters <- k::async.waiters;
             yield())) 
| AssertE e ->
  interpret_exp context e (fun  v ->
  if bool_of_V v
  then k(unitV)
  else failwith  "Assert failure %" (string_of_region e.at))
| IsE(e,t) ->
  failwith "NYI:IsE"
| AnnotE(e,t) ->
  interpret_exp context e k
| DecE ({it=FuncD(v,_,_,_,_)} as d) ->
  interpret_decs context [d] (fun ve ->
  (match lookup ve v.it with
  | Some w -> k (val_of_B(checkV w))
  | None -> failwith "interpret_exp decE"))
| DecE d ->
  interpret_decs context [d] (fun ve ->  k unitV)
    
and interpret_cases context cs v k  =
  match cs with
  | [] -> failwith "match_cases"
  | {it={pat=p;exp=e};at}::cs ->
    match interpret_pat p v with
    | Some ve -> interpret_exp (union_values context ve) e k
    | None -> interpret_cases context cs v k
    
and interpret_block context es k =
  match es with
  | [] -> k unitV
  | {it = DecE d;at}::es ->
    interpret_decs context [d] (fun ve ->
    (* TBR: we currently evaluate decs sequentially, not recursively *)
    interpret_block  (union_values context ve) es k) 
  | [e] -> interpret_exp context e k
  | e::es ->
     interpret_exp context e (fun v ->
     interpret_block context es k)

and declare_dec context d =     
    match d.it with
    | LetD (p,e) ->
       declare_pat context p
    | VarD (v,t,None) ->
       Env.singleton v.it (recR None)
    | VarD (v,t,Some e) ->
       Env.singleton v.it (recR None)
    | TypD(v,ts,t) ->
       Env.empty
    | FuncD(v,ts,p,t,e) ->
       Env.singleton v.it (recR None)
    | ClassD(a,v,ts,p,efs) ->
       Env.singleton v.it (recR None)

and declare_decs context ve ds =
    match ds with
    | [] -> ve
    | d::ds' -> declare_decs context (union ve (declare_dec context d)) ds'

and interpret_decs context ds k =
    let ve = declare_decs context Env.empty ds in
    define_decs (union_values context ve) ds
    (fun () -> k ve)

and define_decs context decs k =
    match decs with
    | [] -> k()
    | dec::decs ->
     define_dec context dec (fun () ->
     define_decs context decs k)

and define_var context var v =
    match rec_of_R (Env.find var.it context.values) with
    | {definition=Some _} -> failwith "duplicated definition"
    | recursive -> recursive.definition <- Some v
    
and define_dec context d k =     
    match d.it with
    | LetD (p,e) ->
      interpret_exp context e (fun v ->
      define_pat context p v;
      k())
    | VarD (v,t,None) ->
      (*TBR leave v uninitialized (yuck!), blackhole on read *)
      k()
    | VarD (var,t,Some e) ->
      interpret_exp context e (fun v ->
      define_var context var (varB (ref v));
      k())
    | TypD(v,ts,t) ->
      k()
    | FuncD(var,ts,p,t,e) ->
      (* TBC: trim callee_context *)
      (define_var context var
        (valB(funcV(fun v k ->
	      if !debug then printf "\n%s%s" var.it (debug_string_of_tuple_val v);
              match interpret_pat p v with
              | Some ve ->
	        let k = if !debug then fun w -> (printf "\n%s%s => %s" var.it (debug_string_of_tuple_val v) (debug_string_of_val w);k w) else k in
                let callee_context = callee_context context ve k in
	      	interpret_exp callee_context e k 
              | None -> failwith "unexpected refuted pattern"))));
      k()
    | ClassD(a,c,ts,p,efs) ->
      (define_var context c
        (valB(funcV(fun v k ->
              match interpret_pat p v with
              | None -> failwith "unexpected refuted pattern";
              | Some ve -> let context = union_values context ve in
                           let rec declare_members private_ve public_ve efs =
                                     match efs with
                                     | [] -> (private_ve,public_ve)
                                     | {it={var;mut;priv;exp=_}}::efs ->
                                        let recR = recR None in
                                        declare_members (Env.add var.it recR private_ve)
                                                        (Env.add var.it recR public_ve) efs
                            in
                            let (private_ve,public_ve) = declare_members Env.empty Env.empty efs
                            in
                            let rec define_members efs =
                                     match efs with
				     | {it={var;mut;priv;exp;}}::efs ->
				        let private_context = union_values context private_ve in
                                        interpret_exp private_context exp (fun v ->
					let v = expand a.it priv.it mut.it exp.note v in 
                                        let defn = match mut.it with
                                                   | ConstMut -> valB v
                                                   | VarMut -> varB (ref v)
                                        in
                                          define_var private_context var defn;
                                          define_members efs)
				     | [] -> k (objV public_ve) 

                            in 
                                 define_members efs))));
       k()
       
and expand actor priv mut t v =
    match (actor,priv,mut,t) with
    | Actor,Public,ConstMut,FuncT(_,_,TupT[]) ->
      let f = func_of_V v in
      funcV (fun w k -> queue(fun () -> f w (fun a->a));
                        k unitV)
    | Actor,Public,ConstMut,FuncT(_,_,AsyncT(_)) ->
      let f = func_of_V v in
      funcV (fun w k ->
      	     let async = {result=None;waiters=[]} in
	     queue(fun () ->
	           f w (fun a ->
		          get_result (async_of_V a) (fun r ->
			     let runnables = set_result async r in
                             List.iter queue runnables;
			     unitV)
                        ));
             k(asyncV async))
    | _ -> v

			  

and declare_pats context ve ps =
   match ps with
   | [] -> ve
   | p::ps ->
     let ve' = declare_pat context p in
     declare_pats context (union ve ve') ps

and declare_pat context p =
   match p.it with
   | WildP ->  Env.empty
   | VarP v -> Env.singleton v.it (recR None)
   | LitP l -> Env.empty
   | TupP ps -> declare_pats context Env.empty ps
   | AnnotP(p,t) ->
     declare_pat context p 

and define_pat context p v =
   match p.it with
   | WildP -> ()
   | VarP var -> define_var context var (valB v)
   | LitP rl -> ()
   | TupP ps ->
     let vs = tup_of_V v in
     define_pats context ps vs
   | AnnotP(p',_) -> 
     define_pat context p' v

and define_pats context ps vs =
   match ps,vs with
   | [],[] -> ()
   | p::ps,v::vs ->
     begin
       define_pat context p v;
       define_pats context ps vs
     end  
   | [],ts -> failwith "Wrong:define_pats"
   | ts,[] -> failwith "Wrong:define_pats"

and match_lit p v rl =
  match !rl with
    | NullLit -> true
    | BoolLit b -> bool_of_V v = b
    | NatLit n -> nat_of_V v = n 
    | IntLit i -> int_of_V v = i
    | WordLit (Word8 w) -> word8_of_V v = w
    | WordLit (Word16 w) -> word16_of_V v = w
    | WordLit (Word32 w) -> word32_of_V v = w
    | WordLit (Word64 w) -> word64_of_V v = w
    | FloatLit f -> float_of_V v = f
    | CharLit c -> char_of_V v = c
    | TextLit s -> text_of_V v = s
    | PreLit s -> failwith "match_lit"
     
and interpret_pat p v =
   match p.it with 
   | WildP -> Some Env.empty
   | VarP var -> Some (Env.singleton var.it (recR (Some (valB v))))
   | LitP rl ->
     if match_lit p v rl 
     then Some Env.empty
     else None
   | TupP ps ->
      let vs = tup_of_V v in
      interpret_pats Env.empty ps vs 
   | AnnotP(p',_) -> 
     interpret_pat p' v

and interpret_pats ve ps vs =
   match ps,vs with
   | [],[] -> Some ve
   | p::ps,v::vs ->
     begin
       match interpret_pat p v with 
       | None -> None
       | Some ve' ->
         interpret_pats (union ve ve') ps vs
     end  
   | [],vs -> failwith "Wrong:match_pats"
   | vs,[] -> failwith "Wrong:match_pats"


let interpret_prog p k =
    let k' = fun v -> (run();k(v)) in
    interpret_decs prelude p.it k'

     

    





