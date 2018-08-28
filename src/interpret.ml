open Syntax
open Source
open Printf

module V = Value
module T = Type


module Scheduler =
  struct
    let q : (unit -> V.value) Queue.t = Queue.create ()
    let queue work  = Queue.add work q
    let yield () =
      (* printf "\n YIELD "; *)
    	let work = Queue.take q in
      work ()

    let rec run () =
      (* printf "\n RUN "; *)
      if not (Queue.is_empty q)
      then (ignore (yield ()); run ())
      else V.unit
  end

open Scheduler
       
exception Trap of Source.region * string

let trap at msg = raise (Trap (at, msg))

type val_env = V.rec_bind V.Env.t
type typ_env = T.con V.Env.t
type con_env = T.con_env
type lab_env = V.cont V.Env.t
type ret_env = V.cont option

type scope = val_env

type context =
  {
    vals : val_env;
    typs : typ_env;
    cons : con_env;
    labs : lab_env;
    rets : ret_env;
    async : bool
  }

let adjoin_vals c ve = {c with vals = V.Env.adjoin c.vals ve}
let adjoin = adjoin_vals

let empty_context =
  {
    vals = V.Env.empty;
    typs = V.Env.empty;
    cons = Con.Env.empty;
    labs = V.Env.empty;
    rets = None;
    async = false
  }

let last_context = ref empty_context
let last_region = ref Source.no_region
let call_depth = ref 0

let get_last_context () = !last_context
let get_last_region () = !last_region
let get_indent () = String.make (2 * !call_depth) ' '

let callee_context context ve k_return =
    {vals = V.Env.adjoin context.vals ve;
     typs = V.Env.empty; (*TBR*)
     cons = Con.Env.empty;    (*TBR*)
     labs = V.Env.empty; 
     rets =  Some k_return;
     async = false}


let projV (V.Tup vs) n = List.nth vs n
let dotV (V.Obj ve) v = V.Env.find v ve
let derefV (V.Var r) = !r
let assignV (V.Var r) v  = r := v; V.unit
let unrollV = V.unroll_rec_bind
let updateV (V.Array a) (V.Nat i) v  = a.(V.Nat.to_int i) <- v; V.unit (* TBR *)
let indexV (V.Array a) (V.Nat i) = a.(V.Nat.to_int i) (*TBR*)
let applyV (V.Func f) v k = f v k

let notV (V.Bool b) = V.Bool (not b)

let set_result async v =
    match async with
  | {V.result=None;waiters=waiters} ->
        async.result <- Some v;
        async.waiters <- [];
        List.fold_left (fun runnables waiter -> (fun () -> waiter v)::runnables) [] waiters; 
  | {V.result=Some _} -> failwith "set_result"
let get_result async k =
    match async with
  | {V.result=Some v} -> k v
        | {V.result=None;waiters} -> (async.waiters <- k::waiters; V.unit)


let sprintf = Printf.sprintf

let rec interpret_lit context rl =
  match !rl with
    | NullLit -> V.Null
    | BoolLit b -> V.Bool b
    | NatLit n -> V.Nat n
    | IntLit i -> V.Int i
    | Word8Lit w -> V.Word8 w
    | Word16Lit w -> V.Word16 w
    | Word32Lit w -> V.Word32 w
    | Word64Lit w -> V.Word64 w
    | FloatLit f -> V.Float f
    | CharLit c -> V.Char c
    | TextLit s -> V.Text s
    | PreLit _ -> assert false

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
    (match e.note.note_typ with
     | T.Pre -> assert false
     | T.Mut _ -> k (derefV (unrollV (V.Env.find x.it context.vals)))
     | _ -> k (V.as_val_bind (unrollV (V.Env.find x.it context.vals))))
| LitE rl ->
    k (interpret_lit context rl)
| UnE(uop,e1) ->
   let t1 = T.immutable e1.note.note_typ in
   interpret_exp context e1 (fun v1 -> k (Operator.unop t1 uop v1))
| BinE (e1,bop,e2) ->
   let t1 = T.immutable e1.note.note_typ in
   interpret_exp context e1 (fun v1 -> interpret_exp context e2 (fun v2 -> k (try Operator.binop t1 bop v1 v2 with _ -> trap e.at "arithmetic overflow")))
| RelE (e1,rop,e2) ->
   let t1 = T.immutable e1.note.note_typ in
   interpret_exp context e1 (fun v1 -> interpret_exp context e2 (fun v2 -> k (Operator.relop t1 rop v1 v2)))
| TupE es ->
    interpret_exps context [] es (fun vs -> k (V.Tup vs))
| ProjE(e1,n) ->
    interpret_exp context e1 (fun v1 -> k (projV v1 n))
| DotE(e1,v) ->
    let it = v.it in
    interpret_exp context e1 (fun v1 ->
    k (match e.note.note_typ with
       | T.Mut _ -> (derefV (unrollV (dotV v1 it)))
       | _ -> (V.as_val_bind (unrollV (dotV v1 it))))
    )
| AssignE(e1,e2) ->
    begin
    match e1.it with
    | VarE v ->
      let v1 = unrollV (V.Env.find v.it context.vals) in
      interpret_exp context e2 (fun v2 ->
      k (assignV v1 v2))
    | DotE(e,v) ->
      interpret_exp context e1 (fun v1 ->
      interpret_exp context e2 (fun v2 ->
      let loc = unrollV (dotV v1 v.it) in
      k(assignV loc v2)))
    | IdxE(ea,ei) ->
      interpret_exp context ea (fun va ->
      interpret_exp context ei (fun vi ->
      interpret_exp context e2 (fun v2 -> k(updateV va vi v2))))
    end
| ArrayE es ->
    interpret_exps context [] es (fun vs ->
    k (V.Array (Array.of_list vs)))
| IdxE(e1,e2) ->
    interpret_exp context e1 (fun v1 ->
    interpret_exp context e2 (fun v2 ->
    k (indexV v1 v2)))
| CallE(e1,ts,e2) ->
    interpret_exp context e1 (fun v1 ->
    interpret_exp context e2 (fun v2 ->
    applyV v1 v2 k))
| BlockE ds ->
    interpret_block_local context ds k
| NotE(e1) ->
    interpret_exp context e1 (fun v -> k (notV v))
| AndE(e1,e2) ->
    interpret_exp context e1
    (fun v -> if V.as_bool v
              then interpret_exp context e2 k
              else k (V.Bool false))
| OrE(e1,e2) ->
    interpret_exp context e1
    (fun v -> if V.as_bool v
              then k (V.Bool true)
              else interpret_exp context e2 k)
| IfE(e0,e1,e2) ->
     interpret_exp context e0
     (fun v -> if V.as_bool v
               then interpret_exp context e1 k
               else interpret_exp context e2 k)
| SwitchE(e,cs) ->
  interpret_exp context e (fun v ->
    interpret_cases context cs v k)
| WhileE(e0,e1) ->
  let e_while = e in
  let k_continue = fun v -> interpret_exp context e_while k in
  interpret_exp context e0 (fun v0 ->
  if V.as_bool v0
  then interpret_exp context e1 k_continue
  else k V.unit)
| LoopE(e0,None) ->
  let e_loop = e in
  let k_continue = fun v -> interpret_exp context e_loop k in
  interpret_exp context e0 k_continue
| LoopE(e0,Some e1) ->
  let e_loop = e in
  let k_continue = fun v ->
      interpret_exp context e1 (fun v1 ->
      if V.as_bool v1
      then interpret_exp context e_loop k
      else k V.unit)
  in
  interpret_exp context e0 k_continue
| ForE(p,e0,e1)->
  failwith "NYI:ForE"
(* labels *)
| LabelE(l,_,e) ->
  let context' = {context with labs = V.Env.add l.it k context.labs} in
  interpret_exp context' e k
| BreakE(l,e) ->
  let k_break = V.Env.find l.it context.labs in
  interpret_exp context e k_break
| RetE e0 ->
  let (Some k_return) = context.rets in 
  interpret_exp context e0 k_return
| AsyncE e0 ->
  let async = {V.result=None;waiters=[]} in
  let k_return = fun v -> let runnables = set_result async v in
                          List.iter queue runnables; 
                          V.unit
  in
  let context = {context with
                 labs = V.Env.empty;
                 rets = Some k_return; 
                 async = true}
  in
  queue (fun()->interpret_exp context e0 k_return);
  k (V.Async async)
| AwaitE e0 ->
  interpret_exp context e0 (fun v ->
  let async = V.as_async v in
  match async.result with
  | Some v -> k v
  | None -> (async.waiters <- k::async.waiters;
             yield())) 
| AssertE e ->
  interpret_exp context e (fun  v ->
  if V.as_bool v
  then k(V.unit)
  else failwith  "Assert failure %" (string_of_region e.at))
| IsE(e,t) ->
  failwith "NYI:IsE"
| AnnotE(e,t) ->
  interpret_exp context e k
| DecE ({it=FuncD(v,_,_,_,_)} as d) ->
  interpret_block context [d] (fun ve ->
  let w = V.Env.find v.it ve in
  k (V.as_val_bind (unrollV w)))
| DecE d ->
  interpret_block context [d] (fun ve ->  k V.unit)
    
and interpret_cases context cs v k  =
  match cs with
  | [] -> failwith "match_cases"
  | {it={pat=p;exp=e};at}::cs ->
    match interpret_pat p v with
    | Some ve -> interpret_exp (adjoin_vals context ve) e k
    | None -> interpret_cases context cs v k
    
and interpret_block_local context ds k =
  match ds with
  | [] -> k V.unit
  | [{it = ExpD e; _}] -> interpret_exp context e k
  | d::ds' ->
    interpret_block context [d] (fun ve ->
    (* TBR: we currently evaluate decs sequentially, not recursively *)
    interpret_block_local (adjoin_vals context ve) ds' k) 

and declare_dec context d =     
    match d.it with
    | ExpD e ->
       V.Env.empty
    | LetD (p,e) ->
       declare_pat context p
    | VarD (v,e) ->
       V.Env.singleton v.it (V.Rec {V.def = None})
    | TypD(v,ts,t) ->
       V.Env.empty
    | FuncD(v,ts,p,t,e) ->
       V.Env.singleton v.it (V.Rec {V.def = None})
    | ClassD(v,ts,a,p,efs) ->
       V.Env.singleton v.it (V.Rec {V.def = None})

and declare_block context ve ds =
    match ds with
    | [] -> ve
    | d::ds' ->
      let ve' = V.Env.adjoin ve (declare_dec context d) in
      declare_block context ve' ds'

and interpret_block context ds k =
    let ve = declare_block context V.Env.empty ds in
    define_block (adjoin_vals context ve) ds
    (fun () -> k ve)

and define_block context ds k =
    match ds with
    | [] -> k()
    | d::ds' ->
      define_dec context d (fun () -> define_block context ds' k)

and define_var context var v =
    match V.as_rec_bind (V.Env.find var.it context.vals) with
    | {def = Some _} -> failwith "duplicated definition"
    | recursive -> recursive.def <- Some v
    
and define_dec context d k =     
    match d.it with
    | ExpD e ->
      interpret_exp context e (fun v ->
      k())
    | LetD (p,e) ->
      interpret_exp context e (fun v ->
      define_pat context p v;
      k())
    | VarD (var,e) ->
      interpret_exp context e (fun v ->
      define_var context var (V.Var (ref v));
      k())
    | TypD(v,ts,t) ->
      k()
    | FuncD(var,ts,p,t,e) ->
      (* TBC: trim callee_context *)
      (define_var context var
        (V.Val (V.Func (fun v k ->
	      if !Flags.debug then printf "%s%s%s\n" (get_indent ()) var.it (V.debug_string_of_tuple_val v);
        incr call_depth;
              match interpret_pat p v with
              | Some ve ->
	        let k = if !Flags.debug then fun w -> (decr call_depth; printf "%s%s%s => %s\n" (get_indent ()) var.it (V.debug_string_of_tuple_val v) (V.debug_string_of_val w);k w) else k in
                let callee_context = callee_context context ve k in
	      	interpret_exp callee_context e k 
              | None -> failwith "unexpected refuted pattern"))));
      k()
    | ClassD(c,ts,a,p,efs) ->
      (define_var context c
        (V.Val (V.Func (fun v k ->
              match interpret_pat p v with
              | None -> failwith "unexpected refuted pattern";
              | Some ve -> let context = adjoin_vals context ve in
                           let rec declare_members private_ve public_ve efs =
                                     match efs with
                                     | [] -> (private_ve,public_ve)
                                     | {it={id;mut;priv;exp=_};_}::efs ->
                                        let recR = V.Rec {V.def = None} in
                                        declare_members (V.Env.add id.it recR private_ve)
                                                        (V.Env.add id.it recR public_ve) efs
                            in
                            let (private_ve,public_ve) = declare_members V.Env.empty V.Env.empty efs
                            in
                            let rec define_members efs =
                                     match efs with
				     | {it={id;mut;priv;exp;}}::efs ->
				        let private_context = adjoin_vals context private_ve in
                                        interpret_exp private_context exp (fun v ->
					let v = expand a.it priv.it mut.it exp.note.note_typ v in 
                                        let defn = match mut.it with
                                                   | Const -> V.Val v
                                                   | Var -> V.Var (ref v)
                                        in
                                          define_var private_context id defn;
                                          define_members efs)
				     | [] -> k (V.Obj public_ve) 

                            in 
                                 define_members efs))));
       k()
       
and expand actor priv mut t v =
    match (actor,priv,mut,t) with
    | T.Actor, Public, Const, T.Func (_, _, T.Tup []) ->
      let f = V.as_func v in
      V.Func (fun w k -> queue(fun () -> f w (fun a->a));
                        k V.unit)
    | T.Actor, Public, Const, T.Func (_, _, T.Async _) ->
      let f = V.as_func v in
      V.Func (fun w k ->
      	     let async = {V.result=None;waiters=[]} in
	     queue(fun () ->
	           f w (fun a ->
		          get_result (V.as_async a) (fun r ->
			     let runnables = set_result async r in
                             List.iter queue runnables;
			     V.unit)
                        ));
             k(V.Async async))
    | _ -> v

			  

and declare_pats context ve ps =
   match ps with
   | [] -> ve
   | p::ps ->
     let ve' = declare_pat context p in
     declare_pats context (V.Env.adjoin ve ve') ps

and declare_pat context p =
   match p.it with
   | WildP ->  V.Env.empty
   | VarP v -> V.Env.singleton v.it (V.Rec {def = None})
   | LitP l -> V.Env.empty
   | TupP ps -> declare_pats context V.Env.empty ps
   | AnnotP(p,t) ->
     declare_pat context p 

and define_pat context p v =
   match p.it with
   | WildP -> ()
   | VarP var -> define_var context var (V.Val v)
   | LitP rl -> ()
   | TupP ps ->
     let vs = V.as_tup v in
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
    | BoolLit b -> V.as_bool v = b
    | NatLit n -> V.as_nat v = n 
    | IntLit i -> V.as_int v = i
    | Word8Lit w -> V.as_word8 v = w
    | Word16Lit w -> V.as_word16 v = w
    | Word32Lit w -> V.as_word32 v = w
    | Word64Lit w -> V.as_word64 v = w
    | FloatLit f -> V.as_float v = f
    | CharLit c -> V.as_char v = c
    | TextLit s -> V.as_text v = s
    | PreLit _ -> assert false

and interpret_pat p v =
   match p.it with 
   | WildP -> Some V.Env.empty
   | VarP var -> Some (V.Env.singleton var.it (V.Rec {V.def = Some (V.Val v)}))
   | LitP rl ->
     if match_lit p v rl 
     then Some V.Env.empty
     else None
   | SignP (op, rl) ->
     let t1 = T.immutable p.note.note_typ in
     if match_lit p (Operator.unop t1 op v) rl 
     then Some V.Env.empty
     else None
   | TupP ps ->
      let vs = V.as_tup v in
      interpret_pats V.Env.empty ps vs 
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
         interpret_pats (V.Env.adjoin ve ve') ps vs
     end  
   | [],vs -> failwith "Wrong:match_pats"
   | vs,[] -> failwith "Wrong:match_pats"


let interpret_prog context p k =
    call_depth := 0;
    let k' = fun v -> (run();k(v)) in
    ignore (interpret_block context p.it k')
