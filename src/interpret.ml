open Syntax
open Source
open Printf

module V = Value
module T = Type


(* Context *)

type val_env = V.def V.Env.t
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
  { vals = V.Env.empty;
    typs = V.Env.empty;
    cons = Con.Env.empty;
    labs = V.Env.empty;
    rets = None;
    async = false
  }

let callee_context context ve k =
  { vals = V.Env.adjoin context.vals ve;
    typs = V.Env.empty; (*TBR*)
    cons = Con.Env.empty; (*TBR*)
    labs = V.Env.empty;
    rets =  Some k;
    async = false
  }


(* Error handling *)

exception Trap of Source.region * string

let trap at msg = raise (Trap (at, msg))

let unimplemented at msg = trap at ("NOT YET IMPLEMENTED: " ^ msg)


(* Debugging Aids *)

let last_context = ref empty_context
let last_region = ref Source.no_region
let call_depth = ref 0

let get_last_context () = !last_context
let get_last_region () = !last_region
let get_indent () = String.make (2 * !call_depth) ' '


(* Schedulinbg *)

module Scheduler =
struct
  let q : (unit -> V.value) Queue.t = Queue.create ()

  let queue work = Queue.add work q
  let yield () = Queue.take q ()

  let rec run () =
    if not (Queue.is_empty q)
    then (ignore (yield ()); run ())
    else V.unit
end

open Scheduler


(* Async auxiliary functions *)

let get_result async k : V.value =
  match async.V.result with
  | Some v -> k v
  | None -> async.V.waiters <- k::async.V.waiters; V.unit

let set_result async v =
  assert (async.V.result = None);
  List.iter (fun waiter -> queue (fun () -> waiter v)) async.waiters;
  async.result <- Some v;
  async.waiters <- []


(* Literals *)

let interpret_lit context lit =
  match !lit with
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


(* Expressions *)

let rec interpret_exp context exp k : V.value =
  interpret_exp_mut context exp (function V.Mut r -> k !r | v -> k v)

and interpret_exp_mut context exp k =
  last_region := exp.at;
  last_context := context;
  match exp.it with
  | VarE id ->
    (match !(V.Env.find id.it context.vals) with
    | Some v -> k v
    | None -> trap exp.at "accessing identifier before becoming defined")
  | LitE lit ->
    k (interpret_lit context lit)
  | UnE (op, exp1) ->
    let t = T.immutable exp1.note.note_typ in
    interpret_exp context exp1 (fun v1 -> k (Operator.unop t op v1))
  | BinE (exp1, op, exp2) ->
    let t = T.immutable exp1.note.note_typ in
    interpret_exp context exp1 (fun v1 ->
      interpret_exp context exp2 (fun v2 ->
        k (try Operator.binop t op v1 v2 with _ ->
          trap exp.at "arithmetic overflow")))
  | RelE (exp1, op, exp2) ->
    let t = T.immutable exp1.note.note_typ in
    interpret_exp context exp1 (fun v1 ->
      interpret_exp context exp2 (fun v2 ->
        k (Operator.relop t op v1 v2)))
  | TupE exps ->
    interpret_exps context [] exps (fun vs -> k (V.Tup vs))
  | ProjE (exp1, n) ->
    interpret_exp context exp1 (fun v1 -> k (List.nth (V.as_tup v1) n))
  | DotE (exp1, id) ->
    interpret_exp context exp1 (fun v1 ->
      match !(V.Env.find id.it (V.as_obj v1)) with
      | Some v -> k v
      | None -> trap exp.at "accessing field before becoming defined")
  | AssignE (exp1, exp2) ->
    interpret_exp_mut context exp1 (fun v1 ->
      interpret_exp context exp2 (fun v2 ->
        V.as_mut v1 := v2; k V.unit))
  | ArrayE exps ->
    let t = exp.note.note_typ in
    interpret_exps context [] exps (fun vs ->
      let vs' =
        match t with
        | T.Array (T.Mut _) -> List.map (fun v -> V.Mut (ref v)) vs
        | _ -> vs
      in k (V.Array (Array.of_list vs')))
  | IdxE (exp1, exp2) ->
    interpret_exp context exp1 (fun v1 ->
      interpret_exp context exp2 (fun v2 ->
        k (V.as_array v1).(V.Nat.to_int (V.as_nat v2)))) (* TBR *)
  | CallE (exp1, typs, exp2) ->
    interpret_exp context exp1 (fun v1 ->
      interpret_exp context exp2 (fun v2 ->
        (V.as_func v1) v2 k))
  | BlockE decs ->
    interpret_block_local context decs k
  | NotE exp1 ->
    interpret_exp context exp1 (fun v1 -> k (V.Bool (not (V.as_bool v1))))
  | AndE (exp1, exp2) ->
    interpret_exp context exp1 (fun v1 ->
      if V.as_bool v1
      then interpret_exp context exp2 k
      else k v1)
  | OrE (exp1, exp2) ->
    interpret_exp context exp1 (fun v1 ->
      if V.as_bool v1
      then k v1
      else interpret_exp context exp2 k)
  | IfE (exp1, exp2, exp3) ->
    interpret_exp context exp1 (fun v1 ->
      if V.as_bool v1
      then interpret_exp context exp2 k
      else interpret_exp context exp3 k)
  | SwitchE (exp1, cases) ->
    interpret_exp context exp1 (fun v1 ->
      interpret_cases context cases v1 k)
  | WhileE (exp1, exp2) ->
    let k_continue = fun v -> interpret_exp context exp k in
    interpret_exp context exp1 (fun v1 ->
      if V.as_bool v1
      then interpret_exp context exp2 k_continue
      else k V.unit)
  | LoopE (exp1, None) ->
    interpret_exp context exp1 (fun v -> interpret_exp context exp k)
  | LoopE (exp1, Some exp2) ->
    interpret_exp context exp1 (fun _v1 ->
      interpret_exp context exp2 (fun v2 ->
        if V.as_bool v2
        then interpret_exp context exp k
        else k V.unit))
  | ForE (pat, exp1, exp2)->
    unimplemented exp.at "for loops"
  | LabelE (id, _typ, exp1) ->
    let context' = {context with labs = V.Env.add id.it k context.labs} in
    interpret_exp context' exp1 k
  | BreakE (id, exp1) ->
    interpret_exp context exp1 (V.Env.find id.it context.labs)
  | RetE exp1 ->
    interpret_exp context exp1 (Lib.Option.value context.rets)
  | AsyncE exp1 ->
    let async = {V.result = None; waiters = []} in
    let k_return = fun v1 -> set_result async v1; V.unit in
    let context' =
      { context with
        labs = V.Env.empty;
        rets = Some k_return; 
        async = true
      }
    in
    queue (fun () -> interpret_exp context' exp1 k_return);
    k (V.Async async)
  | AwaitE exp1 ->
    interpret_exp context exp1 (fun v1 ->
      let async = V.as_async v1 in
      match async.result with
      | Some v -> k v
      | None -> async.waiters <- k::async.waiters; yield ()) 
  | AssertE exp1 ->
    interpret_exp context exp1 (fun  v ->
      if V.as_bool v
      then k V.unit
      else trap exp.at "Assertion failure")
  | IsE (exp1, typ) ->
    unimplemented exp.at "is"
  | AnnotE (exp1, _typ) ->
    interpret_exp context exp1 k
  | DecE ({it = FuncD (id, _, _, _, _); _} as dec) ->
    interpret_block context [dec] (fun ve ->
      k (Lib.Option.value !(V.Env.find id.it ve)))
  | DecE dec ->
    interpret_block context [dec] (fun ve ->  k V.unit)

and interpret_exps context vs exps k =
  match exps with
  | [] -> k (List.rev vs)
  | exp::exps' ->
    interpret_exp context exp (fun v -> interpret_exps context (v::vs) exps' k)



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
       V.Env.singleton v.it (ref None)
    | TypD(v,ts,t) ->
       V.Env.empty
    | FuncD(v,ts,p,t,e) ->
       V.Env.singleton v.it (ref None)
    | ClassD(v,ts,a,p,efs) ->
       V.Env.singleton v.it (ref None)

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
    let d = V.Env.find var.it context.vals in
    assert (!d = None);
    d := Some v
    
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
      define_var context var (V.Mut (ref v));
      k())
    | TypD(v,ts,t) ->
      k()
    | FuncD(var,ts,p,t,e) ->
      (* TBC: trim callee_context *)
      (define_var context var
        (V.Func (fun v k ->
	      if !Flags.debug then printf "%s%s%s\n" (get_indent ()) var.it (V.debug_string_of_tuple_val v);
        incr call_depth;
              match interpret_pat p v with
              | Some ve ->
	        let k = if !Flags.debug then fun w -> (decr call_depth; printf "%s%s%s => %s\n" (get_indent ()) var.it (V.debug_string_of_tuple_val v) (V.debug_string_of_val w);k w) else k in
                let callee_context = callee_context context ve k in
	      	interpret_exp callee_context e k 
              | None -> failwith "unexpected refuted pattern")));
      k()
    | ClassD(c,ts,a,p,efs) ->
      (define_var context c
        (V.Func (fun v k ->
              match interpret_pat p v with
              | None -> failwith "unexpected refuted pattern";
              | Some ve -> let context = adjoin_vals context ve in
                           let rec declare_members private_ve public_ve efs =
                                     match efs with
                                     | [] -> (private_ve,public_ve)
                                     | {it={id;mut;priv;exp=_};_}::efs ->
                                        let d = ref None in
                                        declare_members (V.Env.add id.it d private_ve)
                                                        (V.Env.add id.it d public_ve) efs
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
                                                   | Const -> v
                                                   | Var -> V.Mut (ref v)
                                        in
                                          define_var private_context id defn;
                                          define_members efs)
				     | [] -> k (V.Obj public_ve) 

                            in 
                                 define_members efs)));
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
			     set_result async r;
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
   | VarP v -> V.Env.singleton v.it (ref None)
   | LitP l -> V.Env.empty
   | TupP ps -> declare_pats context V.Env.empty ps
   | AnnotP(p,t) ->
     declare_pat context p 

and define_pat context p v =
   match p.it with
   | WildP -> ()
   | VarP var -> define_var context var v
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
   | VarP var -> Some (V.Env.singleton var.it (ref (Some v)))
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
