open Syntax
open Source
open Types
open Typing
open Printf


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
      | OptV of value option (* TBR *)
      | FuncV of (value -> cont -> value)
      | VarV of value ref
      | RecV of recursive
      | AsyncV of async

  and async = {mutable result: value option; mutable waiters : cont list}
  and recursive = {mutable definition: value option}
  and cont = value -> value

  let nullV = NullV
  let null_of_V (NullV) = ()
  let boolV b = BoolV b
  let bool_of_V (BoolV b) = b
  let natV n = NatV n
  let nat_of_V (NatV n) = n
  let intV n = IntV n
  let int_of_V (IntV n) = n
  let wordV w = WordV w
  let word_of_V (WordV w) = w
  let floatV f = FloatV f
  let float_of_V (FloatV f) = f
  let charV c = CharV c
  let char_of_V (CharV c) = c
  let textV s = TextV s
  let text_of_V (TextV s) = s
  let arrV a = ArrV a
  let arr_of_V (ArrV a) = a
  let tupV vs = TupV vs
  let tup_of_V (TupV vs) = vs
  let objV ve = ObjV ve
  let obj_of_V (ObjV ve) = ve
  let optV ve = OptV ve
  let opt_of_V (OptV v) = v
  let funcV f = FuncV f
  let func_of_V (FuncV f) = f
  let unitV = TupV([])
  let asyncV async = AsyncV async
  let recV d = RecV {definition=d}
  let rec_of_V (RecV r) = r
  
  let projV (TupV vs) n = List.nth vs n
  let dotV (ObjV ve) v = Env.find v ve
  let checkV v =
       match v with
       | RecV r ->
          (match r.definition with
           | Some v -> v
	   | None -> failwith "BlackHole" (*TBR*))
       | v -> v

  let assignV (VarV r) v  = r := v;unitV
  let updateV (ArrV a) (IntV i) v  = a.(i) <- v;unitV
  let indexV (ArrV a) (IntV i) = a.(i)
  let applyV (FuncV f) v k = f v k
  let rec derefV v =
      match v with
      | VarV r -> !r

  let notV (BoolV b) = BoolV (not b)
  let async_of_V(AsyncV async) = async
  let set_result async v =
    	match async with
	| {result=None;waiters=waiters} ->
          async.result <- Some v;
          async.waiters <- [];
          List.fold_left (fun runnables waiter -> (fun () -> waiter v)::runnables) [] waiters; 
	| {result=Some _} -> failwith "set_result"
  let get_result async k =
    	match async with
	| {result=Some v} -> k v
	| {result=None;waiters} -> (async.waiters <- k::waiters; unitV)

  let rec debug_val_to_string v =
      match v with
      | NullV  -> "null"
      | BoolV b -> if b then "true" else "false"
      | NatV n -> sprintf "(Nat %i)" n
      | IntV i -> sprintf "%i" i
      | WordV (Word8 w) -> sprintf "%x" w
      | WordV (Word16 w) -> sprintf "%x" w
      | WordV (Word32 w) -> sprintf "%lx" w
      | WordV (Word64 w) -> sprintf "%Lx" w
      | FloatV f -> sprintf "%f" f
      | CharV d -> sprintf "(Char %li)" d (* TBR *)
      | TextV t -> t (* TBR *)
      | TupV vs -> sprintf "(%s)" (String.concat "," (List.map (debug_val_to_string) vs))
      | ObjV ve -> sprintf "{%s}" (String.concat ";" (List.map (fun (v,w) ->
      	              	                  sprintf "%s=%s " v (debug_val_to_string w)) (Env.bindings ve)))
      | ArrV a ->
         sprintf "[%s]" (String.concat ";" (List.map debug_val_to_string  (Array.to_list a)))
      | OptV o ->
        (match o with
        | None -> "null"
        | Some v -> sprintf "Some (%s)" (debug_val_to_string v))
      | FuncV f ->
      	"<func>"
      | VarV r ->
      	sprintf "Var (%s)" (debug_val_to_string !r) (*TBR show address ?*)
      | RecV r ->
  	sprintf "Rec (%s)" (debug_val_to_string (OptV r.definition))
      | AsyncV async ->
        "<async>"
  
  let rec atomic_val_to_string context t v =
    match norm_typ context t with
    | AnyT -> "any"
    | PrimT p ->
      (match p with
      | NullT -> let _ = null_of_V v in "null"
      | IntT -> let i = int_of_V v in sprintf "%i" i
      | BoolT -> if (bool_of_V v) then "true" else "false"
      | FloatT -> string_of_float(float_of_V v)
      | NatT -> string_of_int(nat_of_V v)
      | CharT -> sprintf "%i" (Int32.to_int(char_of_V v)) (* TBR *)
      | WordT w ->
      	let w = word_of_V v in
        (match w with
        | Word8 w -> sprintf "%x" w
        | Word16 w -> sprintf "%x" w
        | Word32 w -> sprintf "%lx" w
        | Word64 w -> sprintf "%Lx" w)
      | TextT -> text_of_V v)
    | VarT (c,[]) ->
       Con.to_string c
    | VarT (c,ts) ->
       sprintf "%s<%s>" (Con.to_string c) (String.concat "," (List.map typ_to_string ts))
    | TupT ts ->
      let vs = tup_of_V v in
      sprintf "(%s)"  (String.concat "," (List.map2 (val_to_string context) ts vs))
    | ObjT(Object,fs) ->
      let ve = obj_of_V v in
      sprintf "{%s}" (String.concat ";" (List.map (fun {var;mut;typ} ->
      	              	                 let v = checkV (Env.find var ve) in
					 let v = match mut with
					         | VarMut -> derefV v
						 | ConstMut -> v
				         in
                                         sprintf "%s=%s " var (atomic_val_to_string context typ v))
                      fs))
    | _ ->
      sprintf "(%s)" (val_to_string context t v)

and val_to_string context t v =
    match norm_typ context t with
    | ArrayT (m,t) ->
      let a = arr_of_V v in
      sprintf "%s[%s]" (match m with VarMut -> " var " |  ConstMut -> "")
      	       (String.concat ";" (List.map (val_to_string context t) (Array.to_list a)))
    | FuncT(_,_,_) ->
      func_of_V v; (* catch errors *)
      "<func>"
    | OptT t ->
      let v = opt_of_V v in
      (match v with
      | None -> "null"
      | Some v -> sprintf "Some %s" (val_to_string context t v))
    | AsyncT t ->
      let {result;waiters} = async_of_V v in
      sprintf "async{%s,%i}" (match result with
                              | None -> "?"
             		      | Some v -> val_to_string context t v)
			      (List.length waiters)
    | LikeT t -> 
      sprintf "like %s" (atomic_typ_to_string t) (* TBR *)
    | ObjT(Actor,fs) ->
      sprintf "actor%s" (atomic_val_to_string context (ObjT(Object,fs)) v)
    | _ -> atomic_val_to_string context t v

end


open Values

module Scheduler =
  struct
    let q = (Queue.create() : (unit -> value) Queue.t)
    let queue work  = Queue.add work q
    let yield() =
        (* printf "\n YIELD "; *)
    	let work = Queue.take q in
	work() 

    let rec run() =
        (* printf "\n RUN "; *)
    	if not (Queue.is_empty q)
	then
	   (yield();
	    run())
        else unitV
  end

open Scheduler
       
let debug = ref true

exception Trap of Source.region * string

let lookup map k = try Some (Env.find k map)  with _ -> None (* TODO: use find_opt in 4.05 *)

type context = {values: value Env.t; constructors: con Env.t; kinds: kind ConEnv.t; label: string option;  breaks: cont Env.t; continues: cont Env.t ; returns: cont option; awaitable: bool}

let union_values c ve = {c with values = union c.values ve}

let prelude = {values = Env.empty;
               constructors = Env.empty;
               kinds = ConEnv.empty;
               label = None;
               breaks = Env.empty;
               continues = Env.empty;
               returns = None;
               awaitable = false}

let last_context = ref prelude
let last_region = ref Source.no_region

let callee_context context ve k_return =
    {values = union context.values ve;
     constructors = Env.empty; (*TBR*)
     kinds = ConEnv.empty;    (*TBR*)
     label = None;
     breaks = Env.empty; 
     continues = Env.empty;
     returns =  Some k_return;
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

let rec interpret_lit context rl =
  match !rl with
    | NullLit -> nullV
    | BoolLit b -> boolV b
    | NatLit n -> natV n
    | IntLit i -> intV i
    | WordLit w -> wordV w
    | FloatLit f -> floatV f
    | CharLit c -> charV c
    | TextLit s -> textV s
    | PreLit s -> failwith "interpret_lit"

and interpret_binop context (PrimT p1) (PrimT p2) v1 bop v2 = 
    match bop with
    | CatOp ->
      textV ((text_of_V v1) ^ (text_of_V v2)) (*TBR will @ work on unicode *)
    | AddOp ->
      (match p1,p2 with
      | IntT,IntT -> intV (int_of_V v1 + int_of_V v2)   (*TBR overflow*)
      | NatT,NatT -> natV (nat_of_V v1 + nat_of_V v2)  (*TBR overflow*)
      | FloatT,FloatT -> floatV (float_of_V v1 +. float_of_V v2)
      | _, _ -> failwith "NYI")
    | SubOp ->
      (match p1,p2 with
      | IntT,IntT -> intV (int_of_V v1 - int_of_V v2)   (*TBR underflow*)
      | NatT,NatT -> natV (nat_of_V v1 - nat_of_V v2)  (*TBR underflow*)
      | FloatT,FloatT -> floatV (float_of_V v1 -. float_of_V v2)
      | _, _ -> failwith "NYI")
    | MulOp ->
      (match p1,p2 with
      | IntT,IntT -> intV (int_of_V v1 * int_of_V v2)   (*TBR overflow*)
      | NatT,NatT -> natV (nat_of_V v1 * nat_of_V v2)  (*TBR overflow*)
      | FloatT,FloatT -> floatV (float_of_V v1 *. float_of_V v2)
      | _, _ -> failwith "NYI")
    | DivOp ->
      (match p1,p2 with
      | IntT,IntT -> intV (int_of_V v1 / int_of_V v2)   (*TBR overflow*)
      | NatT,NatT -> natV (nat_of_V v1 / nat_of_V v2)  (*TBR overflow*)
      | FloatT,FloatT -> floatV (float_of_V v1 /. float_of_V v2)
      | _, _ -> failwith "NYI")
    | ModOp ->
      (match p1,p2 with
      | IntT,IntT -> intV (int_of_V v1 mod int_of_V v2)   (*TBR overflow*)
      | NatT,NatT -> natV (nat_of_V v1 mod nat_of_V v2)  (*TBR overflow*)
      | FloatT,FloatT -> floatV (mod_float (float_of_V v1) (float_of_V v2))
      | _, _ -> failwith "NYI")
    | AndOp | OrOp | XorOp | ShiftLOp | ShiftROp | RotLOp | RotROp ->
      failwith "NYI"

and interpret_relop context (PrimT p1) (PrimT p2) v1 rop v2 =
    (* this is a terrible cheat and probably wrong *)
      match rop with
      | EqOp -> boolV(v1 = v2)
      | NeqOp -> boolV(v1 <> v2)
      | LtOp -> 
       (match p1,p2 with
        | IntT,IntT -> boolV (int_of_V v1 < int_of_V v2)  
        | NatT,NatT -> boolV (nat_of_V v1 < nat_of_V v2)  
        | FloatT,FloatT -> boolV (float_of_V v1 < float_of_V v2)
        | _, _ -> failwith "NYI")
      | LeOp -> 
        (match p1,p2 with
        | IntT,IntT -> boolV (int_of_V v1 <= int_of_V v2) 
        | NatT,NatT -> boolV (nat_of_V v1 <= nat_of_V v2) 
        | FloatT,FloatT -> boolV (float_of_V v1 <= float_of_V v2)
        | _, _ -> failwith "NYI")
      | GtOp -> 
        (match p1,p2 with
        | IntT,IntT -> boolV (int_of_V v1 > int_of_V v2)  
        | NatT,NatT -> boolV (nat_of_V v1 > nat_of_V v2)  
        | FloatT,FloatT -> boolV (float_of_V v1 > float_of_V v2)
        | _, _ -> failwith "NYI")
      | GeOp -> 
        (match p1,p2 with
        | IntT,IntT -> boolV (int_of_V v1 >= int_of_V v2) 
        | NatT,NatT -> boolV (nat_of_V v1 >= nat_of_V v2) 
        | FloatT,FloatT -> boolV (float_of_V v1 >= float_of_V v2)
        | _, _ -> failwith "NYI")


and interpret_uop context (PrimT p) uop v = 
    match uop with
    | PosOp ->
      (match p with
       | IntT -> intV (~+ (int_of_V v))
       | NatT -> v
       | FloatT -> floatV (~+. (float_of_V v)))
    | NegOp ->
      (match p with
       | IntT -> intV (~- (int_of_V v))
       | NatT -> v
       | FloatT -> floatV (~-. (float_of_V v)))
    | NotOp ->
       failwith "NYI"
 
and interpret_exps context vs es k =
    match es with
    | [] -> k (List.rev vs)
    | (e::es) -> interpret_exp context e (fun v -> interpret_exps context (v::vs) es k)
and interpret_exp context e k  =
    last_region := e.at; (* debugging *)
    last_context := context;
    interpret_exp' context e k 

and interpret_exp' (context as context_with_label) e k =
let labelOpt = context.label in
let context = {context with label = None} in
match e.it with
| VarE x ->
    (match x.note with
     | VarMut -> k (derefV (checkV (Env.find x.it context.values)))
     | ConstMut -> k (checkV (Env.find x.it context.values)))
| LitE rl ->
    k (interpret_lit context rl)
| UnE(uop,e1) ->
    let t1 = e1.note in
    interpret_exp context e1 (fun v1 -> k (interpret_uop context t1 uop v1))
| BinE (e1,bop,e2) ->
   let t1 = e1.note in
   let t2 = e2.note in
   interpret_exp context e1 (fun v1 -> interpret_exp context e2 (fun v2 -> k (interpret_binop context t1 t2 v1 bop v2 )))
| RelE (e1,rop,e2) ->
   let t1 = e1.note in
   let t2 = e2.note in
   interpret_exp context e1 (fun v1 -> interpret_exp context e2 (fun v2 -> k (interpret_relop context t1 t2 v1 rop v2 )))
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
       | ConstMut -> (checkV (dotV v1 it)))
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
      let loc = dotV v1 v.it in
      k(assignV loc v2)))
    | IdxE(ea,ei) ->
      interpret_exp context ea (fun va ->
      interpret_exp context ei (fun vi ->
      interpret_exp context e2 (fun v2 -> k(updateV va vi v2))))
    end
| ArrayE es ->
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
    let k_break = k in
    let context' = addBreak context labelOpt k_break in
    interpret_block context' es k
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
  let k_continue = fun v ->
      interpret_exp context_with_label e_while k in
  let context' = addBreakAndContinue context labelOpt k k_continue in
  interpret_exp context e0 (fun v0 ->
  if (bool_of_V v0)
  then interpret_exp context' e1 k_continue
  else k unitV)
| LoopE(e0,None) ->
  let e_loop = e in
  let k_continue = fun v -> interpret_exp context_with_label e_loop k in
  let context' = addBreakAndContinue context labelOpt k k_continue in
  interpret_exp context' e0 k_continue
| LoopE(e0,Some e1) ->
  let e_loop = e in
  let k_continue = fun v ->
      interpret_exp context e1 (fun v1 ->
      if (bool_of_V v1)
      then interpret_exp context_with_label e_loop k
      else k unitV)
  in
  let context' = addBreakAndContinue context labelOpt k k_continue in
  interpret_exp context' e0 k_continue
| ForE(p,e0,e1)->
  failwith "NYI:ForE"
(* labels *)
| LabelE(l,e) ->
  let context = {context with label = Some l.it} in
  interpret_exp context e k
| BreakE(l,e) ->
  let k_break = Env.find l.it context.breaks in
  interpret_exp context e k_break
| ContE l ->
  let k_continue = Env.find l.it context.continues in
  k_continue unitV
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
                 breaks = Env.empty;
                 label = None;
                 continues = Env.empty;
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
  | Some w -> k (checkV w)
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
       Env.singleton v.it (recV None)
    | VarD (v,t,Some e) ->
       Env.singleton v.it (recV None)
    | TypD(v,ts,t) ->
       Env.empty
    | FuncD(v,ts,p,t,e) ->
       Env.singleton v.it (recV None)
    | ClassD(a,v,ts,p,efs) ->
       Env.singleton v.it (recV None)

and declare_decs context ve d =
    match d with
    | [] -> ve
    | d::ds -> declare_decs context (union ve (declare_dec context d)) ds

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
    match rec_of_V (Env.find var.it context.values) with
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
      define_var context var (VarV (ref v));
      k())
    | TypD(v,ts,t) ->
      k()
    | FuncD(var,ts,p,t,e) ->
      (* TBC: trim callee_context *)
      (define_var context var 
         (funcV(fun v k ->
	      if !debug then printf "\n%s(%s)" var.it (debug_val_to_string v);
              match interpret_pat p v with
              | Some ve ->
	        let k = if !debug then fun w -> (printf "\n%s(%s)<-%s" var.it (debug_val_to_string v) (debug_val_to_string w);k w) else k in
                let callee_context = callee_context context ve k in
	      	interpret_exp callee_context e k 
              | None -> failwith "unexpected refuted pattern")));
      k()
    | ClassD(a,c,ts,p,efs) ->
      (define_var context c
         (funcV(fun v k ->
              match interpret_pat p v with
              | None -> failwith "unexpected refuted pattern";
              | Some ve -> let context = union_values context ve in
                           let rec declare_members private_ve public_ve efs =
                                     match efs with
                                     | [] -> (private_ve,public_ve)
                                     | {it={var;mut;priv;exp=_}}::efs ->
                                        let recV = recV None in
                                        declare_members (Env.add var.it recV private_ve)
                                                        (Env.add var.it recV public_ve) efs
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
                                                   | ConstMut -> v
                                                   | VarMut -> VarV (ref v)
                                        in
                                          define_var private_context var defn;
                                          define_members efs)
				     | [] -> k (objV public_ve) 

                            in 
                                 define_members efs)));
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
   | VarP v -> Env.singleton v.it (recV None)
   | LitP l -> Env.empty
   | TupP ps -> declare_pats context Env.empty ps
   | AnnotP(p,t) ->
     declare_pat context p 

and define_pat context p v =
   match p.it with
   | WildP -> ()
   | VarP var -> define_var context var v
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
    | WordLit w -> word_of_V v = w
    | FloatLit f -> float_of_V v = f
    | CharLit c -> char_of_V v = c
    | TextLit s -> text_of_V v = s
    | PreLit s -> failwith "match_lit"
     
and interpret_pat p v =
   match p.it with
   | WildP -> Some Env.empty
   | VarP var -> Some (Env.singleton var.it v)
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

     

    





