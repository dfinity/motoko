open Syntax
open Source

module V = Value
module T = Type


(* Context *)

type val_env = V.def V.Env.t
type lab_env = V.value V.cont V.Env.t
type ret_env = V.value V.cont option

type scope = val_env

type env =
  { vals : val_env;
    labs : lab_env;
    rets : ret_env;
    async : bool
  }

let adjoin_vals c ve = {c with vals = V.Env.adjoin c.vals ve}
let adjoin = adjoin_vals

let empty_env =
  { vals = V.Env.empty;
    labs = V.Env.empty;
    rets = None;
    async = false;
  }


(* Error handling *)

exception Trap of Source.region * string

let trap at fmt = Printf.ksprintf (fun s -> raise (Trap (at, s))) fmt

let find id env =
  try V.Env.find id env
  with Not_found ->
    trap no_region "unbound identifier %s" id
         
(* Tracing *)

let trace_depth = ref 0

let trace fmt =
  Printf.ksprintf (fun s ->
    Printf.printf "%s%s\n" (String.make (2 * !trace_depth) ' ') s
  ) fmt

let string_of_arg = function
  | V.Tup _ as v -> V.string_of_val v
  | v -> "(" ^ V.string_of_val v ^ ")"


(* Debugging aids *)

let last_env = ref empty_env
let last_region = ref Source.no_region

let get_last_env () = !last_env
let get_last_region () = !last_region


(* Scheduling *)

module Scheduler =
struct
  let q : (unit -> unit) Queue.t = Queue.create ()

  let queue work = Queue.add work q
  let yield () =
    trace_depth := 0;
    try Queue.take q () with Trap (at, msg) ->
      Printf.printf "%s: execution error, %s\n" (Source.string_of_region at) msg

  let rec run () =
    if not (Queue.is_empty q) then (yield (); run ())
end


(* Async auxiliary functions *)

let make_async () : V.async =
  {V.result = Lib.Promise.make (); waiters = []}

let get_async async (k : V.value V.cont) =
  match Lib.Promise.value_opt async.V.result with
  | Some v -> k v
  | None -> async.V.waiters <- k::async.V.waiters

let set_async async v =
  List.iter (fun k -> Scheduler.queue (fun () -> k v)) async.V.waiters;
  Lib.Promise.fulfill async.V.result v;
  async.V.waiters <- []

let fulfill async v =
  Scheduler.queue (fun () -> set_async async v)

          
let async at (f: (V.value V.cont) -> unit) (k:V.value V.cont) =
    let async = make_async () in
    (*    let k' = fun v1 -> set_async async v1 in *)
    let k' = fun v1 -> fulfill async v1 in 
    if !Flags.trace then trace "-> async %s" (string_of_region at);
    Scheduler.queue (fun () ->
      if !Flags.trace then trace "<- async %s" (string_of_region at);
      incr trace_depth;
      f (fun v ->
         if !Flags.trace then trace "<= %s" (V.string_of_val v);
         decr trace_depth;
         k' v)
    );
    k (V.Async async)

let await at async k =
  if !Flags.trace then trace "=> await %s" (string_of_region at);
  decr trace_depth;
  get_async async (fun v ->
      Scheduler.queue (fun () ->
          if !Flags.trace then
            trace "<- await %s%s" (string_of_region at) (string_of_arg v);
          incr trace_depth;
          k v
        )
    )
(*;  Scheduler.yield () *)

let actor_msg id f v (k : V.value V.cont) =
  if !Flags.trace then trace "-> message %s%s" id (string_of_arg v);
  Scheduler.queue (fun () ->
    if !Flags.trace then trace "<- message %s%s" id (string_of_arg v);
    incr trace_depth;
    f v k
  )
                  
let make_unit_message id v =
    let _, f = V.as_func v in
    V.Func (None, fun v k -> actor_msg id f v (fun _ -> ()); k V.unit)

let make_async_message id v =
    assert (not !Flags.async_lowering);
    let _, f = V.as_func v in
    V.Func (None, fun v k ->
      let async = make_async () in
      actor_msg id f v (fun v_async ->
        get_async (V.as_async v_async) (fun v_r -> set_async async v_r)
      );
      k (V.Async async)
    )

let make_message id t v : V.value =
  match t with
  | T.Func (_, _, _, T.Tup []) ->
     make_unit_message id.it v
  | T.Func (_, _, _, T.Async _) ->
     assert (not !Flags.async_lowering);
     make_async_message id.it v
  | _ ->
     failwith (Printf.sprintf "actorfield: %s %s" id.it (T.string_of_typ t))
     (*     assert false *)

  
let extended_prim s at =
  match s with
  | "@async" ->
     assert(!Flags.await_lowering && not(!Flags.async_lowering)); 
     fun v k ->
     let (call,f) = V.as_func v in
     async at
       (fun k' ->
         let k' = V.Func(None,fun v _ -> k' v) in
         f k' V.as_unit)
       k
  | "@await" ->
     assert(!Flags.await_lowering && not(!Flags.async_lowering)); 
     fun v k ->
     begin
       match V.as_tup v with
      | [async; w] ->
        let (_,f) = V.as_func w in
        await at (V.as_async async) (fun v -> f v k) 
      | _ -> assert false
     end
  | _ -> Prelude.prim s

                       
(* Literals *)

let interpret_lit env lit : V.value =
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

let rec interpret_exp env exp (k : V.value V.cont) =
  interpret_exp_mut env exp (function V.Mut r -> k !r | v -> k v)

and interpret_exp_mut env exp (k : V.value V.cont) =
  last_region := exp.at;
  last_env := env;
  match exp.it with
  | PrimE s ->
    let at = exp.at in
    k (V.Func (None, extended_prim s at))
  | VarE id ->
    (match Lib.Promise.value_opt (find id.it env.vals) with
    | Some v -> k v
    | None -> trap exp.at "accessing identifier before its definition"
    )
  | LitE lit ->
    k (interpret_lit env lit)
  | UnE (op, exp1) ->
    let t = T.as_immut exp.note.note_typ in
    interpret_exp env exp1 (fun v1 -> k (Operator.unop t op v1))
  | BinE (exp1, op, exp2) ->
    let t = T.as_immut exp.note.note_typ in
    interpret_exp env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        k (try Operator.binop t op v1 v2 with _ ->
          trap exp.at "arithmetic overflow")
      )
    )
  | RelE (exp1, op, exp2) ->
    let t = T.lub Con.Env.empty (* both types are primitive *)
      (T.as_immut exp1.note.note_typ) (T.as_immut exp2.note.note_typ) in
    interpret_exp env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        k (Operator.relop t op v1 v2)
      )
    )
  | TupE exps ->
    interpret_exps env exps [] (fun vs -> k (V.Tup vs))
  | OptE exp1 ->
    interpret_exp env exp1 (fun v1 -> k (V.Opt v1))
  | ProjE (exp1, n) ->
    interpret_exp env exp1 (fun v1 -> k (List.nth (V.as_tup v1) n))
  | ObjE (sort, id, fields) ->
    interpret_obj env sort id fields k
  | DotE (exp1, {it = Name n;_}) ->
    interpret_exp env exp1 (fun v1 ->
      let _, fs = V.as_obj v1 in
      k (find n fs)
    )
  | AssignE (exp1, exp2) ->
    interpret_exp_mut env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        V.as_mut v1 := v2; k V.unit
      )
    )
  | ArrayE exps ->
    let t = exp.note.note_typ in
    interpret_exps env exps [] (fun vs ->
      let vs' =
        match t with
        | T.Array (T.Mut _) -> List.map (fun v -> V.Mut (ref v)) vs
        | _ -> vs
      in k (V.Array (Array.of_list vs'))
    )
  | IdxE (exp1, exp2) ->
    interpret_exp env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        k (V.as_array v1).(V.Nat.to_int (V.as_nat v2)) (* TBR *)
      )
    )
  | CallE (exp1, typs, exp2) ->
    interpret_exp env exp1 (fun v1 ->
        interpret_exp env exp2 (fun v2 ->
            let _, f = V.as_func v1 in f v2 k
                                    
(*
        try     
          let _, f = V.as_func v1 in f v2 k
        with Invalid_argument s ->
          trap exp.at "%s" s
*)
      )
    )
  | BlockE decs ->
    interpret_block env decs None k
  | NotE exp1 ->
    interpret_exp env exp1 (fun v1 -> k (V.Bool (not (V.as_bool v1))))
  | AndE (exp1, exp2) ->
    interpret_exp env exp1 (fun v1 ->
      if V.as_bool v1
      then interpret_exp env exp2 k
      else k v1
    )
  | OrE (exp1, exp2) ->
    interpret_exp env exp1 (fun v1 ->
      if V.as_bool v1
      then k v1
      else interpret_exp env exp2 k
    )
  | IfE (exp1, exp2, exp3) ->
    interpret_exp env exp1 (fun v1 ->
      if V.as_bool v1
      then interpret_exp env exp2 k
      else interpret_exp env exp3 k
    )
  | SwitchE (exp1, cases) ->
    interpret_exp env exp1 (fun v1 ->
      interpret_cases env cases exp.at v1 k
    )
  | WhileE (exp1, exp2) ->
    let k_continue = fun v -> V.as_unit v; interpret_exp env exp k in
    interpret_exp env exp1 (fun v1 ->
      if V.as_bool v1
      then interpret_exp env exp2 k_continue
      else k V.unit
    )
  | LoopE (exp1, None) ->
    interpret_exp env exp1 (fun v -> V.as_unit v; interpret_exp env exp k)
  | LoopE (exp1, Some exp2) ->
    interpret_exp env exp1 (fun v1 ->
      V.as_unit v1;
      interpret_exp env exp2 (fun v2 ->
        if V.as_bool v2
        then interpret_exp env exp k
        else k V.unit
      )
    )
  | ForE (pat, exp1, exp2) ->
    interpret_exp env exp1 (fun v1 ->
      let _, fs = V.as_obj v1 in
      let _, next = V.as_func (find "next" fs) in
      let rec k_continue = fun v ->
        V.as_unit v;
        next V.unit (fun v' ->
          match v' with
          | V.Opt v1 -> (match match_pat pat v1 with
             | None ->
               trap pat.at "value %s does not match pattern" (V.string_of_val v')
             | Some ve ->
               interpret_exp (adjoin_vals env ve) exp2 k_continue)
          | V.Null -> k V.unit
          | _ -> assert false
        )
      in k_continue V.unit
    )
  | LabelE (id, _typ, exp1) ->
    let env' = {env with labs = V.Env.add id.it k env.labs} in
    interpret_exp env' exp1 k
  | BreakE (id, exp1) ->
    interpret_exp env exp1 (find id.it env.labs)
  | RetE exp1 ->
     interpret_exp env exp1 (Lib.Option.value env.rets)
  | AsyncE exp1 ->
    assert(not(!Flags.await_lowering)); 
    async
      exp.at
      (fun k' -> let env' = {env with labs = V.Env.empty; rets = Some k'; async = true}
                 in interpret_exp env' exp1 k')
      k

  | AwaitE exp1 ->
    assert(not(!Flags.await_lowering));
    interpret_exp env exp1
      (fun v1 -> await exp.at (V.as_async v1) k) 
  | AssertE exp1 ->
    interpret_exp env exp1 (fun  v ->
      if V.as_bool v
      then k V.unit
      else trap exp.at "assertion failure"
    )
  | IsE (exp1, exp2) ->
    interpret_exp env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        let b =
          match v1 with
          | V.Obj (Some c1, _) ->
            let c2, _ = V.as_func v2 in
            Some c1 = c2
          | _ -> false
        in k (V.Bool b)
      )
    )
  | AnnotE (exp1, _typ) ->
    interpret_exp env exp1 k
  | DecE dec ->
    interpret_block env [dec] None k
  | DeclareE (id, typ, exp1) ->
    let env = adjoin_vals env (declare_id id) in
    interpret_exp env exp1 k
  | DefineE (id, mut, exp1) ->
    interpret_exp env exp1 (fun v ->
      let v' =
        match mut.it with
        | Const -> v
        | Var -> V.Mut (ref v)
      in
      define_id env id v';
      k V.unit
      )
  | NewObjE (sort,ids) ->
     let ve = List.fold_left
                (fun ve (name,id) -> V.Env.disjoint_add (string_of_name name.it)
                                (Lib.Promise.value (find id.it env.vals)) ve)
                V.Env.empty ids
     in
     k (V.Obj (None, ve))
     


and interpret_exps env exps vs (k : V.value list V.cont) =
  match exps with
  | [] -> k (List.rev vs)
  | exp::exps' ->
    interpret_exp env exp (fun v -> interpret_exps env exps' (v::vs) k)


(* Cases *)

and interpret_cases env cases at v (k : V.value V.cont) =
  match cases with
  | [] ->
    trap at "switch value %s does not match any case" (V.string_of_val v)
  | {it = {pat; exp}; at; _}::cases' ->
    match match_pat pat v with
    | Some ve -> interpret_exp (adjoin_vals env ve) exp k
    | None -> interpret_cases env cases' at v k


(* Patterns *)

and declare_id id =
  V.Env.singleton id.it (Lib.Promise.make ())
    
and declare_pat pat : val_env =
  match pat.it with
  | WildP | LitP _ | SignP _ ->  V.Env.empty
  | VarP id -> declare_id id
  | TupP pats -> declare_pats pats V.Env.empty
  | OptP pat1 -> declare_pat pat1
  | AltP (pat1, pat2) -> declare_pat pat1
  | AnnotP (pat1, _typ) -> declare_pat pat1

and declare_pats pats ve : val_env =
  match pats with
  | [] -> ve
  | pat::pats' ->
    let ve' = declare_pat pat in
    declare_pats pats' (V.Env.adjoin ve ve')


and define_id env id v =
  Lib.Promise.fulfill (find id.it env.vals) v
    
and define_pat env pat v =
  match pat.it with
  | WildP -> ()
  | LitP _ | SignP _ | AltP _ ->
    if match_pat pat v = None
    then trap pat.at "value %s does not match pattern" (V.string_of_val v)
    else ()
  | VarP id -> define_id env id v
  | TupP pats -> define_pats env pats (V.as_tup v)
  | OptP pat1 ->
    (match v with
    | V.Opt v1 -> define_pat env pat1 v1
    | V.Null ->
      trap pat.at "value %s does not match pattern" (V.string_of_val v)
    | _ -> assert false
    )
  | AnnotP (pat1, _typ) -> define_pat env pat1 v

and define_pats env pats vs =
  List.iter2 (define_pat env) pats vs


and match_lit lit v : bool =
  match !lit, v with
  | NullLit, V.Null -> true
  | BoolLit b, V.Bool b' -> b = b'
  | NatLit n, V.Nat n' -> V.Nat.eq n n'
  | IntLit i, V.Int i' -> V.Int.eq i i'
  | Word8Lit w, V.Word8 w' -> w = w'
  | Word16Lit w, V.Word16 w' -> w = w'
  | Word32Lit w, V.Word32 w' -> w = w'
  | Word64Lit w, V.Word64 w' -> w = w'
  | FloatLit z, V.Float z' -> z = z'
  | CharLit c, V.Char c' -> c = c'
  | TextLit u, V.Text u' -> u = u'
  | PreLit _, _ -> assert false
  | _ -> false

and match_pat pat v : val_env option =
  match pat.it with 
  | WildP -> Some V.Env.empty
  | VarP id -> Some (V.Env.singleton id.it (Lib.Promise.make_fulfilled v))
  | LitP lit ->
    if match_lit lit v
    then Some V.Env.empty
    else None
  | SignP (op, lit) ->
    let t = T.as_immut pat.note.note_typ in
    match_pat {pat with it = LitP lit} (Operator.unop t op v)
  | TupP pats ->
    match_pats pats (V.as_tup v) V.Env.empty
  | OptP pat1 ->
    (match v with
    | V.Opt v1 -> match_pat pat1 v1
    | V.Null -> None
    | _ -> assert false
    )
  | AltP (pat1, pat2) ->
    (match match_pat pat1 v with
    | None -> match_pat pat2 v
    | some -> some
    )
  | AnnotP (pat1, _typ) ->
    match_pat pat1 v

and match_pats pats vs ve : val_env option =
  match pats, vs with
  | [], [] -> Some ve
  | pat::pats', v::vs' ->
    (match match_pat pat v with 
    | Some ve' -> match_pats pats' vs' (V.Env.adjoin ve ve')
    | None -> None
    )
  | _ -> assert false


(* Objects *)

and interpret_obj env sort id fields (k : V.value V.cont) =
  let ve0 = declare_id id in
  let private_ve, public_ve = declare_exp_fields fields ve0 V.Env.empty in
  let env' = adjoin_vals env private_ve in
  interpret_fields env' sort.it None fields public_ve (fun v ->
    define_id (adjoin_vals env ve0) id v;
    k v
  )

and declare_field (id,name) =
  let p = Lib.Promise.make () in
  V.Env.singleton id.it p,
  V.Env.singleton (string_of_name name.it) p
  
                   
and declare_exp_fields fields private_ve public_ve : val_env * val_env =
  match fields with
  | [] -> private_ve, public_ve
  | {it = {id; name; mut; priv; _}; _}::fields' ->
    let private_ve', public_ve' = declare_field (id,name) in
    declare_exp_fields fields'
      (V.Env.adjoin private_ve private_ve') (V.Env.adjoin public_ve public_ve')


and interpret_fields env s co fields ve (k : V.value V.cont) =
  match fields with
  | [] -> k (V.Obj (co, V.Env.map Lib.Promise.value ve))
  | {it = {id; name; mut; priv; exp}; _}::fields' ->
    interpret_exp env exp (fun v ->
      let v' =
        match mut.it with
        | Const -> v
        | Var -> V.Mut (ref v)
      in
      define_id env id v';
      interpret_fields env s co fields' ve k
    )

(* Blocks and Declarations *)

and interpret_block env decs ro (k : V.value V.cont) =
  let ve = declare_decs decs V.Env.empty in
  Lib.Option.app (fun r -> r := ve) ro;
  interpret_decs (adjoin_vals env ve) decs k


and declare_dec dec : val_env =
  match dec.it with
  | ExpD _
  | TypD _ -> V.Env.empty
  | LetD (pat, _) -> declare_pat pat
  | VarD (id, _)
  | FuncD (_, id, _, _, _, _)
  | ClassD (id, _, _, _, _, _) -> declare_id id

and declare_decs decs ve : val_env =
  match decs with
  | [] -> ve
  | dec::decs' ->
    let ve' = declare_dec dec in
    declare_decs decs' (V.Env.adjoin ve ve')


and interpret_dec env dec (k : V.value V.cont) =
  match dec.it with
  | ExpD exp ->
    interpret_exp env exp k
  | LetD (pat, exp) ->
    interpret_exp env exp (fun v ->
      define_pat env pat v;
      k V.unit
    )
  | VarD (id, exp) ->
    interpret_exp env exp (fun v ->
      define_id env id (V.Mut (ref v));
      k V.unit
    )
  | TypD _ ->
    k V.unit
  | FuncD (_sort, id, _typbinds, pat, _typ, exp) ->
    let f = interpret_func env id pat
      (fun env' -> interpret_exp env' exp) in
    let v = V.Func (None, f) in
    let v =
      match _sort.it with
      | T.Sharable ->
         make_message id dec.note.note_typ v
      |  T.Local -> v
    in                      
    define_id env id v;
    k v
  | ClassD (id, _,  _typbinds, sort, pat, fields) ->
    let c = V.new_class () in
    let f = interpret_func env id pat
      (fun env' k' ->
        let private_ve, public_ve =
          declare_exp_fields fields V.Env.empty V.Env.empty in
        interpret_fields (adjoin_vals env' private_ve) sort.it (Some c)
          fields public_ve k'
      )
    in
    let v = V.Func (Some c, f) in
    define_id env id v;
    k v

and interpret_decs env decs (k : V.value V.cont) =
  match decs with
  | [] -> k V.unit
  | [dec] -> interpret_dec env dec k
  | dec::decs' ->
    interpret_dec env dec (fun _v -> interpret_decs env decs' k)


and interpret_func env id pat f v (k : V.value V.cont) =
  if !Flags.trace then trace "%s%s" id.it (string_of_arg v);
  match match_pat pat v with
  | None ->
    trap pat.at "argument value %s does not match parameter list"
      (V.string_of_val v)
  | Some ve ->
    incr trace_depth;
    let k' = fun v' ->
      if !Flags.trace then trace "<= %s" (V.string_of_val v');
      decr trace_depth;
      k v'
    in
    let env' =
      { vals = V.Env.adjoin env.vals ve;
        labs = V.Env.empty;
        rets = Some k';
        async = false
      }
    in f env' k'


(* Programs *)

let interpret_prog env p : V.value option * scope =
  trace_depth := 0;
  let vo = ref None in
  let ve = ref V.Env.empty in
  Scheduler.queue (fun () ->
    interpret_block env p.it (Some ve) (fun v -> vo := Some v)
  );
  Scheduler.run ();
  !vo, !ve
