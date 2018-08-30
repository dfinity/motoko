open Syntax
open Source
open Printf

module V = Value
module T = Type


(* Context *)

type val_env = V.def V.Env.t
type lab_env = V.value V.cont V.Env.t
type ret_env = V.value V.cont option

type scope = val_env

type context =
  {
    vals : val_env;
    labs : lab_env;
    rets : ret_env;
    async : bool
  }

let adjoin_vals c ve = {c with vals = V.Env.adjoin c.vals ve}
let adjoin = adjoin_vals

let empty_context =
  { vals = V.Env.empty;
    labs = V.Env.empty;
    rets = None;
    async = false
  }


(* Error handling *)

exception Trap of Source.region * string

let trap at fmt =  Printf.ksprintf (fun s -> raise (Trap (at, s))) fmt

let unimplemented at msg = trap at "NOT YET IMPLEMENTED: %s" msg


(* Debugging aids *)

let last_context = ref empty_context
let last_region = ref Source.no_region
let call_depth = ref 0

let get_last_context () = !last_context
let get_last_region () = !last_region
let get_indent () = String.make (2 * !call_depth) ' '


(* Schedulinbg *)

module Scheduler =
struct
  let q : (unit -> unit) Queue.t = Queue.create ()

  let queue work = Queue.add work q
  let yield () =
    call_depth := 0;
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


(* Literals *)

let interpret_lit context lit : V.value =
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

let rec interpret_exp context exp (k : V.value V.cont) =
  interpret_exp_mut context exp (function V.Mut r -> k !r | v -> k v)

and interpret_exp_mut context exp (k : V.value V.cont) =
  last_region := exp.at;
  last_context := context;
  match exp.it with
  | VarE id ->
    (match Lib.Promise.value_opt (V.Env.find id.it context.vals) with
    | Some v -> k v
    | None -> trap exp.at "accessing identifier before its definition"
    )
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
          trap exp.at "arithmetic overflow")
      )
    )
  | RelE (exp1, op, exp2) ->
    let t = T.immutable exp1.note.note_typ in
    interpret_exp context exp1 (fun v1 ->
      interpret_exp context exp2 (fun v2 ->
        k (Operator.relop t op v1 v2)
      )
    )
  | TupE exps ->
    interpret_exps context exps [] (fun vs -> k (V.Tup vs))
  | ProjE (exp1, n) ->
    interpret_exp context exp1 (fun v1 -> k (List.nth (V.as_tup v1) n))
  | ObjE (sort, id, fields) ->
    interpret_obj context sort id fields k
  | DotE (exp1, id) ->
    interpret_exp context exp1 (fun v1 -> k (V.Env.find id.it (V.as_obj v1)))
  | AssignE (exp1, exp2) ->
    interpret_exp_mut context exp1 (fun v1 ->
      interpret_exp context exp2 (fun v2 ->
        V.as_mut v1 := v2; k V.unit
      )
    )
  | ArrayE exps ->
    let t = exp.note.note_typ in
    interpret_exps context exps [] (fun vs ->
      let vs' =
        match t with
        | T.Array (T.Mut _) -> List.map (fun v -> V.Mut (ref v)) vs
        | _ -> vs
      in k (V.Array (Array.of_list vs'))
    )
  | IdxE (exp1, exp2) ->
    interpret_exp context exp1 (fun v1 ->
      interpret_exp context exp2 (fun v2 ->
        k (V.as_array v1).(V.Nat.to_int (V.as_nat v2)) (* TBR *)
      )
    )
  | CallE (exp1, typs, exp2) ->
    interpret_exp context exp1 (fun v1 ->
      interpret_exp context exp2 (fun v2 ->
        (V.as_func v1) v2 k
      )
    )
  | BlockE decs ->
    interpret_block context decs None k
  | NotE exp1 ->
    interpret_exp context exp1 (fun v1 -> k (V.Bool (not (V.as_bool v1))))
  | AndE (exp1, exp2) ->
    interpret_exp context exp1 (fun v1 ->
      if V.as_bool v1
      then interpret_exp context exp2 k
      else k v1
    )
  | OrE (exp1, exp2) ->
    interpret_exp context exp1 (fun v1 ->
      if V.as_bool v1
      then k v1
      else interpret_exp context exp2 k
    )
  | IfE (exp1, exp2, exp3) ->
    interpret_exp context exp1 (fun v1 ->
      if V.as_bool v1
      then interpret_exp context exp2 k
      else interpret_exp context exp3 k
    )
  | SwitchE (exp1, cases) ->
    interpret_exp context exp1 (fun v1 ->
      interpret_cases context cases exp.at v1 k
    )
  | WhileE (exp1, exp2) ->
    let k_continue = fun v -> interpret_exp context exp k in
    interpret_exp context exp1 (fun v1 ->
      if V.as_bool v1
      then interpret_exp context exp2 k_continue
      else k V.unit
    )
  | LoopE (exp1, None) ->
    interpret_exp context exp1 (fun v -> interpret_exp context exp k)
  | LoopE (exp1, Some exp2) ->
    interpret_exp context exp1 (fun _v1 ->
      interpret_exp context exp2 (fun v2 ->
        if V.as_bool v2
        then interpret_exp context exp k
        else k V.unit
      )
    )
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
    let async = make_async () in
    let k' = fun v1 -> set_async async v1 in
    let context' =
      { context with
        labs = V.Env.empty;
        rets = Some k';
        async = true
      }
    in
    if !Flags.debug then
      printf "%s-> async %s\n" (get_indent ()) (string_of_region exp.at);
    Scheduler.queue (fun () ->
      if !Flags.debug then
        printf "%s<- async %s\n" (get_indent ()) (string_of_region exp.at);
      incr call_depth;
      interpret_exp context' exp1 (fun v ->
        assert (!call_depth > 0);
        if !Flags.debug then
          printf "%s<= %s\n" (get_indent ()) (V.debug_string_of_val v);
        decr call_depth;
        k' v
      )
    );
    k (V.Async async)
  | AwaitE exp1 ->
    interpret_exp context exp1 (fun v1 ->
      if !Flags.debug then
        printf "%s-> await %s\n" (get_indent ()) (string_of_region exp.at);
      decr call_depth;
      get_async (V.as_async v1) (fun v ->
        Scheduler.queue (fun () ->
          if !Flags.debug then
            printf "%s<- await %s%s\n" (get_indent ()) (string_of_region exp.at)
              (V.debug_string_of_tuple_val v);
          incr call_depth;
          k v
        )
      );
      Scheduler.yield ()
    ) 
  | AssertE exp1 ->
    interpret_exp context exp1 (fun  v ->
      if V.as_bool v
      then k V.unit
      else trap exp.at "Assertion failure"
    )
  | IsE (exp1, typ) ->
    unimplemented exp.at "is"
  | AnnotE (exp1, _typ) ->
    interpret_exp context exp1 k
  | DecE dec ->
    interpret_block context [dec] None k

and interpret_exps context exps vs (k : V.value list V.cont) =
  match exps with
  | [] -> k (List.rev vs)
  | exp::exps' ->
    interpret_exp context exp (fun v -> interpret_exps context exps' (v::vs) k)


(* Cases *)

and interpret_cases context cases at v (k : V.value V.cont) =
  match cases with
  | [] ->
    trap at "switch value %s does not match any case" (V.debug_string_of_val v)
  | {it = {pat; exp}; at; _}::cases' ->
    match match_pat pat v with
    | Some ve -> interpret_exp (adjoin_vals context ve) exp k
    | None -> interpret_cases context cases' at v k


(* Patterns *)

and declare_id id =
  V.Env.singleton id.it (Lib.Promise.make ())
    
and declare_pat pat : val_env =
  match pat.it with
  | WildP | LitP _ | SignP _ ->  V.Env.empty
  | VarP id -> declare_id id
  | TupP pats -> declare_pats pats V.Env.empty
  | AnnotP (pat, _typ) -> declare_pat pat

and declare_pats pats ve : val_env =
  match pats with
  | [] -> ve
  | pat::pats' ->
    let ve' = declare_pat pat in
    declare_pats pats' (V.Env.adjoin ve ve')


and define_id context id v =
  Lib.Promise.fulfill (V.Env.find id.it context.vals) v
    
and define_pat context pat v =
  match pat.it with
  | WildP | LitP _ | SignP _ -> ()
  | VarP id -> define_id context id v
  | TupP pats -> define_pats context pats (V.as_tup v)
  | AnnotP (pat1, _typ) -> define_pat context pat1 v

and define_pats context pats vs =
  List.iter2 (define_pat context) pats vs


and match_lit lit v : bool =
  match !lit, v with
  | NullLit, V.Null -> true
  | BoolLit b, V.Bool b' -> b = b'
  | NatLit n, V.Nat n' -> n = n'
  | IntLit i, V.Int i' -> i = i'
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
    let t = T.immutable pat.note.note_typ in
    match_pat {pat with it = LitP lit} (Operator.unop t op v)
  | TupP pats ->
    match_pats pats (V.as_tup v) V.Env.empty
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

and interpret_obj context sort id fields (k : V.value V.cont) =
  let ve0 = declare_id id in
  let private_ve, public_ve = declare_exp_fields fields ve0 V.Env.empty in
  let context' = adjoin_vals context private_ve in
  interpret_fields context' sort.it fields public_ve (fun v ->
    define_id (adjoin_vals context ve0) id v;
    k v
  )


and declare_exp_fields fields private_ve public_ve : val_env * val_env =
  match fields with
  | [] -> private_ve, public_ve
  | {it = {id; mut; priv; _}; _}::fields' ->
    let ve = declare_id id in
    declare_exp_fields fields'
      (V.Env.adjoin private_ve ve) (V.Env.adjoin public_ve ve)


and interpret_fields context s fields ve (k : V.value V.cont) =
  match fields with
  | [] -> k (V.Obj (V.Env.map Lib.Promise.value ve))
  | {it = {id; mut; priv; exp}; _}::fields' ->
    interpret_exp context exp (fun v ->
      let v' =
        if s = T.Actor && priv.it = Public
        then actor_field id exp.note.note_typ v
        else v
      in
      let v'' =
        match mut.it with
        | Const -> v'
        | Var -> V.Mut (ref v')
      in
      define_id context id v'';
      interpret_fields context s fields' ve k
    )


and actor_field id t v : V.value =
  match t with
  | T.Func (_, _, T.Tup []) ->
    let f = V.as_func v in
    V.Func (fun v k -> actor_msg id f v (fun _ -> ()); k V.unit)
  | T.Func (_, _, T.Async _) ->
    let f = V.as_func v in
    V.Func (fun v k ->
      let async = make_async () in
      actor_msg id f v (fun v_async ->
        get_async (V.as_async v_async) (fun v_r -> set_async async v_r)
      );
      k (V.Async async)
    )
  | _ -> assert false


and actor_msg id f v (k : V.value V.cont) =
  if !Flags.debug then
    printf "%s-> message %s%s\n" (get_indent ()) id.it
      (V.debug_string_of_tuple_val v);
  Scheduler.queue (fun () ->
    if !Flags.debug then
      printf "%s<- message %s%s\n" (get_indent ()) id.it
        (V.debug_string_of_tuple_val v);
    incr call_depth;
    f v k
  )


(* Blocks and Declarations *)

and interpret_block context decs ro (k : V.value V.cont) =
  let ve = declare_decs decs V.Env.empty in
  Lib.Option.app (fun r -> r := ve) ro;
  interpret_decs (adjoin_vals context ve) decs k


and declare_dec dec : val_env =     
  match dec.it with
  | ExpD _
  | TypD _ -> V.Env.empty
  | LetD (pat, _) -> declare_pat pat
  | VarD (id, _)
  | FuncD (id, _, _, _, _)
  | ClassD (id, _, _, _, _) -> declare_id id

and declare_decs decs ve : val_env =
  match decs with
  | [] -> ve
  | dec::decs' ->
    let ve' = declare_dec dec in
    declare_decs decs' (V.Env.adjoin ve ve')


and interpret_dec context dec (k : V.value V.cont) =     
  match dec.it with
  | ExpD exp ->
    interpret_exp context exp k
  | LetD (pat, exp) ->
    interpret_exp context exp (fun v ->
      define_pat context pat v;
      k V.unit
    )
  | VarD (id, exp) ->
    interpret_exp context exp (fun v ->
      define_id context id (V.Mut (ref v));
      k V.unit
    )
  | TypD _ ->
    k V.unit
  | FuncD (id, _typbinds, pat, _typ, exp) ->
    let f = interpret_func context id pat
      (fun context' -> interpret_exp context' exp) in
    let v = V.Func f in
    define_id context id v;
    k v
  | ClassD (id, _typbinds, sort, pat, fields) ->
    let f = interpret_func context id pat
      (fun context' k' ->
        let private_ve, public_ve =
          declare_exp_fields fields V.Env.empty V.Env.empty in
        interpret_fields (adjoin_vals context' private_ve) sort.it fields public_ve k'
      )
    in
    let v = V.Func f in
    define_id context id v;
    k v

and interpret_decs context decs (k : V.value V.cont) =
  match decs with
  | [] -> k V.unit
  | [dec] -> interpret_dec context dec k
  | dec::decs' ->
    interpret_dec context dec (fun _v -> interpret_decs context decs' k)


and interpret_func context id pat f v (k : V.value V.cont) =
  if !Flags.debug then
    printf "%s%s%s\n" (get_indent ()) id.it (V.debug_string_of_tuple_val v);
  match match_pat pat v with
  | None ->
    trap pat.at "argument value %s does not patch parameter list"
      (V.debug_string_of_val v)
  | Some ve ->
    incr call_depth;
    let k' = fun v' ->
      if !Flags.debug then
        printf "%s<= %s\n" (get_indent ()) (V.debug_string_of_val v');
      decr call_depth;
      k v'
    in
    let context' =
      { vals = V.Env.adjoin context.vals ve;
        labs = V.Env.empty;
        rets = Some k';
        async = false
      }
    in f context' k'


(* Programs *)

let interpret_prog context p : scope =
  call_depth := 0;
  let r = ref V.Env.empty in
  Scheduler.queue (fun () -> interpret_block context p.it (Some r) ignore);
  Scheduler.run ();
  !r
