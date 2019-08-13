open As_types
open As_values
open Ir_def

open Ir
open Source

module V = Value
module T = Type
module CC = As_types.Call_conv

(* Context *)

type val_env = V.def V.Env.t
type lab_env = V.value V.cont V.Env.t
type ret_env = V.value V.cont option
type throw_env = V.value V.cont option

type scope = val_env

type flags = {
  trace : bool;
  print_depth : int;
}

type env =
  { flags : flags;
    flavor : Ir.flavor;
    vals : val_env;
    labs : lab_env;
    rets : ret_env;
    throws : throw_env;
    async : bool
  }

let adjoin_scope s ve = V.Env.adjoin s ve
let adjoin_vals c ve = {c with vals = adjoin_scope c.vals ve}

let empty_scope = V.Env.empty

let env_of_scope flags flavor ve =
  { flags;
    flavor;
    vals = ve;
    labs = V.Env.empty;
    rets = None;
    throws = None;
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
    Printf.printf "%s%s\n%!" (String.make (2 * !trace_depth) ' ') s
  ) fmt

let string_of_val env = V.string_of_val env.flags.print_depth
let string_of_def flags = V.string_of_def flags.print_depth
let string_of_arg env = function
  | V.Tup _ as v -> string_of_val env v
  | v -> "(" ^ string_of_val env v ^ ")"


(* Debugging aids *)

let last_env = ref (env_of_scope { trace = false; print_depth = 2} Ir.full_flavor empty_scope)
let last_region = ref Source.no_region

let print_exn flags exn =
  Printf.printf "%!";
  let at = Source.string_of_region !last_region in
  Printf.eprintf "%s: internal error, %s\n" at (Printexc.to_string exn);
  Printf.eprintf "\nLast environment:\n";
  Value.Env.iter
    (fun x d -> Printf.eprintf "%s = %s\n" x (string_of_def flags d))
    !last_env.vals;
  Printf.eprintf "\n";
  Printexc.print_backtrace stderr;
  Printf.eprintf "%!"

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

(* Are these just duplicated of the corresponding functions in interpret.ml? If so, refactor *)

let make_async () : V.async =
  {V.result = Lib.Promise.make (); waiters = []}

let get_async async (k : V.value V.cont) (r : V.value V.cont) =
  match Lib.Promise.value_opt async.V.result with
  | Some v -> k v
  | None -> async.V.waiters <- (k,r)::async.V.waiters

let set_async async v =
  List.iter (fun (k,_) -> Scheduler.queue (fun () -> k v)) async.V.waiters;
  Lib.Promise.fulfill async.V.result v;
  async.V.waiters <- []

let reject_async async v =
  List.iter (fun (_,r) -> Scheduler.queue (fun () -> r v)) async.V.waiters;
  Lib.Promise.fulfill async.V.result v;
  async.V.waiters <- []

let fulfill async v =
  Scheduler.queue (fun () -> set_async async v)

let reject async v =
  Scheduler.queue (fun () -> reject_async async v)

let async env at (f: (V.value V.cont) -> (V.value V.cont) -> unit) (k : V.value V.cont) =
    let async = make_async () in
    (*    let k' = fun v1 -> set_async async v1 in *)
    let k' = fulfill async in
    let r = reject async in
    if env.flags.trace then trace "-> async %s" (string_of_region at);
    Scheduler.queue (fun () ->
      if env.flags.trace then trace "<- async %s" (string_of_region at);
      incr trace_depth;
      f (fun v ->
        if env.flags.trace then trace "<= %s" (string_of_val env v);
        decr trace_depth;
        k' v)
        r
    );
    k (V.Async async)

let await env at async k =
  if env.flags.trace then trace "=> await %s" (string_of_region at);
  decr trace_depth;
  get_async async (fun v ->
      Scheduler.queue (fun () ->
          if env.flags.trace then
            trace "<- await %s%s" (string_of_region at) (string_of_arg env v);
          incr trace_depth;
          k v
        )
    )
(*;  Scheduler.yield () *)

let actor_msg env id f v (k : V.value V.cont) =
  if env.flags.trace then trace "-> message %s%s" id (string_of_arg env v);
  Scheduler.queue (fun () ->
    if env.flags.trace then trace "<- message %s%s" id (string_of_arg env v);
    incr trace_depth;
    f v k
  )

let make_unit_message env id v =
  let open CC in
  let call_conv, f = V.as_func v in
  match call_conv with
  | {sort = T.Shared; n_res = 0; _} ->
    Value.message_func call_conv.n_args (fun v k ->
      actor_msg env id f v (fun _ -> ());
      k V.unit
    )
  | _ ->
    failwith ("unexpected call_conv " ^ string_of_call_conv call_conv)
(* assert (false) *)

let make_async_message env id v =
  assert env.flavor.has_async_typ;
  let open CC in
  let call_conv, f = V.as_func v in
  match call_conv with
  | {sort = T.Shared; control = T.Promises; n_res = 1; _} ->
    Value.async_func call_conv.n_args (fun v k ->
      let async = make_async () in
      actor_msg env id f v (fun v_async ->
        get_async (V.as_async v_async) (set_async async) (reject_async async)
      );
      k (V.Async async)
    )
  | _ ->
    failwith ("unexpected call_conv " ^ string_of_call_conv call_conv)
    (* assert (false) *)


let make_message env x cc v : V.value =
  match cc.CC.control with
  | T.Returns -> make_unit_message env x v
  | T.Promises -> make_async_message env x v


let extended_prim env s typ at =
  match s with
  | "@async" ->
    assert (not env.flavor.has_await && env.flavor.has_async_typ);
    (fun v k ->
      let (_, f) = V.as_func v in
      match typ with
      | T.Func(_, _, _, [T.Func(_, _, _, [f_dom], _)], _) ->
        let call_conv = CC.call_conv_of_typ f_dom in
        async env at
          (fun k' r ->
            let k' = Value.Func (call_conv, fun v _ -> k' v) in
            f k' V.as_unit
          )
          k
(* TBC: One await/async.ml are done, this should become something like
      | T.Func(_, _, _, [T.Func(_, _, _, [f_dom,r_dom], _)], _) ->
        let call_conv = CC.call_conv_of_typ f_dom in
        let call_conv_r = CC.call_conv_of_typ r_dom in
        async env at
          (fun k' r ->
            let k' = Value.Func (call_conv, fun v _ -> k' v) in
            let r' = Value.Func (call_conv_r, fun v _ -> r' v) in
            f (V.Tup(k',r')) V.as_unit
          )
          k
*)
      | _ -> assert false
    )
  | "@await" ->
    assert (not env.flavor.has_await && env.flavor.has_async_typ);
    fun v k ->
      (match V.as_tup v with
      | [async; w] ->
        let (_, f) = V.as_func w in
        await env at (V.as_async async) (fun v -> f v k) (Lib.Option.value env.throws)
      | _ -> assert false
      )
  | _ -> Prim.prim s


(* Literals *)

let interpret_lit env lit : V.value =
  match lit with
  | NullLit -> V.Null
  | BoolLit b -> V.Bool b
  | NatLit n -> V.Int n
  | Nat8Lit n -> V.Nat8 n
  | Nat16Lit n -> V.Nat16 n
  | Nat32Lit n -> V.Nat32 n
  | Nat64Lit n -> V.Nat64 n
  | IntLit i -> V.Int i
  | Int8Lit i -> V.Int8 i
  | Int16Lit i -> V.Int16 i
  | Int32Lit i -> V.Int32 i
  | Int64Lit i -> V.Int64 i
  | Word8Lit w -> V.Word8 w
  | Word16Lit w -> V.Word16 w
  | Word32Lit w -> V.Word32 w
  | Word64Lit w -> V.Word64 w
  | FloatLit f -> V.Float f
  | CharLit c -> V.Char c
  | TextLit s -> V.Text s

(* Expressions *)

let check_call_conv exp call_conv =
  let open Call_conv in
  let exp_call_conv = call_conv_of_typ exp.note.note_typ in
  if not (exp_call_conv = call_conv) then
    failwith (Printf.sprintf "call_conv mismatch: function %s of type %s expecting %s, found %s"
      (Wasm.Sexpr.to_string 80 (Arrange_ir.exp exp))
      (T.string_of_typ exp.note.note_typ)
      (string_of_call_conv exp_call_conv)
      (string_of_call_conv call_conv))

let check_call_conv_arg env exp v call_conv =
  let open CC in
  if call_conv.n_args <> 1 then
  let es = try V.as_tup v
    with Invalid_argument _ ->
      failwith (Printf.sprintf "call %s: calling convention %s cannot handle non-tuple value %s"
        (Wasm.Sexpr.to_string 80 (Arrange_ir.exp exp))
        (string_of_call_conv call_conv)
        (string_of_val env v)) in
  if List.length es <> call_conv.n_args then
    failwith (Printf.sprintf "call %s: calling convention %s got tuple of wrong length %s"
        (Wasm.Sexpr.to_string 80 (Arrange_ir.exp exp))
        (string_of_call_conv call_conv)
        (string_of_val env v))


let rec interpret_exp env exp (k : V.value V.cont) =
  interpret_exp_mut env exp (function V.Mut r -> k !r | v -> k v)

and interpret_exp_mut env exp (k : V.value V.cont) =
  let open Call_conv in
  last_region := exp.at;
  last_env := env;
  Profiler.bump_region exp.at ;
  match exp.it with
  | VarE id ->
    (match Lib.Promise.value_opt (find id env.vals) with
    | Some v -> k v
    | None -> trap exp.at "accessing identifier before its definition"
    )
  | LitE lit ->
    k (interpret_lit env lit)
  | PrimE (p, es) ->
    begin match p, es with
    | UnPrim (ot, op), [exp1] ->
      interpret_exp env exp1 (fun v1 -> k (try Operator.unop op ot v1 with Invalid_argument s -> trap exp.at "%s" s))
    | BinPrim (ot, op), [exp1; exp2] ->
      interpret_exp env exp1 (fun v1 ->
        interpret_exp env exp2 (fun v2 ->
          k (try Operator.binop op ot v1 v2 with _ ->
            trap exp.at "arithmetic overflow")
        )
      )
    | RelPrim (ot, op), [exp1; exp2] ->
      interpret_exp env exp1 (fun v1 ->
        interpret_exp env exp2 (fun v2 ->
          k (Operator.relop op ot v1 v2)
        )
      )
    | ShowPrim ot, [exp1] ->
      interpret_exp env exp1 (fun v ->
        if Show.can_show ot
        then k (Value.Text (Show.show_val ot v))
        else raise (Invalid_argument "debug_show"))
    | SerializePrim t, [exp1] ->
      interpret_exp env exp1 (fun v -> k (V.Serialized v))
    | DeserializePrim t, [exp1] ->
      interpret_exp env exp1 (fun v -> k (V.as_serialized v))
    | OtherPrim s, exps ->
      interpret_exps env exps [] (fun vs ->
        let at = exp.at in
        let t = exp.note.note_typ in
        let arg = match vs with [v] -> v | _ -> V.Tup vs in
        extended_prim env s t at arg k
      )
    | NumConvPrim (t1, t2), exps ->
      interpret_exps env exps [] (fun vs ->
        let arg = match vs with [v] -> v | _ -> V.Tup vs in
        Prim.num_conv_prim t1 t2 arg k
      )
    | _ ->
      trap exp.at "Unknown prim or wrong number of arguments (%d given):\n  %s"
        (List.length es) (Wasm.Sexpr.to_string 80 (Arrange_ir.prim p))
    end
  | TupE exps ->
    interpret_exps env exps [] (fun vs -> k (V.Tup vs))
  | OptE exp1 ->
    interpret_exp env exp1 (fun v1 -> k (V.Opt v1))
  | TagE (i, exp1) ->
    interpret_exp env exp1 (fun v1 -> k (V.Variant (i, v1)))
  | ProjE (exp1, n) ->
    interpret_exp env exp1 (fun v1 -> k (List.nth (V.as_tup v1) n))
  | DotE (exp1, n)
  | ActorDotE (exp1, n) ->
    interpret_exp env exp1 (fun v1 ->
      let fs = V.as_obj v1 in
      k (try find n fs with _ -> assert false)
    )
  | AssignE (exp1, exp2) ->
    interpret_exp_mut env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        V.as_mut v1 := v2; k V.unit
      )
    )
  | ArrayE (mut, _, exps) ->
    interpret_exps env exps [] (fun vs ->
      let vs' =
        match mut with
        | Var -> List.map (fun v -> V.Mut (ref v)) vs
        | Const -> vs
      in k (V.Array (Array.of_list vs'))
    )
  | IdxE (exp1, exp2) ->
    interpret_exp env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        k (try (V.as_array v1).(V.Int.to_int (V.as_int v2))
           with Invalid_argument s -> trap exp.at "%s" s)
      )
    )
  | CallE (_cc, exp1, typs, exp2) ->
    interpret_exp env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        let call_conv, f = V.as_func v1 in
        check_call_conv exp1 call_conv;
        check_call_conv_arg env exp v2 call_conv;
        last_region := exp.at; (* in case the following throws *)
        f v2 k
      )
    )
  | BlockE (decs, exp1) ->
     interpret_block env None decs exp1 k
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
  | TryE (exp1, cases) ->
    let k' = fun v1 -> interpret_catches env cases exp.at v1 k in
    let env' = { env with throws = Some k' } in
    interpret_exp env' exp1 k
  | LoopE exp1 ->
    interpret_exp env exp1 (fun v -> V.as_unit v; interpret_exp env exp k)
  | LabelE (id, _typ, exp1) ->
    let env' = {env with labs = V.Env.add id k env.labs} in
    interpret_exp env' exp1 k
  | BreakE (id, exp1) ->
    interpret_exp env exp1 (find id env.labs)
  | RetE exp1 ->
    interpret_exp env exp1 (Lib.Option.value env.rets)
  | ThrowE exp1 ->
    interpret_exp env exp1 (Lib.Option.value env.throws)
  | AsyncE exp1 ->
    assert env.flavor.has_await;
    async env
      exp.at
      (fun k' r ->
        let env' = {env with labs = V.Env.empty; rets = Some k'; throws = Some r; async = true}
        in interpret_exp env' exp1 k')
      k

  | AwaitE exp1 ->
    assert env.flavor.has_await;
    interpret_exp env exp1
      (fun v1 -> await env exp.at (V.as_async v1) k (Lib.Option.value env.throws))
  | AssertE exp1 ->
    interpret_exp env exp1 (fun v ->
      if V.as_bool v
      then k V.unit
      else trap exp.at "assertion failure"
    )
  | DeclareE (id, typ, exp1) ->
    let env = adjoin_vals env (declare_id id) in
    interpret_exp env exp1 k
  | DefineE (id, mut, exp1) ->
    interpret_exp env exp1 (fun v ->
      let v' =
        match mut with
        | Const -> v
        | Var -> V.Mut (ref v)
      in
      define_id env id v';
      k V.unit
      )
  | FuncE (x, cc, _typbinds, args, _typ, e) ->
    let f = interpret_func env exp.at x args
      (fun env' -> interpret_exp env' e) in
    let v = V.Func (cc, f) in
    let v =
      match cc.sort with
      | T.Shared -> make_message env x cc v
      | _-> v
    in
    k v
  | ActorE (id, ds, fs, _) ->
    let ve = declare_decs ds (declare_id id) in
    let env' = adjoin_vals env ve in
    interpret_decs env' ds (fun _ ->
      let obj = interpret_fields env' fs in
      define_id env' id obj;
      k obj)
  | NewObjE (sort, fs, _) ->
    k (interpret_fields env fs)

and interpret_fields env fs =
    let ve =
      List.fold_left
        (fun ve (f : field) ->
          V.Env.disjoint_add f.it.name (Lib.Promise.value (find f.it.var env.vals)) ve
        ) V.Env.empty fs in
    V.Obj ve

and interpret_exps env exps vs (k : V.value list V.cont) =
  match exps with
  | [] -> k (List.rev vs)
  | exp::exps' ->
    interpret_exp env exp (fun v -> interpret_exps env exps' (v::vs) k)


(* Cases *)

and interpret_cases env cases at v (k : V.value V.cont) =
  match cases with
  | [] ->
    trap at "switch value %s does not match any case" (string_of_val env v)
  | {it = {pat; exp}; at; _}::cases' ->
    match match_pat pat v with
    | Some ve -> interpret_exp (adjoin_vals env ve) exp k
    | None -> interpret_cases env cases' at v k

(* Catches *)

and interpret_catches env cases at v (k : V.value V.cont) =
  match cases with
  | [] ->
    Lib.Option.value (env.throws) v (* re-throw v *)
  | {it = {pat; exp}; at; _}::cases' ->
    match match_pat pat v with
    | Some ve -> interpret_exp (adjoin_vals env ve) exp k
    | None -> interpret_catches env cases' at v k

(* Argument lists *)

and match_arg a v : val_env = V.Env.singleton a.it (Lib.Promise.make_fulfilled v)

and match_args at args v : val_env =
  match args with
  | [a] -> match_arg a v
  | _ ->
    let vs = V.as_tup v in
    assert (List.length vs = List.length args);
    List.fold_left V.Env.adjoin V.Env.empty (List.map2 match_arg args vs)

(* Patterns *)

and declare_id id =
  V.Env.singleton id (Lib.Promise.make ())

and declare_pat pat : val_env =
  match pat.it with
  | WildP | LitP _ ->  V.Env.empty
  | VarP id -> declare_id id
  | TupP pats -> declare_pats pats V.Env.empty
  | ObjP pfs -> declare_pats (pats_of_obj_pat pfs) V.Env.empty
  | OptP pat1
  | TagP (_, pat1) -> declare_pat pat1
  | AltP (pat1, pat2) -> declare_pat pat1

and declare_pats pats ve : val_env =
  match pats with
  | [] -> ve
  | pat::pats' ->
    let ve' = declare_pat pat in
    declare_pats pats' (V.Env.adjoin ve ve')


and define_id env id v =
  Lib.Promise.fulfill (find id env.vals) v

and define_pat env pat v =
  let err () = trap pat.at "value %s does not match pattern" (string_of_val env v) in
  match pat.it with
  | WildP -> ()
  | LitP _ | AltP _ ->
    if match_pat pat v = None
    then err ()
    else ()
  | VarP id -> define_id env id v
  | TupP pats -> define_pats env pats (V.as_tup v)
  | ObjP pfs -> define_field_pats env pfs (V.as_obj v)
  | OptP pat1 ->
    (match v with
    | V.Opt v1 -> define_pat env pat1 v1
    | V.Null -> err ()
    | _ -> assert false
    )
  | TagP (i, pat1) ->
    let lab, v1 = V.as_variant v in
    if lab = i
    then define_pat env pat1 v1
    else err ()

and define_pats env pats vs =
  List.iter2 (define_pat env) pats vs

and define_field_pats env pfs vs =
  let define_field (pf : pat_field) =
    define_pat env pf.it.pat (V.Env.find pf.it.name vs) in
  List.iter define_field pfs


and match_lit lit v : bool =
  match lit, v with
  | NullLit, V.Null -> true
  | BoolLit b, V.Bool b' -> b = b'
  | NatLit n, V.Int n' -> V.Int.eq n n'
  | Nat8Lit n, V.Nat8 n' -> V.Nat8.eq n n'
  | Nat16Lit n, V.Nat16 n' -> V.Nat16.eq n n'
  | Nat32Lit n, V.Nat32 n' -> V.Nat32.eq n n'
  | Nat64Lit n, V.Nat64 n' -> V.Nat64.eq n n'
  | IntLit i, V.Int i' -> V.Int.eq i i'
  | Int8Lit i, V.Int8 i' -> V.Int_8.eq i i'
  | Int16Lit i, V.Int16 i' -> V.Int_16.eq i i'
  | Int32Lit i, V.Int32 i' -> V.Int_32.eq i i'
  | Int64Lit i, V.Int64 i' -> V.Int_64.eq i i'
  | Word8Lit w, V.Word8 w' -> w = w'
  | Word16Lit w, V.Word16 w' -> w = w'
  | Word32Lit w, V.Word32 w' -> w = w'
  | Word64Lit w, V.Word64 w' -> w = w'
  | FloatLit z, V.Float z' -> z = z'
  | CharLit c, V.Char c' -> c = c'
  | TextLit u, V.Text u' -> u = u'
  | _ -> false

and match_id id v : val_env =
  V.Env.singleton id (Lib.Promise.make_fulfilled v)

and match_pat pat v : val_env option =
  match pat.it with
  | WildP -> Some V.Env.empty
  | VarP id -> Some (match_id id v)
  | LitP lit ->
    if match_lit lit v
    then Some V.Env.empty
    else None
  | TupP pats ->
    match_pats pats (V.as_tup v) V.Env.empty
  | ObjP pfs ->
    match_pat_fields pfs (V.as_obj v) V.Env.empty
  | OptP pat1 ->
    (match v with
    | V.Opt v1 -> match_pat pat1 v1
    | V.Null -> None
    | _ -> assert false
    )
  | TagP (i, pat1) ->
    let tag, v1 = V.as_variant v in
    if i = tag
    then match_pat pat1 v1
    else None
  | AltP (pat1, pat2) ->
    (match match_pat pat1 v with
    | None -> match_pat pat2 v
    | some -> some
    )

and match_pats pats vs ve : val_env option =
  match pats, vs with
  | [], [] -> Some ve
  | pat::pats', v::vs' ->
    (match match_pat pat v with
    | Some ve' -> match_pats pats' vs' (V.Env.adjoin ve ve')
    | None -> None
    )
  | _ -> assert false

and match_pat_fields pfs vs ve : val_env option =
  match pfs with
  | [] -> Some ve
  | pf::pfs' ->
    begin
      match match_pat pf.it.pat (V.Env.find pf.it.name vs) with
      | Some ve' -> match_pat_fields pfs' vs (V.Env.adjoin ve ve')
      | None -> None
    end

(* Blocks and Declarations *)

and interpret_block env ro decs exp k =
  let ve = declare_decs decs V.Env.empty in
  Lib.Option.iter (fun r -> r := ve) ro;
  let env' = adjoin_vals env ve in
  interpret_decs env' decs (fun _ -> interpret_exp env' exp k)

and declare_dec dec : val_env =
  match dec.it with
  | TypD _ -> V.Env.empty
  | LetD (pat, _) -> declare_pat pat
  | VarD (id, _) -> declare_id id

and declare_decs decs ve : val_env =
  match decs with
  | [] -> ve
  | dec::decs' ->
    let ve' = declare_dec dec in
    declare_decs decs' (V.Env.adjoin ve ve')


and interpret_dec env dec k =
  match dec.it with
  | LetD (pat, exp) ->
    interpret_exp env exp (fun v ->
      define_pat env pat v;
      k ()
    )
  | VarD (id, exp) ->
    interpret_exp env exp (fun v ->
      define_id env id (V.Mut (ref v));
      k ()
    )
  | TypD _ -> k ()

and interpret_decs env decs (k : unit V.cont) =
  match decs with
  | [] -> k ()
  | d::ds -> interpret_dec env d (fun () -> interpret_decs env ds k)

and interpret_func env at x args f v (k : V.value V.cont) =
  if env.flags.trace then trace "%s%s" x (string_of_arg env v);
  let ve = match_args at args v in
  incr trace_depth;
  let k' = fun v' ->
    if env.flags.trace then trace "<= %s" (string_of_val env v');
    decr trace_depth;
    k v'
  in
  let env' =
    { env with
      vals = V.Env.adjoin env.vals ve;
      labs = V.Env.empty;
      rets = Some k';
      async = false
    }
  in f env' k'


(* Programs *)

let interpret_prog flags scope ((ds, exp), flavor) : scope =
  let env = env_of_scope flags flavor scope in
  trace_depth := 0;
  let ve = ref V.Env.empty in
  try
    Scheduler.queue (fun () ->
      try interpret_block env (Some ve) ds exp  (fun v -> ())
      with Invalid_argument s -> trap !last_region "%s" s
    );
    Scheduler.run ();
    !ve
  with exn -> print_exn flags exn; !ve

