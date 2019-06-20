open As_values
open As_frontend
open Syntax
open Source

module V = As_values.Value
module T = As_types.Type
module CC = As_types.Call_conv

(* Context *)

type val_env = V.def V.Env.t
type lib_env = V.value V.Env.t
type lab_env = V.value V.cont V.Env.t
type ret_env = V.value V.cont option

type flags = {
  trace : bool;
  print_depth : int
}

type scope = {
  val_env: V.def V.Env.t;
  lib_env: V.value V.Env.t;
}

type env =
  { flags : flags;
    vals : val_env;
    labs : lab_env;
    libs : lib_env;
    rets : ret_env;
    async : bool
  }

let adjoin_scope scope1 scope2 =
  { val_env = V.Env.adjoin scope1.val_env scope2.val_env;
    lib_env = V.Env.adjoin scope1.lib_env scope2.lib_env;
  }

let adjoin_vals env ve = { env with vals = V.Env.adjoin env.vals ve }

let empty_scope = { val_env = V.Env.empty; lib_env = V.Env.empty }

let library_scope f v scope : scope =
  { scope with lib_env = V.Env.add f v scope.lib_env }

let env_of_scope flags scope =
  { flags;
    vals = scope.val_env;
    libs = scope.lib_env;
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
    Printf.printf "%s%s\n%!" (String.make (2 * !trace_depth) ' ') s
  ) fmt

let string_of_val env = V.string_of_val env.flags.print_depth
let string_of_def flags = V.string_of_def flags.print_depth
let string_of_arg env = function
  | V.Tup _ as v -> string_of_val env v
  | v -> "(" ^ string_of_val env v ^ ")"


(* Debugging aids *)

let last_env = ref (env_of_scope {trace = false; print_depth = 2} empty_scope)
let last_region = ref Source.no_region

let print_exn flags exn =
  Printf.printf "%!";
  let at = Source.string_of_region !last_region in
  Printf.eprintf "%s: internal error, %s\n" at (Printexc.to_string exn);
  Printf.eprintf "\nLast environment:\n";
  Value.Env.iter (fun x d -> Printf.eprintf "%s = %s\n" x (string_of_def flags d))
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


let async env at (f: (V.value V.cont) -> unit) (k : V.value V.cont) =
    let async = make_async () in
    (*    let k' = fun v1 -> set_async async v1 in *)
    let k' = fun v1 -> fulfill async v1 in
    if env.flags.trace then trace "-> async %s" (string_of_region at);
    Scheduler.queue (fun () ->
      if env.flags.trace then trace "<- async %s" (string_of_region at);
      incr trace_depth;
      f (fun v ->
        if env.flags.trace then trace "<= %s" (string_of_val env v);
        decr trace_depth;
        k' v)
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
  | {sort = T.Sharable; n_res = 0; _} ->
    Value.message_func call_conv.n_args (fun v k ->
      actor_msg env id f v (fun _ -> ());
      k V.unit
    )
  | _ -> (* assert false *)
    failwith ("unexpected call_conv " ^ (string_of_call_conv call_conv))

let make_async_message env id v =
  let open CC in
  let call_conv, f = V.as_func v in
  match call_conv with
  | {sort = T.Sharable; control = T.Promises; n_res = 1; _} ->
    Value.async_func call_conv.n_args (fun v k ->
      let async = make_async () in
      actor_msg env id f v (fun v_async ->
        get_async (V.as_async v_async) (fun v_r -> set_async async v_r)
      );
      k (V.Async async)
    )
  | _ -> (* assert false *)
    failwith ("unexpected call_conv " ^ (string_of_call_conv call_conv))


let make_message env name t v : V.value =
  match t with
  | T.Func (_, _, _, _, []) -> make_unit_message env name v
  | T.Func (_, _, _, _, [T.Async _]) -> make_async_message env name v
  | _ -> (* assert false *)
    failwith (Printf.sprintf "actorfield: %s %s" name (T.string_of_typ t))


(* Literals *)

let interpret_lit env lit : V.value =
  match !lit with
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
  | PreLit _ -> assert false


(* Expressions *)

let check_call_conv exp call_conv =
  let open CC in
  let exp_call_conv = call_conv_of_typ exp.note.note_typ in
  if not (exp_call_conv = call_conv) then
    failwith (Printf.sprintf
      "call_conv mismatch: function %s of type %s expecting %s, found %s"
      (Wasm.Sexpr.to_string 80 (Arrange.exp exp))
      (T.string_of_typ exp.note.note_typ)
      (string_of_call_conv exp_call_conv)
      (string_of_call_conv call_conv)
    )

let check_call_conv_arg env exp v call_conv =
  let open CC in
  if call_conv.n_args <> 1 then
  let es = try V.as_tup v with Invalid_argument _ ->
    failwith (Printf.sprintf
      "call %s: calling convention %s cannot handle non-tuple value %s"
      (Wasm.Sexpr.to_string 80 (Arrange.exp exp))
      (string_of_call_conv call_conv)
      (string_of_val env v)
    )
  in
  if List.length es <> call_conv.n_args then
    failwith (Printf.sprintf
      "call %s: calling convention %s got tuple of wrong length %s"
      (Wasm.Sexpr.to_string 80 (Arrange.exp exp))
      (string_of_call_conv call_conv)
      (string_of_val env v)
    )


let rec interpret_exp env exp (k : V.value V.cont) =
  interpret_exp_mut env exp (function V.Mut r -> k !r | v -> k v)

and interpret_exp_mut env exp (k : V.value V.cont) =
  last_region := exp.at;
  last_env := env;
  Profiler.bump_region exp.at ;
  match exp.it with
  | PrimE s ->
    k (V.Func (CC.call_conv_of_typ exp.note.note_typ, Prim.prim s))
  | VarE id ->
    begin match Lib.Promise.value_opt (find id.it env.vals) with
    | Some v -> k v
    | None -> trap exp.at "accessing identifier before its definition"
    end
  | ImportE (f, fp) ->
    k (find !fp env.libs)
  | LitE lit ->
    k (interpret_lit env lit)
  | UnE (ot, op, exp1) ->
    interpret_exp env exp1
      (fun v1 ->
        k (try Operator.unop !ot op v1 with Invalid_argument s -> trap exp.at "%s" s))
  | BinE (ot, exp1, op, exp2) ->
    interpret_exp env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        k (try Operator.binop !ot op v1 v2 with _ ->
          trap exp.at "arithmetic overflow")
      )
    )
  | ShowE (ot, exp1) ->
    interpret_exp env exp1 (fun v ->
      if Show.can_show !ot
      then k (Value.Text (Show.show_val !ot v))
      else raise (Invalid_argument "debug_show"))
  | RelE (ot, exp1, op, exp2) ->
    interpret_exp env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        k (Operator.relop !ot op v1 v2)
      )
    )
  | TupE exps ->
    interpret_exps env exps [] (fun vs -> k (V.Tup vs))
  | OptE exp1 ->
    interpret_exp env exp1 (fun v1 -> k (V.Opt v1))
  | ProjE (exp1, n) ->
    interpret_exp env exp1 (fun v1 -> k (List.nth (V.as_tup v1) n))
  | ObjE (sort, fields) ->
    interpret_obj env sort fields k
  | TagE (i, exp1) ->
    interpret_exp env exp1 (fun v1 -> k (V.Variant (i.it, v1)))
  | DotE (exp1, id) ->
    interpret_exp env exp1 (fun v1 ->
      let fs = V.as_obj v1 in
      k (try find id.it fs with _ -> assert false)
    )
  | AssignE (exp1, exp2) ->
    interpret_exp_mut env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        V.as_mut v1 := v2; k V.unit
      )
    )
  | ArrayE (mut, exps) ->
    interpret_exps env exps [] (fun vs ->
      let vs' =
        match mut.it with
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
  | FuncE (name, _sort, _typbinds, pat, _typ, exp2) ->
    let f = interpret_func env name pat (fun env' -> interpret_exp env' exp2) in
    let v = V.Func (CC.call_conv_of_typ exp.note.note_typ, f) in
    let v' =
      match _sort.it with
      | T.Sharable -> make_message env name exp.note.note_typ v
      | T.Local -> v
    in k v'
  | CallE (exp1, typs, exp2) ->
    interpret_exp env exp1 (fun v1 ->
      interpret_exp env exp2 (fun v2 ->
        let call_conv, f = V.as_func v1 in
        check_call_conv exp1 call_conv;
        check_call_conv_arg env exp v2 call_conv;
        last_region := exp.at; (* in case the following throws *)
        f v2 k
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
      let fs = V.as_obj v1 in
      let _, next = V.as_func (find "next" fs) in
      let rec k_continue = fun v ->
        V.as_unit v;
        next V.unit (fun v' ->
          match v' with
          | V.Opt v1 ->
            (match match_pat pat v1 with
            | None ->
              trap pat.at "value %s does not match pattern" (string_of_val env v')
            | Some ve ->
              interpret_exp (adjoin_vals env ve) exp2 k_continue
            )
          | V.Null -> k V.unit
          | _ -> assert false
        )
      in k_continue V.unit
    )
  | LabelE (id, _typ, exp1) ->
    let env' = {env with labs = V.Env.add id.it k env.labs} in
    Profiler.bump_label id.at id.it ;
    interpret_exp env' exp1 k
  | BreakE (id, exp1) ->
    interpret_exp env exp1 (find id.it env.labs)
  | RetE exp1 ->
    interpret_exp env exp1 (Lib.Option.value env.rets)
  | AsyncE exp1 ->
    async env
      exp.at
      (fun k' ->
        let env' = {env with labs = V.Env.empty; rets = Some k'; async = true}
        in interpret_exp env' exp1 k')
      k

  | AwaitE exp1 ->
    interpret_exp env exp1
      (fun v1 -> await env exp.at (V.as_async v1) k)
  | AssertE exp1 ->
    interpret_exp env exp1 (fun v ->
      if V.as_bool v
      then k V.unit
      else trap exp.at "assertion failure"
    )
  | AnnotE (exp1, _typ) ->
    interpret_exp env exp1 k

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


(* Patterns *)

and declare_id id =
  V.Env.singleton id.it (Lib.Promise.make ())

and declare_pat pat : val_env =
  match pat.it with
  | WildP | LitP _ | SignP _ ->  V.Env.empty
  | VarP id -> declare_id id
  | TupP pats -> declare_pats pats V.Env.empty
  | ObjP pfs -> declare_pat_fields pfs V.Env.empty
  | OptP pat1
  | TagP (_, pat1)
  | AltP (pat1, _)    (* both have empty binders *)
  | AnnotP (pat1, _)
  | ParP pat1 -> declare_pat pat1

and declare_pats pats ve : val_env =
  match pats with
  | [] -> ve
  | pat::pats' ->
    let ve' = declare_pat pat in
    declare_pats pats' (V.Env.adjoin ve ve')

and declare_pat_fields pfs ve : val_env =
  match pfs with
  | [] -> ve
  | pf::pfs' ->
    let ve' = declare_pat pf.it.pat in
    declare_pat_fields pfs' (V.Env.adjoin ve ve')

and define_id env id v =
  Lib.Promise.fulfill (find id.it env.vals) v

and define_pat env pat v =
  let err () = trap pat.at "value %s does not match pattern" (string_of_val env v) in
  match pat.it with
  | WildP -> ()
  | LitP _ | SignP _ | AltP _ ->
    if match_pat pat v = None
    then err ()
    else ()
  | VarP id -> define_id env id v
  | TupP pats -> define_pats env pats (V.as_tup v)
  | ObjP pfs -> define_pat_fields env pfs (V.as_obj v)
  | OptP pat1 ->
    (match v with
    | V.Opt v1 -> define_pat env pat1 v1
    | V.Null -> err ()
    | _ -> assert false
    )
  | TagP (i, pat1) ->
    let lab, v1 = V.as_variant v in
    if lab = i.it
    then define_pat env pat1 v1
    else err ()
  | AnnotP (pat1, _)
  | ParP pat1 -> define_pat env pat1 v

and define_pats env pats vs =
  List.iter2 (define_pat env) pats vs

and define_pat_fields env pfs vs =
  List.iter (define_pat_field env vs) pfs

and define_pat_field env vs pf =
  let v = V.Env.find pf.it.id.it vs in
  define_pat env pf.it.pat v

and match_lit lit v : bool =
  match !lit, v with
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
    let t = T.as_immut pat.note in
    match_pat {pat with it = LitP lit} (Operator.unop t op v)
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
    let k, v1 = V.as_variant v in
    if i.it = k
    then match_pat pat1 v1
    else None
  | AltP (pat1, pat2) ->
    (match match_pat pat1 v with
    | None -> match_pat pat2 v
    | some -> some
    )
  | AnnotP (pat1, _)
  | ParP pat1 ->
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

and match_pat_fields pfs vs ve : val_env option =
  match pfs with
  | [] -> Some ve
  | pf::pfs' ->
    let v = V.Env.find pf.it.id.it vs in
    begin match match_pat pf.it.pat v with
    | Some ve' -> match_pat_fields pfs' vs (V.Env.adjoin ve ve')
    | None -> None
    end

(* Objects *)

and interpret_obj env sort fields (k : V.value V.cont) =
  let ve_ex, ve_in = declare_exp_fields fields V.Env.empty V.Env.empty in
  let env' = adjoin_vals env ve_in in
  interpret_exp_fields env' sort.it fields ve_ex k

and declare_exp_fields fields ve_ex ve_in : val_env * val_env =
  match fields with
  | [] -> ve_ex, ve_in
  | {it = {dec; vis}; _}::fields' ->
    let ve' = declare_dec dec in
    let ve_ex' = if vis.it = Private then ve_ex else V.Env.adjoin ve_ex ve' in
    let ve_in' = V.Env.adjoin ve_in ve' in
    declare_exp_fields fields' ve_ex' ve_in'

and interpret_exp_fields env s fields ve (k : V.value V.cont) =
  match fields with
  | [] -> k (V.Obj (V.Env.map Lib.Promise.value ve))
  | {it = {dec; _}; _}::fields' ->
    interpret_dec env dec (fun _v -> interpret_exp_fields env s fields' ve k)


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
  | VarD (id, _) -> declare_id id
  | ClassD (id, _, _, _, _, _) -> declare_id {id with note = ()}

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
      k v
    )
  | VarD (id, exp) ->
    interpret_exp env exp (fun v ->
      define_id env id (V.Mut (ref v));
      k V.unit
    )
  | TypD _ ->
    k V.unit
  | ClassD (id, _typbinds, sort, pat, id', fields) ->
    let f = interpret_func env id.it pat (fun env' k' ->
      let env'' = adjoin_vals env' (declare_id id') in
      interpret_obj env'' sort fields (fun v' ->
        define_id env'' id' v';
        k' v'
      )
    ) in
    let v = V.Func (CC.call_conv_of_typ dec.note.note_typ, f) in
    define_id env {id with note = ()} v;
    k v

and interpret_decs env decs (k : V.value V.cont) =
  match decs with
  | [] -> k V.unit
  | [dec] -> interpret_dec env dec k
  | dec::decs' ->
    interpret_dec env dec (fun _v -> interpret_decs env decs' k)


and interpret_func env name pat f v (k : V.value V.cont) =
  if env.flags.trace then trace "%s%s" name (string_of_arg env v);
  match match_pat pat v with
  | None ->
    trap pat.at "argument value %s does not match parameter list" (string_of_val env v)
  | Some ve ->
    incr trace_depth;
    let k' = fun v' ->
      if env.flags.trace then trace "<= %s" (string_of_val env v');
      decr trace_depth;
      k v'
    in
    let env' =
      { env with
        vals = V.Env.adjoin env.vals ve;
        libs = env.libs;
        labs = V.Env.empty;
        rets = Some k';
        async = false
      }
    in f env' k'


(* Programs *)

let interpret_prog flags scope p : (V.value * scope) option =
  try
    let env = env_of_scope flags scope in
    trace_depth := 0;
    let vo = ref None in
    let ve = ref V.Env.empty in
    Scheduler.queue (fun () ->
      try interpret_block env p.it (Some ve) (fun v -> vo := Some v)
      with Invalid_argument s -> trap !last_region "%s" s
    );
    Scheduler.run ();
    let scope = { val_env = !ve; lib_env = scope.lib_env } in
    match !vo with
    | Some v -> Some (v, scope)
    | None -> None
  with exn ->
    (* For debugging, should never happen. *)
    print_exn flags exn;
    None


let interpret_library flags scope (filename, p) : scope =
  let env = env_of_scope flags scope in
  trace_depth := 0;
  let vo = ref None in
  let ve = ref V.Env.empty in
  Scheduler.queue (fun () ->
    interpret_block env p.it (Some ve) (fun v -> vo := Some v)
  );
  Scheduler.run ();
  let v = match p.it with
    | [ { it = ExpD _ ; _ } ] ->
      Lib.Option.value !vo
    (* HACK: to be removed once we restrict libraries to expressions *)
    | ds ->
      V.Obj (V.Env.map Lib.Promise.value (!ve))
  in
  library_scope filename v scope

