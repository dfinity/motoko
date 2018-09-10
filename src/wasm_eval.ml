open Wasm
open Values
open Types
open Instance
open Ast
open Source


(* Errors *)

exception Trap of Source.region * string
exception Crash of Source.region * string

let trap at fmt = Printf.ksprintf (fun s -> raise (Trap (at, s))) fmt
let crash at fmt = Printf.ksprintf (fun s -> raise (Crash (at, s))) fmt

let memory_error at = function
  | Memory.Bounds -> "out of bounds memory access"
  | Memory.SizeOverflow -> "memory size overflow"
  | Memory.SizeLimit -> "memory size limit reached"
  | Memory.Type -> crash at "type mismatch at memory access"
  | exn -> raise exn

let numeric_error at = function
  | Numeric_error.IntegerOverflow -> "integer overflow"
  | Numeric_error.IntegerDivideByZero -> "integer divide by zero"
  | Numeric_error.InvalidConversionToInteger -> "invalid conversion to integer"
  | Eval_numeric.TypeError (i, v, t) ->
    crash at "type error, expected %s as operand %d, got %s"
      (Types.string_of_value_type t) i
      (Types.string_of_value_type (type_of v))
  | exn -> raise exn


(* Configurations *)

type 'a stack = 'a list  (* reversed *)
type label = {branch : 'a. value stack -> Source.region -> 'a}

type frame =
{
  inst : module_inst;
  locals : value ref list;
}

type config =
{
  frame : frame;
  labels : label list;
  return : label;
  steps : int ref;
}

let return = {branch = fun _ _ -> crash no_region "misplaced return"}

let frame inst locals = {inst; locals}
let config inst = {frame = frame inst []; steps = ref 0; labels = []; return}

let lookup category list x =
  try Lib.List32.nth list x.it with Failure _ ->
    crash x.at "undefined %s %ld" category x.it

let type_ (inst : module_inst) x = lookup "type" inst.types x
let func (inst : module_inst) x = lookup "function" inst.funcs x
let table (inst : module_inst) x = lookup "table" inst.tables x
let memory (inst : module_inst) x = lookup "memory" inst.memories x
let global (inst : module_inst) x = lookup "global" inst.globals x
let local (frame : frame) x = lookup "local" frame.locals x
let label (c : config) x = lookup "label" c.labels x

let elem inst x i at =
  match Table.load (table inst x) i with
  | Table.Uninitialized ->
    trap at "uninitialized element %s" (Int32.to_string i)
  | f -> f
  | exception Table.Bounds ->
    trap at "undefined element %s" (Int32.to_string i)

let func_elem inst x i at =
  match elem inst x i at with
  | FuncElem f -> f
  | _ -> crash at "type mismatch for element %s" (Int32.to_string i)

let take n (vs : 'a stack) at =
  try Lib.List.take n vs with Failure _ -> crash at "stack underflow"

let drop n (vs : 'a stack) at =
  try Lib.List.drop n vs with Failure _ -> crash at "stack underflow"


(* Evaluation *)

(*
 * Conventions:
 *   e  : instr
 *   v  : value
 *   es : instr list
 *   vs : value stack
 *   c : config
 *)

module Label () =
struct
  exception Branch of value list
  let label ts =
    {branch = fun vs at -> raise (Branch (take (List.length ts) vs at))}
end

let rec eval_instr (c : config) (vs : value stack) (e : instr) : value stack =
  incr c.steps;
  match e.it, vs with
  | Unreachable, vs ->
    trap e.at "unreachable executed, current operand stack: %s"
      (string_of_values (List.rev vs))

  | Nop, vs ->
    vs

  | Block (ts, es), vs ->
    let module L = Label () in
    (try
      eval_instrs {c with labels = L.label ts :: c.labels} [] es @ vs
    with L.Branch vs' ->
      vs' @ vs
    )

  | Loop (_ts, es), vs ->
    let module L = Label () in
    (try
      eval_instrs {c with labels = L.label [] :: c.labels} [] es @ vs
    with L.Branch vs' ->
      eval_instr c (vs' @ vs) e
    )

  | If (ts, es1, es2), I32 i :: vs' ->
    let module L = Label () in
    (try
      if i <> 0l
      then eval_instrs {c with labels = L.label ts :: c.labels} [] es1 @ vs'
      else eval_instrs {c with labels = L.label ts :: c.labels} [] es2 @ vs'
    with L.Branch vs'' ->
      vs'' @ vs'
    )

  | Br x, vs ->
    (label c x).branch vs e.at

  | BrIf x, I32 i :: vs' ->
    if i <> 0l
    then (label c x).branch vs' e.at
    else vs'

  | BrTable (xs, x), I32 i :: vs' ->
    if I32.lt_u i (Lib.List32.length xs)
    then (label c (Lib.List32.nth xs i)).branch vs' e.at
    else (label c x).branch vs' e.at

  | Return, vs ->
    c.return.branch vs e.at

  | Call x, vs ->
    eval_func c vs (func c.frame.inst x)

  | CallIndirect x, I32 i :: vs ->
    let func = func_elem c.frame.inst (0l @@ e.at) i e.at in
    if type_ c.frame.inst x <> Func.type_of func then
      trap e.at "indirect call type mismatch"
    else
      eval_func c vs func

  | Drop, v :: vs' ->
    vs'

  | Select, I32 i :: v2 :: v1 :: vs' ->
    (if i <> 0l then v1 else v2) :: vs'

  | GetLocal x, vs ->
    !(local c.frame x) :: vs

  | SetLocal x, v :: vs' ->
    local c.frame x := v;
    vs'

  | TeeLocal x, v :: vs' ->
    local c.frame x := v;
    v :: vs'

  | GetGlobal x, vs ->
    Global.load (global c.frame.inst x) :: vs

  | SetGlobal x, v :: vs' ->
    (try Global.store (global c.frame.inst x) v; vs'
    with Global.NotMutable -> crash e.at "write to immutable global"
       | Global.Type -> crash e.at "type mismatch at global write")

  | Load {offset; ty; sz; _}, I32 i :: vs' ->
    let mem = memory c.frame.inst (0l @@ e.at) in
    let addr = I64_convert.extend_u_i32 i in
    (try
      let v =
        match sz with
        | None -> Memory.load_value mem addr offset ty
        | Some (sz, ext) -> Memory.load_packed sz ext mem addr offset ty
      in v :: vs'
    with exn -> trap e.at "%s" (memory_error e.at exn))

  | Store {offset; sz; _}, v :: I32 i :: vs' ->
    let mem = memory c.frame.inst (0l @@ e.at) in
    let addr = I64_convert.extend_u_i32 i in
    (try
      (match sz with
      | None -> Memory.store_value mem addr offset v
      | Some sz -> Memory.store_packed sz mem addr offset v
      );
      vs'
    with exn -> trap e.at "%s" (memory_error e.at exn))

  | CurrentMemory, vs ->
    let mem = memory c.frame.inst (0l @@ e.at) in
    I32 (Memory.size mem) :: vs

  | GrowMemory, I32 delta :: vs' ->
    let mem = memory c.frame.inst (0l @@ e.at) in
    let old_size = Memory.size mem in
    let result =
      try Memory.grow mem delta; old_size
      with Memory.SizeOverflow | Memory.SizeLimit | Memory.OutOfMemory -> -1l
    in I32 result :: vs'

  | Const v, vs ->
    v.it :: vs

  | Test testop, v :: vs' ->
    (try value_of_bool (Eval_numeric.eval_testop testop v) :: vs'
    with exn -> trap e.at "%s" (numeric_error e.at exn))

  | Compare relop, v2 :: v1 :: vs' ->
    (try value_of_bool (Eval_numeric.eval_relop relop v1 v2) :: vs'
    with exn -> trap e.at "%s" (numeric_error e.at exn))

  | Unary unop, v :: vs' ->
    (try Eval_numeric.eval_unop unop v :: vs'
    with exn -> trap e.at "%s" (numeric_error e.at exn))

  | Binary binop, v2 :: v1 :: vs' ->
    (try Eval_numeric.eval_binop binop v1 v2 :: vs'
    with exn -> trap e.at "%s" (numeric_error e.at exn))

  | Convert cvtop, v :: vs' ->
    (try Eval_numeric.eval_cvtop cvtop v :: vs'
    with exn -> trap e.at "%s" (numeric_error e.at exn))

  | _ ->
    crash e.at "missing or ill-typed operand on stack (%s : %s)"
      (string_of_values (List.rev vs))
      (string_of_value_types (List.map type_of (List.rev vs)))

and eval_instrs (c : config) (vs : value stack) (es : instr list) : value stack =
  match es with
  | [] -> vs
  | e::es' ->
    let vs' = eval_instr c vs e in
    eval_instrs c vs' es'

and eval_func (c : config) (vs : value stack) func : value stack =
  let FuncType (ins, out) = Func.type_of func in
  let n = List.length ins in
  match func with
  | Func.AstFunc (t, inst', f) ->
    let args, vs' = take n vs f.at, drop n vs f.at in
    let locals' = List.rev args @ List.map default_value f.it.locals in
    let frame' = {inst = !inst'; locals = List.map ref locals'} in
    let module L = Label () in
    let l = L.label out in
    let c' = {c with frame = frame'; labels = [l]; return = l} in
    (try
      eval_instrs c' args f.it.body @ vs'
    with L.Branch vs'' ->
      vs'' @ vs'
    )

  | Func.HostFunc (t, f) ->
    let args, vs' = take n vs no_region, drop n vs no_region in
    List.rev (f (List.rev args)) @ vs'


let invoke (func : func_inst) (vs : value list) : value list * int =
  let FuncType (ins, out) = Func.type_of func in
  assert (List.length vs = List.length ins);
  let c = config empty_module_inst in
  let vs' = eval_func c vs func in
  assert (List.length vs' = List.length out);
  List.rev vs', !(c.steps)
