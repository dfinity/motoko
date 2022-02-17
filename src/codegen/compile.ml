(*
This module is the backend of the Motoko compiler. It takes a program in
the intermediate representation (ir.ml), and produces a WebAssembly module,
with Internet Computer extensions (customModule.ml). An important helper module is
instrList.ml, which provides a more convenient way of assembling WebAssembly
instruction lists, as it takes care of (1) source locations and (2) labels.

This file is split up in a number of modules, purely for namespacing and
grouping. Every module has a high-level prose comment explaining the concept;
this keeps documentation close to the code (a lesson learned from Simon PJ).
*)

open Ir_def
open Mo_values
open Mo_types
open Mo_config

open Wasm_exts.Ast
open Wasm.Types
open Source
(* Re-shadow Source.(@@), to get Stdlib.(@@) *)
let (@@) = Stdlib.(@@)

module G = InstrList
let (^^) = G.(^^) (* is this how we import a single operator from a module that we otherwise use qualified? *)

(* WebAssembly pages are 64kb. *)
let page_size = Int32.of_int (64*1024)
let page_size64 = Int64.of_int32 page_size
let page_size_bits = 16

(*
Pointers are skewed (translated) -1 relative to the actual offset.
See documentation of module BitTagged for more detail.
*)
let ptr_skew = -1l
let ptr_unskew = 1l

(* Generating function names for functions parametrized by prim types *)
let prim_fun_name p stem = Printf.sprintf "%s<%s>" stem (Type.string_of_prim p)


(* Helper functions to produce annotated terms (Wasm.AST) *)
let nr x = { Wasm.Source.it = x; Wasm.Source.at = Wasm.Source.no_region }

let todo fn se x = Printf.eprintf "%s: %s" fn (Wasm.Sexpr.to_string 80 se); x

exception CodegenError of string
let fatal fmt = Printf.ksprintf (fun s -> raise (CodegenError s)) fmt

module StaticBytes = struct
  (* A very simple DSL to describe static memory *)

  type t_ =
    | I32 of int32
    | I64 of int64
    | Seq of t
    | Bytes of string

  and t = t_ list

  let i32s is = Seq (List.map (fun i -> I32 i) is)

  let rec add : Buffer.t -> t_ -> unit = fun buf -> function
    | I32 i -> Buffer.add_int32_le buf i
    | I64 i -> Buffer.add_int64_le buf i
    | Seq xs -> List.iter (add buf) xs
    | Bytes b -> Buffer.add_string buf b

  let as_bytes : t -> string = fun xs ->
    let buf = Buffer.create 16 in
    List.iter (add buf) xs;
    Buffer.contents buf

end (* StaticBytes *)

module Const = struct

  (* Literals, as used in constant values. This is a projection of Ir.Lit,
     combining cases whose details we no longer care about.
     Should be still precise enough to map to the cases supported by SR.t.

     In other words: It is the smallest type that allows these three functions:

       (* projection of Ir.list. NB: pure, no access to env *)
       const_lit_of_lit : Ir.lit -> Const.lit (* NB: pure, no access to env *)

       (* creates vanilla representation (e.g. to put in static data structures *)
       vanilla_lit : E.env -> Const.lit -> i32

       (* creates efficient stack representation *)
       compile_lit : E.env -> Const.lit -> (SR.t, code)

  *)

  type lit =
    | Vanilla of int32 (* small words, no static data, already in vanilla format *)
    | BigInt of Big_int.big_int
    | Bool of bool
    | Word32 of int32
    | Word64 of int64
    | Float64 of Numerics.Float.t
    | Blob of string

  (* Constant known values.

     These are values that
     * are completely known constantly
     * do not require Wasm code to be executed (e.g. in `start`)
     * can be used directly (e.g. Call, not CallIndirect)
     * can be turned into Vanilla heap data on demand

     See ir_passes/const.ml for what precisely we can compile as const now.
  *)

  type v =
    | Fun of (unit -> int32) (* function pointer calculated upon first use *)
    | Message of int32 (* anonymous message, only temporary *)
    | Obj of (string * t) list
    | Unit
    | Array of t list (* also tuples, but not nullary *)
    | Lit of lit

  (* A constant known value together with a vanilla pointer.
     Typically a static memory location, could be an unboxed scalar.
     Filled on demand.
   *)
  and t = (int32 Lib.Promise.t * v)

  let t_of_v v = (Lib.Promise.make (), v)

end (* Const *)

module SR = struct
  (* This goes with the StackRep module, but we need the types earlier *)


  (* Value representation on the stack:

     Compiling an expression means putting its value on the stack. But
     there are various ways of putting a value onto the stack -- unboxed,
     tupled etc.
   *)
  type t =
    | Vanilla
    | UnboxedBool (* 0 or 1 *)
    | UnboxedTuple of int
    | UnboxedWord64
    | UnboxedWord32
    | UnboxedFloat64
    | Unreachable
    | Const of Const.t

  let unit = UnboxedTuple 0

  let bool = UnboxedBool

  (* Because t contains Const.t, and that contains Const.v, and that contains
     Const.lit, and that contains Big_int, we cannot just use normal `=`. So we
     have to write our own equality.

     This equalty is, I believe, used when joining branches. So for Const, we
     just compare the promises, and do not descend into the Const.v. This is
     conservative; the only downside is that if a branch returns different
     Const.t with (semantically) the same Const.v we do not propagate that as
     Const, but materialize before the branch.
     Which is not really expected or important.
  *)
  let eq (t1 : t) (t2 : t) = match t1, t2 with
    | Const (p1, _), Const (p2, _) -> p1 == p2
    | _ -> t1 = t2

end (* SR *)

(*

** The compiler environment.

Of course, as we go through the code we have to track a few things; these are
put in the compiler environment, type `E.t`. Some fields are valid globally, some
only make sense locally, i.e. within a single function (but we still put them
in one big record, for convenience).

The fields fall into the following categories:

 1. Static global fields. Never change.
    Example: whether we are compiling with -no-system-api

 2. Mutable global fields. Change only monotonically.
    These are used to register things like functions. This should be monotone
    in the sense that entries are only added, and that the order should not
    matter in a significant way. In some instances, the list contains futures
    so that we can reserve and know the _position_ of the thing before we have
    to actually fill it in.

 3. Static local fields. Never change within a function.
    Example: number of parameters and return values

 4. Mutable local fields. See above
    Example: Name and type of locals.

**)

(* Before we can define the environment, we need some auxillary types *)

module E = struct

  (* Utilities, internal to E *)
  let reg (ref : 'a list ref) (x : 'a) : int32 =
      let i = Wasm.I32.of_int_u (List.length !ref) in
      ref := !ref @ [ x ];
      i

  let reserve_promise (ref : 'a Lib.Promise.t list ref) _s : (int32 * ('a -> unit)) =
      let p = Lib.Promise.make () in (* For debugging with named promises, use s here *)
      let i = Wasm.I32.of_int_u (List.length !ref) in
      ref := !ref @ [ p ];
      (i, Lib.Promise.fulfill p)


  (* The environment type *)
  module NameEnv = Env.Make(String)
  module StringEnv = Env.Make(String)
  module LabSet = Set.Make(String)

  module FunEnv = Env.Make(Int32)
  type local_names = (int32 * string) list (* For the debug section: Names of locals *)
  type func_with_names = func * local_names
  type lazy_function = (int32, func_with_names) Lib.AllocOnUse.t
  type t = {
    (* Global fields *)
    (* Static *)
    mode : Flags.compile_mode;
    rts : Wasm_exts.CustomModule.extended_module option; (* The rts. Re-used when compiling actors *)
    trap_with : t -> string -> G.t;
      (* Trap with message; in the env for dependency injection *)

    (* Per module fields (only valid/used inside a module) *)
    (* Immutable *)

    (* Mutable *)
    func_types : func_type list ref;
    func_imports : import list ref;
    other_imports : import list ref;
    exports : export list ref;
    funcs : (func * string * local_names) Lib.Promise.t list ref;
    func_ptrs : int32 FunEnv.t ref;
    end_of_table : int32 ref;
    globals : (global Lib.Promise.t * string) list ref;
    global_names : int32 NameEnv.t ref;
    named_imports : int32 NameEnv.t ref;
    built_in_funcs : lazy_function NameEnv.t ref;
    static_strings : int32 StringEnv.t ref;
    end_of_static_memory : int32 ref; (* End of statically allocated memory *)
    static_memory : (int32 * string) list ref; (* Content of static memory *)
    static_memory_frozen : bool ref;
      (* Sanity check: Nothing should bump end_of_static_memory once it has been read *)
    static_roots : int32 list ref;
      (* GC roots in static memory. (Everything that may be mutable.) *)

    (* Metadata *)
    args : (bool * string) option ref;
    service : (bool * string) option ref;
    stable_types : (bool * string) option ref;
    labs : LabSet.t ref; (* Used labels (fields and variants),
                            collected for Motoko custom section 0 *)


    (* Local fields (only valid/used inside a function) *)
    (* Static *)
    n_param : int32; (* Number of parameters (to calculate indices of locals) *)
    return_arity : int; (* Number of return values (for type of Return) *)

    (* Mutable *)
    locals : value_type list ref; (* Types of locals *)
    local_names : (int32 * string) list ref; (* Names of locals *)
  }


  (* The initial global environment *)
  let mk_global mode rts trap_with dyn_mem : t = {
    mode;
    rts;
    trap_with;
    func_types = ref [];
    func_imports = ref [];
    other_imports = ref [];
    exports = ref [];
    funcs = ref [];
    func_ptrs = ref FunEnv.empty;
    end_of_table = ref 0l;
    globals = ref [];
    global_names = ref NameEnv.empty;
    named_imports = ref NameEnv.empty;
    built_in_funcs = ref NameEnv.empty;
    static_strings = ref StringEnv.empty;
    end_of_static_memory = ref dyn_mem;
    static_memory = ref [];
    static_memory_frozen = ref false;
    static_roots = ref [];
    (* Metadata *)
    args = ref None;
    service = ref None;
    stable_types = ref None;
    labs = ref LabSet.empty;
    (* Actually unused outside mk_fun_env: *)
    n_param = 0l;
    return_arity = 0;
    locals = ref [];
    local_names = ref [];
  }

  (* This wraps Mo_types.Hash.hash to also record which labels we have seen,
      so that that data can be put in a custom section, useful for debugging.
      Thus Mo_types.Hash.hash should not be called directly!
   *)
  let hash (env : t) lab =
    env.labs := LabSet.add lab (!(env.labs));
    Mo_types.Hash.hash lab

  let get_labs env = LabSet.elements (!(env.labs))

  let mk_fun_env env n_param return_arity =
    { env with
      n_param;
      return_arity;
      locals = ref [];
      local_names = ref [];
    }

  (* We avoid accessing the fields of t directly from outside of E, so here are a
     bunch of accessors. *)

  let mode (env : t) = env.mode


  let add_anon_local (env : t) ty =
      let i = reg env.locals ty in
      Wasm.I32.add env.n_param i

  let add_local_name (env : t) li name =
      let _ = reg env.local_names (li, name) in ()

  let get_locals (env : t) = !(env.locals)
  let get_local_names (env : t) : (int32 * string) list = !(env.local_names)

  let _add_other_import (env : t) m =
    ignore (reg env.other_imports m)

  let add_export (env : t) e =
    ignore (reg env.exports e)

  let add_global (env : t) name g =
    assert (not (NameEnv.mem name !(env.global_names)));
    let gi = reg env.globals (g, name) in
    env.global_names := NameEnv.add name gi !(env.global_names)

  let add_global32_delayed (env : t) name mut : int32 -> unit =
    let p = Lib.Promise.make () in
    add_global env name p;
    (fun init ->
      Lib.Promise.fulfill p (nr {
        gtype = GlobalType (I32Type, mut);
        value = nr (G.to_instr_list (G.i (Const (nr (Wasm.Values.I32 init)))))
      })
    )

  let add_global32 (env : t) name mut init =
    add_global32_delayed env name mut init

  (* TODO, refactor with previous two *)
  let add_global64_delayed (env : t) name mut : int64 -> unit =
    let p = Lib.Promise.make () in
    add_global env name p;
    (fun init ->
      Lib.Promise.fulfill p (nr {
        gtype = GlobalType (I64Type, mut);
        value = nr (G.to_instr_list (G.i (Const (nr (Wasm.Values.I64 init)))))
      })
    )
  let add_global64 (env : t) name mut init =
    add_global64_delayed env name mut init

  let get_global (env : t) name : int32 =
    match NameEnv.find_opt name !(env.global_names) with
    | Some gi -> gi
    | None -> raise (Invalid_argument (Printf.sprintf "No global named %s declared" name))

  let get_global32_lazy (env : t) name mut init : int32 =
    match NameEnv.find_opt name !(env.global_names) with
    | Some gi -> gi
    | None -> add_global32 env name mut init; get_global env name

  let export_global env name =
    add_export env (nr {
      name = Wasm.Utf8.decode name;
      edesc = nr (GlobalExport (nr (get_global env name)))
    })

  let get_globals (env : t) = List.map (fun (g,n) -> Lib.Promise.value g) !(env.globals)

  let reserve_fun (env : t) name =
    let (j, fill) = reserve_promise env.funcs name in
    let n = Int32.of_int (List.length !(env.func_imports)) in
    let fi = Int32.add j n in
    let fill_ (f, local_names) = fill (f, name, local_names) in
    (fi, fill_)

  let add_fun (env : t) name (f, local_names) =
    let (fi, fill) = reserve_fun env name in
    fill (f, local_names);
    fi

  let make_lazy_function env name : lazy_function =
    Lib.AllocOnUse.make (fun () -> reserve_fun env name)

  let lookup_built_in (env : t) name : lazy_function =
    match NameEnv.find_opt name !(env.built_in_funcs) with
    | None ->
      let lf = make_lazy_function env name in
      env.built_in_funcs := NameEnv.add name lf !(env.built_in_funcs);
      lf
    | Some lf -> lf

  let built_in (env : t) name : int32 =
    Lib.AllocOnUse.use (lookup_built_in env name)

  let define_built_in (env : t) name mk_fun : unit =
    Lib.AllocOnUse.def  (lookup_built_in env name) mk_fun

  let get_return_arity (env : t) = env.return_arity

  let get_func_imports (env : t) = !(env.func_imports)
  let get_other_imports (env : t) = !(env.other_imports)
  let get_exports (env : t) = !(env.exports)
  let get_funcs (env : t) = List.map Lib.Promise.value !(env.funcs)

  let func_type (env : t) ty =
    let rec go i = function
      | [] -> env.func_types := !(env.func_types) @ [ ty ]; Int32.of_int i
      | ty'::tys when ty = ty' -> Int32.of_int i
      | _ :: tys -> go (i+1) tys
       in
    go 0 !(env.func_types)

  let get_types (env : t) = !(env.func_types)

  let add_func_import (env : t) modname funcname arg_tys ret_tys =
    if !(env.funcs) <> [] then
      raise (CodegenError "Add all imports before all functions!");

    let i = {
      module_name = Wasm.Utf8.decode modname;
      item_name = Wasm.Utf8.decode funcname;
      idesc = nr (FuncImport (nr (func_type env (FuncType (arg_tys, ret_tys)))))
    } in
    let fi = reg env.func_imports (nr i) in
    let name = modname ^ "." ^ funcname in
    assert (not (NameEnv.mem name !(env.named_imports)));
    env.named_imports := NameEnv.add name fi !(env.named_imports)

  let call_import (env : t) modname funcname =
    let name = modname ^ "." ^ funcname in
    match NameEnv.find_opt name !(env.named_imports) with
      | Some fi -> G.i (Call (nr fi))
      | _ ->
        raise (Invalid_argument (Printf.sprintf "Function import not declared: %s\n" name))

  let get_rts (env : t) = env.rts

  let as_block_type env : stack_type -> block_type = function
    | [] -> ValBlockType None
    | [t] -> ValBlockType (Some t)
    | ts -> VarBlockType (nr (func_type env (FuncType ([], ts))))

  let if_ env tys thn els = G.if_ (as_block_type env tys) thn els
  let block_ env tys bdy = G.block_ (as_block_type env tys) bdy

  let trap_with env msg = env.trap_with env msg
  let then_trap_with env msg = G.if0 (trap_with env msg) G.nop
  let else_trap_with env msg = G.if0 G.nop (trap_with env msg)

  let reserve_static_memory (env : t) size : int32 =
    if !(env.static_memory_frozen) then raise (Invalid_argument "Static memory frozen");
    let ptr = !(env.end_of_static_memory) in
    let aligned = Int32.logand (Int32.add size 3l) (Int32.lognot 3l) in
    env.end_of_static_memory := Int32.add ptr aligned;
    ptr

  let add_mutable_static_bytes (env : t) data : int32 =
    let ptr = reserve_static_memory env (Int32.of_int (String.length data)) in
    env.static_memory := !(env.static_memory) @ [ (ptr, data) ];
    Int32.(add ptr ptr_skew) (* Return a skewed pointer *)

  let add_fun_ptr (env : t) fi : int32 =
    match FunEnv.find_opt fi !(env.func_ptrs) with
    | Some fp -> fp
    | None ->
      let fp = !(env.end_of_table) in
      env.func_ptrs := FunEnv.add fi fp !(env.func_ptrs);
      env.end_of_table := Int32.add !(env.end_of_table) 1l;
      fp

  let get_elems env =
    FunEnv.bindings !(env.func_ptrs)

  let get_end_of_table env : int32 =
    !(env.end_of_table)

  let add_static (env : t) (data : StaticBytes.t) : int32 =
    let b = StaticBytes.as_bytes data in
    match StringEnv.find_opt b !(env.static_strings)  with
    | Some ptr -> ptr
    | None ->
      let ptr = add_mutable_static_bytes env b  in
      env.static_strings := StringEnv.add b ptr !(env.static_strings);
      ptr

  let get_end_of_static_memory env : int32 =
    env.static_memory_frozen := true;
    !(env.end_of_static_memory)

  let add_static_root (env : t) ptr =
    env.static_roots := ptr :: !(env.static_roots)

  let get_static_roots (env : t) =
    !(env.static_roots)

  let get_static_memory env =
    !(env.static_memory)

  let mem_size env =
    Int32.(add (div (get_end_of_static_memory env) page_size) 1l)

  let collect_garbage env =
    (* GC function name = "schedule_"? ("compacting" | "copying") "_gc" *)
    let gc_fn = match !Flags.gc_strategy with
    | Mo_config.Flags.MarkCompact -> "compacting"
    | Mo_config.Flags.Copying -> "copying"
    in
    let gc_fn = if !Flags.force_gc then gc_fn else "schedule_" ^ gc_fn in
    call_import env "rts" (gc_fn ^ "_gc")
end


(* General code generation functions:
   Rule of thumb: Here goes stuff that independent of the Motoko AST.
*)

(* Function called compile_* return a list of instructions (and maybe other stuff) *)

let compile_unboxed_const i = G.i (Const (nr (Wasm.Values.I32 i)))
let compile_const_64 i = G.i (Const (nr (Wasm.Values.I64 i)))
let compile_unboxed_zero = compile_unboxed_const 0l
let compile_unboxed_one = compile_unboxed_const 1l

(* Some common arithmetic, used for pointer and index arithmetic *)
let compile_op_const op i =
    compile_unboxed_const i ^^
    G.i (Binary (Wasm.Values.I32 op))
let compile_add_const = compile_op_const I32Op.Add
let compile_sub_const = compile_op_const I32Op.Sub
let compile_mul_const = compile_op_const I32Op.Mul
let compile_divU_const = compile_op_const I32Op.DivU
let compile_shrU_const = compile_op_const I32Op.ShrU
let compile_shrS_const = compile_op_const I32Op.ShrS
let compile_shl_const = compile_op_const I32Op.Shl
let compile_rotl_const = compile_op_const I32Op.Rotl
let compile_rotr_const = compile_op_const I32Op.Rotr
let compile_bitand_const = compile_op_const I32Op.And
let compile_bitor_const = function
  | 0l -> G.nop | n -> compile_op_const I32Op.Or n
let compile_rel_const rel i =
  compile_unboxed_const i ^^
  G.i (Compare (Wasm.Values.I32 rel))
let compile_eq_const = compile_rel_const I32Op.Eq

let compile_op64_const op i =
    compile_const_64 i ^^
    G.i (Binary (Wasm.Values.I64 op))
let compile_add64_const = compile_op64_const I64Op.Add
let compile_sub64_const = compile_op64_const I64Op.Sub
let _compile_mul64_const = compile_op64_const I64Op.Mul
let _compile_divU64_const = compile_op64_const I64Op.DivU
let compile_shrU64_const = function
  | 0L -> G.nop | n -> compile_op64_const I64Op.ShrU n
let compile_shrS64_const = function
  | 0L -> G.nop | n -> compile_op64_const I64Op.ShrS n
let compile_shl64_const = function
  | 0L -> G.nop | n -> compile_op64_const I64Op.Shl n
let compile_bitand64_const = compile_op64_const I64Op.And
let _compile_bitor64_const = function
  | 0L -> G.nop | n -> compile_op64_const I64Op.Or n
let compile_eq64_const i =
  compile_const_64 i ^^
  G.i (Compare (Wasm.Values.I64 I64Op.Eq))

(* more random utilities *)

let bytes_of_int32 (i : int32) : string =
  let b = Buffer.create 4 in
  let i = Int32.to_int i in
  Buffer.add_char b (Char.chr (i land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 8) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 24) land 0xff));
  Buffer.contents b

(* A common variant of todo *)

let todo_trap env fn se = todo fn se (E.trap_with env ("TODO: " ^ fn))
let _todo_trap_SR env fn se = todo fn se (SR.Unreachable, E.trap_with env ("TODO: " ^ fn))

(* Locals *)

let new_local_ env t name =
  let i = E.add_anon_local env t in
  E.add_local_name env i name;
  ( G.i (LocalSet (nr i))
  , G.i (LocalGet (nr i))
  , i
  )

let new_local env name =
  let (set_i, get_i, _) = new_local_ env I32Type name
  in (set_i, get_i)

let new_local64 env name =
  let (set_i, get_i, _) = new_local_ env I64Type name
  in (set_i, get_i)

let new_float_local env name =
  let (set_i, get_i, _) = new_local_ env F64Type name
  in (set_i, get_i)

(* Some common code macros *)

(* Iterates while cond is true. *)
let compile_while env cond body =
    G.loop0 (
      cond ^^ G.if0 (body ^^ G.i (Br (nr 1l))) G.nop
    )

(* Expects a number n on the stack. Iterates from m to below that number. *)
let from_m_to_n env m mk_body =
    let (set_n, get_n) = new_local env "n" in
    let (set_i, get_i) = new_local env "i" in
    set_n ^^
    compile_unboxed_const m ^^
    set_i ^^

    compile_while env
      ( get_i ^^
        get_n ^^
        G.i (Compare (Wasm.Values.I32 I32Op.LtU))
      ) (
        mk_body get_i ^^

        get_i ^^
        compile_add_const 1l ^^
        set_i
      )

(* Expects a number on the stack. Iterates from zero to below that number. *)
let from_0_to_n env mk_body = from_m_to_n env 0l mk_body

(* Pointer reference and dereference  *)

let load_unskewed_ptr : G.t =
  G.i (Load {ty = I32Type; align = 2; offset = 0l; sz = None})

let store_unskewed_ptr : G.t =
  G.i (Store {ty = I32Type; align = 2; offset = 0l; sz = None})

let load_ptr : G.t =
  G.i (Load {ty = I32Type; align = 2; offset = ptr_unskew; sz = None})

let store_ptr : G.t =
  G.i (Store {ty = I32Type; align = 2; offset = ptr_unskew; sz = None})

module FakeMultiVal = struct
  (* For some use-cases (e.g. processing the compiler output with analysis
     tools) it is useful to avoid the multi-value extension.

     This module provides mostly transparent wrappers that put multiple values
     in statically allocated globals and pull them off again.

     So far only does I32Type (but that could be changed).

     If the multi_value flag is on, these do not do anything.
  *)
  let ty tys =
    if !Flags.multi_value || List.length tys <= 1
    then tys
    else []

  let global env i =
    E.get_global32_lazy env (Printf.sprintf "multi_val_%d" i) Mutable 0l

  let store env tys =
    if !Flags.multi_value || List.length tys <= 1 then G.nop else
    G.concat_mapi (fun i _ ->
      G.i (GlobalSet (nr (global env i)))
    ) tys

  let load env tys =
    if !Flags.multi_value || List.length tys <= 1 then G.nop else
    let n = List.length tys - 1 in
    G.concat_mapi (fun i _ ->
      G.i (GlobalGet (nr (global env (n - i))))
    ) tys

end (* FakeMultiVal *)

module Func = struct
  (* This module contains basic bookkeeping functionality to define functions,
     in particular creating the environment, and finally adding it to the environment.
  *)

  let of_body env params retty mk_body =
    let env1 = E.mk_fun_env env (Int32.of_int (List.length params)) (List.length retty) in
    List.iteri (fun i (n,_t) -> E.add_local_name env1 (Int32.of_int i) n) params;
    let ty = FuncType (List.map snd params, FakeMultiVal.ty retty) in
    let body = G.to_instr_list (
      mk_body env1 ^^ FakeMultiVal.store env1 retty
    ) in
    (nr { ftype = nr (E.func_type env ty);
          locals = E.get_locals env1;
          body }
    , E.get_local_names env1)

  let define_built_in env name params retty mk_body =
    E.define_built_in env name (lazy (of_body env params retty mk_body))

  (* (Almost) transparently lift code into a function and call this function. *)
  (* Also add a hack to support multiple return values *)
  let share_code env name params retty mk_body =
    define_built_in env name params retty mk_body;
    G.i (Call (nr (E.built_in env name))) ^^
    FakeMultiVal.load env retty


  (* Shorthands for various arities *)
  let share_code0 env name retty mk_body =
    share_code env name [] retty (fun env -> mk_body env)
  let share_code1 env name p1 retty mk_body =
    share_code env name [p1] retty (fun env -> mk_body env
        (G.i (LocalGet (nr 0l)))
    )
  let share_code2 env name (p1,p2) retty mk_body =
    share_code env name [p1; p2] retty (fun env -> mk_body env
        (G.i (LocalGet (nr 0l)))
        (G.i (LocalGet (nr 1l)))
    )
  let share_code3 env name (p1, p2, p3) retty mk_body =
    share_code env name [p1; p2; p3] retty (fun env -> mk_body env
        (G.i (LocalGet (nr 0l)))
        (G.i (LocalGet (nr 1l)))
        (G.i (LocalGet (nr 2l)))
    )
  let _share_code4 env name (p1, p2, p3, p4) retty mk_body =
    share_code env name [p1; p2; p3; p4] retty (fun env -> mk_body env
        (G.i (LocalGet (nr 0l)))
        (G.i (LocalGet (nr 1l)))
        (G.i (LocalGet (nr 2l)))
        (G.i (LocalGet (nr 3l)))
    )
  let share_code7 env name (p1, p2, p3, p4, p5, p6, p7) retty mk_body =
    share_code env name [p1; p2; p3; p4; p5; p6; p7] retty (fun env -> mk_body env
        (G.i (LocalGet (nr 0l)))
        (G.i (LocalGet (nr 1l)))
        (G.i (LocalGet (nr 2l)))
        (G.i (LocalGet (nr 3l)))
        (G.i (LocalGet (nr 4l)))
        (G.i (LocalGet (nr 5l)))
        (G.i (LocalGet (nr 6l)))
    )

end (* Func *)

module RTS = struct
  (* The connection to the C and Rust parts of the RTS *)
  let system_imports env =
    E.add_func_import env "rts" "memcpy" [I32Type; I32Type; I32Type] [I32Type]; (* standard libc memcpy *)
    E.add_func_import env "rts" "memcmp" [I32Type; I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "version" [] [I32Type];
    E.add_func_import env "rts" "parse_idl_header" [I32Type; I32Type; I32Type; I32Type; I32Type] [];
    E.add_func_import env "rts" "leb128_decode" [I32Type] [I32Type];
    E.add_func_import env "rts" "sleb128_decode" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_word32" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_int32" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word32_wrap" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word32_trap" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word32_trap_with" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_word64" [I64Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_int64" [I64Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_float64" [F64Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_float64" [I32Type] [F64Type];
    E.add_func_import env "rts" "bigint_to_word64_wrap" [I32Type] [I64Type];
    E.add_func_import env "rts" "bigint_to_word64_trap" [I32Type] [I64Type];
    E.add_func_import env "rts" "bigint_eq" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_isneg" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_count_bits" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_2complement_bits" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_lt" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_gt" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_le" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_ge" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_add" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_sub" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_mul" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_rem" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_div" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_pow" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_neg" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_lsh" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_abs" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_leb128_size" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_leb128_encode" [I32Type; I32Type] [];
    E.add_func_import env "rts" "bigint_leb128_decode" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_sleb128_size" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_sleb128_encode" [I32Type; I32Type] [];
    E.add_func_import env "rts" "bigint_sleb128_decode" [I32Type] [I32Type];
    E.add_func_import env "rts" "leb128_encode" [I32Type; I32Type] [];
    E.add_func_import env "rts" "sleb128_encode" [I32Type; I32Type] [];
    E.add_func_import env "rts" "utf8_valid" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "utf8_validate" [I32Type; I32Type] [];
    E.add_func_import env "rts" "skip_leb128" [I32Type] [];
    E.add_func_import env "rts" "skip_any" [I32Type; I32Type; I32Type; I32Type] [];
    E.add_func_import env "rts" "find_field" [I32Type; I32Type; I32Type; I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "skip_fields" [I32Type; I32Type; I32Type; I32Type] [];
    E.add_func_import env "rts" "remember_continuation" [I32Type] [I32Type];
    E.add_func_import env "rts" "recall_continuation" [I32Type] [I32Type];
    E.add_func_import env "rts" "peek_future_continuation" [I32Type] [I32Type];
    E.add_func_import env "rts" "continuation_count" [] [I32Type];
    E.add_func_import env "rts" "continuation_table_size" [] [I32Type];
    E.add_func_import env "rts" "blob_of_text" [I32Type] [I32Type];
    E.add_func_import env "rts" "text_compare" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "text_concat" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "text_iter_done" [I32Type] [I32Type];
    E.add_func_import env "rts" "text_iter" [I32Type] [I32Type];
    E.add_func_import env "rts" "text_iter_next" [I32Type] [I32Type];
    E.add_func_import env "rts" "text_len" [I32Type] [I32Type];
    E.add_func_import env "rts" "text_of_ptr_size" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "text_singleton" [I32Type] [I32Type];
    E.add_func_import env "rts" "text_size" [I32Type] [I32Type];
    E.add_func_import env "rts" "text_to_buf" [I32Type; I32Type] [];
    E.add_func_import env "rts" "blob_of_principal" [I32Type] [I32Type];
    E.add_func_import env "rts" "principal_of_blob" [I32Type] [I32Type];
    E.add_func_import env "rts" "compute_crc32" [I32Type] [I32Type];
    E.add_func_import env "rts" "blob_iter_done" [I32Type] [I32Type];
    E.add_func_import env "rts" "blob_iter" [I32Type] [I32Type];
    E.add_func_import env "rts" "blob_iter_next" [I32Type] [I32Type];
    E.add_func_import env "rts" "pow" [F64Type; F64Type] [F64Type]; (* musl *)
    E.add_func_import env "rts" "sin" [F64Type] [F64Type]; (* musl *)
    E.add_func_import env "rts" "cos" [F64Type] [F64Type]; (* musl *)
    E.add_func_import env "rts" "tan" [F64Type] [F64Type]; (* musl *)
    E.add_func_import env "rts" "asin" [F64Type] [F64Type]; (* musl *)
    E.add_func_import env "rts" "acos" [F64Type] [F64Type]; (* musl *)
    E.add_func_import env "rts" "atan" [F64Type] [F64Type]; (* musl *)
    E.add_func_import env "rts" "atan2" [F64Type; F64Type] [F64Type]; (* musl *)
    E.add_func_import env "rts" "exp" [F64Type] [F64Type]; (* musl *)
    E.add_func_import env "rts" "log" [F64Type] [F64Type]; (* musl *)
    E.add_func_import env "rts" "fmod" [F64Type; F64Type] [F64Type]; (* remainder, musl *)
    E.add_func_import env "rts" "float_fmt" [F64Type; I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "char_to_upper" [I32Type] [I32Type];
    E.add_func_import env "rts" "char_to_lower" [I32Type] [I32Type];
    E.add_func_import env "rts" "char_is_whitespace" [I32Type] [I32Type];
    E.add_func_import env "rts" "char_is_lowercase" [I32Type] [I32Type];
    E.add_func_import env "rts" "char_is_uppercase" [I32Type] [I32Type];
    E.add_func_import env "rts" "char_is_alphabetic" [I32Type] [I32Type];
    E.add_func_import env "rts" "get_max_live_size" [] [I32Type];
    E.add_func_import env "rts" "get_reclaimed" [] [I64Type];
    E.add_func_import env "rts" "copying_gc" [] [];
    E.add_func_import env "rts" "compacting_gc" [] [];
    E.add_func_import env "rts" "schedule_copying_gc" [] [];
    E.add_func_import env "rts" "schedule_compacting_gc" [] [];
    E.add_func_import env "rts" "alloc_words" [I32Type] [I32Type];
    E.add_func_import env "rts" "get_total_allocations" [] [I64Type];
    E.add_func_import env "rts" "get_heap_size" [] [I32Type];
    E.add_func_import env "rts" "init" [I32Type] [];
    E.add_func_import env "rts" "alloc_blob" [I32Type] [I32Type];
    E.add_func_import env "rts" "alloc_array" [I32Type] [I32Type];
    ()

end (* RTS *)

module Heap = struct
  (* General heap object functionality (allocation, setting fields, reading fields) *)

  (* Memory addresses are 32 bit (I32Type). *)
  let word_size = 4l

  (* The heap base global can only be used late, see conclude_module
     and GHC.register *)
  let get_heap_base env =
    G.i (GlobalGet (nr (E.get_global env "__heap_base")))

  let get_total_allocation env =
    E.call_import env "rts" "get_total_allocations"

  let get_reclaimed env =
    E.call_import env "rts" "get_reclaimed"

  let get_memory_size =
    G.i MemorySize ^^
    compile_mul_const page_size

  let get_max_live_size env =
    E.call_import env "rts" "get_max_live_size"

  let dyn_alloc_words env =
    E.call_import env "rts" "alloc_words"

  (* Static allocation (always words)
     (uses dynamic allocation for smaller and more readable code) *)
  let alloc env (n : int32) : G.t =
    compile_unboxed_const n  ^^
    dyn_alloc_words env

  (* Heap objects *)

  (* At this level of abstraction, heap objects are just flat arrays of words *)

  let load_field (i : int32) : G.t =
    let offset = Int32.(add (mul word_size i) ptr_unskew) in
    G.i (Load {ty = I32Type; align = 2; offset; sz = None})

  let store_field (i : int32) : G.t =
    let offset = Int32.(add (mul word_size i) ptr_unskew) in
    G.i (Store {ty = I32Type; align = 2; offset; sz = None})

  (* Although we occasionally want to treat two 32 bit fields as one 64 bit number *)

  let load_field64 (i : int32) : G.t =
    let offset = Int32.(add (mul word_size i) ptr_unskew) in
    G.i (Load {ty = I64Type; align = 2; offset; sz = None})

  let store_field64 (i : int32) : G.t =
    let offset = Int32.(add (mul word_size i) ptr_unskew) in
    G.i (Store {ty = I64Type; align = 2; offset; sz = None})

  (* Or even as a single 64 bit float *)

  let load_field_float64 (i : int32) : G.t =
    let offset = Int32.(add (mul word_size i) ptr_unskew) in
    G.i (Load {ty = F64Type; align = 2; offset; sz = None})

  let store_field_float64 (i : int32) : G.t =
    let offset = Int32.(add (mul word_size i) ptr_unskew) in
    G.i (Store {ty = F64Type; align = 2; offset; sz = None})

  (* Create a heap object with instructions that fill in each word *)
  let obj env element_instructions : G.t =
    let (set_heap_obj, get_heap_obj) = new_local env "heap_object" in

    let n = List.length element_instructions in
    alloc env (Wasm.I32.of_int_u n) ^^
    set_heap_obj ^^

    let init_elem idx instrs : G.t =
      get_heap_obj ^^
      instrs ^^
      store_field (Wasm.I32.of_int_u idx)
    in
    G.concat_mapi init_elem element_instructions ^^
    get_heap_obj

  (* Convenience functions related to memory *)
  (* Copying bytes (works on unskewed memory addresses) *)
  let memcpy env = E.call_import env "rts" "memcpy" ^^ G.i Drop
  (* Comparing bytes (works on unskewed memory addresses) *)
  let memcmp env = E.call_import env "rts" "memcmp"

  let register env =
    let get_heap_base_fn = E.add_fun env "get_heap_base" (Func.of_body env [] [I32Type] (fun env ->
      get_heap_base env
    )) in

    E.add_export env (nr {
      name = Wasm.Utf8.decode "get_heap_base";
      edesc = nr (FuncExport (nr get_heap_base_fn))
    })

  let get_heap_size env =
    E.call_import env "rts" "get_heap_size"

end (* Heap *)

module Stack = struct
  (* The RTS includes C code which requires a shadow stack in linear memory.
     We reserve some space for it at the beginning of memory space (just like
     wasm-l would), this way stack overflow would cause out-of-memory, and not
     just overwrite static data.

     We sometimes use the stack space if we need small amounts of scratch space.

     All pointers here are unskewed.
  *)

  let end_ = page_size (* 64k of stack *)

  let register_globals env =
    (* stack pointer *)
    E.add_global32 env "__stack_pointer" Mutable end_;
    E.export_global env "__stack_pointer"

  let get_stack_ptr env =
    G.i (GlobalGet (nr (E.get_global env "__stack_pointer")))
  let set_stack_ptr env =
    G.i (GlobalSet (nr (E.get_global env "__stack_pointer")))

  let alloc_words env n =
    get_stack_ptr env ^^
    compile_unboxed_const (Int32.mul n Heap.word_size) ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
    set_stack_ptr env ^^
    get_stack_ptr env

  let free_words env n =
    get_stack_ptr env ^^
    compile_unboxed_const (Int32.mul n Heap.word_size) ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
    set_stack_ptr env

  let with_words env name n f =
    let (set_x, get_x) = new_local env name in
    alloc_words env n ^^ set_x ^^
    f get_x ^^
    free_words env n

end (* Stack *)


module ContinuationTable = struct
  (* See rts/motoko-rts/src/closure_table.rs *)
  let remember env : G.t = E.call_import env "rts" "remember_continuation"
  let recall env : G.t = E.call_import env "rts" "recall_continuation"
  let peek_future env : G.t = E.call_import env "rts" "peek_future_continuation"
  let count env : G.t = E.call_import env "rts" "continuation_count"
  let size env : G.t = E.call_import env "rts" "continuation_table_size"
end (* ContinuationTable *)

module Bool = struct
  (* Boolean literals are either 0 or 1,
     at StackRep UnboxedWord32
     They need to be shifted before put in the heap
  *)

  (* in SR.Bool *)
  let lit = function
    | false -> compile_unboxed_const 0l
    | true -> compile_unboxed_const 1l

  let vanilla_lit = function
    | false -> 0l
    | true -> 2l

  let neg = G.i (Test (Wasm.Values.I32 I32Op.Eqz))

end (* Bool *)

module BitTagged = struct

  (* This module takes care of pointer tagging:

     A pointer to an object at offset `i` on the heap is represented as
     `i-1`, so the low two bits of the pointer are always set (0b…11).
     We call `i-1` a *skewed* pointer, in a feeble attempt to avoid the term
     shifted, which may sound like a logical shift.

     We use the constants ptr_skew and ptr_unskew to change a pointer as a
     signpost where we switch between raw pointers to skewed ones.

     This means we can store a small unboxed scalar x as (x `lsl` 1), and still
     tell it apart from a pointer by looking at the last bits: if set, it is a
     pointer.

     Small here means -2^30 ≤ x < 2^30, and untagging needs to happen with an
     _arithmetic_ right shift. This is the right thing to do for signed
     numbers, and because we want to apply a consistent logic for all types,
     especially as there is only one wasm type, we use the same range for
     signed numbers as well.

     Boolean false is a non-pointer by construction.
     Boolean true (1) needs to be shifted to be a non-pointer.
     No unshifting necessary before a branch.

     Summary:

       0b…11: A pointer
       0b…x0: A shifted scalar
       0b000: `false`
       0b010: `true`

     Note that {Nat,Int}{8,16} do not need to be explicitly bit-tagged:
     The bytes are stored in the _most_ significant byte(s) of the `i32`,
     thus lowest two bits are always 0.
     All arithmetic is implemented directly on that representation, see
     module TaggedSmallWord.
  *)
  let if_tagged_scalar env retty is1 is2 =
    compile_bitand_const 0x1l ^^
    E.if_ env retty is2 is1

  (* With two bit-tagged pointers on the stack, decide
     whether both are scalars and invoke is1 (the fast path)
     if so, and otherwise is2 (the slow path).
  *)
  let if_both_tagged_scalar env retty is1 is2 =
    G.i (Binary (Wasm.Values.I32 I32Op.Or)) ^^
    compile_bitand_const 0x1l ^^
    E.if_ env retty is2 is1

  (* 64 bit numbers *)

  (* static *)
  let can_tag_const (n : int64) =
    let lower_bound = Int64.(neg (shift_left 1L 30)) in
    let upper_bound = Int64.shift_left 1L 30 in
    lower_bound <= n && n < upper_bound

  let tag_const i = Int32.shift_left (Int64.to_int32 i) 1


  (* dynamic *)
  let if_can_tag_i64 env retty is1 is2 =
    Func.share_code1 env "can_tag_i64" ("x", I64Type) [I32Type] (fun env get_x ->
      (* checks that all but the low 30 bits are either all 0 or all 1 *)
      get_x ^^ compile_shl64_const 1L ^^
      get_x ^^ G.i (Binary (Wasm.Values.I64 I32Op.Xor)) ^^
      compile_shrU64_const 31L ^^
      G.i (Test (Wasm.Values.I64 I64Op.Eqz))
    ) ^^
    E.if_ env retty is1 is2

  let if_can_tag_u64 env retty is1 is2 =
    compile_shrU64_const 30L ^^
    G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
    E.if_ env retty is1 is2

  let tag =
    G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
    compile_shl_const 1l

  let untag env =
    compile_shrS_const 1l ^^
    G.i (Convert (Wasm.Values.I64 I64Op.ExtendSI32))

  (* 32 bit numbers, dynamic *)

  let if_can_tag_i32 env retty is1 is2 =
    Func.share_code1 env "can_tag_i32" ("x", I32Type) [I32Type] (fun env get_x ->
      (* checks that all but the low 30 bits are either all 0 or all 1 *)
      get_x ^^ compile_shl_const 1l ^^
      get_x ^^ G.i (Binary (Wasm.Values.I32 I32Op.Xor)) ^^
      compile_shrU_const 31l
    ) ^^
    E.if_ env retty is2 is1 (* NB: swapped branches *)

  let if_can_tag_u32 env retty is1 is2 =
    compile_shrU_const 30l ^^
    E.if_ env retty is2 is1 (* NB: swapped branches *)

  let tag_i32 = compile_shl_const 1l
  let untag_i32 = compile_shrS_const 1l

end (* BitTagged *)

module Tagged = struct
  (* Tagged objects have, well, a tag to describe their runtime type.
     This tag is used to traverse the heap (serialization, GC), but also
     for objectification of arrays.

     The tag is a word at the beginning of the object.

     All tagged heap objects have a size of at least two words
     (important for GC, which replaces them with an Indirection).

     Attention: This mapping is duplicated in these places
       * here
       * rts/rts.h
       * motoko-rts/src/types.rs
     so update all!
   *)

  type [@warning "-37"] tag  =
    | Object
    | ObjInd (* The indirection used for object fields *)
    | Array (* Also a tuple *)
    | Bits64 (* Contains a 64 bit number *)
    | MutBox (* used for mutable heap-allocated variables *)
    | Closure
    | Some (* For opt *)
    | Variant
    | Blob
    | Indirection (* Only used by the GC *)
    | Bits32 (* Contains a 32 bit unsigned number *)
    | BigInt
    | Concat (* String concatenation, used by rts/text.c *)
    | Null (* For opt. Static singleton! *)
    | StableSeen (* Marker that we have seen this thing before *)
    | CoercionFailure (* Used in the Candid decoder. Static singleton! *)
    | OneWordFiller (* Only used by the RTS *)
    | FreeSpace (* Only used by the RTS *)

  (* Tags needs to have the lowest bit set, to allow distinguishing object
     headers from heap locations (object or field addresses).

     (Reminder: objects and fields are word-aligned so will have the lowest two
     bits unset) *)
  let int_of_tag = function
    | Object -> 1l
    | ObjInd -> 3l
    | Array -> 5l
    | Bits64 -> 7l
    | MutBox -> 9l
    | Closure -> 11l
    | Some -> 13l
    | Variant -> 15l
    | Blob -> 17l
    | Indirection -> 19l
    | Bits32 -> 21l
    | BigInt -> 23l
    | Concat -> 25l
    | Null -> 27l
    | OneWordFiller -> 29l
    | FreeSpace -> 31l
    (* Next two tags won't be seen by the GC, so no need to set the lowest bit
       for `CoercionFailure` and `StableSeen` *)
    | CoercionFailure -> 0xfffffffel
    | StableSeen -> 0xffffffffl

  (* The tag *)
  let header_size = 1l
  let tag_field = 0l

  (* Assumes a pointer to the object on the stack *)
  let store tag =
    compile_unboxed_const (int_of_tag tag) ^^
    Heap.store_field tag_field

  let load =
    Heap.load_field tag_field

  (* Branches based on the tag of the object pointed to,
     leaving the object on the stack afterwards. *)
  let branch_default env retty def (cases : (tag * G.t) list) : G.t =
    let (set_tag, get_tag) = new_local env "tag" in

    let rec go = function
      | [] -> def
      | ((tag, code) :: cases) ->
        get_tag ^^
        compile_eq_const (int_of_tag tag) ^^
        E.if_ env retty code (go cases)
    in
    load ^^
    set_tag ^^
    go cases

  (* like branch_default but also pushes the scrutinee on the stack for the
   * branch's consumption *)
  let _branch_default_with env retty def cases =
    let (set_o, get_o) = new_local env "o" in
    let prep (t, code) = (t, get_o ^^ code)
    in set_o ^^ get_o ^^ branch_default env retty def (List.map prep cases)

  (* like branch_default_with but the tag is known statically *)
  let branch_with env retty = function
    | [] -> G.i Unreachable
    | [_, code] -> code
    | (_, code) :: cases ->
       let (set_o, get_o) = new_local env "o" in
       let prep (t, code) = (t, get_o ^^ code)
       in set_o ^^ get_o ^^ branch_default env retty (get_o ^^ code) (List.map prep cases)

  (* Can a value of this type be represented by a heap object with this tag? *)
  (* Needs to be conservative, i.e. return `true` if unsure *)
  (* This function can also be used as assertions in a lint mode, e.g. in compile_exp *)
  let can_have_tag ty tag =
    let open Mo_types.Type in
    match (tag : tag) with
    | Array ->
      begin match normalize ty with
      | (Con _ | Any) -> true
      | (Array _ | Tup _) -> true
      | (Prim _ |  Obj _ | Opt _ | Variant _ | Func _ | Non) -> false
      | (Pre | Async _ | Mut _ | Var _ | Typ _) -> assert false
      end
    | Blob ->
      begin match normalize ty with
      | (Con _ | Any) -> true
      | (Prim (Text|Blob|Principal)) -> true
      | (Prim _ | Obj _ | Array _ | Tup _ | Opt _ | Variant _ | Func _ | Non) -> false
      | (Pre | Async _ | Mut _ | Var _ | Typ _) -> assert false
      end
    | Object ->
      begin match normalize ty with
      | (Con _ | Any) -> true
      | (Obj _) -> true
      | (Prim _ | Array _ | Tup _ | Opt _ | Variant _ | Func _ | Non) -> false
      | (Pre | Async _ | Mut _ | Var _ | Typ _) -> assert false
      end
    | _ -> true

  (* like branch_with but with type information to statically skip some branches *)
  let _branch_typed_with env ty retty branches =
    branch_with env retty (List.filter (fun (tag,c) -> can_have_tag ty tag) branches)

  let obj env tag element_instructions : G.t =
    Heap.obj env @@
      compile_unboxed_const (int_of_tag tag) ::
      element_instructions

end (* Tagged *)

module MutBox = struct
  (* Mutable heap objects *)

  let field = Tagged.header_size

  let alloc env =
    Tagged.obj env Tagged.MutBox [ compile_unboxed_zero ]

  let static env =
    let tag = bytes_of_int32 (Tagged.int_of_tag Tagged.MutBox) in
    let zero = bytes_of_int32 0l in
    let ptr = E.add_mutable_static_bytes env (tag ^ zero) in
    E.add_static_root env ptr;
    ptr
end


module Opt = struct
  (* The Option type. Optional values are represented as

    1. ┌──────┐
       │ null │
       └──────┘

       A special null value. It is fully static, and because it is unique, can
       be recognized by pointer comparison (only the GC will care about the heap
       tag).


    2. ┌──────┬─────────┐
       │ some │ payload │
       └──────┴─────────┘

       A heap-allocated box for `?v` values. Should only ever contain null or
       another such box.

    3. Anything else (pointer or unboxed scalar): Constituent value, implicitly
       injected into the opt type.

    This way, `?t` is represented without allocation, with the only exception of
    the value `?ⁿnull` for n>0.

    NB: `?ⁿnull` is essentially represented by the unary encoding of the number
    of n. This could be optimized further, by storing `n` in the Some payload,
    instead of a pointer, but unlikely worth it.

  *)

  let some_payload_field = Tagged.header_size

  (* This relies on the fact that add_static deduplicates *)
  let null_vanilla_lit env : int32 =
    E.add_static env StaticBytes.[
      I32 Tagged.(int_of_tag Null);
    ]
  let null_lit env =
    compile_unboxed_const (null_vanilla_lit env)

  let is_some env =
    null_lit env ^^
    G.i (Compare (Wasm.Values.I32 I32Op.Ne))

  let inject env e =
    e ^^
    Func.share_code1 env "opt_inject" ("x", I32Type) [I32Type] (fun env get_x ->
      get_x ^^ BitTagged.if_tagged_scalar env [I32Type]
        ( get_x ) (* default, no wrapping *)
        ( get_x ^^ Tagged.branch_default env [I32Type]
          ( get_x ) (* default, no wrapping *)
          [ Tagged.Null,
            (* NB: even ?null does not require allocation: We use a static
               singleton for that: *)
            compile_unboxed_const (E.add_static env StaticBytes.[
              I32 Tagged.(int_of_tag Some);
              I32 (null_vanilla_lit env)
            ])
          ; Tagged.Some,
            Tagged.obj env Tagged.Some [get_x]
          ]
        )
    )

  (* This function is used where conceptually, Opt.inject should be used, but
  we know for sure that it wouldn’t do anything anyways *)
  let inject_noop _env e = e


  let project env =
    Func.share_code1 env "opt_project" ("x", I32Type) [I32Type] (fun env get_x ->
      get_x ^^ BitTagged.if_tagged_scalar env [I32Type]
        ( get_x ) (* default, no wrapping *)
        ( get_x ^^ Tagged.branch_default env [I32Type]
          ( get_x ) (* default, no wrapping *)
          [ Tagged.Some,
            get_x ^^ Heap.load_field some_payload_field
          ; Tagged.Null,
            E.trap_with env "Internal error: opt_project: null!"
          ]
        )
    )

end (* Opt *)

module Variant = struct
  (* The Variant type. We store the variant tag in a first word; we can later
     optimize and squeeze it in the Tagged tag. We can also later support unboxing
     variants with an argument of type ().

       ┌─────────┬────────────┬─────────┐
       │ heaptag │ varianttag │ payload │
       └─────────┴────────────┴─────────┘

  *)

  let variant_tag_field = Tagged.header_size
  let payload_field = Int32.add variant_tag_field 1l

  let hash_variant_label env : Mo_types.Type.lab -> int32 =
    E.hash env

  let inject env l e =
    Tagged.obj env Tagged.Variant [compile_unboxed_const (hash_variant_label env l); e]

  let get_variant_tag = Heap.load_field variant_tag_field
  let project = Heap.load_field payload_field

  (* Test if the top of the stack points to a variant with this label *)
  let test_is env l =
    get_variant_tag ^^
    compile_eq_const (hash_variant_label env l)

end (* Variant *)


module Closure = struct
  (* In this module, we deal with closures, i.e. functions that capture parts
     of their environment.

     The structure of a closure is:

       ┌─────┬───────┬──────┬──────────────┐
       │ tag │ funid │ size │ captured ... │
       └─────┴───────┴──────┴──────────────┘

  *)
  let header_size = Int32.add Tagged.header_size 2l

  let funptr_field = Tagged.header_size
  let len_field = Int32.add 1l Tagged.header_size

  let load_data i = Heap.load_field (Int32.add header_size i)
  let store_data i = Heap.store_field (Int32.add header_size i)

  (* Expect on the stack
     * the function closure
     * and arguments (n-ary!)
     * the function closure again!
  *)
  let call_closure env n_args n_res =
    (* Calculate the wasm type for a given calling convention.
       An extra first argument for the closure! *)
    let ty = E.func_type env (FuncType (
      I32Type :: Lib.List.make n_args I32Type,
      FakeMultiVal.ty (Lib.List.make n_res I32Type))) in
    (* get the table index *)
    Heap.load_field funptr_field ^^
    (* All done: Call! *)
    G.i (CallIndirect (nr ty)) ^^
    FakeMultiVal.load env (Lib.List.make n_res I32Type)

  let static_closure env fi : int32 =
    E.add_static env StaticBytes.[
      I32 Tagged.(int_of_tag Closure);
      I32 (E.add_fun_ptr env fi);
      I32 0l
    ]

end (* Closure *)


module BoxedWord64 = struct
  (* We store large word64s, nat64s and int64s in immutable boxed 64bit heap objects.

     Small values are stored unboxed, tagged, see BitTagged. The bit-tagging logic is
     contained in BitTagged; here we just do the boxing.

     The heap layout of a BoxedWord64 is:

       ┌─────┬─────┬─────┐
       │ tag │    i64    │
       └─────┴─────┴─────┘

  *)

  let payload_field = Tagged.header_size

  let vanilla_lit env i =
    if BitTagged.can_tag_const i
    then BitTagged.tag_const i
    else
      E.add_static env StaticBytes.[
        I32 Tagged.(int_of_tag Bits64);
        I64 i
      ]

  let compile_box env compile_elem : G.t =
    let (set_i, get_i) = new_local env "boxed_i64" in
    Heap.alloc env 3l ^^
    set_i ^^
    get_i ^^ Tagged.(store Bits64) ^^
    get_i ^^ compile_elem ^^ Heap.store_field64 payload_field ^^
    get_i

  let box env = Func.share_code1 env "box_i64" ("n", I64Type) [I32Type] (fun env get_n ->
      get_n ^^ BitTagged.if_can_tag_i64 env [I32Type]
        (get_n ^^ BitTagged.tag)
        (compile_box env get_n)
    )

  let unbox env = Func.share_code1 env "unbox_i64" ("n", I32Type) [I64Type] (fun env get_n ->
      get_n ^^
      BitTagged.if_tagged_scalar env [I64Type]
        ( get_n ^^ BitTagged.untag env)
        ( get_n ^^ Heap.load_field64 payload_field)
    )
end (* BoxedWord64 *)

module Word64 = struct

  let compile_add env = G.i (Binary (Wasm.Values.I64 I64Op.Add))
  let compile_signed_sub env = G.i (Binary (Wasm.Values.I64 I64Op.Sub))
  let compile_mul env = G.i (Binary (Wasm.Values.I64 I64Op.Mul))
  let compile_signed_div env = G.i (Binary (Wasm.Values.I64 I64Op.DivS))
  let compile_signed_mod env = G.i (Binary (Wasm.Values.I64 I64Op.RemS))
  let compile_unsigned_div env = G.i (Binary (Wasm.Values.I64 I64Op.DivU))
  let compile_unsigned_rem env = G.i (Binary (Wasm.Values.I64 I64Op.RemU))
  let compile_unsigned_sub env =
    Func.share_code2 env "nat_sub" (("n1", I64Type), ("n2", I64Type)) [I64Type] (fun env get_n1 get_n2 ->
      get_n1 ^^ get_n2 ^^ G.i (Compare (Wasm.Values.I64 I64Op.LtU)) ^^
      E.then_trap_with env "Natural subtraction underflow" ^^
      get_n1 ^^ get_n2 ^^ G.i (Binary (Wasm.Values.I64 I64Op.Sub))
    )

  let compile_unsigned_pow env =
    let name = prim_fun_name Type.Nat64 "wpow_nat" in
    Func.share_code2 env name (("n", I64Type), ("exp", I64Type)) [I64Type]
      (fun env get_n get_exp ->
        let set_n = G.setter_for get_n in
        let set_exp = G.setter_for get_exp in
        let (set_acc, get_acc) = new_local64 env "acc" in

        (* start with result = 1 *)
        compile_const_64 1L ^^ set_acc ^^

        (* handle exp == 0 *)
        get_exp ^^ G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
        G.if1 I64Type get_acc (* done *)
        begin
          G.loop0 begin
            (* Are we done? *)
            get_exp ^^ compile_const_64 1L ^^ G.i (Compare (Wasm.Values.I64 I64Op.LeU)) ^^
            G.if0 G.nop (* done *)
            begin
              (* Check low bit of exp to see if we need to multiply *)
              get_exp ^^ compile_shl64_const 63L ^^ G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
              G.if0 G.nop
              begin
                (* Multiply! *)
                get_acc ^^ get_n ^^ G.i (Binary (Wasm.Values.I64 I64Op.Mul)) ^^ set_acc
              end ^^
              (* Square n, and shift exponent *)
              get_n ^^ get_n ^^ G.i (Binary (Wasm.Values.I64 I64Op.Mul)) ^^ set_n ^^
              get_exp ^^ compile_shrU64_const 1L ^^ set_exp ^^
              (* And loop *)
              G.i (Br (nr 1l))
            end
          end ^^
          (* Multiply a last time *)
          get_acc ^^ get_n ^^ G.i (Binary (Wasm.Values.I64 I64Op.Mul))
        end
      )


  let compile_signed_wpow env =
    Func.share_code2 env "wrap_pow_Int64" (("n", I64Type), ("exp", I64Type)) [I64Type]
      (fun env get_n get_exp ->
        get_exp ^^
        compile_const_64 0L ^^
        G.i (Compare (Wasm.Values.I64 I64Op.GeS)) ^^
        E.else_trap_with env "negative power" ^^
        get_n ^^ get_exp ^^ compile_unsigned_pow env
      )

  let _compile_eq env = G.i (Compare (Wasm.Values.I64 I64Op.Eq))
  let compile_relop env i64op = G.i (Compare (Wasm.Values.I64 i64op))

end (* BoxedWord64 *)


module BoxedSmallWord = struct
  (* We store proper 32bit Word32 in immutable boxed 32bit heap objects.

     Small values are stored unboxed, tagged, see BitTagged.

     The heap layout of a BoxedSmallWord is:

       ┌─────┬─────┐
       │ tag │ i32 │
       └─────┴─────┘

  *)

  let payload_field = Tagged.header_size

  let vanilla_lit env i =
    if BitTagged.can_tag_const (Int64.of_int (Int32.to_int i))
    then BitTagged.tag_const (Int64.of_int (Int32.to_int i))
    else
      E.add_static env StaticBytes.[
        I32 Tagged.(int_of_tag Bits32);
        I32 i
      ]

  let compile_box env compile_elem : G.t =
    let (set_i, get_i) = new_local env "boxed_i32" in
    Heap.alloc env 2l ^^
    set_i ^^
    get_i ^^ Tagged.(store Bits32) ^^
    get_i ^^ compile_elem ^^ Heap.store_field payload_field ^^
    get_i

  let box env = Func.share_code1 env "box_i32" ("n", I32Type) [I32Type] (fun env get_n ->
      get_n ^^ compile_unboxed_const (Int32.of_int (1 lsl 30)) ^^
      G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
      G.if1 I32Type
        (get_n ^^ BitTagged.tag_i32)
        (compile_box env get_n)
    )

  let unbox env = Func.share_code1 env "unbox_i32" ("n", I32Type) [I32Type] (fun env get_n ->
      get_n ^^
      BitTagged.if_tagged_scalar env [I32Type]
        ( get_n ^^ BitTagged.untag_i32)
        ( get_n ^^ Heap.load_field payload_field)
    )

  let _lit env n = compile_unboxed_const n ^^ box env

end (* BoxedSmallWord *)

module TaggedSmallWord = struct
  (* While smaller-than-32bit words are treated as i32 from the WebAssembly
     perspective, there are certain differences that are type based. This module
     provides helpers to abstract over those.

     Caution: Some functions here are also used for Nat32/Int32, while others
     are _only_ used for the small ones. Check call-sites!
  *)

  let bits_of_type = function
    | Type.(Int8|Nat8) -> 8
    | Type.(Int16|Nat16) -> 16
    | _ -> 32

  let shift_of_type ty = Int32.of_int (32 - bits_of_type ty)

  let bitwidth_mask_of_type = function
    | Type.(Int8|Nat8) -> 0b111l
    | Type.(Int16|Nat16) -> 0b1111l
    | p -> todo "bitwidth_mask_of_type" (Arrange_type.prim p) 0l

  let const_of_type ty n = Int32.(shift_left n (to_int (shift_of_type ty)))

  let padding_of_type ty = Int32.(sub (const_of_type ty 1l) one)

  let mask_of_type ty = Int32.lognot (padding_of_type ty)

  (* Makes sure that we only shift/rotate the maximum number of bits available in the word. *)
  let clamp_shift_amount = function
    | Type.(Nat32|Int32) -> G.nop
    | ty -> compile_bitand_const (bitwidth_mask_of_type ty)

  let shift_leftWordNtoI32 = compile_shl_const

  (* Makes sure that the word payload (e.g. shift/rotate amount) is in the LSB bits of the word. *)
  let lsb_adjust = function
    | Type.(Int32|Nat32) -> G.nop
    | Type.(Nat8|Nat16) as ty -> compile_shrU_const (shift_of_type ty)
    | Type.(Int8|Int16) as ty -> compile_shrS_const (shift_of_type ty)
    | _ -> assert false

  (* Makes sure that the word payload (e.g. operation result) is in the MSB bits of the word. *)
  let msb_adjust = function
    | Type.(Int32|Nat32) -> G.nop
    | ty -> shift_leftWordNtoI32 (shift_of_type ty)

  (* Makes sure that the word representation invariant is restored. *)
  let sanitize_word_result = function
    | Type.(Nat32|Int32) -> G.nop
    | ty -> compile_bitand_const (mask_of_type ty)

  (* Sets the number (according to the type's word invariant) of LSBs. *)
  let compile_word_padding = function
    | Type.(Nat32|Int32) -> G.nop
    | ty -> compile_bitor_const (padding_of_type ty)

  (* Kernel for counting leading zeros, according to the word invariant. *)
  let clz_kernel ty =
    compile_word_padding ty ^^
    G.i (Unary (Wasm.Values.I32 I32Op.Clz)) ^^
    msb_adjust ty

  (* Kernel for counting trailing zeros, according to the word invariant. *)
  let ctz_kernel ty =
    compile_word_padding ty ^^
    compile_rotr_const (shift_of_type ty) ^^
    G.i (Unary (Wasm.Values.I32 I32Op.Ctz)) ^^
    msb_adjust ty

  (* Kernel for testing a bit position, according to the word invariant. *)
  let btst_kernel env ty =
    let (set_b, get_b) = new_local env "b"
    in lsb_adjust ty ^^ set_b ^^ lsb_adjust ty ^^
       compile_unboxed_one ^^ get_b ^^ clamp_shift_amount ty ^^
       G.i (Binary (Wasm.Values.I32 I32Op.Shl)) ^^
       G.i (Binary (Wasm.Values.I32 I32Op.And))

  (* Code points occupy 21 bits, so can always be tagged scalars *)
  let untag_codepoint = compile_shrU_const 8l
  let tag_codepoint = compile_shl_const 8l

  (* Checks (n < 0xD800 || 0xE000 ≤ n ≤ 0x10FFFF),
     ensuring the codepoint range and the absence of surrogates. *)
  let check_and_tag_codepoint env =
    Func.share_code1 env "Nat32->Char" ("n", I32Type) [I32Type] (fun env get_n ->
      get_n ^^ compile_unboxed_const 0xD800l ^^
      G.i (Compare (Wasm.Values.I32 I32Op.GeU)) ^^
      get_n ^^ compile_unboxed_const 0xE000l ^^
      G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
      G.i (Binary (Wasm.Values.I32 I32Op.And)) ^^
      get_n ^^ compile_unboxed_const 0x10FFFFl ^^
      G.i (Compare (Wasm.Values.I32 I32Op.GtU)) ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Or)) ^^
      E.then_trap_with env "codepoint out of range" ^^
      get_n ^^ tag_codepoint
    )

  let vanilla_lit ty v =
    Int32.(shift_left (of_int v) (to_int (shift_of_type ty)))

  (* Wrapping implementation for multiplication and exponentiation. *)

  let compile_word_mul env ty =
    lsb_adjust ty ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Mul))

  let compile_nat_power env ty =
    (* Square- and multiply exponentiation *)
    let name = prim_fun_name ty "wpow_nat" in
    Func.share_code2 env name (("n", I32Type), ("exp", I32Type)) [I32Type]
      (fun env get_n get_exp ->
        let set_n = G.setter_for get_n in
        let set_exp = G.setter_for get_exp in
        let (set_acc, get_acc) = new_local env "acc" in

        (* unshift arguments *)
        get_exp ^^ compile_shrU_const (shift_of_type ty) ^^ set_exp ^^
        get_n ^^ compile_shrU_const (shift_of_type ty) ^^ set_n ^^

        (* The accumulator starts with and stays shifted, so no other shifts needed. *)
        compile_unboxed_const (const_of_type ty 1l) ^^ set_acc ^^

        (* handle exp == 0 *)
        get_exp ^^ G.i (Test (Wasm.Values.I32 I32Op.Eqz)) ^^
        G.if1 I32Type get_acc (* done *)
        begin
          G.loop0 begin
            (* Are we done? *)
            get_exp ^^ compile_unboxed_const 1l ^^ G.i (Compare (Wasm.Values.I32 I32Op.LeU)) ^^
            G.if0 G.nop (* done *)
            begin
              (* Check low bit of exp to see if we need to multiply *)
              get_exp ^^ compile_shl_const 31l ^^ G.i (Test (Wasm.Values.I32 I32Op.Eqz)) ^^
              G.if0 G.nop
              begin
                (* Multiply! *)
                get_acc ^^ get_n ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)) ^^ set_acc
              end ^^
              (* Square n, and shift exponent *)
              get_n ^^ get_n ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)) ^^ set_n ^^
              get_exp ^^ compile_shrU_const 1l ^^ set_exp ^^
              (* And loop *)
              G.i (Br (nr 1l))
            end
          end ^^
          (* Multiply a last time *)
          get_acc ^^ get_n ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul))
          (* Accumulator was shifted, so no further shift needed here *)
        end
      )

  let compile_int_power env ty =
    let name = prim_fun_name ty "wpow_int" in
    Func.share_code2 env name (("n", I32Type), ("exp", I32Type)) [I32Type]
      (fun env get_n get_exp ->
        get_exp ^^
        compile_unboxed_const 0l ^^
        G.i (Compare (Wasm.Values.I32 I32Op.GeS)) ^^
        E.else_trap_with env "negative power"  ^^
        get_n ^^ get_exp ^^ compile_nat_power env ty
      )


  (* To rotate, first rotate a copy by bits_of_type into the other direction *)
  let rotl env ty =
     Func.share_code2 env (prim_fun_name ty "rotl") (("n", I32Type), ("by", I32Type)) [I32Type]
       (fun env get_n get_by ->
        let open Wasm.Values in
        let beside_adjust = compile_rotr_const (Int32.of_int (bits_of_type ty)) in
        get_n ^^ get_n ^^ beside_adjust ^^ G.i (Binary (I32 I32Op.Or)) ^^
        get_by ^^ lsb_adjust ty ^^ clamp_shift_amount ty ^^ G.i (Binary (I32 I32Op.Rotl)) ^^
        sanitize_word_result ty
       )

  let rotr env ty =
     Func.share_code2 env (prim_fun_name ty "rotr") (("n", I32Type), ("by", I32Type)) [I32Type]
       (fun env get_n get_by ->
        let open Wasm.Values in
        let beside_adjust = compile_rotl_const (Int32.of_int (bits_of_type ty)) in
        get_n ^^ get_n ^^ beside_adjust ^^ G.i (Binary (I32 I32Op.Or)) ^^
        get_by ^^ lsb_adjust ty ^^ clamp_shift_amount ty ^^ G.i (Binary (I32 I32Op.Rotr)) ^^
        sanitize_word_result ty
       )

end (* TaggedSmallWord *)


module Float = struct
  (* We store floats (C doubles) in immutable boxed 64bit heap objects.

     The heap layout of a Float is:

       ┌─────┬─────┬─────┐
       │ tag │    f64    │
       └─────┴─────┴─────┘

     For now the tag stored is that of a Bits64, because the payload is
     treated opaquely by the RTS. We'll introduce a separate tag when the need of
     debug inspection (or GC representation change) arises.

  *)

  let payload_field = Tagged.header_size

  let compile_unboxed_const f = G.i (Const (nr (Wasm.Values.F64 f)))
  let lit f = compile_unboxed_const (Wasm.F64.of_float f)
  let compile_unboxed_zero = lit 0.0

  let vanilla_lit env f =
    E.add_static env StaticBytes.[
      I32 Tagged.(int_of_tag Bits64);
      I64 (Wasm.F64.to_bits f)
    ]

  let box env = Func.share_code1 env "box_f64" ("f", F64Type) [I32Type] (fun env get_f ->
    let (set_i, get_i) = new_local env "boxed_f64" in
    Heap.alloc env 3l ^^
    set_i ^^
    get_i ^^ Tagged.(store Bits64) ^^
    get_i ^^ get_f ^^ Heap.store_field_float64 payload_field ^^
    get_i
    )

  let unbox env = Heap.load_field_float64 payload_field

end (* Float *)


module ReadBuf = struct
  (*
  Combinators to safely read from a dynamic buffer.

  We represent a buffer by a pointer to two words in memory (usually allocated
  on the shadow stack): The first is a pointer to the current position of the buffer,
  the second one a pointer to the end (to check out-of-bounds).

  Code that reads from this buffer will update the former, i.e. it is mutable.

  The format is compatible with C (pointer to a struct) and avoids the need for the
  multi-value extension that we used before to return both parse result _and_
  updated pointer.

  All pointers here are unskewed!

  This module is mostly for serialization, but because there are bits of
  serialization code in the BigNumType implementations, we put it here.
  *)

  let get_ptr get_buf =
    get_buf ^^ G.i (Load {ty = I32Type; align = 2; offset = 0l; sz = None})
  let get_end get_buf =
    get_buf ^^ G.i (Load {ty = I32Type; align = 2; offset = Heap.word_size; sz = None})
  let set_ptr get_buf new_val =
    get_buf ^^ new_val ^^ G.i (Store {ty = I32Type; align = 2; offset = 0l; sz = None})
  let set_end get_buf new_val =
    get_buf ^^ new_val ^^ G.i (Store {ty = I32Type; align = 2; offset = Heap.word_size; sz = None})
  let set_size get_buf get_size =
    set_end get_buf
      (get_ptr get_buf ^^ get_size ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)))

  let alloc env f = Stack.with_words env "buf" 2l f

  let advance get_buf get_delta =
    set_ptr get_buf (get_ptr get_buf ^^ get_delta ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)))

  let read_leb128 env get_buf =
    get_buf ^^ E.call_import env "rts" "leb128_decode"

  let read_sleb128 env get_buf =
    get_buf ^^ E.call_import env "rts" "sleb128_decode"

  let check_space env get_buf get_delta =
    get_delta ^^
    get_end get_buf ^^ get_ptr get_buf ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
    G.i (Compare (Wasm.Values.I32 I64Op.LeU)) ^^
    E.else_trap_with env "IDL error: out of bounds read"

  let is_empty env get_buf =
    get_end get_buf ^^ get_ptr get_buf ^^
    G.i (Compare (Wasm.Values.I32 I64Op.Eq))

  let read_byte env get_buf =
    check_space env get_buf (compile_unboxed_const 1l) ^^
    get_ptr get_buf ^^
    G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.(Pack8, ZX)}) ^^
    advance get_buf (compile_unboxed_const 1l)

  let read_word16 env get_buf =
    check_space env get_buf (compile_unboxed_const 2l) ^^
    get_ptr get_buf ^^
    G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.(Pack16, ZX)}) ^^
    advance get_buf (compile_unboxed_const 2l)

  let read_word32 env get_buf =
    check_space env get_buf (compile_unboxed_const 4l) ^^
    get_ptr get_buf ^^
    G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = None}) ^^
    advance get_buf (compile_unboxed_const 4l)

  let read_word64 env get_buf =
    check_space env get_buf (compile_unboxed_const 8l) ^^
    get_ptr get_buf ^^
    G.i (Load {ty = I64Type; align = 0; offset = 0l; sz = None}) ^^
    advance get_buf (compile_unboxed_const 8l)

  let read_float64 env get_buf =
    check_space env get_buf (compile_unboxed_const 8l) ^^
    get_ptr get_buf ^^
    G.i (Load {ty = F64Type; align = 0; offset = 0l; sz = None}) ^^
    advance get_buf (compile_unboxed_const 8l)

  let read_blob env get_buf get_len =
    check_space env get_buf get_len ^^
    (* Already has destination address on the stack *)
    get_ptr get_buf ^^
    get_len ^^
    Heap.memcpy env ^^
    advance get_buf get_len

end (* Buf *)


type comparator = Lt | Le | Ge | Gt

module type BigNumType =
sig
  (* word from SR.Vanilla, trapping, unsigned semantics *)
  val to_word32 : E.t -> G.t
  val to_word64 : E.t -> G.t
  val to_word32_with : E.t -> G.t (* with error message on stack (ptr/len) *)

  (* word from SR.Vanilla, lossy, raw bits *)
  val truncate_to_word32 : E.t -> G.t
  val truncate_to_word64 : E.t -> G.t

  (* unsigned word to SR.Vanilla *)
  val from_word30 : E.t -> G.t
  val from_word32 : E.t -> G.t
  val from_word64 : E.t -> G.t

  (* signed word to SR.Vanilla *)
  val from_signed_word32 : E.t -> G.t
  val from_signed_word64 : E.t -> G.t

  (* buffers *)
  (* given a numeric object on stack (vanilla),
     push the number (i32) of bytes necessary
     to externalize the numeric object *)
  val compile_data_size_signed : E.t -> G.t
  val compile_data_size_unsigned : E.t -> G.t
  (* given on stack
     - numeric object (vanilla, TOS)
     - data buffer
    store the binary representation of the numeric object into the data buffer,
    and push the number (i32) of bytes stored onto the stack
   *)
  val compile_store_to_data_buf_signed : E.t -> G.t
  val compile_store_to_data_buf_unsigned : E.t -> G.t
  (* given a ReadBuf on stack, consume bytes from it,
     deserializing to a numeric object
     and leave it on the stack (vanilla).
     The boolean argument is true if the value to be read is signed.
   *)
  val compile_load_from_data_buf : E.t -> bool -> G.t

  (* literals *)
  val vanilla_lit : E.t -> Big_int.big_int -> int32

  (* arithmetic *)
  val compile_abs : E.t -> G.t
  val compile_neg : E.t -> G.t
  val compile_add : E.t -> G.t
  val compile_signed_sub : E.t -> G.t
  val compile_unsigned_sub : E.t -> G.t
  val compile_mul : E.t -> G.t
  val compile_signed_div : E.t -> G.t
  val compile_signed_mod : E.t -> G.t
  val compile_unsigned_div : E.t -> G.t
  val compile_unsigned_rem : E.t -> G.t
  val compile_unsigned_pow : E.t -> G.t
  val compile_lshd : E.t -> G.t

  (* comparisons *)
  val compile_eq : E.t -> G.t
  val compile_is_negative : E.t -> G.t
  val compile_relop : E.t -> comparator -> G.t

  (* representation checks *)
  (* given a numeric object on the stack as skewed pointer, check whether
     it can be faithfully stored in N bits, including a leading sign bit
     leaves boolean result on the stack
     N must be 2..64
   *)
  val fits_signed_bits : E.t -> int -> G.t
  (* given a numeric object on the stack as skewed pointer, check whether
     it can be faithfully stored in N unsigned bits
     leaves boolean result on the stack
     N must be 1..64
   *)
  val fits_unsigned_bits : E.t -> int -> G.t
end

let i64op_from_relop = function
  | Lt -> I64Op.LtS
  | Le -> I64Op.LeS
  | Ge -> I64Op.GeS
  | Gt -> I64Op.GtS

let name_from_relop = function
  | Lt -> "B_lt"
  | Le -> "B_le"
  | Ge -> "B_ge"
  | Gt -> "B_gt"

(* helper, measures the dynamics of the unsigned i32, returns (32 - effective bits) *)
let unsigned_dynamics get_x =
  get_x ^^
  G.i (Unary (Wasm.Values.I32 I32Op.Clz))

(* helper, measures the dynamics of the signed i32, returns (32 - effective bits) *)
let signed_dynamics get_x =
  get_x ^^ compile_shl_const 1l ^^
  get_x ^^
  G.i (Binary (Wasm.Values.I32 I32Op.Xor)) ^^
  G.i (Unary (Wasm.Values.I32 I32Op.Clz))

module I32Leb = struct
  let compile_size dynamics get_x =
    get_x ^^ G.if1 I32Type
      begin
        compile_unboxed_const 38l ^^
        dynamics get_x ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
        compile_divU_const 7l
      end
      compile_unboxed_one

  let compile_leb128_size get_x = compile_size unsigned_dynamics get_x
  let compile_sleb128_size get_x = compile_size signed_dynamics get_x

  let compile_store_to_data_buf_unsigned env get_x get_buf =
    get_x ^^ get_buf ^^ E.call_import env "rts" "leb128_encode" ^^
    compile_leb128_size get_x

  let compile_store_to_data_buf_signed env get_x get_buf =
    get_x ^^ get_buf ^^ E.call_import env "rts" "sleb128_encode" ^^
    compile_sleb128_size get_x

end

module MakeCompact (Num : BigNumType) : BigNumType = struct

  (* Compact BigNums are a representation of signed 31-bit bignums (of the
     underlying boxed representation `Num`), that fit into an i32 as per the
     BitTagged representation.

     Many arithmetic operations can be be performed on this right-zero-padded
     representation directly. For some operations (e.g. multiplication) the
     second argument needs to be furthermore right-shifted to avoid overflow.
     Similarly, for division the result must be left-shifted.

     Generally all operations begin with checking whether both arguments are
     already tagged scalars. If so, the arithmetic can be performed in machine
     registers (fast path). Otherwise one or both arguments need boxing and the
     arithmetic needs to be carried out on the underlying boxed bignum
     representation (slow path).

     The result appears as a boxed number in the latter case, so a check is
     performed if it can be a tagged scalar. Conversely in the former case the
     64-bit result can either be a tagged scalar or needs to be boxed.

     Manipulation of the result is unnecessary for the comparison predicates.

     For the `pow` operation the check that both arguments are tagged scalars
     is not sufficient. Here we count and multiply effective bitwidths to
     figure out whether the operation will overflow 64 bits, and if so, we fall
     back to the slow path.
   *)

  (* TODO: There is some unnecessary result shifting when the div result needs
     to be boxed. Is this possible at all to happen? With (/-1) maybe! *)

  (* TODO: Does the result of the rem/mod fast path ever needs boxing? *)

  (* examine the skewed pointer and determine if number fits into 31 bits *)
  let fits_in_vanilla env = Num.fits_signed_bits env 31

  (* Tagged scalar to right-0-padded signed i64 *)
  let extend64 = G.i (Convert (Wasm.Values.I64 I64Op.ExtendSI32))

  (* A variant of BitTagged.can_tag that works on right-0-tagged 64 bit numbers *)
  let if_can_tag_padded env retty is1 is2 =
    compile_shrS64_const 1L ^^ BitTagged.if_can_tag_i64 env retty is1 is2

  (* right-0-padded signed i64 to tagged scalar *)
  let tag_padded = G.i (Convert (Wasm.Values.I32 I32Op.WrapI64))

  (* creates a boxed bignum from a right-0-padded signed i64 *)
  let box64 env = compile_shrS64_const 1L ^^ Num.from_signed_word64 env

  (* creates a boxed bignum from an right-0-padded signed i32 *)
  let extend_and_box64 env = extend64 ^^ box64 env

  (* check if both arguments are tagged scalars,
     if so, promote to right-0-padded, signed i64 and perform the fast path.
     Otherwise make sure that both arguments are in heap representation,
     and run the slow path on them.
     In both cases bring the results into normal form.
   *)
  let try_unbox2 name fast slow env =
    Func.share_code2 env name (("a", I32Type), ("b", I32Type)) [I32Type]
      (fun env get_a get_b ->
        let set_res, get_res = new_local env "res" in
        let set_res64, get_res64 = new_local64 env "res64" in
        get_a ^^ get_b ^^
        BitTagged.if_both_tagged_scalar env [I32Type]
          begin
            get_a ^^ extend64 ^^
            get_b ^^ extend64 ^^
            fast env ^^ set_res64 ^^
            get_res64 ^^
            if_can_tag_padded env [I32Type]
              (get_res64 ^^ tag_padded)
              (get_res64 ^^ box64 env)
          end
          begin
            get_a ^^ BitTagged.if_tagged_scalar env [I32Type]
              (get_a ^^ extend_and_box64 env)
              get_a ^^
            get_b ^^ BitTagged.if_tagged_scalar env [I32Type]
              (get_b ^^ extend_and_box64 env)
              get_b ^^
            slow env ^^ set_res ^^ get_res ^^
            fits_in_vanilla env ^^
            G.if1 I32Type
              (get_res ^^ Num.truncate_to_word32 env ^^ BitTagged.tag_i32)
              get_res
          end)

  let compile_add = try_unbox2 "B_add" Word64.compile_add Num.compile_add

  let adjust_arg2 code env = compile_shrS64_const 1L ^^ code env
  let adjust_result code env = code env ^^ compile_shl64_const 1L

  let compile_mul = try_unbox2 "B_mul" (adjust_arg2 Word64.compile_mul) Num.compile_mul
  let compile_signed_sub = try_unbox2 "B+sub" Word64.compile_signed_sub Num.compile_signed_sub
  let compile_signed_div = try_unbox2 "B+div" (adjust_result Word64.compile_signed_div) Num.compile_signed_div
  let compile_signed_mod = try_unbox2 "B_mod" Word64.compile_signed_mod Num.compile_signed_mod
  let compile_unsigned_div = try_unbox2 "B_div" (adjust_result Word64.compile_unsigned_div) Num.compile_unsigned_div
  let compile_unsigned_rem = try_unbox2 "B_rem" Word64.compile_unsigned_rem Num.compile_unsigned_rem
  let compile_unsigned_sub = try_unbox2 "B_sub" Word64.compile_unsigned_sub Num.compile_unsigned_sub

  let compile_unsigned_pow env =
    Func.share_code2 env "B_pow" (("a", I32Type), ("b", I32Type)) [I32Type]
    (fun env get_a get_b ->
    let set_res, get_res = new_local env "res" in
    let set_res64, get_res64 = new_local64 env "res64" in
    get_a ^^ get_b ^^
    BitTagged.if_both_tagged_scalar env [I32Type]
      begin
        let set_a64, get_a64 = new_local64 env "a64" in
        let set_b64, get_b64 = new_local64 env "b64" in
        (* Convert to plain Word64 *)
        get_a ^^ extend64 ^^ compile_shrS64_const 1L ^^ set_a64 ^^
        get_b ^^ extend64 ^^ compile_shrS64_const 1L ^^ set_b64 ^^

        (* estimate bitcount of result: `bits(a) * b <= 64` guarantees
           the absence of overflow in 64-bit arithmetic *)
        compile_const_64 64L ^^
        get_a64 ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^ G.i (Binary (Wasm.Values.I64 I64Op.Sub)) ^^
        get_b64 ^^ G.i (Binary (Wasm.Values.I64 I64Op.Mul)) ^^
        compile_const_64 64L ^^ G.i (Compare (Wasm.Values.I64 I64Op.LeU)) ^^
        G.if1 I32Type
          begin
            get_a64 ^^ get_b64 ^^ Word64.compile_unsigned_pow env ^^ set_res64 ^^
            get_res64 ^^ BitTagged.if_can_tag_i64 env [I32Type]
              (get_res64 ^^ BitTagged.tag)
              (get_res64 ^^ Num.from_word64 env)
          end
          begin
            get_a64 ^^ Num.from_signed_word64 env ^^
            get_b64 ^^ Num.from_signed_word64 env ^^
            Num.compile_unsigned_pow env ^^ set_res ^^
            get_res ^^ fits_in_vanilla env ^^
            G.if1 I32Type
              (get_res ^^ Num.truncate_to_word32 env ^^ BitTagged.tag_i32)
              get_res
          end
      end
      begin
        get_a ^^ BitTagged.if_tagged_scalar env [I32Type]
          (get_a ^^ extend_and_box64 env)
          get_a ^^
        get_b ^^ BitTagged.if_tagged_scalar env [I32Type]
          (get_b ^^ extend_and_box64 env)
          get_b ^^
        Num.compile_unsigned_pow env ^^ set_res ^^
        get_res ^^ fits_in_vanilla env ^^
        G.if1 I32Type
          (get_res ^^ Num.truncate_to_word32 env ^^ BitTagged.tag_i32)
          get_res
      end)


  let compile_lshd env = assert false




  let compile_is_negative env =
    let set_n, get_n = new_local env "n" in
    set_n ^^ get_n ^^
    BitTagged.if_tagged_scalar env [I32Type]
      (get_n ^^ compile_unboxed_const 0l ^^ G.i (Compare (Wasm.Values.I32 I32Op.LtS)))
      (get_n ^^ Num.compile_is_negative env)

  let vanilla_lit env = function
    | n when Big_int.is_int_big_int n && BitTagged.can_tag_const (Big_int.int64_of_big_int n) ->
      BitTagged.tag_const (Big_int.int64_of_big_int n)
    | n -> Num.vanilla_lit env n

  let compile_neg env =
    Func.share_code1 env "B_neg" ("n", I32Type) [I32Type] (fun env get_n ->
      get_n ^^ BitTagged.if_tagged_scalar env [I32Type]
        begin
          get_n ^^ compile_eq_const 0x80000000l ^^ (* -2^30 shifted *)
          G.if1 I32Type
            (compile_unboxed_const 0x40000000l ^^ Num.from_word32 env)
            begin
              compile_unboxed_const 0l ^^
              get_n ^^
              G.i (Binary (Wasm.Values.I32 I32Op.Sub))
            end
        end
        (get_n ^^ Num.compile_neg env)
    )

  let try_comp_unbox2 name fast slow env =
    Func.share_code2 env name (("a", I32Type), ("b", I32Type)) [I32Type]
      (fun env get_a get_b ->
        get_a ^^ get_b ^^
        BitTagged.if_both_tagged_scalar env [I32Type]
          begin
            get_a ^^ extend64 ^^
            get_b ^^ extend64 ^^
            fast env
          end
          begin
            get_a ^^ BitTagged.if_tagged_scalar env [I32Type]
              (get_a ^^ extend_and_box64 env)
              get_a ^^
            get_b ^^ BitTagged.if_tagged_scalar env [I32Type]
              (get_b ^^ extend_and_box64 env)
              get_b ^^
            slow env
          end)

  let compile_eq env =
    Func.share_code2 env "B_eq" (("a", I32Type), ("b", I32Type)) [I32Type]
      (fun env get_a get_b ->
        get_a ^^ get_b ^^
        G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
        G.if1 I32Type
          (Bool.lit true)
          (get_a ^^ get_b ^^
           BitTagged.if_both_tagged_scalar env [I32Type]
             (Bool.lit false)
             begin
               get_a ^^ BitTagged.if_tagged_scalar env [I32Type]
                 (get_a ^^ extend_and_box64 env)
                 get_a ^^
               get_b ^^ BitTagged.if_tagged_scalar env [I32Type]
                 (get_b ^^ extend_and_box64 env)
                 get_b ^^
               Num.compile_eq env
             end))

  let compile_relop env bigintop =
    try_comp_unbox2 (name_from_relop bigintop)
      (fun env' -> Word64.compile_relop env' (i64op_from_relop bigintop))
      (fun env' -> Num.compile_relop env' bigintop)
      env

  let try_unbox iN fast slow env =
    let set_a, get_a = new_local env "a" in
    set_a ^^ get_a ^^
    BitTagged.if_tagged_scalar env [iN]
      (get_a ^^ fast env)
      (get_a ^^ slow env)

  let fits_unsigned_bits env n =
    try_unbox I32Type (fun _ -> match n with
        | 32 | 64 -> G.i Drop ^^ Bool.lit true
        | 8 | 16 ->
          compile_bitand_const Int32.(logor 1l (shift_left minus_one (n + 1))) ^^
          G.i (Test (Wasm.Values.I32 I32Op.Eqz))
        | _ -> assert false
      )
      (fun env -> Num.fits_unsigned_bits env n)
      env

  let fits_signed_bits env n =
    let set_a, get_a = new_local env "a" in
    try_unbox I32Type (fun _ -> match n with
        | 32 | 64 -> G.i Drop ^^ Bool.lit true
        | 8 | 16 ->
           set_a ^^
           get_a ^^ get_a ^^ compile_shrS_const 1l ^^
           G.i (Binary (Wasm.Values.I32 I32Op.Xor)) ^^
           compile_bitand_const
             Int32.(shift_left minus_one n) ^^
           G.i (Test (Wasm.Values.I32 I32Op.Eqz))
        | _ -> assert false
      )
      (fun env -> Num.fits_signed_bits env n)
      env

  let compile_abs env =
    try_unbox I32Type
      begin
        fun _ ->
        let set_a, get_a = new_local env "a" in
        set_a ^^
        get_a ^^ compile_unboxed_const 0l ^^ G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^
        G.if1 I32Type
          begin
            get_a ^^
            (* -2^30 is small enough for compact representation, but 2^30 isn't *)
            compile_eq_const 0x80000000l ^^ (* i.e. -2^30 shifted *)
            G.if1 I32Type
              (compile_unboxed_const 0x40000000l ^^ Num.from_word32 env)
              begin
                (* absolute value works directly on shifted representation *)
                compile_unboxed_const 0l ^^
                get_a ^^
                G.i (Binary (Wasm.Values.I32 I32Op.Sub))
              end
          end
          get_a
      end
      Num.compile_abs
      env

  let compile_load_from_data_buf env signed =
    let set_res, get_res = new_local env "res" in
    Num.compile_load_from_data_buf env signed ^^
    set_res ^^
    get_res ^^ fits_in_vanilla env ^^
    G.if1 I32Type
      (get_res ^^ Num.truncate_to_word32 env ^^ BitTagged.tag_i32)
      get_res

  let compile_store_to_data_buf_unsigned env =
    let set_x, get_x = new_local env "x" in
    let set_buf, get_buf = new_local env "buf" in
    set_x ^^ set_buf ^^
    get_x ^^
    try_unbox I32Type
      (fun env ->
        BitTagged.untag_i32 ^^ set_x ^^
        I32Leb.compile_store_to_data_buf_unsigned env get_x get_buf
      )
      (fun env ->
        G.i Drop ^^
        get_buf ^^ get_x ^^ Num.compile_store_to_data_buf_unsigned env)
      env

  let compile_store_to_data_buf_signed env =
    let set_x, get_x = new_local env "x" in
    let set_buf, get_buf = new_local env "buf" in
    set_x ^^ set_buf ^^
    get_x ^^
    try_unbox I32Type
      (fun env ->
        BitTagged.untag_i32 ^^ set_x ^^
        I32Leb.compile_store_to_data_buf_signed env get_x get_buf
      )
      (fun env ->
        G.i Drop ^^
        get_buf ^^ get_x ^^ Num.compile_store_to_data_buf_signed env)
      env

  let compile_data_size_unsigned env =
    try_unbox I32Type
      (fun _ ->
        let set_x, get_x = new_local env "x" in
        BitTagged.untag_i32 ^^ set_x ^^
        I32Leb.compile_leb128_size get_x
      )
      (fun env -> Num.compile_data_size_unsigned env)
      env

  let compile_data_size_signed env =
    try_unbox I32Type
      (fun _ ->
        let set_x, get_x = new_local env "x" in
        BitTagged.untag_i32 ^^ set_x ^^
        I32Leb.compile_sleb128_size get_x
      )
      (fun env -> Num.compile_data_size_signed env)
      env

  let from_signed_word32 env =
    let set_a, get_a = new_local env "a" in
    set_a ^^
    get_a ^^ BitTagged.if_can_tag_i32 env  [I32Type]
      (get_a ^^ BitTagged.tag_i32)
      (get_a ^^ Num.from_signed_word32 env)

  let from_signed_word64 env =
    let set_a, get_a = new_local64 env "a" in
    set_a ^^
    get_a ^^ BitTagged.if_can_tag_i64 env [I32Type]
      (get_a ^^ BitTagged.tag)
      (get_a ^^ Num.from_signed_word64 env)

  let from_word30 env =
    compile_shl_const 1l

  let from_word32 env =
    let set_a, get_a = new_local env "a" in
    set_a ^^
    get_a ^^ BitTagged.if_can_tag_u32 env [I32Type]
      (get_a ^^ BitTagged.tag_i32)
      (get_a ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^ Num.from_word64 env)

  let from_word64 env =
    let set_a, get_a = new_local64 env "a" in
    set_a ^^
    get_a ^^ BitTagged.if_can_tag_u64 env [I32Type]
      (get_a ^^ BitTagged.tag)
      (get_a ^^ Num.from_word64 env)

  let truncate_to_word64 env =
    let set_a, get_a = new_local env "a" in
    set_a ^^ get_a ^^
    BitTagged.if_tagged_scalar env [I64Type]
      (get_a ^^ BitTagged.untag env)
      (get_a ^^ Num.truncate_to_word64 env)

  let truncate_to_word32 env =
    let set_a, get_a = new_local env "a" in
    set_a ^^ get_a ^^
    BitTagged.if_tagged_scalar env [I32Type]
      (get_a ^^ BitTagged.untag_i32)
      (get_a ^^ Num.truncate_to_word32 env)

  let to_word64 env =
    let set_a, get_a = new_local env "a" in
    set_a ^^ get_a ^^
    BitTagged.if_tagged_scalar env [I64Type]
      (get_a ^^ BitTagged.untag env)
      (get_a ^^ Num.to_word64 env)

  let to_word32 env =
    let set_a, get_a = new_local env "a" in
    set_a ^^ get_a ^^
    BitTagged.if_tagged_scalar env [I32Type]
      (get_a ^^ BitTagged.untag_i32)
      (get_a ^^ Num.to_word32 env)

  let to_word32_with env =
    let set_a, get_a = new_local env "a" in
    let set_err_msg, get_err_msg = new_local env "err_msg" in
    set_err_msg ^^ set_a ^^
    get_a ^^
    BitTagged.if_tagged_scalar env [I32Type]
      (get_a ^^ BitTagged.untag_i32)
      (get_a ^^ get_err_msg ^^ Num.to_word32_with env)
end

module BigNumLibtommath : BigNumType = struct

  let to_word32 env = E.call_import env "rts" "bigint_to_word32_trap"
  let to_word64 env = E.call_import env "rts" "bigint_to_word64_trap"
  let to_word32_with env = E.call_import env "rts" "bigint_to_word32_trap_with"

  let truncate_to_word32 env = E.call_import env "rts" "bigint_to_word32_wrap"
  let truncate_to_word64 env = E.call_import env "rts" "bigint_to_word64_wrap"

  let from_word30 env = E.call_import env "rts" "bigint_of_word32"
  let from_word32 env = E.call_import env "rts" "bigint_of_word32"
  let from_word64 env = E.call_import env "rts" "bigint_of_word64"
  let from_signed_word32 env = E.call_import env "rts" "bigint_of_int32"
  let from_signed_word64 env = E.call_import env "rts" "bigint_of_int64"

  let compile_data_size_unsigned env = E.call_import env "rts" "bigint_leb128_size"
  let compile_data_size_signed env = E.call_import env "rts" "bigint_sleb128_size"

  let compile_store_to_data_buf_unsigned env =
    let (set_buf, get_buf) = new_local env "buf" in
    let (set_n, get_n) = new_local env "n" in
    set_n ^^ set_buf ^^
    get_n ^^ get_buf ^^ E.call_import env "rts" "bigint_leb128_encode" ^^
    get_n ^^ E.call_import env "rts" "bigint_leb128_size"
  let compile_store_to_data_buf_signed env =
    let (set_buf, get_buf) = new_local env "buf" in
    let (set_n, get_n) = new_local env "n" in
    set_n ^^ set_buf ^^
    get_n ^^ get_buf ^^ E.call_import env "rts" "bigint_sleb128_encode" ^^
    get_n ^^ E.call_import env "rts" "bigint_sleb128_size"

  let compile_load_from_data_buf env = function
    | false -> E.call_import env "rts" "bigint_leb128_decode"
    | true -> E.call_import env "rts" "bigint_sleb128_decode"

  let vanilla_lit env n =
    (* See enum mp_sign *)
    let sign = if Big_int.sign_big_int n >= 0 then 0l else 1l in

    let n = Big_int.abs_big_int n in

    let limbs =
      (* see MP_DIGIT_BIT *)
      let twoto28 = Big_int.power_int_positive_int 2 28 in
      let rec go n =
        if Big_int.sign_big_int n = 0
        then []
        else
          let (a, b) = Big_int.quomod_big_int n twoto28 in
          [ Big_int.int32_of_big_int b ] @ go a
      in go n
    in
    (* how many 32 bit digits *)
    let size = Int32.of_int (List.length limbs) in

    (* cf. mp_int in tommath.h *)
    let ptr = E.add_static env StaticBytes.[
      I32 Tagged.(int_of_tag BigInt);
      I32 size; (* used *)
      I32 size; (* size; relying on Heap.word_size == size_of(mp_digit) *)
      I32 sign;
      I32 0l; (* dp; this will be patched in BigInt::mp_int_ptr in the RTS when used *)
      i32s limbs

    ] in
    ptr

  let assert_nonneg env =
    Func.share_code1 env "assert_nonneg" ("n", I32Type) [I32Type] (fun env get_n ->
      get_n ^^
      E.call_import env "rts" "bigint_isneg" ^^
      E.then_trap_with env "Natural subtraction underflow" ^^
      get_n
    )

  let compile_abs env = E.call_import env "rts" "bigint_abs"
  let compile_neg env = E.call_import env "rts" "bigint_neg"
  let compile_add env = E.call_import env "rts" "bigint_add"
  let compile_mul env = E.call_import env "rts" "bigint_mul"
  let compile_signed_sub env = E.call_import env "rts" "bigint_sub"
  let compile_signed_div env = E.call_import env "rts" "bigint_div"
  let compile_signed_mod env = E.call_import env "rts" "bigint_rem"
  let compile_unsigned_sub env = E.call_import env "rts" "bigint_sub" ^^ assert_nonneg env
  let compile_unsigned_rem env = E.call_import env "rts" "bigint_rem"
  let compile_unsigned_div env = E.call_import env "rts" "bigint_div"
  let compile_unsigned_pow env = E.call_import env "rts" "bigint_pow"
  let compile_lshd env = E.call_import env "rts" "bigint_lshd"

  let compile_eq env = E.call_import env "rts" "bigint_eq"
  let compile_is_negative env = E.call_import env "rts" "bigint_isneg"
  let compile_relop env = function
      | Lt -> E.call_import env "rts" "bigint_lt"
      | Le -> E.call_import env "rts" "bigint_le"
      | Ge -> E.call_import env "rts" "bigint_ge"
      | Gt -> E.call_import env "rts" "bigint_gt"

  let fits_signed_bits env bits =
    E.call_import env "rts" "bigint_2complement_bits" ^^
    compile_unboxed_const (Int32.of_int bits) ^^
    G.i (Compare (Wasm.Values.I32 I32Op.LeU))
  let fits_unsigned_bits env bits =
    E.call_import env "rts" "bigint_count_bits" ^^
    compile_unboxed_const (Int32.of_int bits) ^^
    G.i (Compare (Wasm.Values.I32 I32Op.LeU))

end (* BigNumLibtommath *)

module BigNum = MakeCompact(BigNumLibtommath)

(* Primitive functions *)
module Prim = struct
  (* The {Nat,Int}{8,16} bits sit in the MSBs of the i32, in this manner
     we can perform almost all operations, with the exception of
     - Mul (needs shr of one operand)
     - Shr (needs masking of result)
     - Rot (needs duplication into LSBs, masking of amount and masking of result)
     - ctz (needs shr of operand or sub from result)

     Both {Nat,Int}{8,16} fit into the vanilla stackrep, so no boxing is necessary.
     This MSB-stored schema is also essentially what the interpreter is using.
  *)
  let prim_word32toNat env = BigNum.from_word32 env
  let prim_shiftWordNtoUnsigned env b =
    compile_shrU_const b ^^
    prim_word32toNat env
  let prim_word32toInt env = BigNum.from_signed_word32 env
  let prim_shiftWordNtoSigned env b =
    compile_shrS_const b ^^
    prim_word32toInt env
  let prim_intToWord32 env = BigNum.truncate_to_word32 env
  let prim_intToWordNShifted env b =
    prim_intToWord32 env ^^
    TaggedSmallWord.shift_leftWordNtoI32 b
end (* Prim *)

module Object = struct
 (* An object with a mutable field1 and immutable field 2 has the following
    heap layout:

    ┌────────┬──────────┬──────────┬─────────┬─────────────┬───┐
    │ Object │ n_fields │ hash_ptr │ ind_ptr │ field2_data │ … │
    └────────┴──────────┴┬─────────┴┬────────┴─────────────┴───┘
         ┌───────────────┘          │
         │   ┌──────────────────────┘
         │   ↓
         │  ╶─┬────────┬─────────────┐
         │    │ ObjInd │ field1_data │
         ↓    └────────┴─────────────┘
        ╶─┬─────────────┬─────────────┬───┐
          │ field1_hash │ field2_hash │ … │
          └─────────────┴─────────────┴───┘


    The field hash array lives in static memory (so no size header needed).
    The hash_ptr is skewed.

    The field2_data for immutable fields is a vanilla word.

    The field1_data for mutable fields are pointers to either an ObjInd, or a
    MutBox (they have the same layout). This indirection is a consequence of
    how we compile object literals with `await` instructions, as these mutable
    fields need to be able to alias local mutable variables.

    We could alternatively switch to an allocate-first approach in the
    await-translation of objects, and get rid of this indirection -- if it were
    not for the implementing of sharing of mutable stable values.
  *)

  let header_size = Int32.add Tagged.header_size 2l

  (* Number of object fields *)
  let size_field = Int32.add Tagged.header_size 0l
  let hash_ptr_field = Int32.add Tagged.header_size 1l

  module FieldEnv = Env.Make(String)

  (* This is for static objects *)
  let vanilla_lit env (fs : (string * int32) list) : int32 =
    let (hashes, ptrs) = fs
      |> List.map (fun (n, ptr) -> (Mo_types.Hash.hash n,ptr))
      |> List.sort compare
      |> List.split
    in

    let hash_ptr = E.add_static env StaticBytes.[ i32s hashes ] in

    E.add_static env StaticBytes.[
      I32 Tagged.(int_of_tag Object);
      I32 (Int32.of_int (List.length fs));
      I32 hash_ptr;
      i32s ptrs;
    ]

  (* This is for non-recursive objects, i.e. ObjNewE *)
  (* The instructions in the field already create the indirection if needed *)
  let lit_raw env (fs : (string * (unit -> G.t)) list ) =
    let name_pos_map =
      fs |>
      (* We could store only public fields in the object, but
         then we need to allocate separate boxes for the non-public ones:
         List.filter (fun (_, vis, f) -> vis.it = Public) |>
      *)
      List.map (fun (n,_) -> (E.hash env n, n)) |>
      List.sort compare |>
      List.mapi (fun i (_h,n) -> (n,Int32.of_int i)) |>
      List.fold_left (fun m (n,i) -> FieldEnv.add n i m) FieldEnv.empty in

    let sz = Int32.of_int (FieldEnv.cardinal name_pos_map) in

    (* Create hash array *)
    let hashes = fs |>
      List.map (fun (n,_) -> E.hash env n) |>
      List.sort compare in
    let hash_ptr = E.add_static env StaticBytes.[ i32s hashes ] in

    (* Allocate memory *)
    let (set_ri, get_ri, ri) = new_local_ env I32Type "obj" in
    Heap.alloc env (Int32.add header_size sz) ^^
    set_ri ^^

    (* Set tag *)
    get_ri ^^
    Tagged.(store Object) ^^

    (* Set size *)
    get_ri ^^
    compile_unboxed_const sz ^^
    Heap.store_field size_field ^^

    (* Set hash_ptr *)
    get_ri ^^
    compile_unboxed_const hash_ptr ^^
    Heap.store_field hash_ptr_field ^^

    (* Write all the fields *)
    let init_field (name, mk_is) : G.t =
      (* Write the pointer to the indirection *)
      get_ri ^^
      mk_is () ^^
      let i = FieldEnv.find name name_pos_map in
      let offset = Int32.add header_size i in
      Heap.store_field offset
    in
    G.concat_map init_field fs ^^

    (* Return the pointer to the object *)
    get_ri


  (* Returns a pointer to the object field (without following the indirection) *)
  let idx_hash_raw env low_bound =
    let name = Printf.sprintf "obj_idx<%d>" low_bound  in
    Func.share_code2 env name (("x", I32Type), ("hash", I32Type)) [I32Type] (fun env get_x get_hash ->
      let set_x = G.setter_for get_x in
      let set_h_ptr, get_h_ptr = new_local env "h_ptr" in

      get_x ^^ Heap.load_field hash_ptr_field ^^

      (* Linearly scan through the fields (binary search can come later) *)
      (* unskew h_ptr and advance both to low bound *)
      compile_add_const Int32.(add ptr_unskew (mul Heap.word_size (of_int low_bound))) ^^
      set_h_ptr ^^
      get_x ^^
      compile_add_const Int32.(mul Heap.word_size (add header_size (of_int low_bound))) ^^
      set_x ^^
      G.loop0 (
          get_h_ptr ^^ load_unskewed_ptr ^^
          get_hash ^^ G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
          G.if0
            (get_x ^^ G.i Return)
            (get_h_ptr ^^ compile_add_const Heap.word_size ^^ set_h_ptr ^^
             get_x ^^ compile_add_const Heap.word_size ^^ set_x ^^
             G.i (Br (nr 1l)))
        ) ^^
      G.i Unreachable
    )

  (* Returns a pointer to the object field (possibly following the indirection) *)
  let idx_hash env low_bound indirect =
    if indirect
    then
      let name = Printf.sprintf "obj_idx_ind<%d>" low_bound in
      Func.share_code2 env name (("x", I32Type), ("hash", I32Type)) [I32Type] (fun env get_x get_hash ->
      get_x ^^ get_hash ^^
      idx_hash_raw env low_bound ^^
      load_ptr ^^ compile_add_const (Int32.mul MutBox.field Heap.word_size)
    )
    else idx_hash_raw env low_bound

  (* Determines whether the field is mutable (and thus needs an indirection) *)
  let is_mut_field env obj_type s =
    let _, fields = Type.as_obj_sub [s] obj_type in
    Type.is_mut (Type.lookup_val_field s fields)

  (* Computes a lower bound for the positional index of a field in an object *)
  let field_lower_bound env obj_type s =
    let open Type in
    let _, fields = as_obj_sub [s] obj_type in
    List.iter (function {typ = Typ _; _} -> assert false | _ -> ()) fields;
    let sorted_by_hash =
      List.sort
        (fun (h1, _) (h2, _) -> Lib.Uint32.compare h1 h2)
        (List.map (fun f -> Lib.Uint32.of_int32 (E.hash env f.lab), f) fields) in
    match Lib.List.index_of s (List.map (fun (_, {lab; _}) -> lab) sorted_by_hash) with
    | Some i -> i
    | _ -> assert false

  (* Returns a pointer to the object field (without following the indirection) *)
  let idx_raw env f =
    compile_unboxed_const (E.hash env f) ^^
    idx_hash_raw env 0

  (* Returns a pointer to the object field (possibly following the indirection) *)
  let idx env obj_type f =
    compile_unboxed_const (E.hash env f) ^^
    idx_hash env (field_lower_bound env obj_type f) (is_mut_field env obj_type f)

  (* load the value (or the mutbox) *)
  let load_idx_raw env f =
    idx_raw env f ^^
    load_ptr

  (* load the actual value (dereferencing the mutbox) *)
  let load_idx env obj_type f =
    idx env obj_type f ^^
    load_ptr

end (* Object *)

module Blob = struct
  (* The layout of a blob object is

     ┌─────┬─────────┬──────────────────┐
     │ tag │ n_bytes │ bytes (padded) … │
     └─────┴─────────┴──────────────────┘

    This heap object is used for various kinds of binary, non-pointer data.

    When used for Text values, the bytes are UTF-8 encoded code points from
    Unicode.
  *)

  let header_size = Int32.add Tagged.header_size 1l
  let len_field = Int32.add Tagged.header_size 0l

  let len env = Heap.load_field len_field

  let vanilla_lit env s =
    E.add_static env StaticBytes.[
      I32 Tagged.(int_of_tag Blob);
      I32 (Int32.of_int (String.length s));
      Bytes s;
    ]

  let lit env s = compile_unboxed_const (vanilla_lit env s)

  let lit_ptr_len env s =
    compile_unboxed_const (Int32.add ptr_unskew (E.add_static env StaticBytes.[Bytes s])) ^^
    compile_unboxed_const (Int32.of_int (String.length s))

  let alloc env = E.call_import env "rts" "alloc_blob"

  let unskewed_payload_offset = Int32.(add ptr_unskew (mul Heap.word_size header_size))
  let payload_ptr_unskewed = compile_add_const unskewed_payload_offset

  let as_ptr_len env = Func.share_code1 env "as_ptr_size" ("x", I32Type) [I32Type; I32Type] (
    fun env get_x ->
      get_x ^^ payload_ptr_unskewed ^^
      get_x ^^ len env
    )

  let of_ptr_size env = Func.share_code2 env "blob_of_ptr_size" (("ptr", I32Type), ("size" , I32Type)) [I32Type] (
    fun env get_ptr get_size ->
      let (set_x, get_x) = new_local env "x" in
      get_size ^^ alloc env ^^ set_x ^^
      get_x ^^ payload_ptr_unskewed ^^
      get_ptr ^^
      get_size ^^
      Heap.memcpy env ^^
      get_x
    )

  let of_size_copy env get_size_fun copy_fun offset_fun =
    let (set_len, get_len) = new_local env "len" in
    let (set_blob, get_blob) = new_local env "blob" in
    get_size_fun env ^^ set_len ^^

    get_len ^^ alloc env ^^ set_blob ^^
    get_blob ^^ payload_ptr_unskewed ^^
    offset_fun env ^^
    get_len ^^
    copy_fun env ^^

    get_blob

  (* Lexicographic blob comparison. Expects two blobs on the stack *)
  let rec compare env op =
    let open Operator in
    let name = match op with
        | LtOp -> "Blob.compare_lt"
        | LeOp -> "Blob.compare_le"
        | GeOp -> "Blob.compare_ge"
        | GtOp -> "Blob.compare_gt"
        | EqOp -> "Blob.compare_eq"
        | NeqOp -> assert false in
    Func.share_code2 env name (("x", I32Type), ("y", I32Type)) [I32Type] (fun env get_x get_y ->
      match op with
        (* Some operators can be reduced to the negation of other operators *)
        | LtOp ->  get_x ^^ get_y ^^ compare env GeOp ^^ Bool.neg
        | GtOp ->  get_x ^^ get_y ^^ compare env LeOp ^^ Bool.neg
        | NeqOp -> assert false
        | _ ->
      begin
        let (set_len1, get_len1) = new_local env "len1" in
        let (set_len2, get_len2) = new_local env "len2" in
        let (set_len, get_len) = new_local env "len" in
        let (set_a, get_a) = new_local env "a" in
        let (set_b, get_b) = new_local env "b" in

        get_x ^^ len env ^^ set_len1 ^^
        get_y ^^ len env ^^ set_len2 ^^

        (* Find mininum length *)
        begin if op = EqOp then
          (* Early exit for equality *)
          get_len1 ^^ get_len2 ^^ G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
          G.if0 G.nop (Bool.lit false ^^ G.i Return) ^^

          get_len1 ^^ set_len
        else
          get_len1 ^^ get_len2 ^^ G.i (Compare (Wasm.Values.I32 I32Op.LeU)) ^^
          G.if0
            (get_len1 ^^ set_len)
            (get_len2 ^^ set_len)
        end ^^

        (* We could do word-wise comparisons if we know that the trailing bytes
           are zeroed *)
        get_len ^^
        from_0_to_n env (fun get_i ->
          get_x ^^
          payload_ptr_unskewed ^^
          get_i ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.(Pack8, ZX)}) ^^
          set_a ^^


          get_y ^^
          payload_ptr_unskewed ^^
          get_i ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.(Pack8, ZX)}) ^^
          set_b ^^

          get_a ^^ get_b ^^ G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
          G.if0 G.nop (
            (* first non-equal elements *)
            begin match op with
            | LeOp -> get_a ^^ get_b ^^ G.i (Compare (Wasm.Values.I32 I32Op.LeU))
            | GeOp -> get_a ^^ get_b ^^ G.i (Compare (Wasm.Values.I32 I32Op.GeU))
            | EqOp -> Bool.lit false
            |_ -> assert false
            end ^^
            G.i Return
          )
        ) ^^
        (* Common prefix is same *)
        match op with
        | LeOp -> get_len1 ^^ get_len2 ^^ G.i (Compare (Wasm.Values.I32 I32Op.LeU))
        | GeOp -> get_len1 ^^ get_len2 ^^ G.i (Compare (Wasm.Values.I32 I32Op.GeU))
        | EqOp -> Bool.lit true
        |_ -> assert false
      end
  )

  let iter env =
    E.call_import env "rts" "blob_iter"
  let iter_done env =
    E.call_import env "rts" "blob_iter_done"
  let iter_next env =
    E.call_import env "rts" "blob_iter_next" ^^
    TaggedSmallWord.msb_adjust Type.Nat8

  let dyn_alloc_scratch env = alloc env ^^ payload_ptr_unskewed

  (* TODO: rewrite using MemoryFill *)
  let clear env =
    Func.share_code1 env "blob_clear" ("x", I32Type) [] (fun env get_x ->
      let (set_ptr, get_ptr) = new_local env "ptr" in
      let (set_len, get_len) = new_local env "len" in
      get_x ^^
      as_ptr_len env ^^
      set_len ^^
      set_ptr ^^

      (* round to word size *)
      get_len ^^
      compile_add_const (Int32.sub Heap.word_size 1l) ^^
      compile_divU_const Heap.word_size ^^

      (* clear all words *)
      from_0_to_n env (fun get_i ->
        get_ptr ^^
        compile_unboxed_const 0l ^^
        store_unskewed_ptr ^^
        get_ptr ^^
        compile_add_const Heap.word_size ^^
        set_ptr))

end (* Blob *)

module Text = struct
  (*
  Most of the heavy lifting around text values is in rts/motoko-rts/src/text.rs
  *)

  (* The layout of a concatenation node is

     ┌─────┬─────────┬───────┬───────┐
     │ tag │ n_bytes │ text1 │ text2 │
     └─────┴─────────┴───────┴───────┘

    This is internal to rts/text.c, with the exception of GC-related code.
  *)

  let of_ptr_size env =
    E.call_import env "rts" "text_of_ptr_size"
  let concat env =
    E.call_import env "rts" "text_concat"
  let size env =
    E.call_import env "rts" "text_size"
  let to_buf env =
    E.call_import env "rts" "text_to_buf"
  let len env =
    E.call_import env "rts" "text_len" ^^ BigNum.from_word32 env
  let prim_showChar env =
    TaggedSmallWord.untag_codepoint ^^
    E.call_import env "rts" "text_singleton"
  let to_blob env = E.call_import env "rts" "blob_of_text"

  let of_blob env =
    let (set_blob, get_blob) = new_local env "blob" in
    set_blob ^^
    get_blob ^^ Blob.as_ptr_len env ^^
    E.call_import env "rts" "utf8_valid" ^^
    G.if1 I32Type (Opt.inject_noop env get_blob) (Opt.null_lit env)


  let iter env =
    E.call_import env "rts" "text_iter"
  let iter_done env =
    E.call_import env "rts" "text_iter_done"
  let iter_next env =
    E.call_import env "rts" "text_iter_next" ^^
    TaggedSmallWord.tag_codepoint

  let compare env op =
    let open Operator in
    let name = match op with
        | LtOp -> "Text.compare_lt"
        | LeOp -> "Text.compare_le"
        | GeOp -> "Text.compare_ge"
        | GtOp -> "Text.compare_gt"
        | EqOp -> "Text.compare_eq"
        | NeqOp -> assert false in
    Func.share_code2 env name (("x", I32Type), ("y", I32Type)) [I32Type] (fun env get_x get_y ->
      get_x ^^ get_y ^^ E.call_import env "rts" "text_compare" ^^
      compile_unboxed_const 0l ^^
      match op with
        | LtOp -> G.i (Compare (Wasm.Values.I32 I32Op.LtS))
        | LeOp -> G.i (Compare (Wasm.Values.I32 I32Op.LeS))
        | GtOp -> G.i (Compare (Wasm.Values.I32 I32Op.GtS))
        | GeOp -> G.i (Compare (Wasm.Values.I32 I32Op.GeS))
        | EqOp -> G.i (Compare (Wasm.Values.I32 I32Op.Eq))
        | NeqOp -> assert false
    )


end (* Text *)

module Arr = struct
  (* Object layout:

     ┌─────┬──────────┬────────┬───┐
     │ tag │ n_fields │ field1 │ … │
     └─────┴──────────┴────────┴───┘

     No difference between mutable and immutable arrays.
  *)

  let header_size = Int32.add Tagged.header_size 1l
  let element_size = 4l
  let len_field = Int32.add Tagged.header_size 0l

  (* Static array access. No checking *)
  let load_field n = Heap.load_field Int32.(add n header_size)

  (* Dynamic array access. Returns the address (not the value) of the field.
     Does bounds checking *)
  let idx env =
    Func.share_code2 env "Array.idx" (("array", I32Type), ("idx", I32Type)) [I32Type] (fun env get_array get_idx ->
      (* No need to check the lower bound, we interpret idx as unsigned *)
      (* Check the upper bound *)
      get_idx ^^
      get_array ^^ Heap.load_field len_field ^^
      G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
      E.else_trap_with env "Array index out of bounds" ^^

      get_idx ^^
      compile_add_const header_size ^^
      compile_mul_const element_size ^^
      get_array ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add))
    )

  (* As above, but taking a bigint (Nat), and reporting overflow as out of bounds *)
  let idx_bigint env =
    Func.share_code2 env "Array.idx_bigint" (("array", I32Type), ("idx", I32Type)) [I32Type] (fun env get_array get_idx ->
      get_array ^^
      get_idx ^^
      Blob.lit env "Array index out of bounds" ^^
      BigNum.to_word32_with env ^^
      idx env
  )


  let vanilla_lit env ptrs =
    E.add_static env StaticBytes.[
      I32 Tagged.(int_of_tag Array);
      I32 (Int32.of_int (List.length ptrs));
      i32s ptrs;
    ]

  (* Compile an array literal. *)
  let lit env element_instructions =
    Tagged.obj env Tagged.Array
     ([ compile_unboxed_const (Wasm.I32.of_int_u (List.length element_instructions))
      ] @ element_instructions)

  (* Does not initialize the fields! *)
  let alloc env = E.call_import env "rts" "alloc_array"

  (* The primitive operations *)
  (* No need to wrap them in RTS functions: They occur only once, in the prelude. *)
  let init env =
    let (set_len, get_len) = new_local env "len" in
    let (set_x, get_x) = new_local env "x" in
    let (set_r, get_r) = new_local env "r" in
    set_x ^^
    BigNum.to_word32 env ^^
    set_len ^^

    (* Allocate *)
    get_len ^^
    alloc env ^^
    set_r ^^

    (* Write fields *)
    get_len ^^
    from_0_to_n env (fun get_i ->
      get_r ^^
      get_i ^^
      idx env ^^
      get_x ^^
      store_ptr
    ) ^^
    get_r

  let tabulate env =
    let (set_len, get_len) = new_local env "len" in
    let (set_f, get_f) = new_local env "f" in
    let (set_r, get_r) = new_local env "r" in
    set_f ^^
    BigNum.to_word32 env ^^
    set_len ^^

    (* Allocate *)
    get_len ^^
    alloc env ^^
    set_r ^^

    (* Write fields *)
    get_len ^^
    from_0_to_n env (fun get_i ->
      (* Where to store *)
      get_r ^^ get_i ^^ idx env ^^
      (* The closure *)
      get_f ^^
      (* The arg *)
      get_i ^^
      BigNum.from_word32 env ^^
      (* The closure again *)
      get_f ^^
      (* Call *)
      Closure.call_closure env 1 1 ^^
      store_ptr
    ) ^^
    get_r

  let ofBlob env =
    Func.share_code1 env "Arr.ofBlob" ("blob", I32Type) [I32Type] (fun env get_blob ->
      let (set_len, get_len) = new_local env "len" in
      let (set_r, get_r) = new_local env "r" in

      get_blob ^^ Blob.len env ^^ set_len ^^

      get_len ^^ alloc env ^^ set_r ^^

      get_len ^^ from_0_to_n env (fun get_i ->
        get_r ^^ get_i ^^ idx env ^^
        get_blob ^^ Blob.payload_ptr_unskewed ^^
        get_i ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.(Pack8, ZX)}) ^^
        TaggedSmallWord.msb_adjust Type.Nat8 ^^
        store_ptr
      ) ^^

      get_r
    )

  let toBlob env =
    Func.share_code1 env "Arr.toBlob" ("array", I32Type) [I32Type] (fun env get_a ->
      let (set_len, get_len) = new_local env "len" in
      let (set_r, get_r) = new_local env "r" in

      get_a ^^ Heap.load_field len_field ^^ set_len ^^

      get_len ^^ Blob.alloc env ^^ set_r ^^

      get_len ^^ from_0_to_n env (fun get_i ->
        get_r ^^ Blob.payload_ptr_unskewed ^^
        get_i ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        get_a ^^ get_i ^^ idx env ^^
        load_ptr ^^
        TaggedSmallWord.lsb_adjust Type.Nat8 ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.Pack8})
      ) ^^

      get_r
    )

end (* Array *)

module Tuple = struct
  (* Tuples use the same object representation (and same tag) as arrays.
     Even though we know the size statically, we still need the size
     information for the GC.

     One could introduce tags for small tuples, to save one word.
  *)

  (* We represent the boxed empty tuple as the unboxed scalar 0, i.e. simply as
     number (but really anything is fine, we never look at this) *)
  let unit_vanilla_lit = 0l
  let compile_unit = compile_unboxed_const unit_vanilla_lit

  (* Expects on the stack the pointer to the array. *)
  let load_n n = Heap.load_field (Int32.add Arr.header_size n)

  (* Takes n elements of the stack and produces an argument tuple *)
  let from_stack env n =
    if n = 0 then compile_unit
    else
      let name = Printf.sprintf "to_%i_tuple" n in
      let args = Lib.List.table n (fun i -> Printf.sprintf "arg%i" i, I32Type) in
      Func.share_code env name args [I32Type] (fun env ->
        Arr.lit env (Lib.List.table n (fun i -> G.i (LocalGet (nr (Int32.of_int i)))))
      )

  (* Takes an argument tuple and puts the elements on the stack: *)
  let to_stack env n =
    if n = 0 then G.i Drop else
    begin
      let name = Printf.sprintf "from_%i_tuple" n in
      let retty = Lib.List.make n I32Type in
      Func.share_code1 env name ("tup", I32Type) retty (fun env get_tup ->
        G.table n (fun i -> get_tup ^^ load_n (Int32.of_int i))
      )
    end

end (* Tuple *)

module Lifecycle = struct
  (*
  This module models the life cycle of a canister as a very simple state machine,
  keeps track of the current state of the canister, and traps noisily if an
  unexpected transition happens. Such a transition would either be a bug in the
  underlying system, or in our RTS.
  *)

  type state =
    | PreInit
  (* We do not use the (start) function when compiling canisters, so skip
     these two:
    | InStart
    | Started (* (start) has run *)
  *)
    | InInit (* canister_init *)
    | Idle (* basic steady state *)
    | InUpdate
    | InQuery
    | PostQuery (* an invalid state *)
    | InPreUpgrade
    | PostPreUpgrade (* an invalid state *)
    | InPostUpgrade

  let string_of_state state = match state with
    | PreInit -> "PreInit"
    | InInit -> "InInit"
    | Idle -> "Idle"
    | InUpdate -> "InUpdate"
    | InQuery -> "InQuery"
    | PostQuery -> "PostQuery"
    | InPreUpgrade -> "InPreUpgrade"
    | PostPreUpgrade -> "PostPreUpgrade"
    | InPostUpgrade -> "InPostUpgrade"

  let int_of_state = function
    | PreInit -> 0l (* Automatically null *)
    (*
    | InStart -> 1l
    | Started -> 2l
    *)
    | InInit -> 3l
    | Idle -> 4l
    | InUpdate -> 5l
    | InQuery -> 6l
    | PostQuery -> 7l
    | InPreUpgrade -> 8l
    | PostPreUpgrade -> 9l
    | InPostUpgrade -> 10l

  let ptr = Stack.end_
  let end_ = Int32.add Stack.end_ Heap.word_size

  (* Which states may come before this *)
  let pre_states = function
    | PreInit -> []
    (*
    | InStart -> [PreInit]
    | Started -> [InStart]
    *)
    | InInit -> [PreInit]
    | Idle -> [InInit; InUpdate; InPostUpgrade]
    | InUpdate -> [Idle]
    | InQuery -> [Idle]
    | PostQuery -> [InQuery]
    | InPreUpgrade -> [Idle]
    | PostPreUpgrade -> [InPreUpgrade]
    | InPostUpgrade -> [InInit]

  let get env =
    compile_unboxed_const ptr ^^
    load_unskewed_ptr

  let set env new_state =
    compile_unboxed_const ptr ^^
    compile_unboxed_const (int_of_state new_state) ^^
    store_unskewed_ptr

  let trans env new_state =
    let name = "trans_state" ^ Int32.to_string (int_of_state new_state) in
    Func.share_code0 env name [] (fun env ->
      G.block0 (
        let rec go = function
        | [] -> E.trap_with env
          ("internal error: unexpected state entering " ^ string_of_state new_state)
        | (s::ss) ->
          get env ^^ compile_eq_const (int_of_state s) ^^
          G.if0 (G.i (Br (nr 1l))) G.nop ^^
          go ss
        in go (pre_states new_state)
        ) ^^
      set env new_state
    )

end (* Lifecycle *)


module IC = struct
  (* IC-specific stuff: System imports, databufs etc. *)

  let i32s n = Lib.List.make n I32Type
  let i64s n = Lib.List.make n I64Type

  let import_ic0 env =
      E.add_func_import env "ic0" "call_data_append" (i32s 2) [];
      E.add_func_import env "ic0" "call_cycles_add128" (i64s 2) [];
      E.add_func_import env "ic0" "call_new" (i32s 8) [];
      E.add_func_import env "ic0" "call_perform" [] [I32Type];
      E.add_func_import env "ic0" "call_on_cleanup" (i32s 2) [];
      E.add_func_import env "ic0" "canister_cycle_balance128" [I32Type] [];
      E.add_func_import env "ic0" "canister_self_copy" (i32s 3) [];
      E.add_func_import env "ic0" "canister_self_size" [] [I32Type];
      E.add_func_import env "ic0" "canister_status" [] [I32Type];
      E.add_func_import env "ic0" "debug_print" (i32s 2) [];
      E.add_func_import env "ic0" "msg_arg_data_copy" (i32s 3) [];
      E.add_func_import env "ic0" "msg_arg_data_size" [] [I32Type];
      E.add_func_import env "ic0" "msg_caller_copy" (i32s 3) [];
      E.add_func_import env "ic0" "msg_caller_size" [] [I32Type];
      E.add_func_import env "ic0" "msg_cycles_available128" [I32Type] [];
      E.add_func_import env "ic0" "msg_cycles_refunded128" [I32Type] [];
      E.add_func_import env "ic0" "msg_cycles_accept128" [I64Type; I64Type; I32Type] [];
      E.add_func_import env "ic0" "certified_data_set" (i32s 2) [];
      E.add_func_import env "ic0" "data_certificate_present" [] [I32Type];
      E.add_func_import env "ic0" "data_certificate_size" [] [I32Type];
      E.add_func_import env "ic0" "data_certificate_copy" (i32s 3) [];
      E.add_func_import env "ic0" "msg_reject_code" [] [I32Type];
      E.add_func_import env "ic0" "msg_reject_msg_size" [] [I32Type];
      E.add_func_import env "ic0" "msg_reject_msg_copy" (i32s 3) [];
      E.add_func_import env "ic0" "msg_reject" (i32s 2) [];
      E.add_func_import env "ic0" "msg_reply_data_append" (i32s 2) [];
      E.add_func_import env "ic0" "msg_reply" [] [];
      E.add_func_import env "ic0" "trap" (i32s 2) [];
      E.add_func_import env "ic0" "stable64_write" (i64s 3) [];
      E.add_func_import env "ic0" "stable64_read" (i64s 3) [];
      E.add_func_import env "ic0" "stable64_size" [] [I64Type];
      E.add_func_import env "ic0" "stable64_grow" [I64Type] [I64Type];
      E.add_func_import env "ic0" "time" [] [I64Type];
      ()

  let system_imports env =
    match E.mode env with
    | Flags.ICMode ->
      import_ic0 env
    | Flags.RefMode  ->
      import_ic0 env
    | Flags.WASIMode ->
      E.add_func_import env "wasi_unstable" "fd_write" [I32Type; I32Type; I32Type; I32Type] [I32Type];
    | Flags.WasmMode -> ()

  let system_call env modname funcname = E.call_import env modname funcname

  let register env =

      Func.define_built_in env "print_ptr" [("ptr", I32Type); ("len", I32Type)] [] (fun env ->
        match E.mode env with
        | Flags.WasmMode -> G.i Nop
        | Flags.ICMode | Flags.RefMode ->
            G.i (LocalGet (nr 0l)) ^^
            G.i (LocalGet (nr 1l)) ^^
            E.call_import env "ic0" "debug_print"
        | Flags.WASIMode -> begin
          let get_ptr = G.i (LocalGet (nr 0l)) in
          let get_len = G.i (LocalGet (nr 1l)) in

          Stack.with_words env "io_vec" 6l (fun get_iovec_ptr ->
            (* We use the iovec functionality to append a newline *)
            get_iovec_ptr ^^
            get_ptr ^^
            G.i (Store {ty = I32Type; align = 2; offset = 0l; sz = None}) ^^

            get_iovec_ptr ^^
            get_len ^^
            G.i (Store {ty = I32Type; align = 2; offset = 4l; sz = None}) ^^

            get_iovec_ptr ^^
            get_iovec_ptr ^^ compile_add_const 16l ^^
            G.i (Store {ty = I32Type; align = 2; offset = 8l; sz = None}) ^^

            get_iovec_ptr ^^
            compile_unboxed_const 1l ^^
            G.i (Store {ty = I32Type; align = 2; offset = 12l; sz = None}) ^^

            get_iovec_ptr ^^
            compile_unboxed_const (Int32.of_int (Char.code '\n')) ^^
            G.i (Store {ty = I32Type; align = 0; offset = 16l; sz = Some Wasm.Types.Pack8}) ^^

            (* Call fd_write twice to work around
               https://github.com/bytecodealliance/wasmtime/issues/629
            *)

            compile_unboxed_const 1l (* stdout *) ^^
            get_iovec_ptr ^^
            compile_unboxed_const 1l (* one string segment (2 doesn't work) *) ^^
            get_iovec_ptr ^^ compile_add_const 20l ^^ (* out for bytes written, we ignore that *)
            E.call_import env "wasi_unstable" "fd_write" ^^
            G.i Drop ^^

            compile_unboxed_const 1l (* stdout *) ^^
            get_iovec_ptr ^^ compile_add_const 8l ^^
            compile_unboxed_const 1l (* one string segment *) ^^
            get_iovec_ptr ^^ compile_add_const 20l ^^ (* out for bytes written, we ignore that *)
            E.call_import env "wasi_unstable" "fd_write" ^^
            G.i Drop)
          end);

      E.add_export env (nr {
        name = Wasm.Utf8.decode "print_ptr";
        edesc = nr (FuncExport (nr (E.built_in env "print_ptr")))
      })

  let print_ptr_len env = G.i (Call (nr (E.built_in env "print_ptr")))

  let print_text env =
    Func.share_code1 env "print_text" ("str", I32Type) [] (fun env get_str ->
      let (set_blob, get_blob) = new_local env "blob" in
      get_str ^^ Text.to_blob env ^^ set_blob ^^
      get_blob ^^ Blob.payload_ptr_unskewed ^^
      get_blob ^^ Blob.len env ^^
      print_ptr_len env
    )

  (* For debugging *)
  let _compile_static_print env s =
    Blob.lit_ptr_len env s ^^ print_ptr_len env

  let ic_trap env = system_call env "ic0" "trap"

  let trap_ptr_len env =
    match E.mode env with
    | Flags.WasmMode -> G.i Unreachable
    | Flags.WASIMode -> print_ptr_len env ^^ G.i Unreachable
    | Flags.ICMode | Flags.RefMode -> ic_trap env ^^ G.i Unreachable

  let trap_with env s =
    Blob.lit_ptr_len env s ^^ trap_ptr_len env

  let trap_text env  =
    Text.to_blob env ^^ Blob.as_ptr_len env ^^ trap_ptr_len env

  let default_exports env =
    (* these exports seem to be wanted by the hypervisor/v8 *)
    E.add_export env (nr {
      name = Wasm.Utf8.decode (
        match E.mode env with
        | Flags.WASIMode -> "memory"
        | _  -> "mem"
      );
      edesc = nr (MemoryExport (nr 0l))
    });
    E.add_export env (nr {
      name = Wasm.Utf8.decode "table";
      edesc = nr (TableExport (nr 0l))
    })

  let export_init env =
    assert (E.mode env = Flags.ICMode || E.mode env = Flags.RefMode);
    let empty_f = Func.of_body env [] [] (fun env ->
      Lifecycle.trans env Lifecycle.InInit ^^

      G.i (Call (nr (E.built_in env "init"))) ^^
      E.collect_garbage env ^^

      Lifecycle.trans env Lifecycle.Idle
    ) in
    let fi = E.add_fun env "canister_init" empty_f in
    E.add_export env (nr {
      name = Wasm.Utf8.decode "canister_init";
      edesc = nr (FuncExport (nr fi))
      })

  let export_heartbeat env =
    assert (E.mode env = Flags.ICMode || E.mode env = Flags.RefMode);
    let fi = E.add_fun env "canister_heartbeat"
      (Func.of_body env [] [] (fun env ->
        G.i (Call (nr (E.built_in env "heartbeat_exp"))) ^^
        E.collect_garbage env))
    in
    E.add_export env (nr {
      name = Wasm.Utf8.decode "canister_heartbeat";
      edesc = nr (FuncExport (nr fi))
    })

  let export_wasi_start env =
    assert (E.mode env = Flags.WASIMode);
    let fi = E.add_fun env "_start" (Func.of_body env [] [] (fun env1 ->
      Lifecycle.trans env Lifecycle.InInit ^^
      G.i (Call (nr (E.built_in env "init"))) ^^
      Lifecycle.trans env Lifecycle.Idle
    )) in
    E.add_export env (nr {
      name = Wasm.Utf8.decode "_start";
      edesc = nr (FuncExport (nr fi))
      })

  let export_upgrade_methods env =
    if E.mode env = Flags.ICMode || E.mode env = Flags.RefMode then
    let status_stopped = 3l in
    let pre_upgrade_fi = E.add_fun env "pre_upgrade" (Func.of_body env [] [] (fun env ->
      Lifecycle.trans env Lifecycle.InPreUpgrade ^^
      (* check status is stopped or trap on outstanding callbacks *)
      system_call env "ic0" "canister_status" ^^ compile_eq_const status_stopped ^^
      G.if0
       (G.nop)
       (ContinuationTable.count env ^^
          E.then_trap_with env "canister_pre_upgrade attempted with outstanding message callbacks (try stopping the canister before upgrade)") ^^
      (* call pre_upgrade expression & any system method *)
      (G.i (Call (nr (E.built_in env "pre_exp")))) ^^
      Lifecycle.trans env Lifecycle.PostPreUpgrade
    )) in

    let post_upgrade_fi = E.add_fun env "post_upgrade" (Func.of_body env [] [] (fun env ->
      Lifecycle.trans env Lifecycle.InInit ^^
      G.i (Call (nr (E.built_in env "init"))) ^^
      Lifecycle.trans env Lifecycle.InPostUpgrade ^^
      G.i (Call (nr (E.built_in env "post_exp"))) ^^
      Lifecycle.trans env Lifecycle.Idle ^^
      E.collect_garbage env
    )) in

    E.add_export env (nr {
      name = Wasm.Utf8.decode "canister_pre_upgrade";
      edesc = nr (FuncExport (nr pre_upgrade_fi))
    });

    E.add_export env (nr {
      name = Wasm.Utf8.decode "canister_post_upgrade";
      edesc = nr (FuncExport (nr post_upgrade_fi))
    })


  let get_self_reference env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code0 env "canister_self" [I32Type] (fun env ->
        Blob.of_size_copy env
          (fun env -> system_call env "ic0" "canister_self_size")
          (fun env -> system_call env "ic0" "canister_self_copy")
          (fun env -> compile_unboxed_const 0l)
      )
    | _ ->
      E.trap_with env "cannot get self-actor-reference when running locally"

  let get_system_time env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      system_call env "ic0" "time"
    | _ ->
      E.trap_with env "cannot get system time when running locally"

  let caller env =
    SR.Vanilla,
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Blob.of_size_copy env
        (fun env -> system_call env "ic0" "msg_caller_size")
        (fun env -> system_call env "ic0" "msg_caller_copy")
        (fun env -> compile_unboxed_const 0l)
    | _ ->
      E.trap_with env (Printf.sprintf "cannot get caller  when running locally")

  let reject env arg_instrs =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      arg_instrs ^^
      Text.to_blob env ^^
      Blob.as_ptr_len env ^^
      system_call env "ic0" "msg_reject"
    | _ ->
      E.trap_with env (Printf.sprintf "cannot reject when running locally")

  let error_code env =
     Func.share_code0 env "error_code" [I32Type] (fun env ->
      let (set_code, get_code) = new_local env "code" in
      system_call env "ic0" "msg_reject_code" ^^ set_code ^^
      List.fold_right (fun (tag, const) code ->
        get_code ^^ compile_unboxed_const const ^^
        G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
        G.if1 I32Type
          (Variant.inject env tag Tuple.compile_unit)
          code)
        ["system_fatal", 1l;
         "system_transient", 2l;
         "destination_invalid", 3l;
         "canister_reject", 4l;
         "canister_error", 5l]
        (Variant.inject env "future" (get_code ^^ BoxedSmallWord.box env)))
  let error_message env =
    Func.share_code0 env "error_message" [I32Type] (fun env ->
      Blob.of_size_copy env
        (fun env -> system_call env "ic0" "msg_reject_msg_size")
        (fun env -> system_call env "ic0" "msg_reject_msg_copy")
        (fun env -> compile_unboxed_const 0l)
    )

  let error_value env =
    Func.share_code0 env "error_value" [I32Type] (fun env ->
      error_code env ^^
      error_message env ^^
      Tuple.from_stack env 2
    )

  let reply_with_data env =
    Func.share_code2 env "reply_with_data" (("start", I32Type), ("size", I32Type)) [] (
      fun env get_data_start get_data_size ->
        get_data_start ^^
        get_data_size ^^
        system_call env "ic0" "msg_reply_data_append" ^^
        system_call env "ic0" "msg_reply"
   )

  (* Actor reference on the stack *)
  let actor_public_field env name =
    (* simply tuple canister name and function name *)
    Blob.lit env name ^^
    Tuple.from_stack env 2

  let fail_assert env at =
    let open Source in
    let at = {
        left = {at.left with file = Filename.basename at.left.file};
        right = {at.right with file = Filename.basename at.right.file}
      }
    in
    E.trap_with env (Printf.sprintf "assertion failed at %s" (string_of_region at))

  let async_method_name = "__motoko_async_helper"

  let assert_caller_self env =
    let (set_len1, get_len1) = new_local env "len1" in
    let (set_len2, get_len2) = new_local env "len2" in
    let (set_str1, get_str1) = new_local env "str1" in
    let (set_str2, get_str2) = new_local env "str2" in
    system_call env "ic0" "canister_self_size" ^^ set_len1 ^^
    system_call env "ic0" "msg_caller_size" ^^ set_len2 ^^
    get_len1 ^^ get_len2 ^^ G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
    E.else_trap_with env "not a self-call" ^^

    get_len1 ^^ Blob.dyn_alloc_scratch env ^^ set_str1 ^^
    get_str1 ^^ compile_unboxed_const 0l ^^ get_len1 ^^
    system_call env "ic0" "canister_self_copy" ^^

    get_len2 ^^ Blob.dyn_alloc_scratch env ^^ set_str2 ^^
    get_str2 ^^ compile_unboxed_const 0l ^^ get_len2 ^^
    system_call env "ic0" "msg_caller_copy" ^^


    get_str1 ^^ get_str2 ^^ get_len1 ^^ Heap.memcmp env ^^
    compile_unboxed_const 0l ^^ G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
    E.else_trap_with env "not a self-call"

  (* Cycles *)

  let cycle_balance env =
    match E.mode env with
    | Flags.ICMode
    | Flags.RefMode ->
      system_call env "ic0" "canister_cycle_balance128"
    | _ ->
      E.trap_with env "cannot read balance when running locally"

  let cycles_add env =
    match E.mode env with
    | Flags.ICMode
    | Flags.RefMode ->
      system_call env "ic0" "call_cycles_add128"
    | _ ->
      E.trap_with env "cannot accept cycles when running locally"

  let cycles_accept env =
    match E.mode env with
    | Flags.ICMode
    | Flags.RefMode ->
      system_call env "ic0" "msg_cycles_accept128"
    | _ ->
      E.trap_with env "cannot accept cycles when running locally"

  let cycles_available env =
    match E.mode env with
    | Flags.ICMode
    | Flags.RefMode ->
      system_call env "ic0" "msg_cycles_available128"
    | _ ->
      E.trap_with env "cannot get cycles available when running locally"

  let cycles_refunded env =
    match E.mode env with
    | Flags.ICMode
    | Flags.RefMode ->
      system_call env "ic0" "msg_cycles_refunded128"
    | _ ->
      E.trap_with env "cannot get cycles refunded when running locally"

  let set_certified_data env =
    match E.mode env with
    | Flags.ICMode
    | Flags.RefMode ->
      Blob.as_ptr_len env ^^
      system_call env "ic0" "certified_data_set"
    | _ ->
      E.trap_with env "cannot set certified data when running locally"

  let get_certificate env =
    match E.mode env with
    | Flags.ICMode
    | Flags.RefMode ->
      system_call env "ic0" "data_certificate_present" ^^
      G.if1 I32Type
      begin
        Opt.inject_noop env (
          Blob.of_size_copy env
            (fun env -> system_call env "ic0" "data_certificate_size")
            (fun env -> system_call env "ic0" "data_certificate_copy")
            (fun env -> compile_unboxed_const 0l)
        )
      end (Opt.null_lit env)
    | _ ->
      E.trap_with env "cannot get certificate when running locally"

end (* IC *)



module StableMem = struct

  (* start from 1 to avoid accidental reads of 0 *)
  let version = Int32.of_int 1

  let register_globals env =
    (* size (in pages) *)
    E.add_global64 env "__stablemem_size" Mutable 0L

  let get_mem_size env =
    G.i (GlobalGet (nr (E.get_global env "__stablemem_size")))
  let set_mem_size env =
    G.i (GlobalSet (nr (E.get_global env "__stablemem_size")))

  (* stable memory bounds check *)
  let guard env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code1 env "__stablemem_guard"
        ("offset", I64Type) []
        (fun env get_offset ->
          get_offset ^^
          compile_const_64 (Int64.of_int page_size_bits) ^^
          G.i (Binary (Wasm.Values.I64 I64Op.ShrU)) ^^
          get_mem_size env  ^^
          G.i (Compare (Wasm.Values.I64 I64Op.LtU)) ^^
          E.else_trap_with env "StableMemory offset out of bounds")
    | _ -> assert false

  (* check [offset,.., offset + size) within bounds, assumes size > 0 *)
  let guard_range env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code2 env "__stablemem_guard_range"
        (("offset", I64Type), ("size", I32Type)) []
        (fun env get_offset get_size ->
          let (set_sum, get_sum) = new_local64 env "sum" in
          get_offset ^^
          get_size ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
          G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
          set_sum ^^
          get_sum ^^
          get_offset ^^
          G.i (Compare (Wasm.Values.I64 I64Op.LtU)) ^^
          E.then_trap_with env "StableMemory range overflow" ^^
          get_sum
          ^^
          get_mem_size env ^^
          compile_const_64 (Int64.of_int page_size_bits) ^^
          G.i (Binary (Wasm.Values.I64 I64Op.Shl)) ^^
          G.i (Compare (Wasm.Values.I64 I64Op.LeU)) ^^
          E.else_trap_with env "StableMemory range out of bounds")
    | _ -> assert false

  let add_guard env guarded get_offset bytes =
    if guarded then
     (get_offset ^^
      if bytes = 1l then
        guard env
      else
        compile_unboxed_const bytes ^^
        guard_range env)
    else G.nop

  let read env guarded name typ bytes load =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code1 env (Printf.sprintf "__stablemem_%sread_%s" (if guarded then "guarded_" else "") name)
        ("offset", I64Type) [typ]
        (fun env get_offset ->
          let words = Int32.div (Int32.add bytes 3l) 4l in
          add_guard env guarded get_offset bytes ^^
          Stack.with_words env "temp_ptr" words (fun get_temp_ptr ->
            get_temp_ptr ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
            get_offset ^^
            compile_const_64 (Int64.of_int32 bytes) ^^
            IC.system_call env "ic0" "stable64_read" ^^
            get_temp_ptr ^^ load))
    | _ -> assert false

  let write env guarded name typ bytes store =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code2 env (Printf.sprintf "__stablemem_%swrite_%s" (if guarded then "guarded_" else "") name)
        (("offset", I64Type), ("value", typ)) []
        (fun env get_offset get_value ->
          let words = Int32.div (Int32.add bytes 3l) 4l in
          add_guard env guarded get_offset bytes ^^
          Stack.with_words env "temp_ptr" words (fun get_temp_ptr ->
            get_temp_ptr ^^ get_value ^^ store ^^
            get_offset ^^
            get_temp_ptr ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
            compile_const_64 (Int64.of_int32 bytes) ^^
            IC.system_call env "ic0" "stable64_write"))
    | _ -> assert false

  let _read_word32 env =
    read env false "word32" I32Type 4l load_unskewed_ptr
  let write_word32 env =
    write env false "word32" I32Type 4l store_unskewed_ptr


  (* read and clear word32 from stable mem offset on stack *)
  let read_and_clear_word32 env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code1 env "__stablemem_read_and_clear_word32"
        ("offset", I64Type) [I32Type]
        (fun env get_offset ->
          Stack.with_words env "temp_ptr" 1l (fun get_temp_ptr ->
            let (set_word, get_word) = new_local env "word" in
            (* read word *)
            get_temp_ptr ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
            get_offset ^^
            compile_const_64 4L ^^
            IC.system_call env "ic0" "stable64_read" ^^
            get_temp_ptr ^^ load_unskewed_ptr ^^
            set_word ^^
            (* write 0 *)
            get_temp_ptr ^^ compile_unboxed_const 0l ^^ store_unskewed_ptr ^^
            get_offset ^^
            get_temp_ptr ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
            compile_const_64 4L ^^
            IC.system_call env "ic0" "stable64_write" ^^
            (* return word *)
            get_word
        ))
    | _ -> assert false

  (* ensure_pages : ensure at least num pages allocated,
     growing (real) stable memory if needed *)
  let ensure_pages env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code1 env "__stablemem_grow"
        ("pages", I64Type) [I64Type]
        (fun env get_pages ->
          let (set_size, get_size) = new_local64 env "size" in
          let (set_pages_needed, get_pages_needed) = new_local64 env "pages_needed" in

          E.call_import env "ic0" "stable64_size" ^^
          set_size ^^

          get_pages ^^
          get_size ^^
          G.i (Binary (Wasm.Values.I64 I64Op.Sub)) ^^
          set_pages_needed ^^

          get_pages_needed ^^
          compile_const_64 0L ^^
          G.i (Compare (Wasm.Values.I64 I64Op.GtS)) ^^
          G.if1 I64Type
            (get_pages_needed ^^
             E.call_import env "ic0" "stable64_grow")
            get_size)
    | _ -> assert false

  (* ensure stable memory includes [offset..offset+size), assumes size > 0 *)
  let ensure env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code2 env "__stablemem_ensure"
        (("offset", I64Type), ("size", I64Type)) []
        (fun env get_offset get_size ->
          let (set_sum, get_sum) = new_local64 env "sum" in
          get_offset ^^
          get_size ^^
          G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
          set_sum ^^
          (* check for overflow *)
          get_sum ^^
          get_offset ^^
          G.i (Compare (Wasm.Values.I64 I64Op.LtU)) ^^
          E.then_trap_with env "Range overflow" ^^
          (* ensure page *)
          get_sum ^^
          compile_const_64 (Int64.of_int page_size_bits) ^^
          G.i (Binary (Wasm.Values.I64 I64Op.ShrU)) ^^
          compile_add64_const 1L ^^
          ensure_pages env ^^
          (* Check result *)
          compile_const_64 0L ^^
          G.i (Compare (Wasm.Values.I64 I64Op.LtS)) ^^
          E.then_trap_with env "Out of stable memory.")
    | _ -> assert false

  (* API *)

  let logical_grow env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code1 env "__stablemem_logical_grow"
        ("pages", I64Type) [I64Type] (fun env get_pages ->
          let (set_size, get_size) = new_local64 env "size" in
          get_mem_size env ^^
          set_size ^^

          (* check within --max-stable-pages *)
          get_size ^^
          get_pages ^^
          G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
          compile_const_64 (Int64.of_int (!Flags.max_stable_pages)) ^^
          G.i (Compare (Wasm.Values.I64 I64Op.GtU)) ^^
          G.if1 I64Type
            begin
             compile_const_64 (-1L) ^^
             G.i Return
            end
            begin
              let (set_new_size, get_new_size) = new_local64 env "new_size" in
              get_size ^^
              get_pages ^^
              G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
              set_new_size ^^

              (* physical grow if necessary *)
              let (set_ensured, get_ensured) = new_local64 env "ensured" in
              get_new_size ^^
              ensure_pages env ^^
              set_ensured ^^

              (* Check result *)
              get_ensured ^^
              compile_const_64 0L ^^
              G.i (Compare (Wasm.Values.I64 I64Op.LtS)) ^^
              G.if1 I64Type
                ((* propagate failure -1; preserve logical size *)
                 get_ensured)
                ((* update logical size *)
                 get_new_size ^^
                 set_mem_size env ^^
                 (* return old logical size *)
                 get_size)
            end)
   | _ -> assert false

  let load_word32 env =
    read env true "word32" I32Type 4l load_unskewed_ptr
  let store_word32 env =
    write env true "word32" I32Type 4l store_unskewed_ptr

  let load_word8 env =
    read env true "word8" I32Type 1l
      (G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.(Pack8, ZX)}))
  let store_word8 env =
    write env true "word8" I32Type 1l store_unskewed_ptr

  let load_word16 env =
    read env true "word16" I32Type 2l
      (G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.(Pack16, ZX)}))
  let store_word16 env =
    write env true "word16" I32Type 2l store_unskewed_ptr

  let load_word64 env =
    read env true "word64" I64Type 8l
      (G.i (Load {ty = I64Type; align = 0; offset = 0l; sz = None }))
  let store_word64 env =
    write env true "word64" I64Type 8l
      (G.i (Store {ty = I64Type; align = 0; offset = 0l; sz = None}))

  let load_float64 env =
    read env true "float64" F64Type 8l
      (G.i (Load {ty = F64Type; align = 0; offset = 0l; sz = None }))
  let store_float64 env =
    write env true "float64" F64Type 8l
      (G.i (Store {ty = F64Type; align = 0; offset = 0l; sz = None}))


  let load_blob env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code2 env "__stablemem_load_blob"
        (("offset", I64Type), ("len", I32Type)) [I32Type]
        (fun env get_offset get_len ->
          let (set_blob, get_blob) = new_local env "blob" in
          get_offset ^^
          get_len ^^
          guard_range env ^^
          get_len ^^ Blob.alloc env ^^ set_blob ^^
          get_blob ^^ Blob.payload_ptr_unskewed ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
          get_offset ^^
          get_len ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
          IC.system_call env "ic0" "stable64_read" ^^
          get_blob)
    | _ -> assert false

  let store_blob env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.share_code2 env "__stablemem_store_blob"
        (("offset", I64Type), ("blob", I32Type)) []
        (fun env get_offset get_blob ->
         let (set_len, get_len) = new_local env "len" in
          get_blob ^^ Blob.len env ^^ set_len ^^
          get_offset ^^
          get_len ^^
          guard_range env ^^
          get_offset ^^
          get_blob ^^ Blob.payload_ptr_unskewed ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
          get_len ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
          IC.system_call env "ic0" "stable64_write")
    | _ -> assert false

end (* Stack *)

module RTS_Exports = struct
  let system_exports env =
    let bigint_trap_fi = E.add_fun env "bigint_trap" (
      Func.of_body env [] [] (fun env ->
        E.trap_with env "bigint function error"
      )
    ) in
    E.add_export env (nr {
      name = Wasm.Utf8.decode "bigint_trap";
      edesc = nr (FuncExport (nr bigint_trap_fi))
    });
    let rts_trap_fi = E.add_fun env "rts_trap" (
      Func.of_body env ["str", I32Type; "len", I32Type] [] (fun env ->
        let get_str = G.i (LocalGet (nr 0l)) in
        let get_len = G.i (LocalGet (nr 1l)) in
        get_str ^^ get_len ^^ IC.trap_ptr_len env
      )
    ) in
    E.add_export env (nr {
      name = Wasm.Utf8.decode "rts_trap";
      edesc = nr (FuncExport (nr rts_trap_fi))
    })

end (* RTS_Exports *)

module Serialization = struct
  (*
    The general serialization strategy is as follows:
    * We statically generate the IDL type description header.
    * We traverse the data to calculate the size needed for the data buffer and the
      reference buffer.
    * We allocate memory for the data buffer and the reference buffer
      (this memory area is not referenced, so will be dead with the next GC)
    * We copy the IDL type header to the data buffer.
    * We traverse the data and serialize it into the data buffer.
      This is type driven, and we use the `share_code` machinery and names that
      properly encode the type to resolve loops in a convenient way.
    * We externalize all that new data space into a databuf
    * We externalize the reference space into a elembuf
    * We pass both databuf and elembuf to shared functions
      (this mimicks the future system API)

    The deserialization is analogous:
    * We allocate some scratch space, and internalize the databuf and elembuf into it.
    * We parse the data, in a type-driven way, using normal construction and
      allocation, while keeping tabs on the type description header for subtyping.
    * At the end, the scratch space is a hole in the heap, and will be reclaimed
      by the next GC.
  *)

  open Typ_hash

  let sort_by_hash fs =
    List.sort
      (fun (h1,_) (h2,_) -> Lib.Uint32.compare h1 h2)
      (List.map (fun f -> (Idllib.Escape.unescape_hash f.Type.lab, f)) fs)

  (* The IDL serialization prefaces the data with a type description.
     We can statically create the type description in Ocaml code,
     store it in the program, and just copy it to the beginning of the message.

     At some point this can be factored into a function from AS type to IDL type,
     and a function like this for IDL types. But due to recursion handling
     it is easier to start like this.
  *)

  module TM = Map.Make (struct type t = Type.typ let compare = compare end)
  let to_idl_prim = let open Type in function
    | Prim Null | Tup [] -> Some 1l
    | Prim Bool -> Some 2l
    | Prim Nat -> Some 3l
    | Prim Int -> Some 4l
    | Prim Nat8 -> Some 5l
    | Prim Nat16 -> Some 6l
    | Prim (Nat32|Char) -> Some 7l
    | Prim Nat64 -> Some 8l
    | Prim Int8 -> Some 9l
    | Prim Int16 -> Some 10l
    | Prim Int32 -> Some 11l
    | Prim Int64 -> Some 12l
    | Prim Float -> Some 14l
    | Prim Text -> Some 15l
    (* NB: Prim Blob does not map to a primitive IDL type *)
    | Any -> Some 16l
    | Non -> Some 17l
    | Prim Principal -> Some 24l
    | _ -> None

  (* some constants, also see rts/idl.c *)
  let idl_opt       = -18l
  let idl_vec       = -19l
  let idl_record    = -20l
  let idl_variant   = -21l
  let idl_func      = -22l
  let idl_service   = -23l
  let idl_alias     = 1l (* see Note [mutable stable values] *)


  let type_desc env ts : string =
    let open Type in

    (* Type traversal *)
    (* We do a first traversal to find out the indices of non-primitive types *)
    let (typs, idx) =
      let typs = ref [] in
      let idx = ref TM.empty in
      let rec go t =
        let t = Type.normalize t in
        if to_idl_prim t <> None then () else
        if TM.mem t !idx then () else begin
          idx := TM.add t (Lib.List32.length !typs) !idx;
          typs := !typs @ [ t ];
          match t with
          | Tup ts -> List.iter go ts
          | Obj (_, fs) ->
            List.iter (fun f -> go f.typ) fs
          | Array (Mut t) -> go (Array t)
          | Array t -> go t
          | Opt t -> go t
          | Variant vs -> List.iter (fun f -> go f.typ) vs
          | Func (s, c, tbs, ts1, ts2) ->
            List.iter go ts1; List.iter go ts2
          | Prim Blob -> ()
          | Mut t -> go t
          | _ ->
            Printf.eprintf "type_desc: unexpected type %s\n" (string_of_typ t);
            assert false
        end
      in
      List.iter go ts;
      (!typs, !idx)
    in

    (* buffer utilities *)
    let buf = Buffer.create 16 in

    let add_u8 i =
      Buffer.add_char buf (Char.chr (i land 0xff)) in

    let rec add_leb128_32 (i : Lib.Uint32.t) =
      let open Lib.Uint32 in
      let b = logand i (of_int32 0x7fl) in
      if of_int32 0l <= i && i < of_int32 128l
      then add_u8 (to_int b)
      else begin
        add_u8 (to_int (logor b (of_int32 0x80l)));
        add_leb128_32 (shift_right_logical i 7)
      end in

    let add_leb128 i =
      assert (i >= 0);
      add_leb128_32 (Lib.Uint32.of_int i) in

    let rec add_sleb128 (i : int32) =
      let open Int32 in
      let b = logand i 0x7fl in
      if -64l <= i && i < 64l
      then add_u8 (to_int b)
      else begin
        add_u8 (to_int (logor b 0x80l));
        add_sleb128 (shift_right i 7)
      end in

    (* Actual binary data *)

    let add_idx t =
      let t = Type.normalize t in
      match to_idl_prim t with
      | Some i -> add_sleb128 (Int32.neg i)
      | None -> add_sleb128 (TM.find (normalize t) idx) in

    let rec add_typ t =
      match t with
      | Non -> assert false
      | Prim Blob ->
        add_typ Type.(Array (Prim Nat8))
      | Prim _ -> assert false
      | Tup ts ->
        add_sleb128 idl_record;
        add_leb128 (List.length ts);
        List.iteri (fun i t ->
          add_leb128 i;
          add_idx t;
        ) ts
      | Obj ((Object | Memory), fs) ->
        add_sleb128 idl_record;
        add_leb128 (List.length fs);
        List.iter (fun (h, f) ->
          add_leb128_32 h;
          add_idx f.typ
        ) (sort_by_hash fs)
      | Array (Mut t) ->
        add_sleb128 idl_alias; add_idx (Array t)
      | Array t ->
        add_sleb128 idl_vec; add_idx t
      | Opt t ->
        add_sleb128 idl_opt; add_idx t
      | Variant vs ->
        add_sleb128 idl_variant;
        add_leb128 (List.length vs);
        List.iter (fun (h, f) ->
          add_leb128_32 h;
          add_idx f.typ
        ) (sort_by_hash vs)
      | Func (s, c, tbs, ts1, ts2) ->
        assert (Type.is_shared_sort s);
        add_sleb128 idl_func;
        add_leb128 (List.length ts1);
        List.iter add_idx ts1;
        add_leb128 (List.length ts2);
        List.iter add_idx ts2;
        begin match s, c with
          | _, Returns ->
            add_leb128 1; add_u8 2; (* oneway *)
          | Shared Write, _ ->
            add_leb128 0; (* no annotation *)
          | Shared Query, _ ->
            add_leb128 1; add_u8 1; (* query *)
          | _ -> assert false
        end
      | Obj (Actor, fs) ->
        add_sleb128 idl_service;
        add_leb128 (List.length fs);
        List.iter (fun f ->
          add_leb128 (String.length f.lab);
          Buffer.add_string buf f.lab;
          add_idx f.typ
        ) fs
      | Mut t ->
        add_sleb128 idl_alias; add_idx t
      | _ -> assert false in

    Buffer.add_string buf "DIDL";
    add_leb128 (List.length typs);
    List.iter add_typ typs;
    add_leb128 (List.length ts);
    List.iter add_idx ts;
    Buffer.contents buf

  (* Returns data (in bytes) and reference buffer size (in entries) needed *)
  let rec buffer_size env t =
    let open Type in
    let t = Type.normalize t in
    let name = "@buffer_size<" ^ typ_hash t ^ ">" in
    Func.share_code1 env name ("x", I32Type) [I32Type; I32Type]
    (fun env get_x ->

      (* Some combinators for writing values *)
      let (set_data_size, get_data_size) = new_local64 env "data_size" in
      let (set_ref_size, get_ref_size) = new_local env "ref_size" in
      compile_const_64 0L ^^ set_data_size ^^
      compile_unboxed_const 0l ^^ set_ref_size ^^

      let inc_data_size code =
        get_data_size ^^
        code ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
        G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
        set_data_size
      in

      let size_word env code =
        let (set_word, get_word) = new_local env "word" in
        code ^^ set_word ^^
        inc_data_size (I32Leb.compile_leb128_size get_word)
      in

      let size env t =
        let (set_inc, get_inc) = new_local env "inc" in
        buffer_size env t ^^
        get_ref_size ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^ set_ref_size ^^
        set_inc ^^ inc_data_size get_inc
      in

      let size_alias size_thing =
        (* see Note [mutable stable values] *)
        let (set_tag, get_tag) = new_local env "tag" in
        get_x ^^ Tagged.load ^^ set_tag ^^
        (* Sanity check *)
        get_tag ^^ compile_eq_const Tagged.(int_of_tag StableSeen) ^^
        get_tag ^^ compile_eq_const Tagged.(int_of_tag MutBox) ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Or)) ^^
        get_tag ^^ compile_eq_const Tagged.(int_of_tag ObjInd) ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Or)) ^^
        get_tag ^^ compile_eq_const Tagged.(int_of_tag Array) ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Or)) ^^
        E.else_trap_with env "object_size/Mut: Unexpected tag " ^^
        (* Check if we have seen this before *)
        get_tag ^^ compile_eq_const Tagged.(int_of_tag StableSeen) ^^
        G.if0 begin
          (* Seen before *)
          (* One byte marker, one word offset *)
          inc_data_size (compile_unboxed_const 5l)
        end begin
          (* Not yet seen *)
          (* One byte marker, two words scratch space *)
          inc_data_size (compile_unboxed_const 9l) ^^
          (* Mark it as seen *)
          get_x ^^ Tagged.(store StableSeen) ^^
          (* and descend *)
          size_thing ()
        end
      in

      (* Now the actual type-dependent code *)
      begin match t with
      | Prim Nat -> inc_data_size (get_x ^^ BigNum.compile_data_size_unsigned env)
      | Prim Int -> inc_data_size (get_x ^^ BigNum.compile_data_size_signed env)
      | Prim (Int8|Nat8) -> inc_data_size (compile_unboxed_const 1l)
      | Prim (Int16|Nat16) -> inc_data_size (compile_unboxed_const 2l)
      | Prim (Int32|Nat32|Char) -> inc_data_size (compile_unboxed_const 4l)
      | Prim (Int64|Nat64|Float) -> inc_data_size (compile_unboxed_const 8l)
      | Prim Bool -> inc_data_size (compile_unboxed_const 1l)
      | Prim Null -> G.nop
      | Any -> G.nop
      | Tup [] -> G.nop (* e(()) = null *)
      | Tup ts ->
        G.concat_mapi (fun i t ->
          get_x ^^ Tuple.load_n (Int32.of_int i) ^^
          size env t
          ) ts
      | Obj ((Object | Memory), fs) ->
        G.concat_map (fun (_h, f) ->
          get_x ^^ Object.load_idx_raw env f.Type.lab ^^
          size env f.typ
          ) (sort_by_hash fs)
      | Array (Mut t) ->
        size_alias (fun () -> get_x ^^ size env (Array t))
      | Array t ->
        size_word env (get_x ^^ Heap.load_field Arr.len_field) ^^
        get_x ^^ Heap.load_field Arr.len_field ^^
        from_0_to_n env (fun get_i ->
          get_x ^^ get_i ^^ Arr.idx env ^^ load_ptr ^^
          size env t
        )
      | Prim Blob ->
        let (set_len, get_len) = new_local env "len" in
        get_x ^^ Blob.len env ^^ set_len ^^
        size_word env get_len ^^
        inc_data_size get_len
      | Prim Text ->
        let (set_len, get_len) = new_local env "len" in
        get_x ^^ Text.size env ^^ set_len ^^
        size_word env get_len ^^
        inc_data_size get_len
      | Opt t ->
        inc_data_size (compile_unboxed_const 1l) ^^ (* one byte tag *)
        get_x ^^ Opt.is_some env ^^
        G.if0 (get_x ^^ Opt.project env ^^ size env t) G.nop
      | Variant vs ->
        List.fold_right (fun (i, {lab = l; typ = t; _}) continue ->
            get_x ^^
            Variant.test_is env l ^^
            G.if0
              ( size_word env (compile_unboxed_const (Int32.of_int i)) ^^
                get_x ^^ Variant.project ^^ size env t
              ) continue
          )
          ( List.mapi (fun i (_h, f) -> (i,f)) (sort_by_hash vs) )
          ( E.trap_with env "buffer_size: unexpected variant" )
      | Func _ ->
        inc_data_size (compile_unboxed_const 1l) ^^ (* one byte tag *)
        get_x ^^ Arr.load_field 0l ^^ size env (Obj (Actor, [])) ^^
        get_x ^^ Arr.load_field 1l ^^ size env (Prim Text)
      | Obj (Actor, _) | Prim Principal ->
        inc_data_size (compile_unboxed_const 1l) ^^ (* one byte tag *)
        get_x ^^ size env (Prim Blob)
      | Non ->
        E.trap_with env "buffer_size called on value of type None"
      | Mut t ->
        size_alias (fun () -> get_x ^^ Heap.load_field MutBox.field ^^ size env t)
      | _ -> todo "buffer_size" (Arrange_ir.typ t) G.nop
      end ^^
      (* Check 32-bit overflow of buffer_size *)
      get_data_size ^^
      compile_shrU64_const 32L ^^
      G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
      E.else_trap_with env "buffer_size overflow" ^^
      (* Convert to 32-bit *)
      get_data_size ^^
      G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
      get_ref_size
    )

  (* Copies x to the data_buffer, storing references after ref_count entries in ref_base *)
  let rec serialize_go env t =
    let open Type in
    let t = Type.normalize t in
    let name = "@serialize_go<" ^ typ_hash t ^ ">" in
    Func.share_code3 env name (("x", I32Type), ("data_buffer", I32Type), ("ref_buffer", I32Type)) [I32Type; I32Type]
    (fun env get_x get_data_buf get_ref_buf ->
      let set_data_buf = G.setter_for get_data_buf in
      let set_ref_buf = G.setter_for get_ref_buf in

      (* Some combinators for writing values *)

      let advance_data_buf =
        get_data_buf ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^ set_data_buf in

      let write_word code =
        let (set_word, get_word) = new_local env "word" in
        code ^^ set_word ^^
        I32Leb.compile_store_to_data_buf_unsigned env get_word get_data_buf ^^
        advance_data_buf
      in

      let write_word32 code =
        get_data_buf ^^ code ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = None}) ^^
        compile_unboxed_const Heap.word_size ^^ advance_data_buf
      in

      let write_byte code =
        get_data_buf ^^ code ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.Pack8}) ^^
        compile_unboxed_const 1l ^^ advance_data_buf
      in

      let write env t =
        get_data_buf ^^
        get_ref_buf ^^
        serialize_go env t ^^
        set_ref_buf ^^
        set_data_buf
      in

      let write_alias write_thing =
        (* see Note [mutable stable values] *)
        (* Check heap tag *)
        let (set_tag, get_tag) = new_local env "tag" in
        get_x ^^ Tagged.load ^^ set_tag ^^
        get_tag ^^ compile_eq_const Tagged.(int_of_tag StableSeen) ^^
        G.if0
        begin
          (* This is the real data *)
          write_byte (compile_unboxed_const 0l) ^^
          (* Remember the current offset in the tag word *)
          get_x ^^ get_data_buf ^^ Heap.store_field Tagged.tag_field ^^
          (* Leave space in the output buffer for the decoder's bookkeeping *)
          write_word32 (compile_unboxed_const 0l) ^^
          write_word32 (compile_unboxed_const 0l) ^^
          (* Now the data, following the object field mutbox indirection *)
          write_thing ()
        end
        begin
          (* This is a reference *)
          write_byte (compile_unboxed_const 1l) ^^
          (* Sanity Checks *)
          get_tag ^^ compile_eq_const Tagged.(int_of_tag MutBox) ^^
          E.then_trap_with env "unvisited mutable data in serialize_go (MutBox)" ^^
          get_tag ^^ compile_eq_const Tagged.(int_of_tag ObjInd) ^^
          E.then_trap_with env "unvisited mutable data in serialize_go (ObjInd)" ^^
          get_tag ^^ compile_eq_const Tagged.(int_of_tag Array) ^^
          E.then_trap_with env "unvisited mutable data in serialize_go (Array)" ^^
          (* Second time we see this *)
          (* Calculate relative offset *)
          let (set_offset, get_offset) = new_local env "offset" in
          get_tag ^^ get_data_buf ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
          set_offset ^^
          (* A sanity check *)
          get_offset ^^ compile_unboxed_const 0l ^^
          G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^
          E.else_trap_with env "Odd offset" ^^
          (* Write the offset to the output buffer *)
          write_word32 get_offset
        end
      in

      (* Now the actual serialization *)

      begin match t with
      | Prim Nat ->
        get_data_buf ^^
        get_x ^^
        BigNum.compile_store_to_data_buf_unsigned env ^^
        advance_data_buf
      | Prim Int ->
        get_data_buf ^^
        get_x ^^
        BigNum.compile_store_to_data_buf_signed env ^^
        advance_data_buf
      | Prim Float ->
        get_data_buf ^^
        get_x ^^ Float.unbox env ^^
        G.i (Store {ty = F64Type; align = 0; offset = 0l; sz = None}) ^^
        compile_unboxed_const 8l ^^ advance_data_buf
      | Prim (Int64|Nat64) ->
        get_data_buf ^^
        get_x ^^ BoxedWord64.unbox env ^^
        G.i (Store {ty = I64Type; align = 0; offset = 0l; sz = None}) ^^
        compile_unboxed_const 8l ^^ advance_data_buf
      | Prim (Int32|Nat32) ->
        write_word32 (get_x ^^ BoxedSmallWord.unbox env)
      | Prim Char ->
        write_word32 (get_x ^^ TaggedSmallWord.untag_codepoint)
      | Prim (Int16|Nat16) ->
        get_data_buf ^^
        get_x ^^ TaggedSmallWord.lsb_adjust Nat16 ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.Pack16}) ^^
        compile_unboxed_const 2l ^^ advance_data_buf
      | Prim (Int8|Nat8) ->
        write_byte (get_x ^^ TaggedSmallWord.lsb_adjust Nat8)
      | Prim Bool ->
        write_byte (get_x ^^ BoxedSmallWord.unbox env) (* essentially SR.adjust SR.Vanilla SR.bool *)
      | Tup [] -> (* e(()) = null *)
        G.nop
      | Tup ts ->
        G.concat_mapi (fun i t ->
          get_x ^^ Tuple.load_n (Int32.of_int i) ^^
          write env t
        ) ts
      | Obj ((Object | Memory), fs) ->
        G.concat_map (fun (_h, f) ->
          get_x ^^ Object.load_idx_raw env f.Type.lab ^^
          write env f.typ
        ) (sort_by_hash fs)
      | Array (Mut t) ->
        write_alias (fun () -> get_x ^^ write env (Array t))
      | Array t ->
        write_word (get_x ^^ Heap.load_field Arr.len_field) ^^
        get_x ^^ Heap.load_field Arr.len_field ^^
        from_0_to_n env (fun get_i ->
          get_x ^^ get_i ^^ Arr.idx env ^^ load_ptr ^^
          write env t
        )
      | Prim Null -> G.nop
      | Any -> G.nop
      | Opt t ->
        get_x ^^
        Opt.is_some env ^^
        G.if0
          ( write_byte (compile_unboxed_const 1l) ^^ get_x ^^ Opt.project env ^^ write env t )
          ( write_byte (compile_unboxed_const 0l) )
      | Variant vs ->
        List.fold_right (fun (i, {lab = l; typ = t; _}) continue ->
            get_x ^^
            Variant.test_is env l ^^
            G.if0
              ( write_word (compile_unboxed_const (Int32.of_int i)) ^^
                get_x ^^ Variant.project ^^ write env t)
              continue
          )
          ( List.mapi (fun i (_h, f) -> (i,f)) (sort_by_hash vs) )
          ( E.trap_with env "serialize_go: unexpected variant" )
      | Prim Blob ->
        let (set_len, get_len) = new_local env "len" in
        get_x ^^ Blob.len env ^^ set_len ^^
        write_word get_len ^^
        get_data_buf ^^
        get_x ^^ Blob.payload_ptr_unskewed ^^
        get_len ^^
        Heap.memcpy env ^^
        get_len ^^ advance_data_buf
      | Prim Text ->
        let (set_len, get_len) = new_local env "len" in
        get_x ^^ Text.size env ^^ set_len ^^
        write_word get_len ^^
        get_x ^^ get_data_buf ^^ Text.to_buf env ^^
        get_len ^^ advance_data_buf
      | Func _ ->
        write_byte (compile_unboxed_const 1l) ^^
        get_x ^^ Arr.load_field 0l ^^ write env (Obj (Actor, [])) ^^
        get_x ^^ Arr.load_field 1l ^^ write env (Prim Text)
      | Obj (Actor, _) | Prim Principal ->
        write_byte (compile_unboxed_const 1l) ^^
        get_x ^^ write env (Prim Blob)
      | Non ->
        E.trap_with env "serializing value of type None"
      | Mut t ->
        write_alias (fun () ->
          get_x ^^ Heap.load_field MutBox.field ^^ write env t
        )
      | _ -> todo "serialize" (Arrange_ir.typ t) G.nop
      end ^^
      get_data_buf ^^
      get_ref_buf
    )

  (* This value is returned by deserialize_go if deserialization fails in a way
     that should be recoverable by opt parsing.
     By virtue of being a deduped static value, it can be detected by pointer
     comparison.
  *)
  let coercion_error_value env : int32 =
    E.add_static env StaticBytes.[
      I32 Tagged.(int_of_tag CoercionFailure);
    ]

  (* The main deserialization function, generated once per type hash.
     Is parameters are:
       * data_buffer: The current position of the input data buffer
       * ref_buffer:  The current position of the input references buffer
       * typtbl:      The type table, as returned by parse_idl_header
       * idltyp:      The idl type (prim type or table index) to decode now
       * typtbl_size: The size of the type table, used to limit recursion
       * depth:       Recursion counter; reset when we make progres on the value
       * can_recover: Whether coercion errors are recoverable, see coercion_failed below

     It returns the value of type t (vanilla representation) or coercion_error_value,
     It advances the data_buffer past the decoded value (even if it returns coercion_error_value!)
   *)
  let rec deserialize_go env t =
    let open Type in
    let t = Type.normalize t in
    let name = "@deserialize_go<" ^ typ_hash t ^ ">" in
    Func.share_code7 env name
      (("data_buffer", I32Type),
       ("ref_buffer", I32Type),
       ("typtbl", I32Type),
       ("idltyp", I32Type),
       ("typtbl_size", I32Type),
       ("depth", I32Type),
       ("can_recover", I32Type)
      ) [I32Type]
    (fun env get_data_buf get_ref_buf get_typtbl get_idltyp get_typtbl_size get_depth get_can_recover ->

      (* Check recursion depth (protects against empty record etc.) *)
      (* Factor 2 because at each step, the expected type could go through one
         level of opt that is not present in the value type
      *)
      get_depth ^^
      get_typtbl_size ^^ compile_add_const 1l ^^ compile_mul_const 2l ^^
      G.i (Compare (Wasm.Values.I32 I32Op.LeU)) ^^
      E.else_trap_with env ("IDL error: circular record read") ^^

      (* Remember data buffer position, to detect progress *)
      let (set_old_pos, get_old_pos) = new_local env "old_pos" in
      ReadBuf.get_ptr get_data_buf ^^ set_old_pos ^^

      let go' can_recover env t =
        let (set_idlty, get_idlty) = new_local env "idl_ty" in
        set_idlty ^^
        get_data_buf ^^
        get_ref_buf ^^
        get_typtbl ^^
        get_idlty ^^
        get_typtbl_size ^^
        ( (* Reset depth counter if we made progress *)
          ReadBuf.get_ptr get_data_buf ^^ get_old_pos ^^
          G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
          G.if1 I32Type
          (get_depth ^^ compile_add_const 1l)
          (compile_unboxed_const 0l)
        ) ^^
        (if can_recover
         then compile_unboxed_const 1l
         else get_can_recover) ^^
        deserialize_go env t
      in

      let go = go' false in
      let go_can_recover = go' true in

      let skip get_typ =
        get_data_buf ^^ get_typtbl ^^ get_typ ^^ compile_unboxed_const 0l ^^
        E.call_import env "rts" "skip_any"
      in

      (* This flag is set to return a coercion error at the very end
         We cannot use (G.i Return) for early exit, or we’d leak stack space,
         as Stack.with_words is used to allocate scratch space.
      *)
      let (set_failed, get_failed) = new_local env "failed" in
      let set_failure = compile_unboxed_const 1l ^^ set_failed in
      let when_failed f = get_failed ^^ G.if0 f G.nop in

      (* This looks at a value and if it is coercion_error_value, sets the failure flag.
         This propagates the error out of arrays, records, etc.
       *)
      let remember_failure get_val =
          get_val ^^ compile_eq_const (coercion_error_value env) ^^
          G.if0 set_failure G.nop
      in

      (* This sets the failure flag and puts coercion_error_value on the stack *)
      let coercion_failed msg =
        (* If we know that there is no backtracking `opt t` around, then just trap.
           This gives a better error message
        *)
        get_can_recover ^^ E.else_trap_with env msg ^^
        set_failure ^^ compile_unboxed_const (coercion_error_value env) in

      (* returns true if we are looking at primitive type with this id *)
      let check_prim_typ t =
        get_idltyp ^^
        compile_eq_const (Int32.neg (Option.get (to_idl_prim t)))
      in

      let with_prim_typ t f =
        check_prim_typ t ^^
        G.if1 I32Type f
          ( skip get_idltyp ^^
            coercion_failed ("IDL error: unexpected IDL type when parsing " ^ string_of_typ t)
          )
      in

      let read_byte_tagged = function
        | [code0; code1] ->
          ReadBuf.read_byte env get_data_buf ^^
          let (set_b, get_b) = new_local env "b" in
          set_b ^^
          get_b ^^
          compile_eq_const 0l ^^
          G.if1 I32Type
          begin code0
          end begin
            get_b ^^ compile_eq_const 1l ^^
            E.else_trap_with env "IDL error: byte tag not 0 or 1" ^^
            code1
          end
        | _ -> assert false; (* can be generalized later as needed *)
      in

      let read_blob () =
        let (set_len, get_len) = new_local env "len" in
        let (set_x, get_x) = new_local env "x" in
        ReadBuf.read_leb128 env get_data_buf ^^ set_len ^^

        get_len ^^ Blob.alloc env ^^ set_x ^^
        get_x ^^ Blob.payload_ptr_unskewed ^^
        ReadBuf.read_blob env get_data_buf get_len ^^
        get_x
      in

      let read_principal () =
        let (set_len, get_len) = new_local env "len" in
        let (set_x, get_x) = new_local env "x" in
        ReadBuf.read_leb128 env get_data_buf ^^ set_len ^^

        (* at most 29 bytes, according to
           https://sdk.dfinity.org/docs/interface-spec/index.html#principal
        *)
        get_len ^^ compile_unboxed_const 29l ^^ G.i (Compare (Wasm.Values.I32 I32Op.LeU)) ^^
        E.else_trap_with env "IDL error: principal too long" ^^

        get_len ^^ Blob.alloc env ^^ set_x ^^
        get_x ^^ Blob.payload_ptr_unskewed ^^
        ReadBuf.read_blob env get_data_buf get_len ^^
        get_x
      in

      let read_text () =
        let (set_len, get_len) = new_local env "len" in
        ReadBuf.read_leb128 env get_data_buf ^^ set_len ^^
        let (set_ptr, get_ptr) = new_local env "x" in
        ReadBuf.get_ptr get_data_buf ^^ set_ptr ^^
        ReadBuf.advance get_data_buf get_len ^^
        (* validate *)
        get_ptr ^^ get_len ^^ E.call_import env "rts" "utf8_validate" ^^
        (* copy *)
        get_ptr ^^ get_len ^^ Text.of_ptr_size env
      in

      let read_actor_data () =
        read_byte_tagged
          [ E.trap_with env "IDL error: unexpected actor reference"
          ; read_principal ()
          ]
      in

      (* returns true if get_arg_typ is a composite type of this id *)
      let check_composite_typ get_arg_typ idl_tycon_id =
        get_arg_typ ^^
        compile_unboxed_const 0l ^^ G.i (Compare (Wasm.Values.I32 I32Op.GeS)) ^^
        G.if1 I32Type
        begin
          ReadBuf.alloc env (fun get_typ_buf ->
            (* Update typ_buf *)
            ReadBuf.set_ptr get_typ_buf (
              get_typtbl ^^
              get_arg_typ ^^ compile_mul_const Heap.word_size ^^
              G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
              load_unskewed_ptr
            ) ^^
            ReadBuf.set_end get_typ_buf (ReadBuf.get_end get_data_buf) ^^
            (* read sleb128 *)
            ReadBuf.read_sleb128 env get_typ_buf ^^
            (* Check it is the expected value *)
            compile_eq_const idl_tycon_id
          )
        end
        (compile_unboxed_const 0l)
      in


      (* checks that arg_typ is positive, looks it up in the table,
         creates a fresh typ_buf pointing into the type description,
         reads the type constructor index and traps or fails if it is the wrong one.
         and passes the typ_buf to a subcomputation to read the type arguments *)
      let with_composite_arg_typ get_arg_typ idl_tycon_id f =
        (* make sure index is not negative *)
        get_arg_typ ^^
        compile_unboxed_const 0l ^^ G.i (Compare (Wasm.Values.I32 I32Op.GeS)) ^^
        G.if1 I32Type
        begin
          ReadBuf.alloc env (fun get_typ_buf ->
            (* Update typ_buf *)
            ReadBuf.set_ptr get_typ_buf (
              get_typtbl ^^
              get_arg_typ ^^ compile_mul_const Heap.word_size ^^
              G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
              load_unskewed_ptr
            ) ^^
            ReadBuf.set_end get_typ_buf (ReadBuf.get_end get_data_buf) ^^
            (* read sleb128 *)
            ReadBuf.read_sleb128 env get_typ_buf ^^
            (* Check it is the expected type constructor *)
            compile_eq_const idl_tycon_id ^^
            G.if1 I32Type
            begin
              f get_typ_buf
            end
            begin
              skip get_arg_typ ^^
              coercion_failed ("IDL error: unexpected IDL type when parsing " ^ string_of_typ t)
            end
          )
        end
        begin
          skip get_arg_typ ^^
          coercion_failed ("IDL error: unexpected IDL type when parsing " ^ string_of_typ t)
        end
      in

      let with_composite_typ idl_tycon_id f =
        with_composite_arg_typ get_idltyp idl_tycon_id f
      in

      let with_record_typ f = with_composite_typ idl_record (fun get_typ_buf ->
        Stack.with_words env "get_n_ptr" 1l (fun get_n_ptr ->
          get_n_ptr ^^
          ReadBuf.read_leb128 env get_typ_buf ^^
          store_unskewed_ptr ^^
          f get_typ_buf get_n_ptr
        )
      ) in

      let with_blob_typ env f =
        with_composite_typ idl_vec (fun get_typ_buf ->
          ReadBuf.read_sleb128 env get_typ_buf ^^
          compile_eq_const (-5l) (* Nat8 *) ^^
          G.if1 I32Type
            f
            begin
              skip get_idltyp ^^
              coercion_failed "IDL error: blob not a vector of nat8"
            end
        )
      in

      let read_alias env t read_thing =
        (* see Note [mutable stable values] *)
        let (set_is_ref, get_is_ref) = new_local env "is_ref" in
        let (set_result, get_result) = new_local env "result" in
        let (set_cur, get_cur) = new_local env "cur" in
        let (set_memo, get_memo) = new_local env "memo" in

        let (set_arg_typ, get_arg_typ) = new_local env "arg_typ" in
        with_composite_typ idl_alias (ReadBuf.read_sleb128 env) ^^ set_arg_typ ^^

        (* Find out if it is a reference or not *)
        ReadBuf.read_byte env get_data_buf ^^ set_is_ref ^^

        (* If it is a reference, temporarily set the read buffer to that place *)
        get_is_ref ^^
        G.if0 begin
          let (set_offset, get_offset) = new_local env "offset" in
          ReadBuf.read_word32 env get_data_buf ^^ set_offset ^^
          (* A sanity check *)
          get_offset ^^ compile_unboxed_const 0l ^^
          G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^
          E.else_trap_with env "Odd offset" ^^

          ReadBuf.get_ptr get_data_buf ^^ set_cur ^^
          ReadBuf.advance get_data_buf (get_offset ^^ compile_add_const (-4l))
        end G.nop ^^

        (* Remember location of ptr *)
        ReadBuf.get_ptr get_data_buf ^^ set_memo ^^
        (* Did we decode this already? *)
        ReadBuf.read_word32 env get_data_buf ^^ set_result ^^
        get_result ^^ compile_eq_const 0l ^^
        G.if0 begin
          (* No, not yet decoded *)
          (* Skip over type hash field *)
          ReadBuf.read_word32 env get_data_buf ^^ compile_eq_const 0l ^^
          E.else_trap_with env "Odd: Type hash scratch space not empty" ^^

          (* Read the content *)
          read_thing get_arg_typ (fun get_thing ->
            (* This is called after allocation, but before descending
               We update the memo location here so that loops work
            *)
            get_thing ^^ set_result ^^
            get_memo ^^ get_result ^^ store_unskewed_ptr ^^
            get_memo ^^ compile_add_const 4l ^^ Blob.lit env (typ_hash t) ^^ store_unskewed_ptr
          )
        end begin
          (* Decoded before. Check type hash *)
          ReadBuf.read_word32 env get_data_buf ^^ Blob.lit env (typ_hash t) ^^
          G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
          E.else_trap_with env ("Stable memory error: Aliased at wrong type, expected: " ^ typ_hash t)
        end ^^

        (* If this was a reference, reset read buffer *)
        get_is_ref ^^
        G.if0 (ReadBuf.set_ptr get_data_buf get_cur) G.nop ^^

        get_result
      in


      (* Now the actual deserialization *)
      begin match t with
      (* Primitive types *)
      | Prim Nat ->
        with_prim_typ t
        begin
          get_data_buf ^^
          BigNum.compile_load_from_data_buf env false
        end
      | Prim Int ->
        (* Subtyping with nat *)
        check_prim_typ (Prim Nat) ^^
        G.if1 I32Type
          begin
            get_data_buf ^^
            BigNum.compile_load_from_data_buf env false
          end
          begin
            with_prim_typ t
            begin
              get_data_buf ^^
              BigNum.compile_load_from_data_buf env true
            end
          end
      | Prim Float ->
        with_prim_typ t
        begin
          ReadBuf.read_float64 env get_data_buf ^^
          Float.box env
        end
      | Prim (Int64|Nat64) ->
        with_prim_typ t
        begin
          ReadBuf.read_word64 env get_data_buf ^^
          BoxedWord64.box env
        end
      | Prim (Int32|Nat32) ->
        with_prim_typ t
        begin
          ReadBuf.read_word32 env get_data_buf ^^
          BoxedSmallWord.box env
        end
      | Prim Char ->
        with_prim_typ t
        begin
          ReadBuf.read_word32 env get_data_buf ^^
          TaggedSmallWord.check_and_tag_codepoint env
        end
      | Prim (Int16|Nat16) ->
        with_prim_typ t
        begin
          ReadBuf.read_word16 env get_data_buf ^^
          TaggedSmallWord.msb_adjust Nat16
        end
      | Prim (Int8|Nat8) ->
        with_prim_typ t
        begin
          ReadBuf.read_byte env get_data_buf ^^
          TaggedSmallWord.msb_adjust Nat8
        end
      | Prim Bool ->
        with_prim_typ t
        begin
          read_byte_tagged
            [ Bool.lit false
            ; Bool.lit true
            ] ^^
          BoxedSmallWord.box env (* essentially SR.adjust SR.bool SR.Vanilla *)
        end
      | Prim Null ->
        with_prim_typ t (Opt.null_lit env)
      | Any ->
        skip get_idltyp ^^
        (* Any vanilla value works here *)
        Opt.null_lit env
      | Prim Blob ->
        with_blob_typ env (read_blob ())
      | Prim Principal ->
        with_prim_typ t
        begin
          read_byte_tagged
            [ E.trap_with env "IDL error: unexpected principal reference"
            ; read_principal ()
            ]
        end
      | Prim Text ->
        with_prim_typ t (read_text ())
      | Tup [] -> (* e(()) = null *)
        with_prim_typ t (Tuple.from_stack env 0)
      (* Composite types *)
      | Tup ts ->
        with_record_typ (fun get_typ_buf get_n_ptr ->
          let (set_val, get_val) = new_local env "val" in

          G.concat_mapi (fun i t ->
            (* skip all possible intermediate extra fields *)
            get_typ_buf ^^ get_data_buf ^^ get_typtbl ^^ compile_unboxed_const (Int32.of_int i) ^^ get_n_ptr ^^
            E.call_import env "rts" "find_field" ^^
            G.if1 I32Type
              begin
                ReadBuf.read_sleb128 env get_typ_buf ^^
                go env t ^^ set_val ^^
                remember_failure get_val ^^
                get_val
              end
              begin
                match normalize t with
                | Opt _ | Any -> Opt.null_lit env
                | _ -> coercion_failed "IDL error: did not find tuple field in record"
              end
          ) ts ^^

          (* skip all possible trailing extra fields *)
          get_typ_buf ^^ get_data_buf ^^ get_typtbl ^^ get_n_ptr ^^
          E.call_import env "rts" "skip_fields" ^^

          Tuple.from_stack env (List.length ts)
        )
      | Obj ((Object | Memory), fs) ->
        with_record_typ (fun get_typ_buf get_n_ptr ->
          let (set_val, get_val) = new_local env "val" in

          Object.lit_raw env (List.map (fun (h,f) ->
            f.Type.lab, fun () ->
              (* skip all possible intermediate extra fields *)
              get_typ_buf ^^ get_data_buf ^^ get_typtbl ^^ compile_unboxed_const (Lib.Uint32.to_int32 h) ^^ get_n_ptr ^^
              E.call_import env "rts" "find_field" ^^
              G.if1 I32Type
                begin
                  ReadBuf.read_sleb128 env get_typ_buf ^^
                  go env f.typ ^^ set_val ^^
                  remember_failure get_val ^^
                  get_val
                  end
                begin
                  match normalize f.typ with
                  | Opt _ | Any -> Opt.null_lit env
                  | _ -> coercion_failed (Printf.sprintf "IDL error: did not find field %s in record" f.lab)
                end
          ) (sort_by_hash fs)) ^^

          (* skip all possible trailing extra fields *)
          get_typ_buf ^^ get_data_buf ^^ get_typtbl ^^ get_n_ptr ^^
          E.call_import env "rts" "skip_fields"
          )
      | Array (Mut t) ->
        read_alias env (Array (Mut t)) (fun get_array_typ on_alloc ->
          let (set_len, get_len) = new_local env "len" in
          let (set_x, get_x) = new_local env "x" in
          let (set_val, get_val) = new_local env "val" in
          let (set_arg_typ, get_arg_typ) = new_local env "arg_typ" in
          with_composite_arg_typ get_array_typ idl_vec (ReadBuf.read_sleb128 env) ^^ set_arg_typ ^^
          ReadBuf.read_leb128 env get_data_buf ^^ set_len ^^
          get_len ^^ Arr.alloc env ^^ set_x ^^
          on_alloc get_x ^^
          get_len ^^ from_0_to_n env (fun get_i ->
            get_x ^^ get_i ^^ Arr.idx env ^^
            get_arg_typ ^^ go env t ^^ set_val ^^
            remember_failure get_val ^^
            get_val ^^ store_ptr
          )
        )
      | Array t ->
        let (set_len, get_len) = new_local env "len" in
        let (set_x, get_x) = new_local env "x" in
        let (set_val, get_val) = new_local env "val" in
        let (set_arg_typ, get_arg_typ) = new_local env "arg_typ" in
        with_composite_typ idl_vec (ReadBuf.read_sleb128 env) ^^ set_arg_typ ^^
        ReadBuf.read_leb128 env get_data_buf ^^ set_len ^^
        get_len ^^ Arr.alloc env ^^ set_x ^^
        get_len ^^ from_0_to_n env (fun get_i ->
          get_x ^^ get_i ^^ Arr.idx env ^^
          get_arg_typ ^^ go env t ^^ set_val ^^
          remember_failure get_val ^^
          get_val ^^ store_ptr
        ) ^^
        get_x
      | Opt t ->
        check_prim_typ (Prim Null) ^^
        G.if1 I32Type (Opt.null_lit env)
        begin
          check_prim_typ Any ^^ (* reserved *)
          G.if1 I32Type (Opt.null_lit env)
          begin
            check_composite_typ get_idltyp idl_opt ^^
            G.if1 I32Type
            begin
              let (set_arg_typ, get_arg_typ) = new_local env "arg_typ" in
              with_composite_typ idl_opt (ReadBuf.read_sleb128 env) ^^ set_arg_typ ^^
              read_byte_tagged
                [ Opt.null_lit env
                ; let (set_val, get_val) = new_local env "val" in
                  get_arg_typ ^^ go_can_recover env t ^^ set_val ^^
                  get_val ^^ compile_eq_const (coercion_error_value env) ^^
                  G.if1 I32Type
                    (* decoding failed, but this is opt, so: return null *)
                    (Opt.null_lit env)
                    (* decoding succeeded, return opt value *)
                    (Opt.inject env get_val)
                ]
            end
            begin
              (* this check corresponds to `not (null <: <t>)` in the spec *)
              match normalize t with
              | Prim Null | Opt _ | Any ->
                (* Ignore and return null *)
                skip get_idltyp ^^
                Opt.null_lit env
              | _ ->
                (* Try constituent type *)
                let (set_val, get_val) = new_local env "val" in
                get_idltyp ^^ go_can_recover env t ^^ set_val ^^
                get_val ^^ compile_eq_const (coercion_error_value env) ^^
                G.if1 I32Type
                  (* decoding failed, but this is opt, so: return null *)
                  (Opt.null_lit env)
                  (* decoding succeeded, return opt value *)
                  (Opt.inject env get_val)
            end
          end
        end
      | Variant vs ->
        let (set_val, get_val) = new_local env "val" in
        with_composite_typ idl_variant (fun get_typ_buf ->
          (* Find the tag *)
          let (set_n, get_n) = new_local env "len" in
          ReadBuf.read_leb128 env get_typ_buf ^^ set_n ^^

          let (set_tagidx, get_tagidx) = new_local env "tagidx" in
          ReadBuf.read_leb128 env get_data_buf ^^ set_tagidx ^^

          get_tagidx ^^ get_n ^^
          G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
          E.else_trap_with env "IDL error: variant index out of bounds" ^^

          (* Zoom past the previous entries *)
          get_tagidx ^^ from_0_to_n env (fun _ ->
            get_typ_buf ^^ E.call_import env "rts" "skip_leb128" ^^
            get_typ_buf ^^ E.call_import env "rts" "skip_leb128"
          ) ^^

          (* Now read the tag *)
          let (set_tag, get_tag) = new_local env "tag" in
          ReadBuf.read_leb128 env get_typ_buf ^^ set_tag ^^
          let (set_arg_typ, get_arg_typ) = new_local env "arg_typ" in
          ReadBuf.read_sleb128 env get_typ_buf ^^ set_arg_typ ^^

          List.fold_right (fun (h, {lab = l; typ = t; _}) continue ->
              get_tag ^^ compile_eq_const (Lib.Uint32.to_int32 h) ^^
              G.if1 I32Type
                ( Variant.inject env l (
                  get_arg_typ ^^ go env t ^^ set_val ^^
                  remember_failure get_val ^^
                  get_val
                ))
                continue
            )
            ( sort_by_hash vs )
            ( coercion_failed "IDL error: unexpected variant tag" )
        )
      | Func _ ->
        with_composite_typ idl_func (fun _get_typ_buf ->
          read_byte_tagged
            [ E.trap_with env "IDL error: unexpected function reference"
            ; read_actor_data () ^^
              read_text () ^^
              Tuple.from_stack env 2
            ]
        );
      | Obj (Actor, _) ->
        with_composite_typ idl_service (fun _get_typ_buf -> read_actor_data ())
      | Mut t ->
        read_alias env (Mut t) (fun get_arg_typ on_alloc ->
          let (set_result, get_result) = new_local env "result" in
          Tagged.obj env Tagged.ObjInd [ compile_unboxed_const 0l ] ^^ set_result ^^
          on_alloc get_result ^^
          get_result ^^
            get_arg_typ ^^ go env t ^^
          Heap.store_field MutBox.field
        )
      | Non ->
        E.trap_with env "IDL error: deserializing value of type None"
      | _ -> todo_trap env "deserialize" (Arrange_ir.typ t)
      end ^^
      (* Parsed value on the stack, return that, unless the failure flag is set *)
      when_failed (compile_unboxed_const (coercion_error_value env) ^^ G.i Return)
    )

  let serialize env ts : G.t =
    let ts_name = typ_seq_hash ts in
    let name = "@serialize<" ^ ts_name ^ ">" in
    (* returns data/length pointers (will be GC’ed next time!) *)
    Func.share_code1 env name ("x", I32Type) [I32Type; I32Type] (fun env get_x ->
      let (set_data_size, get_data_size) = new_local env "data_size" in
      let (set_refs_size, get_refs_size) = new_local env "refs_size" in

      let tydesc = type_desc env ts in
      let tydesc_len = Int32.of_int (String.length tydesc) in

      (* Get object sizes *)
      get_x ^^
      buffer_size env (Type.seq ts) ^^
      set_refs_size ^^
      (* add tydesc_len *)
      compile_add_const tydesc_len  ^^
      set_data_size ^^
      (* check for overflow *)
      get_data_size ^^
      compile_unboxed_const tydesc_len ^^
      G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
      E.then_trap_with env "serialization overflow" ^^

      let (set_data_start, get_data_start) = new_local env "data_start" in
      let (set_refs_start, get_refs_start) = new_local env "refs_start" in

      get_data_size ^^ Blob.dyn_alloc_scratch env ^^ set_data_start ^^
      get_refs_size ^^ compile_mul_const Heap.word_size ^^ Blob.dyn_alloc_scratch env ^^ set_refs_start ^^

      (* Write ty desc *)
      get_data_start ^^
      Blob.lit env tydesc ^^ Blob.payload_ptr_unskewed ^^
      compile_unboxed_const tydesc_len ^^
      Heap.memcpy env ^^

      (* Serialize x into the buffer *)
      get_x ^^
      get_data_start ^^ compile_add_const tydesc_len ^^
      get_refs_start ^^
      serialize_go env (Type.seq ts) ^^

      (* Sanity check: Did we fill exactly the buffer *)
      get_refs_start ^^ get_refs_size ^^ compile_mul_const Heap.word_size ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
      E.else_trap_with env "reference buffer not filled " ^^

      get_data_start ^^ get_data_size ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
      E.else_trap_with env "data buffer not filled " ^^

      get_refs_size ^^
      compile_unboxed_const 0l ^^
      G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
      E.else_trap_with env "cannot send references on IC System API" ^^

      get_data_start ^^
      get_data_size
    )

  let deserialize_from_blob extended env ts =
    let ts_name = typ_seq_hash ts in
    let name =
      if extended
      then "@deserialize_extended<" ^ ts_name ^ ">"
      else "@deserialize<" ^ ts_name ^ ">" in
    Func.share_code1 env name ("blob", I32Type) (List.map (fun _ -> I32Type) ts) (fun env get_blob ->
      let (set_data_size, get_data_size) = new_local env "data_size" in
      let (set_refs_size, get_refs_size) = new_local env "refs_size" in
      let (set_data_start, get_data_start) = new_local env "data_start" in
      let (set_refs_start, get_refs_start) = new_local env "refs_start" in
      let (set_arg_count, get_arg_count) = new_local env "arg_count" in
      let (set_val, get_val) = new_local env "val" in

      get_blob ^^ Blob.len env ^^ set_data_size ^^
      get_blob ^^ Blob.payload_ptr_unskewed ^^ set_data_start ^^

      (* Allocate space for the reference buffer and copy it *)
      compile_unboxed_const 0l ^^ set_refs_size (* none yet *) ^^

      (* Allocate space for out parameters of parse_idl_header *)
      Stack.with_words env "get_typtbl_size_ptr" 1l (fun get_typtbl_size_ptr ->
      Stack.with_words env "get_typtbl_ptr" 1l (fun get_typtbl_ptr ->
      Stack.with_words env "get_maintyps_ptr" 1l (fun get_maintyps_ptr ->

      (* Set up read buffers *)
      ReadBuf.alloc env (fun get_data_buf -> ReadBuf.alloc env (fun get_ref_buf ->

      ReadBuf.set_ptr get_data_buf get_data_start ^^
      ReadBuf.set_size get_data_buf get_data_size ^^
      ReadBuf.set_ptr get_ref_buf get_refs_start ^^
      ReadBuf.set_size get_ref_buf (get_refs_size ^^ compile_mul_const Heap.word_size) ^^

      (* Go! *)
      Bool.lit extended ^^ get_data_buf ^^ get_typtbl_ptr ^^ get_typtbl_size_ptr ^^ get_maintyps_ptr ^^
      E.call_import env "rts" "parse_idl_header" ^^

      (* set up a dedicated read buffer for the list of main types *)
      ReadBuf.alloc env (fun get_main_typs_buf ->
        ReadBuf.set_ptr get_main_typs_buf (get_maintyps_ptr ^^ load_unskewed_ptr) ^^
        ReadBuf.set_end get_main_typs_buf (ReadBuf.get_end get_data_buf) ^^
        ReadBuf.read_leb128 env get_main_typs_buf ^^ set_arg_count ^^

        get_arg_count ^^
        compile_rel_const I32Op.GeU (Int32.of_int (List.length ts)) ^^
        E.else_trap_with env ("IDL error: too few arguments " ^ ts_name) ^^

        G.concat_map (fun t ->
          get_data_buf ^^ get_ref_buf ^^
          get_typtbl_ptr ^^ load_unskewed_ptr ^^
          ReadBuf.read_sleb128 env get_main_typs_buf ^^
          get_typtbl_size_ptr ^^ load_unskewed_ptr ^^
          compile_unboxed_const 0l ^^ (* initial depth *)
          compile_unboxed_const 0l ^^ (* initially, cannot recover *)
          deserialize_go env t ^^ set_val ^^
          get_val ^^ compile_eq_const (coercion_error_value env) ^^
          E.then_trap_with env ("IDL error: coercion failure encountered") ^^
          get_val
        ) ts ^^

        (* Skip any extra arguments *)
        get_arg_count ^^ compile_sub_const (Int32.of_int (List.length ts)) ^^
        from_0_to_n env (fun _ ->
          get_data_buf ^^
          get_typtbl_ptr ^^ load_unskewed_ptr ^^
          ReadBuf.read_sleb128 env get_main_typs_buf ^^
          compile_unboxed_const 0l ^^
          E.call_import env "rts" "skip_any"
        ) ^^

        ReadBuf.is_empty env get_data_buf ^^
        E.else_trap_with env ("IDL error: left-over bytes " ^ ts_name) ^^
        ReadBuf.is_empty env get_ref_buf ^^
        E.else_trap_with env ("IDL error: left-over references " ^ ts_name)
      ))))))
    )

  let deserialize env ts =
    Blob.of_size_copy env
      (fun env -> IC.system_call env "ic0" "msg_arg_data_size")
      (fun env -> IC.system_call env "ic0" "msg_arg_data_copy")
      (fun env -> compile_unboxed_const 0l) ^^
    deserialize_from_blob false env ts

(*
Note [mutable stable values]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We currently use a Candid derivative to serialize stable values. In addition to
storing sharable data, we can also store mutable data (records with mutable
fields and mutable arrays), and we need to preserve aliasing.

To that end we extend Candid with a type constructor `alias t`.

In the type table, alias t is represented by type code 1. All Candid type constructors
are represented by negative numbers, so this cannot clash with anything and,
conveniently, makes such values illegal Candid.

The values of `alias t` are either

 * i8(0) 0x00000000 0x00000000 M(v)
   for one (typically the first) occurrence of v
   The first 0x00000000 is the “memo field”, the second is the “type hash field”.
   Both are scratch spaces for the benefit of the decoder.

or

 * i8(1) i32(offset) M(v)
   for all other occurrences of v, where offset is the relative position of the
   above occurrences from this reference.

We map Motoko types to this as follows:

  e([var t]) = alias e([t]) = alias vec e(t)
  e({var field : t}) = record { field : alias e(t) }

Why different? Because we need to alias arrays as a whole (we can’t even alias
their fields, as they are manifestly part of the array heap structure), but
aliasing records does not work, as aliased record values may appear at
different types (due to subtyping), and Candid serialization is type-driven.
Luckily records put all mutable fields behind an indirection (ObjInd), so this
works.

The type-driven code in this module treats `Type.Mut` to always refer to an
`ObjInd`; for arrays the mutable case is handled directly.

To detect and preserve aliasing, these steps are taken:

 * In `buffer_size`, when we see a mutable thing (`Array` or `ObjInd`), the
   first time, we mark it by setting the heap tag to `StableSeen`.
   This way, when we see it a second time, we can skip the value in the size
   calculation.
 * In `serialize`, when we see it a first time (tag still `StableSeen`),
   we serialize it (first form above), and remember the absolute position
   in the output buffer, abusing the heap tag here.
   (Invariant: This absolute position is never `StableSeen`)
   Upon a second visit (tag not `StableSeen`), we can thus fetch that absolute
   position and calculate the offset.
 * In `deserialize`, when we come across a `alias t`, we follow the offset (if
   needed) to find the content.

   If the memo field is still `0x00000000`, this is the first time we read
   this, so we deserialize to the Motoko heap, and remember the heap position
   (vanilla pointer) by overriding the memo field.
   We also store the type hash of the type we are serializing at in the type
   hash field.

   If it is not `0x00000000` then we can simply read the pointer from there,
   after checking the type hash field to make sure we are aliasing at the same
   type.

*)

end (* Serialization *)


(* Stabilization (serialization to/from stable memory) of both:
   * stable variables; and
   * virtual stable memory.
   c.f.
   * ../../design/Stable.md
   * ../../design/StableMemory.md
*)

module Stabilization = struct

  let stabilize env t =
    let (set_dst, get_dst) = new_local env "dst" in
    let (set_len, get_len) = new_local env "len" in
    Serialization.serialize env [t] ^^
    set_len ^^
    set_dst ^^

    StableMem.get_mem_size env ^^
    G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
    G.if0
      begin (* ensure [0,..,3,...len+4) *)
        compile_const_64 0L ^^
        get_len ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
        compile_add64_const 4L ^^  (* reserve one word for size *)
        StableMem.ensure env ^^

        (* write len to initial word of stable memory*)
        compile_const_64 0L ^^
        get_len ^^
        StableMem.write_word32 env ^^

        (* copy data to following stable memory *)
        compile_const_64 4L ^^
        get_dst ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
        get_len ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
        E.call_import env "ic0" "stable64_write"
      end
      begin
        let (set_N, get_N) = new_local64 env "N" in

        (* let N = !size * page_size *)
        StableMem.get_mem_size env ^^
        compile_shl64_const (Int64.of_int page_size_bits) ^^
        set_N ^^

        (* grow mem to page including address
           N + 4 + len + 4 + 4 + 4 = N + len + 16
        *)
        get_N ^^
        get_len ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
        compile_add64_const 16L ^^
        StableMem.ensure env  ^^

        get_N ^^
        get_len ^^
        StableMem.write_word32 env ^^

        get_N ^^
        compile_add64_const 4L ^^
        get_dst ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
        get_len ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
        E.call_import env "ic0" "stable64_write" ^^

        (* let M = pagesize * ic0.stable64_size64() - 1 *)
        (* M is beginning of last page *)
        let (set_M, get_M) = new_local64 env "M" in
        E.call_import env "ic0" "stable64_size" ^^
        compile_sub64_const 1L ^^
        compile_shl64_const (Int64.of_int page_size_bits) ^^
        set_M ^^

        (* store mem_size at M + (pagesize - 12) *)
        get_M ^^
        compile_add64_const (Int64.sub page_size64 12L) ^^
        StableMem.get_mem_size env ^^
          G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
        (* TODO: write word64 *)
        StableMem.write_word32 env ^^

        (* save first word at M + (pagesize - 8);
           mark first word as 0 *)
        get_M ^^
        compile_add64_const (Int64.sub page_size64 8L) ^^
        compile_const_64 0L ^^
        StableMem.read_and_clear_word32 env ^^
        StableMem.write_word32 env ^^

        (* save version at M + (pagesize - 4) *)
        get_M ^^
          compile_add64_const (Int64.sub page_size64 4L) ^^
        (* TODO bump version? *)
        compile_unboxed_const StableMem.version ^^
        StableMem.write_word32 env

      end

  let destabilize env ty =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      let (set_pages, get_pages) = new_local64 env "pages" in
      E.call_import env "ic0" "stable64_size" ^^
      set_pages ^^

      get_pages ^^
      G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
      G.if1 I32Type
        begin
          let (_, fs) = Type.as_obj ty in
          let fs' = List.map
           (fun f -> (f.Type.lab, fun () -> Opt.null_lit env))
           fs
          in
          StableMem.get_mem_size env ^^
          G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
          E.else_trap_with env "StableMem.mem_size non-zero" ^^
          Object.lit_raw env fs'
        end
        begin
          let (set_marker, get_marker) = new_local env "marker" in
          let (set_len, get_len) = new_local env "len" in
          let (set_offset, get_offset) = new_local64 env "offset" in
          compile_const_64 0L ^^
          StableMem.read_and_clear_word32 env ^^
          set_marker ^^

          get_marker ^^
          G.i (Test (Wasm.Values.I32 I32Op.Eqz)) ^^
          G.if0
            begin
              let (set_M, get_M) = new_local64 env "M" in
              let (set_version, get_version) = new_local env "version" in
              let (set_N, get_N) = new_local64 env "N" in

              E.call_import env "ic0" "stable64_size" ^^
              compile_sub64_const 1L ^^
              compile_shl64_const (Int64.of_int page_size_bits) ^^
              set_M ^^

              (* read version *)
              get_M ^^
              compile_add64_const (Int64.sub page_size64 4L) ^^
              StableMem.read_and_clear_word32 env ^^
              set_version ^^

              (* check version *)
              get_version ^^
              compile_unboxed_const StableMem.version ^^
              G.i (Compare (Wasm.Values.I32 I32Op.GtU)) ^^
              E.then_trap_with env (Printf.sprintf
                "higher stable memory version (expected %s)"
                (Int32.to_string StableMem.version)) ^^

              (* restore StableMem bytes [0..4) *)
              compile_const_64 0L ^^
              get_M ^^
              compile_add64_const (Int64.sub page_size64 8L) ^^
              StableMem.read_and_clear_word32 env ^^
              StableMem.write_word32 env ^^

              (* restore mem_size *)
              get_M ^^
              compile_add64_const (Int64.sub page_size64 12L) ^^
              StableMem.read_and_clear_word32 env ^^
                G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^ (*TODO: use 64 bits *)
              StableMem.set_mem_size env ^^

              StableMem.get_mem_size env ^^
              compile_shl64_const (Int64.of_int page_size_bits) ^^
              set_N ^^

              (* set len *)
              get_N ^^
              StableMem.read_and_clear_word32 env ^^
              set_len ^^

              (* set offset *)
              get_N ^^
              compile_add64_const 4L ^^
              set_offset
            end
            begin
              (* assert mem_size == 0 *)
              StableMem.get_mem_size env ^^
              G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
              E.else_trap_with env "unexpected, non-zero stable memory size" ^^

              (* set len *)
              get_marker ^^
              set_len ^^

              (* set offset *)
              compile_const_64 4L ^^
              set_offset
            end ^^ (* if_ *)

          let (set_blob, get_blob) = new_local env "blob" in
          (* read blob from stable memory *)
          get_len ^^ Blob.alloc env ^^ set_blob ^^
          get_blob ^^ Blob.payload_ptr_unskewed ^^ (G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32))) ^^
          get_offset ^^
          get_len ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
          IC.system_call env "ic0" "stable64_read" ^^

          let (set_val, get_val) = new_local env "val" in
          (* deserialize blob to val *)
          get_blob ^^
          Serialization.deserialize_from_blob true env [ty] ^^
          set_val ^^

          (* clear blob contents *)
          get_blob ^^
          Blob.clear env ^^

          (* copy zeros from blob to stable memory *)
          get_offset ^^
          get_blob ^^ Blob.payload_ptr_unskewed ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
          get_blob ^^ Blob.len env ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
          IC.system_call env "ic0" "stable64_write" ^^

          (* return val *)
          get_val
        end
    | _ -> assert false
end

module GC = struct

  let register env static_roots =

    let get_static_roots = E.add_fun env "get_static_roots" (Func.of_body env [] [I32Type] (fun env ->
      compile_unboxed_const static_roots
    )) in

    E.add_export env (nr {
      name = Wasm.Utf8.decode "get_static_roots";
      edesc = nr (FuncExport (nr get_static_roots))
    })

  let store_static_roots env =
    Arr.vanilla_lit env (E.get_static_roots env)

end (* GC *)

module StackRep = struct
  open SR

  (*
     Most expressions have a “preferred”, most optimal, form. Hence,
     compile_exp put them on the stack in that form, and also returns
     the form it chose.

     But the users of compile_exp usually want a specific form as well.
     So they use compile_exp_as, indicating the form they expect.
     compile_exp_as then does the necessary coercions.
   *)

  let of_arity n =
    if n = 1 then Vanilla else UnboxedTuple n

  (* The stack rel of a primitive type, i.e. what the binary operators expect *)
  let of_type t =
    let open Type in
    match normalize t with
    | Prim Bool -> SR.bool
    | Prim (Nat | Int) -> Vanilla
    | Prim (Nat64 | Int64) -> UnboxedWord64
    | Prim (Nat32 | Int32) -> UnboxedWord32
    | Prim (Nat8 | Nat16 | Int8 | Int16 | Char) -> Vanilla
    | Prim (Text | Blob | Principal) -> Vanilla
    | Prim Float -> UnboxedFloat64
    | Obj (Actor, _) -> Vanilla
    | Func (Shared _, _, _, _, _) -> Vanilla
    | p -> todo "StackRep.of_type" (Arrange_ir.typ p) Vanilla

  let to_block_type env = function
    | Vanilla -> [I32Type]
    | UnboxedBool -> [I32Type]
    | UnboxedWord64 -> [I64Type]
    | UnboxedWord32 -> [I32Type]
    | UnboxedFloat64 -> [F64Type]
    | UnboxedTuple n ->
      if n > 1 then assert !Flags.multi_value;
      Lib.List.make n I32Type
    | Const _ -> []
    | Unreachable -> []

  let to_string = function
    | Vanilla -> "Vanilla"
    | UnboxedBool -> "UnboxedBool"
    | UnboxedWord64 -> "UnboxedWord64"
    | UnboxedWord32 -> "UnboxedWord32"
    | UnboxedFloat64 -> "UnboxedFloat64"
    | UnboxedTuple n -> Printf.sprintf "UnboxedTuple %d" n
    | Unreachable -> "Unreachable"
    | Const _ -> "Const"

  let join (sr1 : t) (sr2 : t) = match sr1, sr2 with
    | _, _ when SR.eq sr1 sr2 -> sr1
    | Unreachable, sr2 -> sr2
    | sr1, Unreachable -> sr1
    | UnboxedWord64, UnboxedWord64 -> UnboxedWord64
    | UnboxedTuple n, UnboxedTuple m when n = m -> sr1
    | _, Vanilla -> Vanilla
    | Vanilla, _ -> Vanilla
    | Const _, Const _ -> Vanilla

    | Const _, UnboxedBool -> UnboxedBool
    | UnboxedBool, Const _ -> UnboxedBool
    | Const _, UnboxedWord32 -> UnboxedWord32
    | UnboxedWord32, Const _ -> UnboxedWord32
    | Const _, UnboxedWord64 -> UnboxedWord64
    | UnboxedWord64, Const _ -> UnboxedWord64
    | Const _, UnboxedFloat64 -> UnboxedFloat64
    | UnboxedFloat64, Const _ -> UnboxedFloat64

    | Const _, UnboxedTuple 0 -> UnboxedTuple 0
    | UnboxedTuple 0, Const _-> UnboxedTuple 0
    | _, _ ->
      Printf.eprintf "Invalid stack rep join (%s, %s)\n"
        (to_string sr1) (to_string sr2); sr1

  (* This is used when two blocks join, e.g. in an if. In that
     case, they cannot return multiple values. *)
  let relax =
    if !Flags.multi_value
    then fun sr -> sr
    else function
      | UnboxedTuple n when n > 1 -> Vanilla
      | sr -> sr

  let drop env (sr_in : t) =
    match sr_in with
    | Vanilla | UnboxedBool | UnboxedWord64 | UnboxedWord32 | UnboxedFloat64 -> G.i Drop
    | UnboxedTuple n -> G.table n (fun _ -> G.i Drop)
    | Const _ | Unreachable -> G.nop

  (* Materializes a Const.lit: If necessary, puts
     bytes into static memory, and returns a vanilla value.
  *)
  let materialize_lit env (lit : Const.lit) : int32 =
    match lit with
      | Const.Vanilla n  -> n
      | Const.Bool n     -> Bool.vanilla_lit n
      | Const.BigInt n   -> BigNum.vanilla_lit env n
      | Const.Word32 n   -> BoxedSmallWord.vanilla_lit env n
      | Const.Word64 n   -> BoxedWord64.vanilla_lit env n
      | Const.Float64 f  -> Float.vanilla_lit env f
      | Const.Blob t     -> Blob.vanilla_lit env t

  let rec materialize_const_t env (p, cv) : int32 =
    Lib.Promise.lazy_value p (fun () -> materialize_const_v env cv)

  and materialize_const_v env = function
    | Const.Fun get_fi -> Closure.static_closure env (get_fi ())
    | Const.Message fi -> assert false
    | Const.Obj fs ->
      let fs' = List.map (fun (n, c) -> (n, materialize_const_t env c)) fs in
      Object.vanilla_lit env fs'
    | Const.Unit -> Tuple.unit_vanilla_lit
    | Const.Array cs ->
      let ptrs = List.map (materialize_const_t env) cs in
      Arr.vanilla_lit env ptrs
    | Const.Lit l -> materialize_lit env l

  let adjust env (sr_in : t) sr_out =
    if eq sr_in sr_out
    then G.nop
    else match sr_in, sr_out with
    | Unreachable, Unreachable -> G.nop
    | Unreachable, _ -> G.i Unreachable

    | UnboxedTuple n, Vanilla -> Tuple.from_stack env n
    | Vanilla, UnboxedTuple n -> Tuple.to_stack env n

    | UnboxedBool, Vanilla -> BitTagged.tag_i32
    | Vanilla, UnboxedBool -> BitTagged.untag_i32

    | UnboxedWord64, Vanilla -> BoxedWord64.box env
    | Vanilla, UnboxedWord64 -> BoxedWord64.unbox env

    | UnboxedWord32, Vanilla -> BoxedSmallWord.box env
    | Vanilla, UnboxedWord32 -> BoxedSmallWord.unbox env

    | UnboxedFloat64, Vanilla -> Float.box env
    | Vanilla, UnboxedFloat64 -> Float.unbox env

    | Const c, Vanilla -> compile_unboxed_const (materialize_const_t env c)
    | Const (_, Const.Lit (Const.Bool b)), UnboxedBool -> Bool.lit b
    | Const (_, Const.Lit (Const.Word32 n)), UnboxedWord32 -> compile_unboxed_const n
    | Const (_, Const.Lit (Const.Word64 n)), UnboxedWord64 -> compile_const_64 n
    | Const (_, Const.Lit (Const.Float64 f)), UnboxedFloat64 -> Float.compile_unboxed_const f
    | Const c, UnboxedTuple 0 -> G.nop
    | Const (_, Const.Array cs), UnboxedTuple n ->
      assert (n = List.length cs);
      G.concat_map (fun c -> compile_unboxed_const (materialize_const_t env c)) cs
    | _, _ ->
      Printf.eprintf "Unknown stack_rep conversion %s -> %s\n"
        (to_string sr_in) (to_string sr_out);
      G.nop

end (* StackRep *)

module VarEnv = struct

  (* A type to record where Motoko names are stored. *)
  type varloc =
    (* A Wasm Local of the current function, directly containing the value
       (note that most values are pointers, but not all)
       Used for immutable and mutable, non-captured data *)
    | Local of int32
    (* A Wasm Local of the current function, that points to memory location,
       which is a MutBox.  Used for mutable captured data *)
    | HeapInd of int32
    (* A static mutable memory location (static address of a MutBox object) *)
    (* TODO: Do we need static immutable? *)
    | HeapStatic of int32
    (* Not materialized (yet), statically known constant, static location on demand *)
    | Const of Const.t
    (* public method *)
    | PublicMethod of int32 * string

  let is_non_local : varloc -> bool = function
    | Local _
    | HeapInd _ -> false
    | HeapStatic _
    | PublicMethod _
    | Const _ -> true

  type lvl = TopLvl | NotTopLvl

  (*
  The source variable environment:
   - Whether we are on the top level
   - In-scope variables
   - scope jump labels
  *)


  module NameEnv = Env.Make(String)
  type t = {
    lvl : lvl;
    vars : varloc NameEnv.t; (* variables ↦ their location *)
    labels : G.depth NameEnv.t; (* jump label ↦ their depth *)
  }

  let empty_ae = {
    lvl = TopLvl;
    vars = NameEnv.empty;
    labels = NameEnv.empty;
  }

  (* Creating a local environment, resetting the local fields,
     and removing bindings for local variables (unless they are at global locations)
  *)

  let mk_fun_ae ae = { ae with
    lvl = NotTopLvl;
    vars = NameEnv.filter (fun v l ->
      let non_local = is_non_local l in
      (* For debugging, enable this:
      (if not non_local then Printf.eprintf "VarEnv.mk_fun_ae: Removing %s\n" v);
      *)
      non_local
    ) ae.vars;
  }
  let lookup_var ae var =
    match NameEnv.find_opt var ae.vars with
      | Some l -> Some l
      | None   -> Printf.eprintf "Could not find %s\n" var; None

  let needs_capture ae var = match lookup_var ae var with
    | Some l -> not (is_non_local l)
    | None -> assert false

  let add_local_with_heap_ind env (ae : t) name =
      let i = E.add_anon_local env I32Type in
      E.add_local_name env i name;
      ({ ae with vars = NameEnv.add name (HeapInd i) ae.vars }, i)

  let add_local_heap_static (ae : t) name ptr =
      { ae with vars = NameEnv.add name (HeapStatic ptr) ae.vars }

  let add_local_public_method (ae : t) name (fi, exported_name) =
      { ae with vars = NameEnv.add name (PublicMethod (fi, exported_name) : varloc) ae.vars }

  let add_local_const (ae : t) name cv =
      { ae with vars = NameEnv.add name (Const cv : varloc) ae.vars }

  let add_local_local env (ae : t) name i =
      { ae with vars = NameEnv.add name (Local i) ae.vars }

  let add_direct_local env (ae : t) name =
      let i = E.add_anon_local env I32Type in
      E.add_local_name env i name;
      (add_local_local env ae name i, i)

  (* Adds the names to the environment and returns a list of setters *)
  let rec add_arguments env (ae : t) as_local = function
    | [] -> ae
    | (name :: names) ->
      if as_local name then
        let i = E.add_anon_local env I32Type in
        E.add_local_name env i name;
        let ae' = { ae with vars = NameEnv.add name (Local i) ae.vars } in
        add_arguments env ae' as_local names
      else (* needs to go to static memory *)
        let ptr = MutBox.static env in
        let ae' = add_local_heap_static ae name ptr in
        add_arguments env ae' as_local names

  let add_argument_locals env (ae : t) =
    add_arguments env ae (fun _ -> true)

  let add_label (ae : t) name (d : G.depth) =
      { ae with labels = NameEnv.add name d ae.labels }

  let get_label_depth (ae : t) name : G.depth  =
    match NameEnv.find_opt name ae.labels with
      | Some d -> d
      | None   -> raise (CodegenError (Printf.sprintf "Could not find %s\n" name))

end (* VarEnv *)

(* type for wrapping code with context, context is establishment
   of (pattern) binding, argument is the code using the binding,
   result is e.g. the code for `case p e`. *)
type scope_wrap = G.t -> G.t

let unmodified : scope_wrap = fun code -> code

module Var = struct
  (* This module is all about looking up Motoko variables in the environment,
     and dealing with mutable variables *)

  open VarEnv

  (* Stores the payload (which is found on the stack) *)
  let set_val env ae var = match VarEnv.lookup_var ae var with
    | Some (Local i) ->
      G.i (LocalSet (nr i))
    | Some (HeapInd i) ->
      let (set_new_val, get_new_val) = new_local env "new_val" in
      set_new_val ^^
      G.i (LocalGet (nr i)) ^^
      get_new_val ^^
      Heap.store_field MutBox.field
    | Some (HeapStatic ptr) ->
      let (set_new_val, get_new_val) = new_local env "new_val" in
      set_new_val ^^
      compile_unboxed_const ptr ^^
      get_new_val ^^
      Heap.store_field MutBox.field
    | Some (Const _) -> fatal "set_val: %s is const" var
    | Some (PublicMethod _) -> fatal "set_val: %s is PublicMethod" var
    | None   -> fatal "set_val: %s missing" var

  (* Returns the payload (optimized representation) *)
  let get_val (env : E.t) (ae : VarEnv.t) var = match VarEnv.lookup_var ae var with
    | Some (Local i) ->
      SR.Vanilla, G.i (LocalGet (nr i))
    | Some (HeapInd i) ->
      SR.Vanilla, G.i (LocalGet (nr i)) ^^ Heap.load_field MutBox.field
    | Some (HeapStatic i) ->
      SR.Vanilla, compile_unboxed_const i ^^ Heap.load_field MutBox.field
    | Some (Const c) ->
      SR.Const c, G.nop
    | Some (PublicMethod (_, name)) ->
      SR.Vanilla,
      IC.get_self_reference env ^^
      IC.actor_public_field env name
    | None -> assert false

  (* Returns the payload (vanilla representation) *)
  let get_val_vanilla (env : E.t) (ae : VarEnv.t) var =
    let sr, code = get_val env ae var in
    code ^^ StackRep.adjust env sr SR.Vanilla

  (* Returns the value to put in the closure,
     and code to restore it, including adding to the environment
  *)
  let capture old_env ae0 var : G.t * (E.t -> VarEnv.t -> VarEnv.t * scope_wrap) =
    match VarEnv.lookup_var ae0 var with
    | Some (Local i) ->
      ( G.i (LocalGet (nr i))
      , fun new_env ae1 ->
        let ae2, j = VarEnv.add_direct_local new_env ae1 var in
        let restore_code = G.i (LocalSet (nr j))
        in ae2, fun body -> restore_code ^^ body
      )
    | Some (HeapInd i) ->
      ( G.i (LocalGet (nr i))
      , fun new_env ae1 ->
        let ae2, j = VarEnv.add_local_with_heap_ind new_env ae1 var in
        let restore_code = G.i (LocalSet (nr j))
        in ae2, fun body -> restore_code ^^ body
      )
    | _ -> assert false

  (* This is used when putting a mutable field into an object.
     In the IR, mutable fields of objects can alias pre-allocated
     MutBox objects, to allow the async/await. So if we already have
     this variable in a MutBox, then use that, else create a new one.
  *)
  let get_aliased_box env ae var = match VarEnv.lookup_var ae var with
    | Some (HeapInd i) -> G.i (LocalGet (nr i))
    | Some (HeapStatic _) -> assert false (* we never do this on the toplevel *)
    | _  -> Tagged.obj env Tagged.ObjInd [ get_val_vanilla env ae var ]

end (* Var *)

(* Calling well-known prelude functions *)
(* FIXME: calling into the prelude will not work if we ever need to compile a program
   that requires top-level cps conversion;
   use new prims instead *)
module Internals = struct
  let call_prelude_function env ae var =
    match VarEnv.lookup_var ae var with
    | Some (VarEnv.Const(_, Const.Fun mk_fi)) ->
       compile_unboxed_zero ^^ (* A dummy closure *)
       G.i (Call (nr (mk_fi ())))
    | _ -> assert false

  let add_cycles env ae = call_prelude_function env ae "@add_cycles"
  let reset_cycles env ae = call_prelude_function env ae "@reset_cycles"
  let reset_refund env ae = call_prelude_function env ae "@reset_refund"
end

(* This comes late because it also deals with messages *)
module FuncDec = struct
  let bind_args env ae0 first_arg args =
    let rec go i ae = function
    | [] -> ae
    | a::args ->
      let ae' = VarEnv.add_local_local env ae a.it (Int32.of_int i) in
      go (i+1) ae' args in
    go first_arg ae0 args

  (* Create a WebAssembly func from a pattern (for the argument) and the body.
   Parameter `captured` should contain the, well, captured local variables that
   the function will find in the closure. *)
  let compile_local_function outer_env outer_ae restore_env args mk_body ret_tys at =
    let arg_names = List.map (fun a -> a.it, I32Type) args in
    let return_arity = List.length ret_tys in
    let retty = Lib.List.make return_arity I32Type in
    let ae0 = VarEnv.mk_fun_ae outer_ae in
    Func.of_body outer_env (["clos", I32Type] @ arg_names) retty (fun env -> G.with_region at (
      let get_closure = G.i (LocalGet (nr 0l)) in

      let ae1, closure_codeW = restore_env env ae0 get_closure in

      (* Add arguments to the environment (shifted by 1) *)
      let ae2 = bind_args env ae1 1 args in

      closure_codeW (mk_body env ae2)
    ))

  let message_start env sort = match sort with
      | Type.Shared Type.Write ->
        Lifecycle.trans env Lifecycle.InUpdate
      | Type.Shared Type.Query ->
        Lifecycle.trans env Lifecycle.InQuery
      | _ -> assert false

  let message_cleanup env sort = match sort with
      | Type.Shared Type.Write ->
        E.collect_garbage env ^^
        Lifecycle.trans env Lifecycle.Idle
      | Type.Shared Type.Query ->
        Lifecycle.trans env Lifecycle.PostQuery
      | _ -> assert false

  let compile_const_message outer_env outer_ae sort control args mk_body ret_tys at : E.func_with_names =
    let ae0 = VarEnv.mk_fun_ae outer_ae in
    Func.of_body outer_env [] [] (fun env -> G.with_region at (
      message_start env sort ^^
      (* cycles *)
      Internals.reset_cycles env outer_ae ^^
      Internals.reset_refund env outer_ae ^^
      (* reply early for a oneway *)
      (if control = Type.Returns
       then
         Tuple.compile_unit ^^
         Serialization.serialize env [] ^^
         IC.reply_with_data env
       else G.nop) ^^
      (* Deserialize argument and add params to the environment *)
      let arg_names = List.map (fun a -> a.it) args in
      let arg_tys = List.map (fun a -> a.note) args in
      let ae1 = VarEnv.add_argument_locals env ae0 arg_names in
      Serialization.deserialize env arg_tys ^^
      G.concat_map (Var.set_val env ae1) (List.rev arg_names) ^^
      mk_body env ae1 ^^
      message_cleanup env sort
    ))

  (* Compile a closed function declaration (captures no local variables) *)
  let closed pre_env sort control name args mk_body ret_tys at =
    if Type.is_shared_sort sort
    then begin
      let (fi, fill) = E.reserve_fun pre_env name in
      ( Const.t_of_v (Const.Message fi), fun env ae ->
        fill (compile_const_message env ae sort control args mk_body ret_tys at)
      )
    end else begin
      assert (control = Type.Returns);
      let lf = E.make_lazy_function pre_env name in
      ( Const.t_of_v (Const.Fun (fun () -> Lib.AllocOnUse.use lf)), fun env ae ->
        let restore_no_env _env ae _ = ae, unmodified in
        Lib.AllocOnUse.def lf (lazy (compile_local_function env ae restore_no_env args mk_body ret_tys at))
      )
    end

  (* Compile a closure declaration (captures local variables) *)
  let closure env ae sort control name captured args mk_body ret_tys at =
      let is_local = sort = Type.Local in

      let set_clos, get_clos = new_local env (name ^ "_clos") in

      let len = Wasm.I32.of_int_u (List.length captured) in
      let store_env, restore_env =
        let rec go i = function
          | [] -> (G.nop, fun _env ae1 _ -> ae1, unmodified)
          | (v::vs) ->
              let store_rest, restore_rest = go (i + 1) vs in
              let store_this, restore_this = Var.capture env ae v in
              let store_env =
                get_clos ^^
                store_this ^^
                Closure.store_data (Wasm.I32.of_int_u i) ^^
                store_rest in
              let restore_env env ae1 get_env =
                let ae2, codeW = restore_this env ae1 in
                let ae3, code_restW = restore_rest env ae2 get_env in
                (ae3,
                 fun body ->
                 get_env ^^
                 Closure.load_data (Wasm.I32.of_int_u i) ^^
                 codeW (code_restW body)
                )
              in store_env, restore_env in
        go 0 captured in

      let f =
        if is_local
        then compile_local_function env ae restore_env args mk_body ret_tys at
        else assert false (* no first class shared functions yet *) in

      let fi = E.add_fun env name f in

      let code =
        (* Allocate a heap object for the closure *)
        Heap.alloc env (Int32.add Closure.header_size len) ^^
        set_clos ^^

        (* Store the tag *)
        get_clos ^^
        Tagged.(store Closure) ^^

        (* Store the function pointer number: *)
        get_clos ^^
        compile_unboxed_const (E.add_fun_ptr env fi) ^^
        Heap.store_field Closure.funptr_field ^^

        (* Store the length *)
        get_clos ^^
        compile_unboxed_const len ^^
        Heap.store_field Closure.len_field ^^

        (* Store all captured values *)
        store_env
      in

      if is_local
      then
        SR.Vanilla,
        code ^^
        get_clos
      else assert false (* no first class shared functions *)

  let lit env ae name sort control free_vars args mk_body ret_tys at =
    let captured = List.filter (VarEnv.needs_capture ae) free_vars in

    if ae.VarEnv.lvl = VarEnv.TopLvl then assert (captured = []);

    if captured = []
    then
      let (ct, fill) = closed env sort control name args mk_body ret_tys at in
      fill env ae;
      (SR.Const ct, G.nop)
    else closure env ae sort control name captured args mk_body ret_tys at

  (* Returns a closure corresponding to a future (async block) *)
  let async_body env ae ts free_vars mk_body at =
    (* We compile this as a local, returning function, so set return type to [] *)
    let sr, code = lit env ae "anon_async" Type.Local Type.Returns free_vars [] mk_body [] at in
    code ^^
    StackRep.adjust env sr SR.Vanilla

  (* Takes the reply and reject callbacks, tuples them up (with administrative extras),
     adds them to the continuation table, and returns the two callbacks expected by
     ic.call_new.

     The tupling is necessary because we want to free _both_/_all_ closures
     when the call is answered.

     The reply callback function exists once per type (as it has to do
     deserialization); the reject callback function is unique.
  *)

  let closures_to_reply_reject_callbacks_aux env ts_opt =
    let arity, reply_name, from_arg_data =
      match ts_opt with
      | Some ts ->
        (List.length ts,
         "@callback<" ^ Typ_hash.typ_hash (Type.Tup ts) ^ ">",
         fun env -> Serialization.deserialize env ts)
      | None ->
        (1,
         "@callback",
         (fun env ->
           Blob.of_size_copy env
           (fun env -> IC.system_call env "ic0" "msg_arg_data_size")
           (fun env -> IC.system_call env "ic0" "msg_arg_data_copy")
           (fun env -> compile_unboxed_const 0l)))
    in
    Func.define_built_in env reply_name ["env", I32Type] [] (fun env ->
        message_start env (Type.Shared Type.Write) ^^
        (* Look up continuation *)
        let (set_closure, get_closure) = new_local env "closure" in
        G.i (LocalGet (nr 0l)) ^^
        ContinuationTable.recall env ^^
        Arr.load_field 0l ^^ (* get the reply closure *)
        set_closure ^^
        get_closure ^^

        (* Deserialize/Blobify reply arguments  *)
        from_arg_data env ^^

        get_closure ^^
        Closure.call_closure env arity 0 ^^

        message_cleanup env (Type.Shared Type.Write)
      );

    let reject_name = "@reject_callback" in
    Func.define_built_in env reject_name ["env", I32Type] [] (fun env ->
        message_start env (Type.Shared Type.Write) ^^
        (* Look up continuation *)
        let (set_closure, get_closure) = new_local env "closure" in
        G.i (LocalGet (nr 0l)) ^^
        ContinuationTable.recall env ^^
        Arr.load_field 1l ^^ (* get the reject closure *)
        set_closure ^^
        get_closure ^^
        (* Synthesize value of type `Text`, the error message
           (The error code is fetched via a prim)
        *)
        IC.error_value env ^^

        get_closure ^^
        Closure.call_closure env 1 0 ^^

        message_cleanup env (Type.Shared Type.Write)
      );

    (* result is a function that accepts a list of closure getters, from which
       the first and second must be the reply and reject continuations. *)
    fun closure_getters ->
      let (set_cb_index, get_cb_index) = new_local env "cb_index" in
      Arr.lit env closure_getters ^^
      ContinuationTable.remember env ^^
      set_cb_index ^^

      (* return arguments for the ic.call *)
      compile_unboxed_const (E.add_fun_ptr env (E.built_in env reply_name)) ^^
      get_cb_index ^^
      compile_unboxed_const (E.add_fun_ptr env (E.built_in env reject_name)) ^^
      get_cb_index

  let closures_to_reply_reject_callbacks env ts =
    closures_to_reply_reject_callbacks_aux env (Some ts)
  let closures_to_raw_reply_reject_callbacks env  =
    closures_to_reply_reject_callbacks_aux env None

  let ignoring_callback env =
    (* for one-way calls, we use an invalid table entry as the callback. this
       way, the callback, when it comes back, will (safely) trap, even if the
       module has completely changed in between. This way, one-way calls do not
       get in the way of safe instantaneous upgrades *)
    compile_unboxed_const (-1l)

  let cleanup_callback env =
    let name = "@cleanup_callback" in
    Func.define_built_in env name ["env", I32Type] [] (fun env ->
        G.i (LocalGet (nr 0l)) ^^
        ContinuationTable.recall env ^^
        G.i Drop);
    compile_unboxed_const (E.add_fun_ptr env (E.built_in env name))

  let ic_call_threaded env purpose get_meth_pair push_continuations add_data add_cycles =
    match E.mode env with
    | Flags.ICMode
    | Flags.RefMode ->
      let (set_cb_index, get_cb_index) = new_local env "cb_index" in
      (* The callee *)
      get_meth_pair ^^ Arr.load_field 0l ^^ Blob.as_ptr_len env ^^
      (* The method name *)
      get_meth_pair ^^ Arr.load_field 1l ^^ Blob.as_ptr_len env ^^
      (* The reply and reject callback *)
      push_continuations ^^
      set_cb_index  ^^ get_cb_index ^^
      (* initiate call *)
      IC.system_call env "ic0" "call_new" ^^
      cleanup_callback env ^^ get_cb_index ^^
      IC.system_call env "ic0" "call_on_cleanup" ^^
      (* the data *)
      add_data get_cb_index ^^
      IC.system_call env "ic0" "call_data_append" ^^
      (* the cycles *)
      add_cycles ^^
      (* done! *)
      IC.system_call env "ic0" "call_perform" ^^
      (* Check error code *)
      G.i (Test (Wasm.Values.I32 I32Op.Eqz)) ^^
      E.else_trap_with env (Printf.sprintf "could not perform %s" purpose)
    | _ ->
      E.trap_with env (Printf.sprintf "cannot perform %s when running locally" purpose)

  let ic_call env ts1 ts2 get_meth_pair get_arg get_k get_r =
    ic_call_threaded
      env
      "remote call"
      get_meth_pair
      (closures_to_reply_reject_callbacks env ts2 [get_k; get_r])
      (fun _ -> get_arg ^^ Serialization.serialize env ts1)

  let ic_call_raw env get_meth_pair get_arg get_k get_r =
    ic_call_threaded
      env
      "raw call"
      get_meth_pair
      (closures_to_raw_reply_reject_callbacks env [get_k; get_r])
      (fun _ -> get_arg ^^ Blob.as_ptr_len env)

  let ic_self_call env ts get_meth_pair get_future get_k get_r =
    ic_call_threaded
      env
      "self call"
      get_meth_pair
      (* Storing the tuple away, future_array_index = 2, keep in sync with rts/continuation_table.rs *)
      (closures_to_reply_reject_callbacks env ts [get_k; get_r; get_future])
      (fun get_cb_index ->
        get_cb_index ^^
        BoxedSmallWord.box env ^^
        Serialization.serialize env Type.[Prim Nat32])

  let ic_call_one_shot env ts get_meth_pair get_arg add_cycles =
    match E.mode env with
    | Flags.ICMode
    | Flags.RefMode ->
      (* The callee *)
      get_meth_pair ^^ Arr.load_field 0l ^^ Blob.as_ptr_len env ^^
      (* The method name *)
      get_meth_pair ^^ Arr.load_field 1l ^^ Blob.as_ptr_len env ^^
      (* The reply callback *)
      ignoring_callback env ^^
      compile_unboxed_zero ^^
      (* The reject callback *)
      ignoring_callback env ^^
      compile_unboxed_zero ^^
      IC.system_call env "ic0" "call_new" ^^
      (* the data *)
      get_arg ^^ Serialization.serialize env ts ^^
      IC.system_call env "ic0" "call_data_append" ^^
      (* the cycles *)
      add_cycles ^^
      IC.system_call env "ic0" "call_perform" ^^
      (* This is a one-shot function: Ignore error code *)
      G.i Drop
    | _ -> assert false

  let equate_msgref env =
    let (set_meth_pair1, get_meth_pair1) = new_local env "meth_pair1" in
    let (set_meth_pair2, get_meth_pair2) = new_local env "meth_pair2" in
    set_meth_pair2 ^^ set_meth_pair1 ^^
    get_meth_pair1 ^^ Arr.load_field 0l ^^
    get_meth_pair2 ^^ Arr.load_field 0l ^^
    Blob.compare env Operator.EqOp ^^
    G.if1 I32Type
    begin
      get_meth_pair1 ^^ Arr.load_field 1l ^^
      get_meth_pair2 ^^ Arr.load_field 1l ^^
      Blob.compare env Operator.EqOp
    end
    begin
      Bool.lit false
    end

  let export_async_method env =
    let name = IC.async_method_name in
    begin match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.define_built_in env name [] [] (fun env ->
        let (set_closure, get_closure) = new_local env "closure" in

        message_start env (Type.Shared Type.Write) ^^

        (* Check that we are calling this *)
        IC.assert_caller_self env ^^

        (* Deserialize and look up continuation argument *)
        Serialization.deserialize env Type.[Prim Nat32] ^^
        BoxedSmallWord.unbox env ^^
        ContinuationTable.peek_future env ^^
        set_closure ^^ get_closure ^^ get_closure ^^
        Closure.call_closure env 0 0 ^^
        message_cleanup env (Type.Shared Type.Write)
      );

      let fi = E.built_in env name in
      E.add_export env (nr {
        name = Wasm.Utf8.decode ("canister_update " ^ name);
        edesc = nr (FuncExport (nr fi))
      })
    | _ -> ()
    end

end (* FuncDec *)


module PatCode = struct
  (* Pattern failure code on demand.

  Patterns in general can fail, so we want a block around them with a
  jump-label for the fail case. But many patterns cannot fail, in particular
  function arguments that are simple variables. In these cases, we do not want
  to create the block and the (unused) jump label. So we first generate the
  code, either as plain code (CannotFail) or as code with hole for code to fun
  in case of failure (CanFail).
  *)

  type patternCode =
    | CannotFail of G.t
    | CanFail of (G.t -> G.t)

  let (^^^) : patternCode -> patternCode -> patternCode = function
    | CannotFail is1 ->
      begin function
      | CannotFail is2 -> CannotFail (is1 ^^ is2)
      | CanFail is2 -> CanFail (fun k -> is1 ^^ is2 k)
      end
    | CanFail is1 ->
      begin function
      | CannotFail is2 -> CanFail (fun k ->  is1 k ^^ is2)
      | CanFail is2 -> CanFail (fun k -> is1 k ^^ is2 k)
      end

  let with_fail (fail_code : G.t) : patternCode -> G.t = function
    | CannotFail is -> is
    | CanFail is -> is fail_code

  let orElse : patternCode -> patternCode -> patternCode = function
    | CannotFail is1 -> fun _ -> CannotFail is1
    | CanFail is1 -> function
      | CanFail is2 -> CanFail (fun fail_code ->
          let inner_fail = G.new_depth_label () in
          let inner_fail_code = Bool.lit false ^^ G.branch_to_ inner_fail in
          G.labeled_block1 I32Type inner_fail (is1 inner_fail_code ^^ Bool.lit true) ^^
          G.if0 G.nop (is2 fail_code)
        )
      | CannotFail is2 -> CannotFail (
          let inner_fail = G.new_depth_label () in
          let inner_fail_code = Bool.lit false ^^ G.branch_to_ inner_fail in
          G.labeled_block1 I32Type inner_fail (is1 inner_fail_code ^^ Bool.lit true) ^^
          G.if0 G.nop is2
        )

  let orTrap env = with_fail (E.trap_with env "pattern failed")

  let with_region at = function
    | CannotFail is -> CannotFail (G.with_region at is)
    | CanFail is -> CanFail (fun k -> G.with_region at (is k))

end (* PatCode *)
open PatCode

(* All the code above is independent of the IR *)
open Ir

module AllocHow = struct
  (*
  When compiling a (recursive) block, we need to do a dependency analysis, to
  find out how the things are allocated. The options are:
  - const:  completely known, constant, not stored anywhere (think static function)
            (no need to mention in a closure)
  - local:  only needed locally, stored in a Wasm local, immutable
            (can be copied into a closure by value)
  - local mutable: only needed locally, stored in a Wasm local, mutable
            (cannot be copied into a closure)
  - heap allocated: stored on the dynamic heap, address in Wasm local
            (can be copied into a closure by reference)
  - static heap: stored on the static heap, address known statically
            (no need to mention in a closure)

  The goal is to avoid dynamic allocation where possible (and use locals), and
  to avoid turning function references into closures.

  The rules are:
  - functions are const, unless they capture something that is not a const
    function or a static heap allocation.
    in particular, top-level functions are always const
  - everything that is captured on the top-level needs to be statically
    heap-allocated
  - everything that is captured before it is defined, or is captured and mutable
    needs to be dynamically heap-allocated
  - the rest can be local
  *)

  module M = Freevars.M
  module S = Freevars.S

  (*
  We represent this as a lattice as follows:
  *)
  type how = Const | LocalImmut | LocalMut | StoreHeap | StoreStatic
  type allocHow = how M.t

  let disjoint_union : allocHow -> allocHow -> allocHow =
    M.union (fun v _ _ -> fatal "AllocHow.disjoint_union: %s" v)

  let join : allocHow -> allocHow -> allocHow =
    M.union (fun _ x y -> Some (match x, y with
      | StoreStatic, StoreHeap | StoreHeap, StoreStatic
      ->  fatal "AllocHow.join: cannot join StoreStatic and StoreHeap"

      | _, StoreHeap   | StoreHeap,   _ -> StoreHeap
      | _, StoreStatic | StoreStatic, _ -> StoreStatic
      | _, LocalMut    | LocalMut,    _ -> LocalMut
      | _, LocalImmut  | LocalImmut,  _ -> LocalImmut

      | Const, Const -> Const
    ))
  let joins = List.fold_left join M.empty

  let map_of_set = Freevars.map_of_set
  let set_of_map = Freevars.set_of_map

  (* Various filters used in the set operations below *)
  let is_local_mut _ = function
    | LocalMut -> true
    | _ -> false

  let is_local _ = function
    | LocalImmut -> true
    | LocalMut -> true
    | _ -> false

  let how_captured lvl how seen captured =
    (* What to do so that we can capture something?
       * For local blocks, put on the dynamic heap:
         - mutable things
         - not yet defined things
       * For top-level blocks, put on the static heap:
         - everything that is non-static (i.e. still in locals)
    *)
    match lvl with
    | VarEnv.NotTopLvl ->
      map_of_set StoreHeap (S.union
        (S.inter (set_of_map (M.filter is_local_mut how)) captured)
        (S.inter (set_of_map (M.filter is_local how)) (S.diff captured seen))
      )
    | VarEnv.TopLvl ->
      map_of_set StoreStatic
        (S.inter (set_of_map (M.filter is_local how)) captured)

  let dec lvl how_outer (seen, how0) dec =
    let how_all = disjoint_union how_outer how0 in

    let (f,d) = Freevars.dec dec in
    let captured = S.inter (set_of_map how0) (Freevars.captured_vars f) in

    (* Which allocation is required for the things defined here? *)
    let how1 = match dec.it with
      (* Mutable variables are, well, mutable *)
      | VarD _ ->
      M.map (fun _t -> LocalMut) d

      (* Constant expressions (trusting static_vals.ml) *)
      | LetD (_, e) when e.note.Note.const ->
      M.map (fun _t -> (Const : how)) d

      (* Everything else needs at least a local *)
      | _ ->
      M.map (fun _t -> LocalImmut) d in

    (* Which allocation does this require for its captured things? *)
    let how2 = how_captured lvl how_all seen captured in

    let how = joins [how0; how1; how2] in
    let seen' = S.union seen (set_of_map d)
    in (seen', how)

  (* find the allocHow for the variables currently in scope *)
  (* we assume things are mutable, as we do not know better here *)
  let how_of_ae ae : allocHow = M.map (function
    | VarEnv.Const _ -> (Const : how)
    | VarEnv.HeapStatic _ -> StoreStatic
    | VarEnv.HeapInd _ -> StoreHeap
    | VarEnv.Local _ -> LocalMut (* conservatively assume immutable *)
    | VarEnv.PublicMethod _ -> LocalMut
    ) ae.VarEnv.vars

  let decs (ae : VarEnv.t) decs captured_in_body : allocHow =
    let lvl = ae.VarEnv.lvl in
    let how_outer = how_of_ae ae in
    let defined_here = snd (Freevars.decs decs) in (* TODO: implement gather_decs more directly *)
    let how_outer = Freevars.diff how_outer defined_here in (* shadowing *)
    let how0 = M.map (fun _t -> (Const : how)) defined_here in
    let captured = S.inter (set_of_map defined_here) captured_in_body in
    let rec go how =
      let seen, how1 = List.fold_left (dec lvl how_outer) (S.empty, how) decs in
      assert (S.equal seen (set_of_map defined_here));
      let how2 = how_captured lvl how1 seen captured in
      let how' = join how1 how2 in
      if M.equal (=) how how' then how' else go how' in
    go how0

  (* Functions to extend the environment (and possibly allocate memory)
     based on how we want to store them. *)
  let add_local env ae how name : VarEnv.t * G.t =
    match M.find name how with
    | (Const : how) -> (ae, G.nop)
    | LocalImmut | LocalMut ->
      let (ae1, i) = VarEnv.add_direct_local env ae name in
      (ae1, G.nop)
    | StoreHeap ->
      let (ae1, i) = VarEnv.add_local_with_heap_ind env ae name in
      let alloc_code = MutBox.alloc env ^^ G.i (LocalSet (nr i)) in
      (ae1, alloc_code)
    | StoreStatic ->
      let ptr = MutBox.static env in
      let ae1 = VarEnv.add_local_heap_static ae name ptr in
      (ae1, G.nop)

end (* AllocHow *)

(* The actual compiler code that looks at the AST *)

(* wraps a bigint in range [0…2^32-1] into range [-2^31…2^31-1] *)
let nat32_to_int32 n =
  let open Big_int in
  if ge_big_int n (power_int_positive_int 2 31)
  then sub_big_int n (power_int_positive_int 2 32)
  else n

(* wraps a bigint in range [0…2^64-1] into range [-2^63…2^63-1] *)
let nat64_to_int64 n =
  let open Big_int in
  if ge_big_int n (power_int_positive_int 2 63)
  then sub_big_int n (power_int_positive_int 2 64)
  else n

let const_lit_of_lit env : Ir.lit -> Const.lit = function
  | BoolLit b     -> Const.Bool b
  | IntLit n
  | NatLit n      -> Const.BigInt (Numerics.Nat.to_big_int n)
  | Int8Lit n     -> Const.Vanilla (TaggedSmallWord.vanilla_lit Type.Int8 (Numerics.Int_8.to_int n))
  | Nat8Lit n     -> Const.Vanilla (TaggedSmallWord.vanilla_lit Type.Nat8 (Numerics.Nat8.to_int n))
  | Int16Lit n    -> Const.Vanilla (TaggedSmallWord.vanilla_lit Type.Int16 (Numerics.Int_16.to_int n))
  | Nat16Lit n    -> Const.Vanilla (TaggedSmallWord.vanilla_lit Type.Nat16 (Numerics.Nat16.to_int n))
  | Int32Lit n    -> Const.Word32 (Big_int.int32_of_big_int (Numerics.Int_32.to_big_int n))
  | Nat32Lit n    -> Const.Word32 (Big_int.int32_of_big_int (nat32_to_int32 (Numerics.Nat32.to_big_int n)))
  | Int64Lit n    -> Const.Word64 (Big_int.int64_of_big_int (Numerics.Int_64.to_big_int n))
  | Nat64Lit n    -> Const.Word64 (Big_int.int64_of_big_int (nat64_to_int64 (Numerics.Nat64.to_big_int n)))
  | CharLit c     -> Const.Vanilla Int32.(shift_left (of_int c) 8)
  | NullLit       -> Const.Vanilla (Opt.null_vanilla_lit env)
  | TextLit t
  | BlobLit t     -> Const.Blob t
  | FloatLit f    -> Const.Float64 f

let const_of_lit env lit =
  Const.t_of_v (Const.Lit (const_lit_of_lit env lit))

let compile_lit env lit =
  SR.Const (const_of_lit env lit), G.nop

let compile_lit_as env sr_out lit =
  let sr_in, code = compile_lit env lit in
  code ^^ StackRep.adjust env sr_in sr_out

module Cycles = struct

  let load_cycles env = Func.share_code1 env "load_cycles" ("ptr", I32Type) [I32Type]
    (fun env get_ptr ->
      get_ptr ^^
      (G.i (Load {ty = I64Type; align = 0; offset = 0l; sz = None })) ^^
      BigNum.from_word64 env ^^
      get_ptr ^^
      compile_add_const 8l ^^
      (G.i (Load {ty = I64Type; align = 0; offset = 0l; sz = None })) ^^
      BigNum.from_word64 env ^^
      (* shift left 64 *)
      compile_lit_as env SR.Vanilla (Ir.NatLit (Numerics.Nat.of_big_int
        (Big_int.power_int_positive_int 2 64))) ^^
      BigNum.compile_mul env ^^ (* TODO: use shift left instead *)
      BigNum.compile_add env)

  let guard env =  Func.share_code1 env "__cycles_guard" ("val", I32Type) []
    (fun env get_val ->
      get_val ^^
      compile_lit_as env SR.Vanilla (Ir.NatLit (Numerics.Nat.of_big_int
        (Big_int.power_int_positive_int 2 128))) ^^
      BigNum.compile_relop env Lt ^^
      E.else_trap_with env "cycles out of bounds")

  let push_high env =  Func.share_code1 env "__cycles_push_high" ("val", I32Type) [I64Type]
    (fun env get_val ->
      get_val ^^
      (* shift right 64 bits *)
      compile_lit_as env SR.Vanilla (Ir.NatLit (Numerics.Nat.of_big_int
        (Big_int.power_int_positive_int 2 64))) ^^
      BigNum.compile_unsigned_div env ^^ (* TODO: use shift right instead *)
      BigNum.truncate_to_word64 env)

  let push_low env =  Func.share_code1 env "__cycles_push_low" ("val", I32Type) [I64Type]
    (fun env get_val ->
      get_val ^^
      BigNum.truncate_to_word64 env)

end

(* helper, traps with message *)
let then_arithmetic_overflow env =
  E.then_trap_with env "arithmetic overflow"

(* The first returned StackRep is for the arguments (expected), the second for the results (produced) *)
let compile_unop env t op =
  let open Operator in
  match op, t with
  | _, Type.Non ->
    SR.Vanilla, SR.Unreachable, G.i Unreachable
  | NegOp, Type.(Prim Int) ->
    SR.Vanilla, SR.Vanilla,
    BigNum.compile_neg env
  | NegOp, Type.(Prim Int64) ->
      SR.UnboxedWord64, SR.UnboxedWord64,
      Func.share_code1 env "neg_trap" ("n", I64Type) [I64Type] (fun env get_n ->
        get_n ^^
        compile_eq64_const 0x8000000000000000L ^^
        then_arithmetic_overflow env ^^
        compile_const_64 0L ^^
        get_n ^^
        G.i (Binary (Wasm.Values.I64 I64Op.Sub))
      )
  | NegOp, Type.(Prim (Int8 | Int16 | Int32)) ->
    StackRep.of_type t, StackRep.of_type t,
    Func.share_code1 env "neg32_trap" ("n", I32Type) [I32Type] (fun env get_n ->
      get_n ^^
      compile_eq_const 0x80000000l ^^
      then_arithmetic_overflow env ^^
      compile_unboxed_zero ^^
      get_n ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Sub))
    )
  | NegOp, Type.(Prim Float) ->
    SR.UnboxedFloat64, SR.UnboxedFloat64,
    let (set_f, get_f) = new_float_local env "f" in
    set_f ^^ Float.compile_unboxed_zero ^^ get_f ^^ G.i (Binary (Wasm.Values.F64 F64Op.Sub))
  | NotOp, Type.(Prim (Nat64|Int64)) ->
     SR.UnboxedWord64, SR.UnboxedWord64,
     compile_const_64 (-1L) ^^
     G.i (Binary (Wasm.Values.I64 I64Op.Xor))
  | NotOp, Type.(Prim (Nat8|Nat16|Nat32|Int8|Int16|Int32 as ty)) ->
     StackRep.of_type t, StackRep.of_type t,
     compile_unboxed_const (TaggedSmallWord.mask_of_type ty) ^^
     G.i (Binary (Wasm.Values.I32 I32Op.Xor))
  | _ ->
    todo "compile_unop"
      (Wasm.Sexpr.Node ("BinOp", [ Arrange_ops.unop op ]))
      (SR.Vanilla, SR.Unreachable, E.trap_with env "TODO: compile_unop")

(* Logarithmic helpers for deciding whether we can carry out operations in constant bitwidth *)

(* helper, traps with message *)
let else_arithmetic_overflow env =
  E.else_trap_with env "arithmetic overflow"

(* helpers to decide if Int64 arithmetic can be carried out on the fast path *)
let additiveInt64_shortcut fast env get_a get_b slow =
  get_a ^^ get_a ^^ compile_shl64_const 1L ^^ G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^ compile_shrU64_const 63L ^^
  get_b ^^ get_b ^^ compile_shl64_const 1L ^^ G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^ compile_shrU64_const 63L ^^
  G.i (Binary (Wasm.Values.I64 I64Op.Or)) ^^
  G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
  G.if1 I64Type
    (get_a ^^ get_b ^^ fast)
    slow

let mulInt64_shortcut fast env get_a get_b slow =
  get_a ^^ get_a ^^ compile_shl64_const 1L ^^ G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^
  get_b ^^ get_b ^^ compile_shl64_const 1L ^^ G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^
  G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
  compile_const_64 65L ^^ G.i (Compare (Wasm.Values.I64 I64Op.GeU)) ^^
  G.if1 I64Type
    (get_a ^^ get_b ^^ fast)
    slow

let powInt64_shortcut fast env get_a get_b slow =
  get_b ^^ G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
  G.if1 I64Type
    (compile_const_64 1L) (* ^0 *)
    begin (* ^(1+n) *)
      get_a ^^ compile_const_64 (-1L) ^^ G.i (Compare (Wasm.Values.I64 I64Op.Eq)) ^^
      G.if1 I64Type
        begin (* -1 ** (1+exp) == if even (1+exp) then 1 else -1 *)
          get_b ^^ compile_const_64 1L ^^
          G.i (Binary (Wasm.Values.I64 I64Op.And)) ^^ G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
          G.if1 I64Type
            (compile_const_64 1L)
            get_a
        end
        begin
          get_a ^^ compile_shrS64_const 1L ^^
          G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
          G.if1 I64Type
            get_a (* {0,1}^(1+n) *)
            begin
              get_b ^^ compile_const_64 64L ^^
              G.i (Compare (Wasm.Values.I64 I64Op.GeU)) ^^ then_arithmetic_overflow env ^^
              get_a ^^ get_a ^^ compile_shl64_const 1L ^^ G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^
              G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^ compile_sub64_const 63L ^^
              get_b ^^ G.i (Binary (Wasm.Values.I64 I64Op.Mul)) ^^
              compile_const_64 (-63L) ^^ G.i (Compare (Wasm.Values.I64 I64Op.GeS)) ^^
              G.if1 I64Type
                (get_a ^^ get_b ^^ fast)
                slow
            end
        end
    end


(* kernel for Int64 arithmetic, invokes estimator for fast path *)
let compile_Int64_kernel env name op shortcut =
  Func.share_code2 env (prim_fun_name Type.Int64 name)
    (("a", I64Type), ("b", I64Type)) [I64Type]
    BigNum.(fun env get_a get_b ->
    shortcut
      env
      get_a
      get_b
      begin
        let (set_res, get_res) = new_local env "res" in
        get_a ^^ from_signed_word64 env ^^
        get_b ^^ from_signed_word64 env ^^
        op env ^^
        set_res ^^ get_res ^^
        fits_signed_bits env 64 ^^
        else_arithmetic_overflow env ^^
        get_res ^^ truncate_to_word64 env
      end)


(* helpers to decide if Nat64 arithmetic can be carried out on the fast path *)
let additiveNat64_shortcut fast env get_a get_b slow =
  get_a ^^ compile_shrU64_const 62L ^^
  get_b ^^ compile_shrU64_const 62L ^^
  G.i (Binary (Wasm.Values.I64 I64Op.Or)) ^^
  G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
  G.if1 I64Type
    (get_a ^^ get_b ^^ fast)
    slow

let mulNat64_shortcut fast env get_a get_b slow =
  get_a ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^
  get_b ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^
  G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
  compile_const_64 64L ^^ G.i (Compare (Wasm.Values.I64 I64Op.GeU)) ^^
  G.if1 I64Type
    (get_a ^^ get_b ^^ fast)
    slow

let powNat64_shortcut fast env get_a get_b slow =
  get_b ^^ G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
  G.if1 I64Type
    (compile_const_64 1L) (* ^0 *)
    begin (* ^(1+n) *)
      get_a ^^ compile_shrU64_const 1L ^^
      G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
      G.if1 I64Type
        get_a (* {0,1}^(1+n) *)
        begin
          get_b ^^ compile_const_64 64L ^^ G.i (Compare (Wasm.Values.I64 I64Op.GeU)) ^^ then_arithmetic_overflow env ^^
          get_a ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^ compile_sub64_const 64L ^^
          get_b ^^ G.i (Binary (Wasm.Values.I64 I64Op.Mul)) ^^ compile_const_64 (-64L) ^^ G.i (Compare (Wasm.Values.I64 I64Op.GeS)) ^^
          G.if1 I64Type
            (get_a ^^ get_b ^^ fast)
            slow
        end
    end


(* kernel for Nat64 arithmetic, invokes estimator for fast path *)
let compile_Nat64_kernel env name op shortcut =
  Func.share_code2 env (prim_fun_name Type.Nat64 name)
    (("a", I64Type), ("b", I64Type)) [I64Type]
    BigNum.(fun env get_a get_b ->
    shortcut
      env
      get_a
      get_b
      begin
        let (set_res, get_res) = new_local env "res" in
        get_a ^^ from_word64 env ^^
        get_b ^^ from_word64 env ^^
        op env ^^
        set_res ^^ get_res ^^
        fits_unsigned_bits env 64 ^^
        else_arithmetic_overflow env ^^
        get_res ^^ truncate_to_word64 env
      end)


(* Compiling Int/Nat32 ops by conversion to/from i64. *)

(* helper, expects i64 on stack *)
let enforce_32_unsigned_bits env =
  compile_bitand64_const 0xFFFFFFFF00000000L ^^
  G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
  else_arithmetic_overflow env

(* helper, expects two identical i64s on stack *)
let enforce_32_signed_bits env =
  compile_shl64_const 1L ^^
  G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^
  enforce_32_unsigned_bits env

let compile_Int32_kernel env name op =
     Func.share_code2 env (prim_fun_name Type.Int32 name)
       (("a", I32Type), ("b", I32Type)) [I32Type]
       (fun env get_a get_b ->
         let (set_res, get_res) = new_local64 env "res" in
         get_a ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendSI32)) ^^
         get_b ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendSI32)) ^^
         G.i (Binary (Wasm.Values.I64 op)) ^^
         set_res ^^ get_res ^^ get_res ^^
         enforce_32_signed_bits env ^^
         get_res ^^ G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)))

let compile_Nat32_kernel env name op =
     Func.share_code2 env (prim_fun_name Type.Nat32 name)
       (("a", I32Type), ("b", I32Type)) [I32Type]
       (fun env get_a get_b ->
         let (set_res, get_res) = new_local64 env "res" in
         get_a ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
         get_b ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
         G.i (Binary (Wasm.Values.I64 op)) ^^
         set_res ^^ get_res ^^
         enforce_32_unsigned_bits env ^^
         get_res ^^ G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)))

(* Customisable kernels for 8/16bit arithmetic via 32 bits. *)

(* helper, expects i32 on stack *)
let enforce_unsigned_bits env n =
  compile_bitand_const Int32.(shift_left minus_one n) ^^
  then_arithmetic_overflow env

let enforce_16_unsigned_bits env = enforce_unsigned_bits env 16

(* helper, expects two identical i32s on stack *)
let enforce_signed_bits env n =
  compile_shl_const 1l ^^ G.i (Binary (Wasm.Values.I32 I32Op.Xor)) ^^
  enforce_unsigned_bits env n

let enforce_16_signed_bits env = enforce_signed_bits env 16

let compile_smallInt_kernel' env ty name op =
  Func.share_code2 env (prim_fun_name ty name)
    (("a", I32Type), ("b", I32Type)) [I32Type]
    (fun env get_a get_b ->
      let (set_res, get_res) = new_local env "res" in
      get_a ^^ compile_shrS_const 16l ^^
      get_b ^^ compile_shrS_const 16l ^^
      op ^^
      set_res ^^ get_res ^^ get_res ^^
      enforce_16_signed_bits env ^^
      get_res ^^ compile_shl_const 16l)

let compile_smallInt_kernel env ty name op =
  compile_smallInt_kernel' env ty name (G.i (Binary (Wasm.Values.I32 op)))

let compile_smallNat_kernel' env ty name op =
  Func.share_code2 env (prim_fun_name ty name)
    (("a", I32Type), ("b", I32Type)) [I32Type]
    (fun env get_a get_b ->
      let (set_res, get_res) = new_local env "res" in
      get_a ^^ compile_shrU_const 16l ^^
      get_b ^^ compile_shrU_const 16l ^^
      op ^^
      set_res ^^ get_res ^^
      enforce_16_unsigned_bits env ^^
      get_res ^^ compile_shl_const 16l)

let compile_smallNat_kernel env ty name op =
  compile_smallNat_kernel' env ty name (G.i (Binary (Wasm.Values.I32 op)))

(* The first returned StackRep is for the arguments (expected), the second for the results (produced) *)
let compile_binop env t op =
  if t = Type.Non then SR.Vanilla, SR.Unreachable, G.i Unreachable else
  StackRep.of_type t,
  StackRep.of_type t,
  Operator.(match t, op with
  | Type.(Prim (Nat | Int)),                  AddOp -> BigNum.compile_add env
  | Type.(Prim (Nat64|Int64)),                WAddOp -> G.i (Binary (Wasm.Values.I64 I64Op.Add))
  | Type.(Prim Int64),                        AddOp ->
    compile_Int64_kernel env "add" BigNum.compile_add
      (additiveInt64_shortcut (G.i (Binary (Wasm.Values.I64 I64Op.Add))))
  | Type.(Prim Nat64),                        AddOp ->
    compile_Nat64_kernel env "add" BigNum.compile_add
      (additiveNat64_shortcut (G.i (Binary (Wasm.Values.I64 I64Op.Add))))
  | Type.(Prim Nat),                          SubOp -> BigNum.compile_unsigned_sub env
  | Type.(Prim Int),                          SubOp -> BigNum.compile_signed_sub env
  | Type.(Prim (Nat | Int)),                  MulOp -> BigNum.compile_mul env
  | Type.(Prim (Nat64|Int64)),                WMulOp -> G.i (Binary (Wasm.Values.I64 I64Op.Mul))
  | Type.(Prim Int64),                        MulOp ->
    compile_Int64_kernel env "mul" BigNum.compile_mul
      (mulInt64_shortcut (G.i (Binary (Wasm.Values.I64 I64Op.Mul))))
  | Type.(Prim Nat64),                        MulOp ->
    compile_Nat64_kernel env "mul" BigNum.compile_mul
      (mulNat64_shortcut (G.i (Binary (Wasm.Values.I64 I64Op.Mul))))
  | Type.(Prim Nat64),                        DivOp -> G.i (Binary (Wasm.Values.I64 I64Op.DivU))
  | Type.(Prim Nat64) ,                       ModOp -> G.i (Binary (Wasm.Values.I64 I64Op.RemU))
  | Type.(Prim Int64),                        DivOp -> G.i (Binary (Wasm.Values.I64 I64Op.DivS))
  | Type.(Prim Int64) ,                       ModOp -> G.i (Binary (Wasm.Values.I64 I64Op.RemS))
  | Type.(Prim Nat),                          DivOp -> BigNum.compile_unsigned_div env
  | Type.(Prim Nat),                          ModOp -> BigNum.compile_unsigned_rem env
  | Type.(Prim (Nat64|Int64)),                WSubOp -> G.i (Binary (Wasm.Values.I64 I64Op.Sub))
  | Type.(Prim Int64),                        SubOp ->
    compile_Int64_kernel env "sub" BigNum.compile_signed_sub
      (additiveInt64_shortcut (G.i (Binary (Wasm.Values.I64 I64Op.Sub))))
  | Type.(Prim Nat64),                        SubOp ->
    compile_Nat64_kernel env "sub" BigNum.compile_unsigned_sub
      (fun env get_a get_b ->
        additiveNat64_shortcut
          (G.i (Compare (Wasm.Values.I64 I64Op.GeU)) ^^
           else_arithmetic_overflow env ^^
           get_a ^^ get_b ^^ G.i (Binary (Wasm.Values.I64 I64Op.Sub)))
          env get_a get_b)
  | Type.(Prim Int),                          DivOp -> BigNum.compile_signed_div env
  | Type.(Prim Int),                          ModOp -> BigNum.compile_signed_mod env

  | Type.Prim Type.(Nat8|Nat16|Nat32|Int8|Int16|Int32),
                                              WAddOp -> G.i (Binary (Wasm.Values.I32 I32Op.Add))
  | Type.(Prim Int32),                        AddOp -> compile_Int32_kernel env "add" I64Op.Add
  | Type.Prim Type.(Int8 | Int16 as ty),      AddOp -> compile_smallInt_kernel env ty "add" I32Op.Add
  | Type.(Prim Nat32),                        AddOp -> compile_Nat32_kernel env "add" I64Op.Add
  | Type.Prim Type.(Nat8 | Nat16 as ty),      AddOp -> compile_smallNat_kernel env ty "add" I32Op.Add
  | Type.(Prim Float),                        AddOp -> G.i (Binary (Wasm.Values.F64 F64Op.Add))
  | Type.Prim Type.(Nat8|Nat16|Nat32|Int8|Int16|Int32),
                                              WSubOp -> G.i (Binary (Wasm.Values.I32 I32Op.Sub))
  | Type.(Prim Int32),                        SubOp -> compile_Int32_kernel env "sub" I64Op.Sub
  | Type.(Prim (Int8|Int16 as ty)),           SubOp -> compile_smallInt_kernel env ty "sub" I32Op.Sub
  | Type.(Prim Nat32),                        SubOp -> compile_Nat32_kernel env "sub" I64Op.Sub
  | Type.(Prim (Nat8|Nat16 as ty)),           SubOp -> compile_smallNat_kernel env ty "sub" I32Op.Sub
  | Type.(Prim Float),                        SubOp -> G.i (Binary (Wasm.Values.F64 F64Op.Sub))
  | Type.Prim Type.(Nat8|Nat16|Nat32|Int8|Int16|Int32 as ty),
                                              WMulOp -> TaggedSmallWord.compile_word_mul env ty
  | Type.(Prim Int32),                        MulOp -> compile_Int32_kernel env "mul" I64Op.Mul
  | Type.(Prim Int16),                        MulOp -> compile_smallInt_kernel env Type.Int16 "mul" I32Op.Mul
  | Type.(Prim Int8),                         MulOp -> compile_smallInt_kernel' env Type.Int8 "mul"
                                                         (compile_shrS_const 8l ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)))
  | Type.(Prim Nat32),                        MulOp -> compile_Nat32_kernel env "mul" I64Op.Mul
  | Type.(Prim Nat16),                        MulOp -> compile_smallNat_kernel env Type.Nat16 "mul" I32Op.Mul
  | Type.(Prim Nat8),                         MulOp -> compile_smallNat_kernel' env Type.Nat8 "mul"
                                                         (compile_shrU_const 8l ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)))
  | Type.(Prim Float),                        MulOp -> G.i (Binary (Wasm.Values.F64 F64Op.Mul))
  | Type.(Prim (Nat8|Nat16|Nat32 as ty)),     DivOp -> G.i (Binary (Wasm.Values.I32 I32Op.DivU)) ^^
                                                       TaggedSmallWord.msb_adjust ty
  | Type.(Prim (Nat8|Nat16|Nat32)),           ModOp -> G.i (Binary (Wasm.Values.I32 I32Op.RemU))
  | Type.(Prim Int32),                        DivOp -> G.i (Binary (Wasm.Values.I32 I32Op.DivS))
  | Type.(Prim (Int8|Int16 as ty)),           DivOp ->
    Func.share_code2 env (prim_fun_name ty "div")
      (("a", I32Type), ("b", I32Type)) [I32Type]
      (fun env get_a get_b ->
        let (set_res, get_res) = new_local env "res" in
        get_a ^^ get_b ^^ G.i (Binary (Wasm.Values.I32 I32Op.DivS)) ^^
        TaggedSmallWord.msb_adjust ty ^^ set_res ^^
        get_a ^^ compile_eq_const 0x80000000l ^^
        E.if_ env (StackRep.to_block_type env SR.UnboxedWord32)
          begin
            get_b ^^ TaggedSmallWord.lsb_adjust ty ^^ compile_eq_const (-1l) ^^
            E.if_ env (StackRep.to_block_type env SR.UnboxedWord32)
              (G.i Unreachable)
              get_res
          end
          get_res)
  | Type.(Prim Float),                        DivOp -> G.i (Binary (Wasm.Values.F64 F64Op.Div))
  | Type.(Prim Float),                        ModOp -> E.call_import env "rts" "fmod" (* musl *)
  | Type.(Prim (Int8|Int16|Int32)),           ModOp -> G.i (Binary (Wasm.Values.I32 I32Op.RemS))
  | Type.(Prim (Nat8|Nat16|Nat32 as ty)),     WPowOp -> TaggedSmallWord.compile_nat_power env ty
  | Type.(Prim (Int8|Int16|Int32 as ty)),     WPowOp -> TaggedSmallWord.compile_int_power env ty
  | Type.(Prim ((Nat8|Nat16) as ty)),         PowOp ->
    Func.share_code2 env (prim_fun_name ty "pow")
      (("n", I32Type), ("exp", I32Type)) [I32Type]
      (fun env get_n get_exp ->
        let (set_res, get_res) = new_local env "res" in
        let bits = TaggedSmallWord.bits_of_type ty in
        get_exp ^^
        G.if1 I32Type
          begin
            get_n ^^ compile_shrU_const Int32.(sub 33l (of_int bits)) ^^
            G.if1 I32Type
              begin
                unsigned_dynamics get_n ^^ compile_sub_const (Int32.of_int bits) ^^
                get_exp ^^ TaggedSmallWord.lsb_adjust ty ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)) ^^
                compile_unboxed_const (-30l) ^^
                G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^ then_arithmetic_overflow env ^^
                get_n ^^ TaggedSmallWord.lsb_adjust ty ^^
                get_exp ^^ TaggedSmallWord.lsb_adjust ty ^^
                TaggedSmallWord.compile_nat_power env Type.Nat32 ^^ set_res ^^
                get_res ^^ enforce_unsigned_bits env bits ^^
                get_res ^^ TaggedSmallWord.msb_adjust ty
              end
              get_n (* n@{0,1} ** (1+exp) == n *)
          end
          (compile_unboxed_const
             Int32.(shift_left one (to_int (TaggedSmallWord.shift_of_type ty))))) (* x ** 0 == 1 *)
  | Type.(Prim Nat32),                        PowOp ->
    Func.share_code2 env (prim_fun_name Type.Nat32 "pow")
      (("n", I32Type), ("exp", I32Type)) [I32Type]
      (fun env get_n get_exp ->
        let (set_res, get_res) = new_local64 env "res" in
        get_exp ^^
        G.if1 I32Type
          begin
            get_n ^^ compile_shrU_const 1l ^^
            G.if1 I32Type
              begin
                get_exp ^^ compile_unboxed_const 32l ^^
                G.i (Compare (Wasm.Values.I32 I32Op.GeU)) ^^ then_arithmetic_overflow env ^^
                unsigned_dynamics get_n ^^ compile_sub_const 32l ^^
                get_exp ^^ TaggedSmallWord.lsb_adjust Type.Nat32 ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)) ^^
                compile_unboxed_const (-62l) ^^
                G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^ then_arithmetic_overflow env ^^
                get_n ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
                get_exp ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
                Word64.compile_unsigned_pow env ^^
                set_res ^^ get_res ^^ enforce_32_unsigned_bits env ^^
                get_res ^^ G.i (Convert (Wasm.Values.I32 I32Op.WrapI64))
              end
              get_n (* n@{0,1} ** (1+exp) == n *)
          end
          compile_unboxed_one) (* x ** 0 == 1 *)
  | Type.(Prim ((Int8|Int16) as ty)),         PowOp ->
    Func.share_code2 env (prim_fun_name ty "pow")
      (("n", I32Type), ("exp", I32Type)) [I32Type]
      (fun env get_n get_exp ->
        let (set_res, get_res) = new_local env "res" in
        let bits = TaggedSmallWord.bits_of_type ty in
        get_exp ^^ compile_unboxed_zero ^^
        G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^ E.then_trap_with env "negative power" ^^
        get_exp ^^
        G.if1 I32Type
          begin
            get_n ^^ compile_shrS_const Int32.(sub 33l (of_int bits)) ^^
            G.if1 I32Type
              begin
                signed_dynamics get_n ^^ compile_sub_const (Int32.of_int (bits - 1)) ^^
                get_exp ^^ TaggedSmallWord.lsb_adjust ty ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)) ^^
                compile_unboxed_const (-30l) ^^
                G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^ then_arithmetic_overflow env ^^
                get_n ^^ TaggedSmallWord.lsb_adjust ty ^^
                get_exp ^^ TaggedSmallWord.lsb_adjust ty ^^
                TaggedSmallWord.compile_nat_power env Type.Nat32 ^^
                set_res ^^ get_res ^^ get_res ^^ enforce_signed_bits env bits ^^
                get_res ^^ TaggedSmallWord.msb_adjust ty
              end
              get_n (* n@{0,1} ** (1+exp) == n *)
          end
          (compile_unboxed_const
             Int32.(shift_left one (to_int (TaggedSmallWord.shift_of_type ty))))) (* x ** 0 == 1 *)
  | Type.(Prim Int32),                        PowOp ->
    Func.share_code2 env (prim_fun_name Type.Int32 "pow")
      (("n", I32Type), ("exp", I32Type)) [I32Type]
      (fun env get_n get_exp ->
        let (set_res, get_res) = new_local64 env "res" in
        get_exp ^^ compile_unboxed_zero ^^
        G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^ E.then_trap_with env "negative power" ^^
        get_exp ^^
        G.if1 I32Type
          begin
            get_n ^^ compile_unboxed_one ^^ G.i (Compare (Wasm.Values.I32 I32Op.LeS)) ^^
            get_n ^^ compile_unboxed_const (-1l) ^^ G.i (Compare (Wasm.Values.I32 I32Op.GeS)) ^^
            G.i (Binary (Wasm.Values.I32 I32Op.And)) ^^
            G.if1 I32Type
              begin
                get_n ^^ compile_unboxed_zero ^^ G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^
                G.if1 I32Type
                  begin
                    (* -1 ** (1+exp) == if even (1+exp) then 1 else -1 *)
                    get_exp ^^ compile_unboxed_one ^^ G.i (Binary (Wasm.Values.I32 I32Op.And)) ^^
                    G.if1 I32Type
                      get_n
                      compile_unboxed_one
                  end
                  get_n (* n@{0,1} ** (1+exp) == n *)
              end
              begin
                get_exp ^^ compile_unboxed_const 32l ^^
                G.i (Compare (Wasm.Values.I32 I32Op.GeU)) ^^ then_arithmetic_overflow env ^^
                signed_dynamics get_n ^^ compile_sub_const 31l ^^
                get_exp ^^ TaggedSmallWord.lsb_adjust Type.Int32 ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)) ^^
                compile_unboxed_const (-62l) ^^
                G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^ then_arithmetic_overflow env ^^
                get_n ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendSI32)) ^^
                get_exp ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendSI32)) ^^
                Word64.compile_unsigned_pow env ^^
                set_res ^^ get_res ^^ get_res ^^ enforce_32_signed_bits env ^^
                get_res ^^ G.i (Convert (Wasm.Values.I32 I32Op.WrapI64))
              end
          end
          compile_unboxed_one) (* x ** 0 == 1 *)
  | Type.(Prim Int),                          PowOp ->
    let pow = BigNum.compile_unsigned_pow env in
    let (set_n, get_n) = new_local env "n" in
    let (set_exp, get_exp) = new_local env "exp" in
    set_exp ^^ set_n ^^
    get_exp ^^ BigNum.compile_is_negative env ^^
    E.then_trap_with env "negative power" ^^
    get_n ^^ get_exp ^^ pow
  | Type.(Prim Nat64),                        WPowOp -> Word64.compile_unsigned_pow env
  | Type.(Prim Int64),                        WPowOp -> Word64.compile_signed_wpow env
  | Type.(Prim Nat64),                        PowOp ->
    compile_Nat64_kernel env "pow"
      BigNum.compile_unsigned_pow
      (powNat64_shortcut (Word64.compile_unsigned_pow env))
  | Type.(Prim Int64),                        PowOp ->
    let (set_exp, get_exp) = new_local64 env "exp" in
    set_exp ^^ get_exp ^^
    compile_const_64 0L ^^
    G.i (Compare (Wasm.Values.I64 I64Op.LtS)) ^^
    E.then_trap_with env "negative power" ^^
    get_exp ^^
    compile_Int64_kernel
      env "pow" BigNum.compile_unsigned_pow
      (powInt64_shortcut (Word64.compile_unsigned_pow env))
  | Type.(Prim Nat),                          PowOp -> BigNum.compile_unsigned_pow env
  | Type.(Prim Float),                        PowOp -> E.call_import env "rts" "pow" (* musl *)
  | Type.(Prim (Nat64|Int64)),                AndOp -> G.i (Binary (Wasm.Values.I64 I64Op.And))
  | Type.(Prim (Nat8|Nat16|Nat32|Int8|Int16|Int32)),
                                              AndOp -> G.i (Binary (Wasm.Values.I32 I32Op.And))
  | Type.(Prim (Nat64|Int64)),                OrOp  -> G.i (Binary (Wasm.Values.I64 I64Op.Or))
  | Type.(Prim (Nat8|Nat16|Nat32|Int8|Int16|Int32)),
                                              OrOp  -> G.i (Binary (Wasm.Values.I32 I32Op.Or))
  | Type.(Prim (Nat64|Int64)),                XorOp -> G.i (Binary (Wasm.Values.I64 I64Op.Xor))
  | Type.(Prim (Nat8|Nat16|Nat32|Int8|Int16|Int32)),
                                              XorOp -> G.i (Binary (Wasm.Values.I32 I32Op.Xor))
  | Type.(Prim (Nat64|Int64)),                ShLOp -> G.i (Binary (Wasm.Values.I64 I64Op.Shl))
  | Type.(Prim (Nat8|Nat16|Nat32|Int8|Int16|Int32 as ty)),
                                              ShLOp -> TaggedSmallWord.(
     lsb_adjust ty ^^ clamp_shift_amount ty ^^
     G.i (Binary (Wasm.Values.I32 I32Op.Shl)))
  | Type.(Prim Nat64),                        ShROp -> G.i (Binary (Wasm.Values.I64 I64Op.ShrU))
  | Type.(Prim (Nat8|Nat16|Nat32 as ty)),     ShROp -> TaggedSmallWord.(
     lsb_adjust ty ^^ clamp_shift_amount ty ^^
     G.i (Binary (Wasm.Values.I32 I32Op.ShrU)) ^^
     sanitize_word_result ty)
  | Type.(Prim Int64),                        ShROp -> G.i (Binary (Wasm.Values.I64 I64Op.ShrS))
  | Type.(Prim (Int8|Int16|Int32 as ty)),     ShROp -> TaggedSmallWord.(
     lsb_adjust ty ^^ clamp_shift_amount ty ^^
     G.i (Binary (Wasm.Values.I32 I32Op.ShrS)) ^^
     sanitize_word_result ty)
  | Type.(Prim (Nat64|Int64)),                RotLOp -> G.i (Binary (Wasm.Values.I64 I64Op.Rotl))
  | Type.(Prim (Nat32|Int32)),                RotLOp -> G.i (Binary (Wasm.Values.I32 I32Op.Rotl))
  | Type.(Prim (Nat8|Nat16|Int8|Int16 as ty)),
                                              RotLOp -> TaggedSmallWord.rotl env ty
  | Type.(Prim (Nat64|Int64)),                RotROp -> G.i (Binary (Wasm.Values.I64 I64Op.Rotr))
  | Type.(Prim (Nat32|Int32)),                RotROp -> G.i (Binary (Wasm.Values.I32 I32Op.Rotr))
  | Type.(Prim (Nat8|Nat16|Int8|Int16 as ty)),
                                              RotROp -> TaggedSmallWord.rotr env ty
  | Type.(Prim Text), CatOp -> Text.concat env
  | Type.Non, _ -> G.i Unreachable
  | _ -> todo_trap env "compile_binop" (Wasm.Sexpr.Node ("BinOp", [ Arrange_ops.binop op; Arrange_type.typ t]))
  )

let compile_eq env =
  let open Type in
  function
  | Prim Text -> Text.compare env Operator.EqOp
  | Prim (Blob|Principal) | Obj (Actor, _) -> Blob.compare env Operator.EqOp
  | Func (Shared _, _, _, _, _) -> FuncDec.equate_msgref env
  | Prim Bool -> G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | Prim (Nat | Int) -> BigNum.compile_eq env
  | Prim (Int64 | Nat64) -> G.i (Compare (Wasm.Values.I64 I64Op.Eq))
  | Prim (Int8 | Nat8 | Int16 | Nat16 | Int32 | Nat32 | Char) ->
    G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | Non -> G.i Unreachable
  | Prim Float -> G.i (Compare (Wasm.Values.F64 F64Op.Eq))
  | t -> todo_trap env "compile_eq" (Arrange_type.typ t)

let get_relops = Operator.(function
  | GeOp -> Ge, I64Op.GeU, I64Op.GeS, I32Op.GeU, I32Op.GeS
  | GtOp -> Gt, I64Op.GtU, I64Op.GtS, I32Op.GtU, I32Op.GtS
  | LeOp -> Le, I64Op.LeU, I64Op.LeS, I32Op.LeU, I32Op.LeS
  | LtOp -> Lt, I64Op.LtU, I64Op.LtS, I32Op.LtU, I32Op.LtS
  | NeqOp -> assert false
  | _ -> failwith "uncovered relop")

let compile_comparison env t op =
  let bigintop, u64op, s64op, u32op, s32op = get_relops op in
  let open Type in
  match t with
    | Nat | Int -> BigNum.compile_relop env bigintop
    | Nat64 -> G.i (Compare (Wasm.Values.I64 u64op))
    | Nat8 | Nat16 | Nat32 | Char -> G.i (Compare (Wasm.Values.I32 u32op))
    | Int64 -> G.i (Compare (Wasm.Values.I64 s64op))
    | Int8 | Int16 | Int32 -> G.i (Compare (Wasm.Values.I32 s32op))
    | _ -> todo_trap env "compile_comparison" (Arrange_type.prim t)

let compile_relop env t op =
  if t = Type.Non then SR.Vanilla, G.i Unreachable else
  StackRep.of_type t,
  let open Operator in
  match t, op with
  | Type.(Prim Text), _ -> Text.compare env op
  | Type.(Prim (Blob|Principal)), _ -> Blob.compare env op
  | _, EqOp -> compile_eq env t
  | Type.(Prim (Nat | Nat8 | Nat16 | Nat32 | Nat64 | Int | Int8 | Int16 | Int32 | Int64 | Char as t1)), op1 ->
    compile_comparison env t1 op1
  | Type.(Prim Float), GtOp -> G.i (Compare (Wasm.Values.F64 F64Op.Gt))
  | Type.(Prim Float), GeOp -> G.i (Compare (Wasm.Values.F64 F64Op.Ge))
  | Type.(Prim Float), LeOp -> G.i (Compare (Wasm.Values.F64 F64Op.Le))
  | Type.(Prim Float), LtOp -> G.i (Compare (Wasm.Values.F64 F64Op.Lt))
  | _ -> todo_trap env "compile_relop" (Arrange_ops.relop op)

let compile_load_field env typ name =
  Object.load_idx env typ name


(* compile_lexp is used for expressions on the left of an
assignment operator, produces some code (with side effect), and some pure code *)
let rec compile_lexp (env : E.t) ae lexp =
  (fun (code, fill_code) -> (G.with_region lexp.at code, G.with_region lexp.at fill_code)) @@
  match lexp.it with
  | VarLE var ->
     G.nop,
     Var.set_val env ae var
  | IdxLE (e1, e2) ->
     compile_exp_vanilla env ae e1 ^^ (* offset to array *)
     compile_exp_vanilla env ae e2 ^^ (* idx *)
     Arr.idx_bigint env,
     store_ptr
  | DotLE (e, n) ->
     compile_exp_vanilla env ae e ^^
     (* Only real objects have mutable fields, no need to branch on the tag *)
     Object.idx env e.note.Note.typ n,
     store_ptr

and compile_exp (env : E.t) ae exp =
  (fun (sr,code) -> (sr, G.with_region exp.at code)) @@
  if exp.note.Note.const
  then let (c, fill) = compile_const_exp env ae exp in fill env ae; (SR.Const c, G.nop)
  else match exp.it with
  | PrimE (p, es) when List.exists (fun e -> Type.is_non e.note.Note.typ) es ->
    (* Handle dead code separately, so that we can rely on useful type
       annotations below *)
    SR.Unreachable,
    G.concat_map (compile_exp_ignore env ae) es ^^
    G.i Unreachable

  | PrimE (p, es) ->
    (* for more concise code when all arguments and result use the same sr *)
    let const_sr sr inst = sr, G.concat_map (compile_exp_as env ae sr) es ^^ inst in

    begin match p, es with
    (* Calls *)
    | CallPrim _, [e1; e2] ->
      let sort, control, _, arg_tys, ret_tys = Type.as_func e1.note.Note.typ in
      let n_args = List.length arg_tys in
      let return_arity = match control with
        | Type.Returns -> List.length ret_tys
        | Type.Replies -> 0
        | Type.Promises -> assert false in

      StackRep.of_arity return_arity,
      let fun_sr, code1 = compile_exp env ae e1 in
      begin match fun_sr, sort with
       | SR.Const (_, Const.Fun mk_fi), _ ->
          code1 ^^
          compile_unboxed_zero ^^ (* A dummy closure *)
          compile_exp_as env ae (StackRep.of_arity n_args) e2 ^^ (* the args *)
          G.i (Call (nr (mk_fi ()))) ^^
          FakeMultiVal.load env (Lib.List.make return_arity I32Type)
       | _, Type.Local ->
          let (set_clos, get_clos) = new_local env "clos" in
          code1 ^^ StackRep.adjust env fun_sr SR.Vanilla ^^
          set_clos ^^
          get_clos ^^
          compile_exp_as env ae (StackRep.of_arity n_args) e2 ^^
          get_clos ^^
          Closure.call_closure env n_args return_arity
       | _, Type.Shared _ ->
          (* Non-one-shot functions have been rewritten in async.ml *)
          assert (control = Type.Returns);

          let (set_meth_pair, get_meth_pair) = new_local env "meth_pair" in
          let (set_arg, get_arg) = new_local env "arg" in
          let _, _, _, ts, _ = Type.as_func e1.note.Note.typ in
          let add_cycles = Internals.add_cycles env ae in
          code1 ^^ StackRep.adjust env fun_sr SR.Vanilla ^^
          set_meth_pair ^^
          compile_exp_vanilla env ae e2 ^^ set_arg ^^

          FuncDec.ic_call_one_shot env ts get_meth_pair get_arg add_cycles
      end

    (* Operators *)
    | UnPrim (_, Operator.PosOp), [e1] -> compile_exp env ae e1
    | UnPrim (t, op), [e1] ->
      let sr_in, sr_out, code = compile_unop env t op in
      sr_out,
      compile_exp_as env ae sr_in e1 ^^
      code
    | BinPrim (t, op), [e1;e2] ->
      let sr_in, sr_out, code = compile_binop env t op in
      sr_out,
      compile_exp_as env ae sr_in e1 ^^
      compile_exp_as env ae sr_in e2 ^^
      code
    (* special case: recognize negation *)
    | RelPrim (Type.(Prim Bool), Operator.EqOp), [e1; {it = LitE (BoolLit false); _}] ->
      SR.bool,
      compile_exp_as_test env ae e1 ^^
      G.i (Test (Wasm.Values.I32 I32Op.Eqz))
    | RelPrim (t, op), [e1;e2] ->
      let sr, code = compile_relop env t op in
      SR.bool,
      compile_exp_as env ae sr e1 ^^
      compile_exp_as env ae sr e2 ^^
      code

    (* Tuples *)
    | TupPrim, es ->
      SR.UnboxedTuple (List.length es),
      G.concat_map (compile_exp_vanilla env ae) es
    | ProjPrim n, [e1] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e1 ^^ (* offset to tuple (an array) *)
      Tuple.load_n (Int32.of_int n)

    | OptPrim, [e] ->
      SR.Vanilla,
      Opt.inject env (compile_exp_vanilla env ae e)
    | TagPrim l, [e] ->
      SR.Vanilla,
      Variant.inject env l (compile_exp_vanilla env ae e)

    | DotPrim name, [e] ->
      let sr, code1 = compile_exp env ae e in
      begin match sr with
      | SR.Const (_, Const.Obj fs) ->
        let c = List.assoc name fs in
        SR.Const c, code1
      | _ ->
        SR.Vanilla,
        code1 ^^ StackRep.adjust env sr SR.Vanilla ^^
        Object.load_idx env e.note.Note.typ name
      end
    | ActorDotPrim name, [e] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^
      IC.actor_public_field env name

    | ArrayPrim (m, t), es ->
      SR.Vanilla,
      Arr.lit env (List.map (compile_exp_vanilla env ae) es)
    | IdxPrim, [e1; e2] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e1 ^^ (* offset to array *)
      compile_exp_vanilla env ae e2 ^^ (* idx *)
      Arr.idx_bigint env ^^
      load_ptr
    | NextArrayOffset spacing, [e] ->
      let advance_by =
        match spacing with
        | ElementSize -> Arr.element_size
        | One -> 2l (* 1 : Nat *) in
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^ (* previous byte offset to array *)
      compile_add_const advance_by
    | ValidArrayOffset, [e1; e2] ->
      SR.bool,
      compile_exp_vanilla env ae e1 ^^
      compile_exp_vanilla env ae e2 ^^
      G.i (Compare (Wasm.Values.I32 I32Op.LtU))
    | DerefArrayOffset, [e1; e2] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e1 ^^ (* skewed pointer to array *)
      compile_exp_vanilla env ae e2 ^^ (* byte offset *)
      (* Note: the below two lines compile to `i32.add; i32.load offset=9`,
         thus together also unskewing the pointer and skipping administrative
         fields, effectively arriving at the desired element *)
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      Arr.load_field 0l                (* loads the element at the byte offset *)
    | GetPastArrayOffset spacing, [e] ->
      let shift =
        match spacing with
        | ElementSize -> compile_shl_const 2l (* effectively a multiplication by word_size *)
        | One -> BigNum.from_word30 env in    (* make it a compact bignum *)
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^ (* array *)
      Heap.load_field Arr.len_field ^^
      shift

    | BreakPrim name, [e] ->
      let d = VarEnv.get_label_depth ae name in
      SR.Unreachable,
      compile_exp_vanilla env ae e ^^
      G.branch_to_ d
    | AssertPrim, [e1] ->
      SR.unit,
      compile_exp_as env ae SR.bool e1 ^^
      G.if0 G.nop (IC.fail_assert env exp.at)
    | RetPrim, [e] ->
      SR.Unreachable,
      compile_exp_as env ae (StackRep.of_arity (E.get_return_arity env)) e ^^
      FakeMultiVal.store env (Lib.List.make (E.get_return_arity env) I32Type) ^^
      G.i Return

    (* Numeric conversions *)
    | NumConvWrapPrim (t1, t2), [e] -> begin
      let open Type in
      match t1, t2 with
      | (Nat|Int), (Nat8|Nat16|Int8|Int16) ->
        SR.Vanilla,
        compile_exp_vanilla env ae e ^^
        Prim.prim_intToWordNShifted env (TaggedSmallWord.shift_of_type t2)

      | (Nat|Int), (Nat32|Int32) ->
        SR.UnboxedWord32,
        compile_exp_vanilla env ae e ^^
        Prim.prim_intToWord32 env

      | (Nat|Int), (Nat64|Int64) ->
        SR.UnboxedWord64,
        compile_exp_vanilla env ae e ^^
        BigNum.truncate_to_word64 env

      | Nat64, Int64 | Int64, Nat64
      | Nat32, Int32 | Int32, Nat32
      | Nat16, Int16 | Int16, Nat16
      | Nat8, Int8 | Int8, Nat8 ->
        compile_exp env ae e

      | Char, Nat32 ->
        SR.UnboxedWord32,
        compile_exp_vanilla env ae e ^^
        TaggedSmallWord.untag_codepoint

      | _ -> SR.Unreachable, todo_trap env "compile_exp u" (Arrange_ir.exp exp)
      end

    | NumConvTrapPrim (t1, t2), [e] -> begin
      let open Type in
      match t1, t2 with

      | Int, Int64 ->
        SR.UnboxedWord64,
        compile_exp_vanilla env ae e ^^
        Func.share_code1 env "Int->Int64" ("n", I32Type) [I64Type] (fun env get_n ->
          get_n ^^
          BigNum.fits_signed_bits env 64 ^^
          E.else_trap_with env "losing precision" ^^
          get_n ^^
          BigNum.truncate_to_word64 env)

      | Int, (Int8|Int16|Int32 as pty) ->
        StackRep.of_type (Prim pty),
        compile_exp_vanilla env ae e ^^
        Func.share_code1 env (prim_fun_name pty "Int->") ("n", I32Type) [I32Type] (fun env get_n ->
          get_n ^^
          BigNum.fits_signed_bits env (TaggedSmallWord.bits_of_type pty) ^^
          E.else_trap_with env "losing precision" ^^
          get_n ^^
          BigNum.truncate_to_word32 env ^^
          TaggedSmallWord.msb_adjust pty)

      | Nat, Nat64 ->
        SR.UnboxedWord64,
        compile_exp_vanilla env ae e ^^
        Func.share_code1 env "Nat->Nat64" ("n", I32Type) [I64Type] (fun env get_n ->
          get_n ^^
          BigNum.fits_unsigned_bits env 64 ^^
          E.else_trap_with env "losing precision" ^^
          get_n ^^
          BigNum.truncate_to_word64 env)

      | Nat, (Nat8|Nat16|Nat32 as pty) ->
        StackRep.of_type (Prim pty),
        compile_exp_vanilla env ae e ^^
        Func.share_code1 env (prim_fun_name pty "Nat->") ("n", I32Type) [I32Type] (fun env get_n ->
          get_n ^^
          BigNum.fits_unsigned_bits env (TaggedSmallWord.bits_of_type pty) ^^
          E.else_trap_with env "losing precision" ^^
          get_n ^^
          BigNum.truncate_to_word32 env ^^
          TaggedSmallWord.msb_adjust pty)

      | (Nat8|Nat16), Nat ->
        SR.Vanilla,
        compile_exp_vanilla env ae e ^^
        Prim.prim_shiftWordNtoUnsigned env (TaggedSmallWord.shift_of_type t1)

      | (Int8|Int16), Int ->
        SR.Vanilla,
        compile_exp_vanilla env ae e ^^
        Prim.prim_shiftWordNtoSigned env (TaggedSmallWord.shift_of_type t1)

      | Nat32, Nat ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedWord32 e ^^
        Prim.prim_word32toNat env

      | Int32, Int ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedWord32 e ^^
        Prim.prim_word32toInt env

      | Nat64, Nat ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedWord64 e ^^
        BigNum.from_word64 env

      | Int64, Int ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedWord64 e ^^
        BigNum.from_signed_word64 env

      | Nat32, Char ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedWord32 e ^^
        TaggedSmallWord.check_and_tag_codepoint env

      | Float, Int ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedFloat64 e ^^
        E.call_import env "rts" "bigint_of_float64"

      | Int, Float ->
        SR.UnboxedFloat64,
        compile_exp_vanilla env ae e ^^
        E.call_import env "rts" "bigint_to_float64"

      | Float, Int64 ->
        SR.UnboxedWord64,
        compile_exp_as env ae SR.UnboxedFloat64 e ^^
        G.i (Convert (Wasm.Values.I64 I64Op.TruncSF64))

      | Int64, Float ->
        SR.UnboxedFloat64,
        compile_exp_as env ae SR.UnboxedWord64 e ^^
        G.i (Convert (Wasm.Values.F64 F64Op.ConvertSI64))

      | _ -> SR.Unreachable, todo_trap env "compile_exp" (Arrange_ir.exp exp)
      end

    | SerializePrim ts, [e] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^
      Serialization.serialize env ts ^^
      Blob.of_ptr_size env

    | DeserializePrim ts, [e] ->
      StackRep.of_arity (List.length ts),
      compile_exp_vanilla env ae e ^^
      Serialization.deserialize_from_blob false env ts

    (* Other prims, unary *)

    | OtherPrim "array_len", [e] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^
      Heap.load_field Arr.len_field ^^
      BigNum.from_word30 env

    | OtherPrim "text_len", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Text.len env
    | OtherPrim "text_iter", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Text.iter env
    | OtherPrim "text_iter_done", [e] ->
      SR.bool, compile_exp_vanilla env ae e ^^ Text.iter_done env
    | OtherPrim "text_iter_next", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Text.iter_next env

    | OtherPrim "blob_size", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Blob.len env ^^ BigNum.from_word32 env
    | OtherPrim "blob_vals_iter", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Blob.iter env
    | OtherPrim "blob_iter_done", [e] ->
      SR.bool, compile_exp_vanilla env ae e ^^ Blob.iter_done env
    | OtherPrim "blob_iter_next", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Blob.iter_next env

    | OtherPrim "lshd", [e1; e2] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e1 ^^
      compile_exp_as env ae SR.UnboxedWord32 e2 ^^
      BigNum.compile_lshd env

    | OtherPrim "abs", [e] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^
      BigNum.compile_abs env

    | OtherPrim "fabs", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      G.i (Unary (Wasm.Values.F64 F64Op.Abs))

    | OtherPrim "fsqrt", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      G.i (Unary (Wasm.Values.F64 F64Op.Sqrt))

    | OtherPrim "fceil", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      G.i (Unary (Wasm.Values.F64 F64Op.Ceil))

    | OtherPrim "ffloor", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      G.i (Unary (Wasm.Values.F64 F64Op.Floor))

    | OtherPrim "ftrunc", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      G.i (Unary (Wasm.Values.F64 F64Op.Trunc))

    | OtherPrim "fnearest", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      G.i (Unary (Wasm.Values.F64 F64Op.Nearest))

    | OtherPrim "fmin", [e; f] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      compile_exp_as env ae SR.UnboxedFloat64 f ^^
      G.i (Binary (Wasm.Values.F64 F64Op.Min))

    | OtherPrim "fmax", [e; f] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      compile_exp_as env ae SR.UnboxedFloat64 f ^^
      G.i (Binary (Wasm.Values.F64 F64Op.Max))

    | OtherPrim "fcopysign", [e; f] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      compile_exp_as env ae SR.UnboxedFloat64 f ^^
      G.i (Binary (Wasm.Values.F64 F64Op.CopySign))

    | OtherPrim "Float->Text", [e] ->
      SR.Vanilla,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      compile_unboxed_const (TaggedSmallWord.vanilla_lit Type.Nat8 6) ^^
      compile_unboxed_const (TaggedSmallWord.vanilla_lit Type.Nat8 0) ^^
      E.call_import env "rts" "float_fmt"

    | OtherPrim "fmtFloat->Text", [f; prec; mode] ->
      SR.Vanilla,
      compile_exp_as env ae SR.UnboxedFloat64 f ^^
      compile_exp_vanilla env ae prec ^^
      compile_exp_vanilla env ae mode ^^
      E.call_import env "rts" "float_fmt"

    | OtherPrim "fsin", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "sin" (* musl *)

    | OtherPrim "fcos", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "cos" (* musl *)

    | OtherPrim "ftan", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "tan" (* musl *)

    | OtherPrim "fasin", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "asin" (* musl *)

    | OtherPrim "facos", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "acos" (* musl *)

    | OtherPrim "fatan", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "atan" (* musl *)

    | OtherPrim "fatan2", [y; x] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 y ^^
      compile_exp_as env ae SR.UnboxedFloat64 x ^^
      E.call_import env "rts" "atan2" (* musl *)

    | OtherPrim "fexp", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "exp" (* musl *)

    | OtherPrim "flog", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "log" (* musl *)

    (* Other prims, nullary *)

    | SystemTimePrim, [] ->
      SR.UnboxedWord64,
      IC.get_system_time env

    | OtherPrim "rts_version", [] ->
      SR.Vanilla,
      E.call_import env "rts" "version"

    | OtherPrim "rts_heap_size", [] ->
      SR.Vanilla,
      Heap.get_heap_size env ^^ Prim.prim_word32toNat env

    | OtherPrim "rts_memory_size", [] ->
      SR.Vanilla,
      Heap.get_memory_size ^^ Prim.prim_word32toNat env

    | OtherPrim "rts_total_allocation", [] ->
      SR.Vanilla,
      Heap.get_total_allocation env ^^ BigNum.from_word64 env

    | OtherPrim "rts_reclaimed", [] ->
      SR.Vanilla,
      Heap.get_reclaimed env ^^ BigNum.from_word64 env

    | OtherPrim "rts_max_live_size", [] ->
      SR.Vanilla,
      Heap.get_max_live_size env ^^ BigNum.from_word32 env

    | OtherPrim "rts_callback_table_count", [] ->
      SR.Vanilla,
      ContinuationTable.count env ^^ Prim.prim_word32toNat env

    | OtherPrim "rts_callback_table_size", [] ->
      SR.Vanilla,
      ContinuationTable.size env ^^ Prim.prim_word32toNat env

    | OtherPrim "crc32Hash", [e] ->
      SR.UnboxedWord32,
      compile_exp_vanilla env ae e ^^
      E.call_import env "rts" "compute_crc32"

    | OtherPrim "idlHash", [e] ->
      SR.Vanilla,
      E.trap_with env "idlHash only implemented in interpreter "


    | OtherPrim "popcnt8", [e] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^
      G.i (Unary (Wasm.Values.I32 I32Op.Popcnt)) ^^
      TaggedSmallWord.msb_adjust Type.Nat8
    | OtherPrim "popcnt16", [e] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^
      G.i (Unary (Wasm.Values.I32 I32Op.Popcnt)) ^^
      TaggedSmallWord.msb_adjust Type.Nat16
    | OtherPrim "popcnt32", [e] ->
      SR.UnboxedWord32,
      compile_exp_as env ae SR.UnboxedWord32 e ^^
      G.i (Unary (Wasm.Values.I32 I32Op.Popcnt))
    | OtherPrim "popcnt64", [e] ->
      SR.UnboxedWord64,
      compile_exp_as env ae SR.UnboxedWord64 e ^^
      G.i (Unary (Wasm.Values.I64 I64Op.Popcnt))
    | OtherPrim "clz8", [e] -> SR.Vanilla, compile_exp_vanilla env ae e ^^ TaggedSmallWord.clz_kernel Type.Nat8
    | OtherPrim "clz16", [e] -> SR.Vanilla, compile_exp_vanilla env ae e ^^ TaggedSmallWord.clz_kernel Type.Nat16
    | OtherPrim "clz32", [e] -> SR.UnboxedWord32, compile_exp_as env ae SR.UnboxedWord32 e ^^ G.i (Unary (Wasm.Values.I32 I32Op.Clz))
    | OtherPrim "clz64", [e] -> SR.UnboxedWord64, compile_exp_as env ae SR.UnboxedWord64 e ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz))
    | OtherPrim "ctz8", [e] -> SR.Vanilla, compile_exp_vanilla env ae e ^^ TaggedSmallWord.ctz_kernel Type.Nat8
    | OtherPrim "ctz16", [e] -> SR.Vanilla, compile_exp_vanilla env ae e ^^ TaggedSmallWord.ctz_kernel Type.Nat16
    | OtherPrim "ctz32", [e] -> SR.UnboxedWord32, compile_exp_as env ae SR.UnboxedWord32 e ^^ G.i (Unary (Wasm.Values.I32 I32Op.Ctz))
    | OtherPrim "ctz64", [e] -> SR.UnboxedWord64, compile_exp_as env ae SR.UnboxedWord64 e ^^ G.i (Unary (Wasm.Values.I64 I64Op.Ctz))

    | OtherPrim "conv_Char_Text", [e] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^
      Text.prim_showChar env

    | OtherPrim "char_to_upper", [e] ->
      compile_char_to_char_rts env ae e "char_to_upper"

    | OtherPrim "char_to_lower", [e] ->
      compile_char_to_char_rts env ae e "char_to_lower"

    | OtherPrim "char_is_whitespace", [e] ->
      compile_char_to_bool_rts env ae e "char_is_whitespace"

    | OtherPrim "char_is_lowercase", [e] ->
      compile_char_to_bool_rts env ae e "char_is_lowercase"

    | OtherPrim "char_is_uppercase", [e] ->
      compile_char_to_bool_rts env ae e "char_is_uppercase"

    | OtherPrim "char_is_alphabetic", [e] ->
      compile_char_to_bool_rts env ae e "char_is_alphabetic"

    | OtherPrim "print", [e] ->
      SR.unit,
      compile_exp_vanilla env ae e ^^
      IC.print_text env

    | OtherPrim "trap", [e] ->
      SR.unit,
      compile_exp_vanilla env ae e ^^
      IC.trap_text env

    | OtherPrim ("blobToArray"|"blobToArrayMut"), e ->
      const_sr SR.Vanilla (Arr.ofBlob env)
    | OtherPrim ("arrayToBlob"|"arrayMutToBlob"), e ->
      const_sr SR.Vanilla (Arr.toBlob env)

    | OtherPrim ("stableMemoryLoadNat32"|"stableMemoryLoadInt32"), [e] ->
      SR.UnboxedWord32,
      compile_exp_as env ae SR.UnboxedWord64 e ^^
      StableMem.load_word32 env

    | OtherPrim ("stableMemoryStoreNat32"|"stableMemoryStoreInt32"), [e1; e2] ->
      SR.unit,
      compile_exp_as env ae SR.UnboxedWord64 e1 ^^
      compile_exp_as env ae SR.UnboxedWord32 e2 ^^
      StableMem.store_word32 env

    | OtherPrim ("stableMemoryLoadNat8"), [e] ->
      SR.Vanilla,
      compile_exp_as env ae SR.UnboxedWord64 e ^^
      StableMem.load_word8 env ^^
      TaggedSmallWord.msb_adjust Type.Nat8

    | OtherPrim ("stableMemoryLoadInt8"), [e] ->
      SR.Vanilla,
      compile_exp_as env ae SR.UnboxedWord64 e ^^
      StableMem.load_word8 env ^^
      TaggedSmallWord.msb_adjust Type.Int8

    | OtherPrim ("stableMemoryStoreNat8"), [e1; e2] ->
      SR.unit,
      compile_exp_as env ae SR.UnboxedWord64 e1 ^^
      compile_exp_as env ae SR.Vanilla e2 ^^ TaggedSmallWord.lsb_adjust Type.Nat8 ^^
      StableMem.store_word8 env

    | OtherPrim ("stableMemoryStoreInt8"), [e1; e2] ->
      SR.unit,
      compile_exp_as env ae SR.UnboxedWord64 e1 ^^
      compile_exp_as env ae SR.Vanilla e2 ^^ TaggedSmallWord.lsb_adjust Type.Int8 ^^
      StableMem.store_word8 env

    | OtherPrim ("stableMemoryLoadNat16"), [e] ->
      SR.Vanilla,
      compile_exp_as env ae SR.UnboxedWord64 e ^^
      StableMem.load_word16 env ^^
      TaggedSmallWord.msb_adjust Type.Nat16

    | OtherPrim ("stableMemoryLoadInt16"), [e] ->
      SR.Vanilla,
      compile_exp_as env ae SR.UnboxedWord64 e ^^
      StableMem.load_word16 env ^^
      TaggedSmallWord.msb_adjust Type.Int16

    | OtherPrim ("stableMemoryStoreNat16"), [e1; e2] ->
      SR.unit,
      compile_exp_as env ae SR.UnboxedWord64 e1 ^^
      compile_exp_as env ae SR.Vanilla e2 ^^ TaggedSmallWord.lsb_adjust Type.Nat16 ^^
      StableMem.store_word16 env

    | OtherPrim ("stableMemoryStoreInt16"), [e1; e2] ->
      SR.unit,
      compile_exp_as env ae SR.UnboxedWord64 e1 ^^
      compile_exp_as env ae SR.Vanilla e2 ^^ TaggedSmallWord.lsb_adjust Type.Int16 ^^
      StableMem.store_word16 env

    | OtherPrim ("stableMemoryLoadNat64" | "stableMemoryLoadInt64"), [e] ->
      SR.UnboxedWord64,
      compile_exp_as env ae SR.UnboxedWord64 e ^^
      StableMem.load_word64 env

    | OtherPrim ("stableMemoryStoreNat64" | "stableMemoryStoreInt64"), [e1; e2] ->
      SR.unit,
      compile_exp_as env ae SR.UnboxedWord64 e1 ^^
      compile_exp_as env ae SR.UnboxedWord64 e2 ^^
      StableMem.store_word64 env

    | OtherPrim ("stableMemoryLoadFloat"), [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedWord64 e ^^
      StableMem.load_float64 env

    | OtherPrim ("stableMemoryStoreFloat"), [e1; e2] ->
      SR.unit,
      compile_exp_as env ae SR.UnboxedWord64 e1 ^^
      compile_exp_as env ae SR.UnboxedFloat64 e2 ^^
      StableMem.store_float64 env

    | OtherPrim ("stableMemoryLoadBlob"), [e1; e2] ->
      SR.Vanilla,
      compile_exp_as env ae SR.UnboxedWord64 e1 ^^
      compile_exp_as env ae SR.Vanilla e2 ^^
      Blob.lit env "Blob size out of bounds" ^^
      BigNum.to_word32_with env ^^
      StableMem.load_blob env

    | OtherPrim ("stableMemoryStoreBlob"), [e1; e2] ->
      SR.unit,
      compile_exp_as env ae SR.UnboxedWord64 e1 ^^
      compile_exp_as env ae SR.Vanilla e2 ^^
      StableMem.store_blob env

    | OtherPrim ("stableMemorySize"), [] ->
      SR.UnboxedWord64,
      StableMem.get_mem_size env
    | OtherPrim ("stableMemoryGrow"), [e] ->
      SR.UnboxedWord64,
      compile_exp_as env ae SR.UnboxedWord64 e ^^
      StableMem.logical_grow env

    (* Other prims, binary*)
    | OtherPrim "Array.init", [_;_] ->
      const_sr SR.Vanilla (Arr.init env)
    | OtherPrim "Array.tabulate", [_;_] ->
      const_sr SR.Vanilla (Arr.tabulate env)
    | OtherPrim "btst8", [_;_] ->
      (* TODO: btstN returns Bool, not a small value *)
      const_sr SR.Vanilla (TaggedSmallWord.btst_kernel env Type.Nat8)
    | OtherPrim "btst16", [_;_] ->
      const_sr SR.Vanilla (TaggedSmallWord.btst_kernel env Type.Nat16)
    | OtherPrim "btst32", [_;_] ->
      const_sr SR.UnboxedWord32 (TaggedSmallWord.btst_kernel env Type.Nat32)
    | OtherPrim "btst64", [_;_] ->
      const_sr SR.UnboxedWord64 (
        let (set_b, get_b) = new_local64 env "b" in
        set_b ^^ compile_const_64 1L ^^ get_b ^^ G.i (Binary (Wasm.Values.I64 I64Op.Shl)) ^^
        G.i (Binary (Wasm.Values.I64 I64Op.And))
      )

    (* Coercions for abstract types *)
    | CastPrim (_,_), [e] ->
      compile_exp env ae e

    | DecodeUtf8, [_] ->
      const_sr SR.Vanilla (Text.of_blob env)
    | EncodeUtf8, [_] ->
      const_sr SR.Vanilla (Text.to_blob env)

    (* textual to bytes *)
    | BlobOfIcUrl, [_] ->
      const_sr SR.Vanilla (E.call_import env "rts" "blob_of_principal")
    (* The other direction *)
    | IcUrlOfBlob, [_] ->
      const_sr SR.Vanilla (E.call_import env "rts" "principal_of_blob")

    (* Actor ids are blobs in the RTS *)
    | ActorOfIdBlob _, [e] ->
      compile_exp env ae e

    | SelfRef _, [] ->
      SR.Vanilla,
      IC.get_self_reference env

    | ICReplyPrim ts, [e] ->
      SR.unit, begin match E.mode env with
      | Flags.ICMode | Flags.RefMode ->
        compile_exp_vanilla env ae e ^^
        (* TODO: We can try to avoid the boxing and pass the arguments to
          serialize individually *)
        Serialization.serialize env ts ^^
        IC.reply_with_data env
      | _ ->
        E.trap_with env (Printf.sprintf "cannot reply when running locally")
      end

    | ICRejectPrim, [e] ->
      SR.unit, IC.reject env (compile_exp_vanilla env ae e)

    | ICCallerPrim, [] ->
      IC.caller env

    | ICCallPrim, [f;e;k;r] ->
      SR.unit, begin
      (* TBR: Can we do better than using the notes? *)
      let _, _, _, ts1, _ = Type.as_func f.note.Note.typ in
      let _, _, _, ts2, _ = Type.as_func k.note.Note.typ in
      let (set_meth_pair, get_meth_pair) = new_local env "meth_pair" in
      let (set_arg, get_arg) = new_local env "arg" in
      let (set_k, get_k) = new_local env "k" in
      let (set_r, get_r) = new_local env "r" in
      let add_cycles = Internals.add_cycles env ae in
      compile_exp_vanilla env ae f ^^ set_meth_pair ^^
      compile_exp_vanilla env ae e ^^ set_arg ^^
      compile_exp_vanilla env ae k ^^ set_k ^^
      compile_exp_vanilla env ae r ^^ set_r ^^
      FuncDec.ic_call env ts1 ts2 get_meth_pair get_arg get_k get_r add_cycles
      end
    | ICCallRawPrim, [p;m;a;k;r] ->
      SR.unit, begin
      let (set_meth_pair, get_meth_pair) = new_local env "meth_pair" in
      let (set_arg, get_arg) = new_local env "arg" in
      let (set_k, get_k) = new_local env "k" in
      let (set_r, get_r) = new_local env "r" in
      let add_cycles = Internals.add_cycles env ae in
      compile_exp_vanilla env ae p ^^
      compile_exp_vanilla env ae m ^^ Text.to_blob env ^^
      Tuple.from_stack env 2 ^^ set_meth_pair ^^
      compile_exp_vanilla env ae a ^^ set_arg ^^
      compile_exp_vanilla env ae k ^^ set_k ^^
      compile_exp_vanilla env ae r ^^ set_r ^^
      FuncDec.ic_call_raw env get_meth_pair get_arg get_k get_r add_cycles
      end
    | ICStableRead ty, [] ->
      (*
        * On initial install:
          1. return record of nulls
        * On upgrade:
          1. deserialize stable store to v : ty,
          2. return v
      *)
      SR.Vanilla,
      Stabilization.destabilize env ty
    | ICStableWrite ty, [e] ->
      SR.unit,
      compile_exp_vanilla env ae e ^^
      Stabilization.stabilize env ty

    (* Cycles *)
    | SystemCyclesBalancePrim, [] ->
      SR.Vanilla,
      Stack.with_words env "dst" 4l (fun get_dst ->
        get_dst ^^
        IC.cycle_balance env ^^
        get_dst ^^
        Cycles.load_cycles env
      )
    | SystemCyclesAddPrim, [e1] ->
      SR.unit,
      let (set_cycles, get_cycles) = new_local env "cycles" in
      compile_exp_vanilla env ae e1 ^^
      set_cycles ^^
      get_cycles ^^
      Cycles.guard env ^^
      get_cycles ^^
      Cycles.push_high env ^^
      get_cycles ^^
      Cycles.push_low env ^^
      IC.cycles_add env
    | SystemCyclesAcceptPrim, [e1] ->
      SR.Vanilla,
      let (set_cycles, get_cycles) = new_local env "cycles" in
      Stack.with_words env "dst" 4l (fun get_dst ->
        compile_exp_vanilla env ae e1 ^^
        set_cycles ^^
        get_cycles ^^
        Cycles.guard env ^^
        get_cycles ^^
        Cycles.push_high env ^^
        get_cycles ^^
        Cycles.push_low env ^^
        get_dst ^^
        IC.cycles_accept env ^^
        get_dst ^^
        Cycles.load_cycles env
      )
    | SystemCyclesAvailablePrim, [] ->
      SR.Vanilla,
      Stack.with_words env "dst" 4l (fun get_dst ->
        get_dst ^^
        IC.cycles_available env ^^
        get_dst ^^
        Cycles.load_cycles env
      )
    | SystemCyclesRefundedPrim, [] ->
      SR.Vanilla,
      Stack.with_words env "dst" 4l (fun get_dst ->
        get_dst ^^
        IC.cycles_refunded env ^^
        get_dst ^^
        Cycles.load_cycles env
      )
    | SetCertifiedData, [e1] ->
      SR.unit,
      compile_exp_vanilla env ae e1 ^^
      IC.set_certified_data env
    | GetCertificate, [] ->
      SR.Vanilla,
      IC.get_certificate env

    (* Unknown prim *)
    | _ -> SR.Unreachable, todo_trap env "compile_exp" (Arrange_ir.exp exp)
    end
  | VarE var ->
    Var.get_val env ae var
  | AssignE (e1,e2) ->
    SR.unit,
    let (prepare_code, store_code) = compile_lexp env ae e1 in
    prepare_code ^^
    compile_exp_vanilla env ae e2 ^^
    store_code
  | LitE l ->
    compile_lit env l
  | IfE (scrut, e1, e2) ->
    let code_scrut = compile_exp_as_test env ae scrut in
    let sr1, code1 = compile_exp env ae e1 in
    let sr2, code2 = compile_exp env ae e2 in
    let sr = StackRep.relax (StackRep.join sr1 sr2) in
    sr,
    code_scrut ^^
    E.if_ env
      (StackRep.to_block_type env sr)
      (code1 ^^ StackRep.adjust env sr1 sr)
      (code2 ^^ StackRep.adjust env sr2 sr)
  | BlockE (decs, exp) ->
    let captured = Freevars.captured_vars (Freevars.exp exp) in
    let ae', codeW1 = compile_decs env ae decs captured in
    let (sr, code2) = compile_exp env ae' exp in
    (sr, codeW1 code2)
  | LabelE (name, _ty, e) ->
    (* The value here can come from many places -- the expression,
       or any of the nested returns. Hard to tell which is the best
       stack representation here.
       So let’s go with Vanilla. *)
    SR.Vanilla,
    E.block_ env (StackRep.to_block_type env SR.Vanilla) (
      G.with_current_depth (fun depth ->
        let ae1 = VarEnv.add_label ae name depth in
        compile_exp_vanilla env ae1 e
      )
    )
  | LoopE e ->
    SR.Unreachable,
    let ae' = VarEnv.{ ae with lvl = NotTopLvl } in
    G.loop0 (compile_exp_unit env ae' e ^^ G.i (Br (nr 0l))
    )
    ^^
   G.i Unreachable
  | SwitchE (e, cs) ->
    SR.Vanilla,
    let code1 = compile_exp_vanilla env ae e in
    let (set_i, get_i) = new_local env "switch_in" in
    let (set_j, get_j) = new_local env "switch_out" in

    let rec go env cs = match cs with
      | [] -> CanFail (fun k -> k)
      | {it={pat; exp=e}; _}::cs ->
          let (ae1, code) = compile_pat_local env ae pat in
          orElse ( CannotFail get_i ^^^ code ^^^
                   CannotFail (compile_exp_vanilla env ae1 e) ^^^ CannotFail set_j)
                 (go env cs)
          in
      let code2 = go env cs in
      code1 ^^ set_i ^^ orTrap env code2 ^^ get_j
  (* Async-wait lowering support features *)
  | DeclareE (name, _, e) ->
    let (ae1, i) = VarEnv.add_local_with_heap_ind env ae name in
    let sr, code = compile_exp env ae1 e in
    sr,
    MutBox.alloc env ^^ G.i (LocalSet (nr i)) ^^
    code
  | DefineE (name, _, e) ->
    SR.unit,
    compile_exp_vanilla env ae e ^^
    Var.set_val env ae name
  | FuncE (x, sort, control, typ_binds, args, res_tys, e) ->
    let captured = Freevars.captured exp in
    let return_tys = match control with
      | Type.Returns -> res_tys
      | Type.Replies -> []
      | Type.Promises -> assert false in
    let return_arity = List.length return_tys in
    let mk_body env1 ae1 = compile_exp_as env1 ae1 (StackRep.of_arity return_arity) e in
    FuncDec.lit env ae x sort control captured args mk_body return_tys exp.at
  | SelfCallE (ts, exp_f, exp_k, exp_r) ->
    SR.unit,
    let (set_future, get_future) = new_local env "future" in
    let (set_k, get_k) = new_local env "k" in
    let (set_r, get_r) = new_local env "r" in
    let mk_body env1 ae1 = compile_exp_as env1 ae1 SR.unit exp_f in
    let captured = Freevars.captured exp_f in
    let add_cycles = Internals.add_cycles env ae in
    FuncDec.async_body env ae ts captured mk_body exp.at ^^
    set_future ^^

    compile_exp_vanilla env ae exp_k ^^ set_k ^^
    compile_exp_vanilla env ae exp_r ^^ set_r ^^

    FuncDec.ic_self_call env ts
      (IC.get_self_reference env ^^
       IC.actor_public_field env (IC.async_method_name))
      get_future
      get_k
      get_r
      add_cycles
  | ActorE (ds, fs, _, _) ->
    fatal "Local actors not supported by backend"
  | NewObjE (Type.(Object | Module | Memory) as _sort, fs, _) ->
    (*
    We can enable this warning once we treat everything as static that
    mo_frontend/static.ml accepts, including _all_ literals.
    if sort = Type.Module then Printf.eprintf "%s" "Warning: Non-static module\n";
    *)
    SR.Vanilla,
    let fs' = fs |> List.map
      (fun (f : Ir.field) -> (f.it.name, fun () ->
        if Type.is_mut f.note
        then Var.get_aliased_box env ae f.it.var
        else Var.get_val_vanilla env ae f.it.var)) in
    Object.lit_raw env fs'
  | _ -> SR.unit, todo_trap env "compile_exp" (Arrange_ir.exp exp)

and compile_exp_as env ae sr_out e =
  G.with_region e.at (
    match sr_out, e.it with
    (* Some optimizations for certain sr_out and expressions *)
    | _ , BlockE (decs, exp) ->
      let captured = Freevars.captured_vars (Freevars.exp exp) in
      let ae', codeW1 = compile_decs env ae decs captured in
      let code2 = compile_exp_as env ae' sr_out exp in
      codeW1 code2
    (* Fallback to whatever stackrep compile_exp chooses *)
    | _ ->
      let sr_in, code = compile_exp env ae e in
      code ^^ StackRep.adjust env sr_in sr_out
  )

and compile_exp_ignore env ae e =
  let sr, code = compile_exp env ae e in
  code ^^ StackRep.drop env sr

and compile_exp_as_opt env ae sr_out_o e =
  let sr_in, code = compile_exp env ae e in
  G.with_region e.at (
    code ^^
    match sr_out_o with
    | None -> StackRep.drop env sr_in
    | Some sr_out -> StackRep.adjust env sr_in sr_out
  )

and compile_exp_vanilla (env : E.t) ae exp =
  compile_exp_as env ae SR.Vanilla exp

and compile_exp_unit (env : E.t) ae exp =
  compile_exp_as env ae SR.unit exp

(* compiles to something that works with IfE or Eqz
   (SR.UnboxedWord32 or SR.Vanilla are _both_ ok)
*)
and compile_exp_as_test env ae e =
  let sr, code = compile_exp env ae e in
  code ^^
  (if sr != SR.bool then StackRep.adjust env sr SR.Vanilla else G.nop)

(* Compile a prim of type Char -> Char to a RTS call. *)
and compile_char_to_char_rts env ae exp rts_fn =
  SR.Vanilla,
  compile_exp_vanilla env ae exp ^^
  TaggedSmallWord.untag_codepoint ^^
  E.call_import env "rts" rts_fn ^^
  TaggedSmallWord.tag_codepoint

(* Compile a prim of type Char -> Bool to a RTS call. The RTS function should
   have type int32_t -> int32_t where the return value is 0 for 'false' and 1
   for 'true'. *)
and compile_char_to_bool_rts (env : E.t) (ae : VarEnv.t) exp rts_fn =
  SR.bool,
  compile_exp_vanilla env ae exp ^^
  TaggedSmallWord.untag_codepoint ^^
  (* The RTS function returns Motoko True/False values (which are represented as
     1 and 0, respectively) so we don't need any marshalling *)
  E.call_import env "rts" rts_fn

(*
The compilation of declarations (and patterns!) needs to handle mutual recursion.
This requires conceptually three passes:
 1. First we need to collect all names bound in a block,
    and find locations for then (which extends the environment).
    The environment is extended monotonously: The type-checker ensures that
    a Block does not bind the same name twice.
    We would not need to pass in the environment, just out ... but because
    it is bundled in the E.t type, threading it through is also easy.

 2. We need to allocate memory for them, and store the pointer in the
    WebAssembly local, so that they can be captured by closures.

 3. We go through the declarations, generate the actual code and fill the
    allocated memory.
    This includes creating the actual closure references.

We could do this in separate functions, but I chose to do it in one
 * it means all code related to one constructor is in one place and
 * when generating the actual code, we still “know” the id of the local that
   has the memory location, and don’t have to look it up in the environment.

The first phase works with the `pre_env` passed to `compile_dec`,
while the third phase is a function that expects the final environment. This
enabled mutual recursion.
*)


and compile_lit_pat env l =
  match l with
  | NullLit ->
    compile_lit_as env SR.Vanilla l ^^
    G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | BoolLit true ->
    G.nop
  | BoolLit false ->
    G.i (Test (Wasm.Values.I32 I32Op.Eqz))
  | (NatLit _ | IntLit _) ->
    compile_lit_as env SR.Vanilla l ^^
    BigNum.compile_eq env
  | Nat8Lit _ ->
    compile_lit_as env SR.Vanilla l ^^
    compile_eq env Type.(Prim Nat8)
  | Nat16Lit _ ->
    compile_lit_as env SR.Vanilla l ^^
    compile_eq env Type.(Prim Nat16)
  | Nat32Lit _ ->
    BoxedSmallWord.unbox env ^^
    compile_lit_as env SR.UnboxedWord32 l ^^
    compile_eq env Type.(Prim Nat32)
  | Nat64Lit _ ->
    BoxedWord64.unbox env ^^
    compile_lit_as env SR.UnboxedWord64 l ^^
    compile_eq env Type.(Prim Nat64)
  | Int8Lit _ ->
    compile_lit_as env SR.Vanilla l ^^
    compile_eq env Type.(Prim Int8)
  | Int16Lit _ ->
    compile_lit_as env SR.Vanilla l ^^
    compile_eq env Type.(Prim Int16)
  | Int32Lit _ ->
    BoxedSmallWord.unbox env ^^
    compile_lit_as env SR.UnboxedWord32 l ^^
    compile_eq env Type.(Prim Int32)
  | Int64Lit _ ->
    BoxedWord64.unbox env ^^
    compile_lit_as env SR.UnboxedWord64 l ^^
    compile_eq env Type.(Prim Int64)
  | CharLit _ ->
    compile_lit_as env SR.Vanilla l ^^
    compile_eq env Type.(Prim Char)
  | TextLit t
  | BlobLit t ->
    compile_lit_as env SR.Vanilla l ^^
    Text.compare env Operator.EqOp
  | FloatLit _ ->
    todo_trap env "compile_lit_pat" (Arrange_ir.lit l)

and fill_pat env ae pat : patternCode =
  PatCode.with_region pat.at @@
  match pat.it with
  | WildP -> CannotFail (G.i Drop)
  | OptP p ->
      let (set_x, get_x) = new_local env "opt_scrut" in
      CanFail (fun fail_code ->
        set_x ^^
        get_x ^^
        Opt.is_some env ^^
        G.if0
          ( get_x ^^
            Opt.project env ^^
            with_fail fail_code (fill_pat env ae p)
          )
          fail_code
      )
  | TagP (l, p) ->
      let (set_x, get_x) = new_local env "tag_scrut" in
      CanFail (fun fail_code ->
        set_x ^^
        get_x ^^
        Variant.test_is env l ^^
        G.if0
          ( get_x ^^
            Variant.project ^^
            with_fail fail_code (fill_pat env ae p)
          )
          fail_code
      )
  | LitP l ->
      CanFail (fun fail_code ->
        compile_lit_pat env l ^^
        G.if0 G.nop fail_code)
  | VarP name ->
      CannotFail (Var.set_val env ae name)
  | TupP ps ->
      let (set_i, get_i) = new_local env "tup_scrut" in
      let rec go i = function
        | [] -> CannotFail G.nop
        | p::ps ->
          let code1 = fill_pat env ae p in
          let code2 = go (Int32.add i 1l) ps in
          CannotFail (get_i ^^ Tuple.load_n i) ^^^ code1 ^^^ code2 in
      CannotFail set_i ^^^ go 0l ps
  | ObjP pfs ->
      let project = compile_load_field env pat.note in
      let (set_i, get_i) = new_local env "obj_scrut" in
      let rec go = function
        | [] -> CannotFail G.nop
        | {it={name; pat}; _}::pfs' ->
          let code1 = fill_pat env ae pat in
          let code2 = go pfs' in
          CannotFail (get_i ^^ project name) ^^^ code1 ^^^ code2 in
      CannotFail set_i ^^^ go pfs
  | AltP (p1, p2) ->
      let code1 = fill_pat env ae p1 in
      let code2 = fill_pat env ae p2 in
      let (set_i, get_i) = new_local env "alt_scrut" in
      CannotFail set_i ^^^
      orElse (CannotFail get_i ^^^ code1)
             (CannotFail get_i ^^^ code2)

and alloc_pat_local env ae pat =
  let d = Freevars.pat pat in
  AllocHow.M.fold (fun v _ty ae ->
    let (ae1, _i) = VarEnv.add_direct_local env ae v
    in ae1
  ) d ae

and alloc_pat env ae how pat : VarEnv.t * G.t  =
  (fun (ae, code) -> (ae, G.with_region pat.at code)) @@
  let d = Freevars.pat pat in
  AllocHow.M.fold (fun v _ty (ae, code0) ->
    let ae1, code1 = AllocHow.add_local env ae how v
    in (ae1, code0 ^^ code1)
  ) d (ae, G.nop)

and compile_pat_local env ae pat : VarEnv.t * patternCode =
  (* It returns:
     - the extended environment
     - the code to do the pattern matching.
       This expects the  undestructed value is on top of the stack,
       consumes it, and fills the heap
       If the pattern does not match, it branches to the depth at fail_depth.
  *)
  let ae1 = alloc_pat_local env ae pat in
  let fill_code = fill_pat env ae1 pat in
  (ae1, fill_code)

(* Used for let patterns: If the patterns is an n-ary tuple pattern,
   we want to compile the expression accordingly, to avoid the reboxing.
*)
and compile_n_ary_pat env ae how pat =
  (* It returns:
     - the extended environment
     - the code to allocate memory
     - the arity
     - the code to do the pattern matching.
       This expects the  undestructed value is on top of the stack,
       consumes it, and fills the heap
       If the pattern does not match, it branches to the depth at fail_depth.
  *)
  let (ae1, alloc_code) = alloc_pat env ae how pat in
  let arity, fill_code =
    (fun (sr, code) -> sr, G.with_region pat.at code) @@
    match pat.it with
    (* Nothing to match: Do not even put something on the stack *)
    | WildP -> None, G.nop
    (* The good case: We have a tuple pattern *)
    | TupP ps when List.length ps <> 1 ->
      Some (SR.UnboxedTuple (List.length ps)),
      (* We have to fill the pattern in reverse order, to take things off the
         stack. This is only ok as long as patterns have no side effects.
      *)
      G.concat_mapi (fun i p -> orTrap env (fill_pat env ae1 p)) (List.rev ps)
    (* The general case: Create a single value, match that. *)
    | _ ->
      Some SR.Vanilla,
      orTrap env (fill_pat env ae1 pat)
  in (ae1, alloc_code, arity, fill_code)

and compile_dec env pre_ae how v2en dec : VarEnv.t * G.t * (VarEnv.t -> scope_wrap) =
  (fun (pre_ae, alloc_code, mk_code, wrap) ->
       (pre_ae, G.with_region dec.at alloc_code, fun ae body_code ->
         G.with_region dec.at (mk_code ae) ^^ wrap body_code)) @@
  match dec.it with
  (* A special case for public methods *)
  (* This relies on the fact that in the top-level mutually recursive group, no shadowing happens. *)
  | LetD ({it = VarP v; _}, e) when E.NameEnv.mem v v2en ->
    let (const, fill) = compile_const_exp env pre_ae e in
    let fi = match const with
      | (_, Const.Message fi) -> fi
      | _ -> assert false in
    let pre_ae1 = VarEnv.add_local_public_method pre_ae v (fi, (E.NameEnv.find v v2en)) in
    G.( pre_ae1, nop, (fun ae -> fill env ae; nop), unmodified)

  (* A special case for constant expressions *)
  | LetD (p, e) when Ir_utils.is_irrefutable p && e.note.Note.const ->
    let extend, fill = compile_const_dec env pre_ae dec in
    G.( extend pre_ae, nop, (fun ae -> fill env ae; nop), unmodified)

  | LetD (p, e) ->
    let (pre_ae1, alloc_code, pat_arity, fill_code) = compile_n_ary_pat env pre_ae how p in
    ( pre_ae1, alloc_code,
      (fun ae -> compile_exp_as_opt env ae pat_arity e ^^ fill_code),
      unmodified
    )

  | VarD (name, _, e) ->
    assert AllocHow.(match M.find_opt name how with
                     | Some (LocalMut | StoreHeap | StoreStatic) -> true
                     | _ -> false);
      let pre_ae1, alloc_code = AllocHow.add_local env pre_ae how name in

      ( pre_ae1,
        alloc_code,
        (fun ae -> compile_exp_vanilla env ae e ^^ Var.set_val env ae name),
        unmodified
      )

and compile_decs_public env pre_ae decs v2en captured_in_body : VarEnv.t * scope_wrap =
  let how = AllocHow.decs pre_ae decs captured_in_body in
  let rec go pre_ae = function
    | []          -> (pre_ae, G.nop, fun _ -> unmodified)
    | [dec]       -> compile_dec env pre_ae how v2en dec
    | (dec::decs) ->
        let (pre_ae1, alloc_code1, mk_codeW1) = compile_dec env pre_ae how v2en dec in
        let (pre_ae2, alloc_code2, mk_codeW2) = go              pre_ae1 decs in
        ( pre_ae2,
          alloc_code1 ^^ alloc_code2,
          fun ae -> let codeW1 = mk_codeW1 ae in
                    let codeW2 = mk_codeW2 ae in
                    fun body_code -> codeW1 (codeW2 body_code)
        ) in
  let (ae1, alloc_code, mk_codeW) = go pre_ae decs in
  (ae1, fun body_code -> alloc_code ^^ mk_codeW ae1 body_code)

and compile_decs env ae decs captured_in_body : VarEnv.t * scope_wrap =
  compile_decs_public env ae decs E.NameEnv.empty captured_in_body

(* This compiles expressions determined to be const as per the analysis in
   ir_passes/const.ml. See there for more details.
*)
and compile_const_exp env pre_ae exp : Const.t * (E.t -> VarEnv.t -> unit) =
  match exp.it with
  | FuncE (name, sort, control, typ_binds, args, res_tys, e) ->
      let return_tys = match control with
        | Type.Returns -> res_tys
        | Type.Replies -> []
        | Type.Promises -> assert false in
      let mk_body env ae =
        List.iter (fun v ->
          if not (VarEnv.NameEnv.mem v ae.VarEnv.vars)
          then fatal "internal error: const \"%s\": captures \"%s\", not found in static environment\n" name v
        ) (Freevars.M.keys (Freevars.exp e));
        compile_exp_as env ae (StackRep.of_arity (List.length return_tys)) e in
      FuncDec.closed env sort control name args mk_body return_tys exp.at
  | BlockE (decs, e) ->
    let (extend, fill1) = compile_const_decs env pre_ae decs in
    let ae' = extend pre_ae in
    let (c, fill2) = compile_const_exp env ae' e in
    (c, fun env ae ->
      let ae' = extend ae in
      fill1 env ae';
      fill2 env ae')
  | VarE v ->
    let c =
      match VarEnv.lookup_var pre_ae v with
      | Some (VarEnv.Const c) -> c
      | _ -> fatal "compile_const_exp/VarE: \"%s\" not found" v
    in
    (c, fun _ _ -> ())
  | NewObjE (Type.(Object | Module | Memory), fs, _) ->
    let static_fs = List.map (fun f ->
          let st =
            match VarEnv.lookup_var pre_ae f.it.var with
            | Some (VarEnv.Const c) -> c
            | _ -> fatal "compile_const_exp/ObjE: \"%s\" not found" f.it.var
          in f.it.name, st) fs
    in
    (Const.t_of_v (Const.Obj static_fs), fun _ _ -> ())
  | PrimE (DotPrim name, [e]) ->
    let (object_ct, fill) = compile_const_exp env pre_ae e in
    let fs = match object_ct with
      | _, Const.Obj fs -> fs
      | _ -> fatal "compile_const_exp/DotE: not a static object" in
    let member_ct = List.assoc name fs in
    (member_ct, fill)
  | PrimE (ProjPrim i, [e]) ->
    let (object_ct, fill) = compile_const_exp env pre_ae e in
    let cs = match object_ct with
      | _, Const.Array cs -> cs
      | _ -> fatal "compile_const_exp/ProjE: not a static tuple" in
    (List.nth cs i, fill)
  | LitE l -> Const.(t_of_v (Lit (const_lit_of_lit env l))), (fun _ _ -> ())
  | PrimE (TupPrim, []) -> Const.t_of_v Const.Unit, (fun _ _ -> ())
  | PrimE (ArrayPrim (Const, _), es)
  | PrimE (TupPrim, es) ->
    let (cs, fills) = List.split (List.map (compile_const_exp env pre_ae) es) in
    Const.t_of_v (Const.Array cs),
    (fun env ae -> List.iter (fun fill -> fill env ae) fills)

  | _ -> assert false

and compile_const_decs env pre_ae decs : (VarEnv.t -> VarEnv.t) * (E.t -> VarEnv.t -> unit) =
  let rec go pre_ae = function
    | []          -> (fun ae -> ae), (fun _ _ -> ())
    | [dec]       -> compile_const_dec env pre_ae dec
    | (dec::decs) ->
        let (extend1, fill1) = compile_const_dec env pre_ae dec in
        let pre_ae1 = extend1 pre_ae in
        let (extend2, fill2) = go                    pre_ae1 decs in
        (fun ae -> extend2 (extend1 ae)),
        (fun env ae -> fill1 env ae; fill2 env ae) in
  go pre_ae decs

and destruct_const_pat ae pat const : VarEnv.t = match pat.it with
  | WildP -> ae
  | VarP v -> VarEnv.add_local_const ae v const
  | ObjP pfs ->
    let fs = match const with (_, Const.Obj fs) -> fs | _ -> assert false in
    List.fold_left (fun ae (pf : pat_field) ->
      match List.find_opt (fun (n, _) -> pf.it.name = n) fs with
      | Some (_, c) -> destruct_const_pat ae pf.it.pat c
      | None -> assert false
    ) ae pfs
  | AltP (p1, p2) -> destruct_const_pat ae p1 const
  | TupP ps ->
    let cs = match const with (_ , Const.Array cs) -> cs | (_, Const.Unit) -> [] | _ -> assert false in
    List.fold_left2 destruct_const_pat ae ps cs
  | LitP _ -> raise (Invalid_argument "LitP in static irrefutable pattern")
  | OptP _ -> raise (Invalid_argument "OptP in static irrefutable pattern")
  | TagP _ -> raise (Invalid_argument "TagP in static irrefutable pattern")

and compile_const_dec env pre_ae dec : (VarEnv.t -> VarEnv.t) * (E.t -> VarEnv.t -> unit) =
  (* This returns a _function_ to extend the VarEnv, instead of doing it, because
  it needs to be extended twice: Once during the pass that gets the outer, static values
  (no forward references), and then to implement the `fill`, which compiles the bodies
  of functions (may contain forward references.) *)
  match dec.it with
  (* This should only contain constants (cf. is_const_exp) *)
  | LetD (p, e) ->
    let (const, fill) = compile_const_exp env pre_ae e in
    (fun ae -> destruct_const_pat ae p const),
    (fun env ae -> fill env ae)
  | VarD _ -> fatal "compile_const_dec: Unexpected VarD"

and compile_init_func mod_env ((cu, flavor) : Ir.prog) =
  assert (not flavor.has_typ_field);
  assert (not flavor.has_poly_eq);
  assert (not flavor.has_show);
  assert (not flavor.has_await);
  assert (not flavor.has_async_typ);
  match cu with
  | LibU _ -> fatal "compile_start_func: Cannot compile library"
  | ProgU ds ->
    Func.define_built_in mod_env "init" [] [] (fun env ->
      let _ae, codeW = compile_decs env VarEnv.empty_ae ds Freevars.S.empty in
      codeW G.nop
    )
  | ActorU (as_opt, ds, fs, up, _t) ->
    main_actor as_opt mod_env ds fs up

and export_actor_field env  ae (f : Ir.field) =
  (* A public actor field is guaranteed to be compiled as a PublicMethod *)
  let fi =
    match VarEnv.lookup_var ae f.it.var with
    | Some (VarEnv.PublicMethod (fi, _)) -> fi
    | _ -> assert false in

  E.add_export env (nr {
    name = Wasm.Utf8.decode (match E.mode env with
      | Flags.ICMode | Flags.RefMode ->
        Mo_types.Type.(
        match normalize f.note with
        |  Func(Shared sort,_,_,_,_) ->
           (match sort with
            | Write -> "canister_update " ^ f.it.name
            | Query -> "canister_query " ^ f.it.name)
        | _ -> assert false)
      | _ -> assert false);
    edesc = nr (FuncExport (nr fi))
  })

(* Main actor *)
and main_actor as_opt mod_env ds fs up =
  Func.define_built_in mod_env "init" [] [] (fun env ->
    let ae0 = VarEnv.empty_ae in

    let captured = Freevars.captured_vars (Freevars.actor ds fs up) in

    (* Add any params to the environment *)
    (* Captured ones need to go into static memory, the rest into locals *)
    let args = match as_opt with None -> [] | Some as_ -> as_ in
    let arg_names = List.map (fun a -> a.it) args in
    let arg_tys = List.map (fun a -> a.note) args in
    let as_local n = not (Freevars.S.mem n captured) in
    let ae1 = VarEnv.add_arguments env ae0 as_local arg_names in

    (* Reverse the fs, to a map from variable to exported name *)
    let v2en = E.NameEnv.from_list (List.map (fun f -> (f.it.var, f.it.name)) fs) in

    (* Compile the declarations *)
    let ae2, decls_codeW = compile_decs_public env ae1 ds v2en
      Freevars.(captured_vars (system up))
    in

    (* Export the public functions *)
    List.iter (export_actor_field env ae2) fs;

    (* Export upgrade hooks *)
    Func.define_built_in env "pre_exp" [] [] (fun env ->
      compile_exp_as env ae2 SR.unit up.preupgrade);
    Func.define_built_in env "post_exp" [] [] (fun env ->
      compile_exp_as env ae2 SR.unit up.postupgrade);
    IC.export_upgrade_methods env;

    (* Export heartbeat (but only when required) *)
    begin match up.heartbeat.it with
     | Ir.PrimE (Ir.TupPrim, []) -> ()
     | _ ->
       Func.define_built_in env "heartbeat_exp" [] [] (fun env ->
         compile_exp_as env ae2 SR.unit up.heartbeat);
       IC.export_heartbeat env;
    end;

    (* Export metadata *)
    env.E.stable_types :=
      Some (
        List.mem "motoko:stable-types" !Flags.public_metadata_names,
        up.meta.sig_);
    env.E.service :=
      Some (
        List.mem "candid:service" !Flags.public_metadata_names,
        up.meta.candid.service);
    env.E.args :=
      Some (
        List.mem "candid:args" !Flags.public_metadata_names,
        up.meta.candid.args);


    (* Deserialize any arguments *)
    begin match as_opt with
      | None
      | Some [] ->
        (* Liberally accept empty as well as unit argument *)
        assert (arg_tys = []);
        IC.system_call env "ic0" "msg_arg_data_size" ^^
        G.if0 (Serialization.deserialize env arg_tys) G.nop
      | Some (_ :: _) ->
        Serialization.deserialize env arg_tys ^^
        G.concat_map (Var.set_val env ae1) (List.rev arg_names)
    end ^^
    (* Continue with decls *)
    decls_codeW G.nop
  )

and conclude_module env start_fi_o =

  FuncDec.export_async_method env;

  let static_roots = GC.store_static_roots env in
  (* declare before building GC *)

  (* add beginning-of-heap pointer, may be changed by linker *)
  (* needs to happen here now that we know the size of static memory *)
  let set_heap_base = E.add_global32_delayed env "__heap_base" Immutable in
  E.export_global env "__heap_base";

  Heap.register env;
  GC.register env static_roots;
  IC.register env;

  set_heap_base (E.get_end_of_static_memory env);

  (* Wrap the start function with the RTS initialization *)
  let rts_start_fi = E.add_fun env "rts_start" (Func.of_body env [] [] (fun env1 ->
    Bool.lit (!Flags.gc_strategy = Mo_config.Flags.MarkCompact) ^^
    E.call_import env "rts" "init" ^^
    match start_fi_o with
    | Some fi ->
      G.i (Call fi)
    | None ->
      Lifecycle.set env Lifecycle.PreInit
  )) in

  IC.default_exports env;

  let func_imports = E.get_func_imports env in
  let ni = List.length func_imports in
  let ni' = Int32.of_int ni in

  let other_imports = E.get_other_imports env in

  let memories = [nr {mtype = MemoryType {min = E.mem_size env; max = None}} ] in

  let funcs = E.get_funcs env in

  let data = List.map (fun (offset, init) -> nr {
    index = nr 0l;
    offset = nr (G.to_instr_list (compile_unboxed_const offset));
    init;
    }) (E.get_static_memory env) in

  let elems = List.map (fun (fi, fp) -> nr {
    index = nr 0l;
    offset = nr (G.to_instr_list (compile_unboxed_const fp));
    init = [ nr fi ];
    }) (E.get_elems env) in

  let table_sz = E.get_end_of_table env in

  let module_ = {
      types = List.map nr (E.get_types env);
      funcs = List.map (fun (f,_,_) -> f) funcs;
      tables = [ nr { ttype = TableType ({min = table_sz; max = Some table_sz}, FuncRefType) } ];
      elems;
      start = Some (nr rts_start_fi);
      globals = E.get_globals env;
      memories;
      imports = func_imports @ other_imports;
      exports = E.get_exports env;
      data
    } in

  let emodule =
    let open Wasm_exts.CustomModule in
    { module_;
      dylink = None;
      name = { empty_name_section with function_names =
                 List.mapi (fun i (f,n,_) -> Int32.(add ni' (of_int i), n)) funcs;
               locals_names =
                 List.mapi (fun i (f,_,ln) -> Int32.(add ni' (of_int i), ln)) funcs; };
      motoko = {
        labels = E.get_labs env;
        stable_types = !(env.E.stable_types);
        compiler = Some
         (List.mem "motoko:compiler" !Flags.public_metadata_names,
          Lib.Option.get Source_id.release Source_id.id)
      };
      candid = {
        args = !(env.E.args);
        service = !(env.E.service);
      };
      source_mapping_url = None;
    } in

  match E.get_rts env with
  | None -> emodule
  | Some rts -> Linking.LinkModule.link emodule "rts" rts

let compile mode rts (prog : Ir.prog) : Wasm_exts.CustomModule.extended_module =
  let env = E.mk_global mode rts IC.trap_with Lifecycle.end_ in

  Stack.register_globals env;
  StableMem.register_globals env;

  IC.system_imports env;
  RTS.system_imports env;
  RTS_Exports.system_exports env;

  compile_init_func env prog;
  let start_fi_o = match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      IC.export_init env;
      None
    | Flags.WASIMode ->
      IC.export_wasi_start env;
      None
    | Flags.WasmMode ->
      Some (nr (E.built_in env "init"))
  in

  conclude_module env start_fi_o
