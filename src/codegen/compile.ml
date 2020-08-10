(*
This module is the backend of the Motoko compiler. It takes a program in
the intermediate representation (ir.ml), and produces a WebAssembly module,
with DFINITY extensions (customModule.ml). An important helper module is
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
    | Word32 of int32
    | Word64 of int64
    | Float64 of Value.Float.t
    | Blob of string

  (* Constant known values.

     These are values that
     * are completely known constantly
     * do not require Wasm code to be be executed (e.g. in `start`)
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
    | UnboxedTuple of int
    | UnboxedWord64
    | UnboxedWord32
    | UnboxedFloat64
    | Unreachable
    | Const of Const.t

  let unit = UnboxedTuple 0

  let bool = UnboxedWord32

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
    (* Actually unused outside mk_fun_env: *)
    n_param = 0l;
    return_arity = 0;
    locals = ref [];
    local_names = ref [];
  }


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

  let add_global64 (env : t) name mut init =
    add_global env name (Lib.Promise.make_fulfilled (
      nr { gtype = GlobalType (I64Type, mut);
        value = nr (G.to_instr_list (G.i (Const (nr (Wasm.Values.I64 init)))))
      }))

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
    if !(env.funcs) = []
    then
      let i = {
        module_name = Wasm.Utf8.decode modname;
        item_name = Wasm.Utf8.decode funcname;
        idesc = nr (FuncImport (nr (func_type env (FuncType (arg_tys, ret_tys)))))
      } in
      let fi = reg env.func_imports (nr i) in
      let name = modname ^ "." ^ funcname in
      assert (not (NameEnv.mem name !(env.named_imports)));
      env.named_imports := NameEnv.add name fi !(env.named_imports);
    else assert false (* "add all imports before all functions!" *)

  let call_import (env : t) modname funcname =
    let name = modname ^ "." ^ funcname in
    match NameEnv.find_opt name !(env.named_imports) with
      | Some fi -> G.i (Call (nr fi))
      | _ ->
        Printf.eprintf "Function import not declared: %s\n" name;
        G.i Unreachable

  let get_rts (env : t) = env.rts

  let trap_with env msg = env.trap_with env msg
  let then_trap_with env msg = G.if_ [] (trap_with env msg) G.nop
  let else_trap_with env msg = G.if_ [] G.nop (trap_with env msg)

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
let _compile_add64_const = compile_op64_const I64Op.Add
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
let compile_while cond body =
    G.loop_ [] (
      cond ^^ G.if_ [] (body ^^ G.i (Br (nr 1l))) G.nop
    )

(* Expects a number on the stack. Iterates from zero to below that number. *)
let from_0_to_n env mk_body =
    let (set_n, get_n) = new_local env "n" in
    let (set_i, get_i) = new_local env "i" in
    set_n ^^
    compile_unboxed_zero ^^
    set_i ^^

    compile_while
      ( get_i ^^
        get_n ^^
        G.i (Compare (Wasm.Values.I32 I32Op.LtU))
      ) (
        mk_body get_i ^^

        get_i ^^
        compile_add_const 1l ^^
        set_i
      )


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
  let share_code4 env name (p1, p2, p3, p4) retty mk_body =
    share_code env name [p1; p2; p3; p4] retty (fun env -> mk_body env
        (G.i (LocalGet (nr 0l)))
        (G.i (LocalGet (nr 1l)))
        (G.i (LocalGet (nr 2l)))
        (G.i (LocalGet (nr 3l)))
    )

end (* Func *)

module RTS = struct
  (* The connection to the C parts of the RTS *)
  let system_imports env =
    E.add_func_import env "rts" "as_memcpy" [I32Type; I32Type; I32Type] [];
    E.add_func_import env "rts" "as_memcmp" [I32Type; I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "version" [] [I32Type];
    E.add_func_import env "rts" "parse_idl_header" [I32Type; I32Type; I32Type; I32Type] [];
    E.add_func_import env "rts" "read_u32_of_leb128" [I32Type] [I32Type];
    E.add_func_import env "rts" "read_i32_of_sleb128" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_word32" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_word32_signed" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word32_wrap" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word32_trap" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word32_trap_with" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word32_signed_trap" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_word64" [I64Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_word64_signed" [I64Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word64_wrap" [I32Type] [I64Type];
    E.add_func_import env "rts" "bigint_to_word64_trap" [I32Type] [I64Type];
    E.add_func_import env "rts" "bigint_to_word64_signed_trap" [I32Type] [I64Type];
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
    E.add_func_import env "rts" "utf8_validate" [I32Type; I32Type] [];
    E.add_func_import env "rts" "skip_leb128" [I32Type] [];
    E.add_func_import env "rts" "skip_any" [I32Type; I32Type; I32Type; I32Type] [];
    E.add_func_import env "rts" "find_field" [I32Type; I32Type; I32Type; I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "skip_fields" [I32Type; I32Type; I32Type; I32Type] [];
    E.add_func_import env "rts" "remember_closure" [I32Type] [I32Type];
    E.add_func_import env "rts" "recall_closure" [I32Type] [I32Type];
    E.add_func_import env "rts" "closure_count" [] [I32Type];
    E.add_func_import env "rts" "closure_table_loc" [] [I32Type];
    E.add_func_import env "rts" "closure_table_size" [] [I32Type];
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
    E.add_func_import env "rts" "float_pow" [F64Type; F64Type] [F64Type];
    E.add_func_import env "rts" "float_sin" [F64Type] [F64Type];
    E.add_func_import env "rts" "float_cos" [F64Type] [F64Type];
    E.add_func_import env "rts" "float_tan" [F64Type] [F64Type];
    E.add_func_import env "rts" "float_arcsin" [F64Type] [F64Type];
    E.add_func_import env "rts" "float_arccos" [F64Type] [F64Type];
    E.add_func_import env "rts" "float_arctan" [F64Type] [F64Type];
    E.add_func_import env "rts" "float_arctan2" [F64Type; F64Type] [F64Type];
    E.add_func_import env "rts" "float_exp" [F64Type] [F64Type];
    E.add_func_import env "rts" "float_log" [F64Type] [F64Type];
    E.add_func_import env "rts" "float_rem" [F64Type; F64Type] [F64Type];
    E.add_func_import env "rts" "float_fmt" [F64Type; I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "char_to_upper" [I32Type] [I32Type];
    E.add_func_import env "rts" "char_to_lower" [I32Type] [I32Type];
    E.add_func_import env "rts" "char_is_whitespace" [I32Type] [I32Type];
    E.add_func_import env "rts" "char_is_lowercase" [I32Type] [I32Type];
    E.add_func_import env "rts" "char_is_uppercase" [I32Type] [I32Type];
    E.add_func_import env "rts" "char_is_alphabetic" [I32Type] [I32Type];
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

  (* We keep track of the end of the used heap in this global, and bump it if
     we allocate stuff. This is the actual memory offset, not-skewed yet *)
  let get_heap_ptr env =
    G.i (GlobalGet (nr (E.get_global env "end_of_heap")))
  let set_heap_ptr env =
    G.i (GlobalSet (nr (E.get_global env "end_of_heap")))
  let get_skewed_heap_ptr env = get_heap_ptr env ^^ compile_add_const ptr_skew

  let register_globals env =
    (* end-of-heap pointer, we set this to __heap_base upon start *)
    E.add_global32 env "end_of_heap" Mutable 0xDEADBEEFl;

    (* counter for total allocations *)
    E.add_global64 env "allocations" Mutable 0L;

    (* counter for total reclaimed bytes *)
    E.add_global64 env "reclaimed" Mutable 0L;

    (* counter for max live bytes *)
    E.add_global64 env "max_live" Mutable 0L

  let count_allocations env =
    (* assumes number of allocated bytes on the stack *)
    G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
    G.i (GlobalGet (nr (E.get_global env "allocations"))) ^^
    G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
    G.i (GlobalSet (nr (E.get_global env "allocations")))

  let get_total_allocation env =
    G.i (GlobalGet (nr (E.get_global env "allocations")))

  let add_reclaimed env =
    (* assumes number of reclaimed bytes on the stack *)
    G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
    G.i (GlobalGet (nr (E.get_global env "reclaimed"))) ^^
    G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
    G.i (GlobalSet (nr (E.get_global env "reclaimed")))

  let get_reclaimed env =
    G.i (GlobalGet (nr (E.get_global env "reclaimed")))

  let get_memory_size =
    G.i MemorySize ^^
    compile_mul_const page_size

  let note_live_size env =
    (* assumes size of live set on the stack *)
    let (set_live_size, get_live_size) = new_local env "live_size" in
    set_live_size ^^
    get_live_size ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
    G.i (GlobalGet (nr (E.get_global env "max_live"))) ^^
    G.i (Compare (Wasm.Values.I64 I64Op.LtU)) ^^
    G.if_ [] G.nop begin
      get_live_size ^^ G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
      G.i (GlobalSet (nr (E.get_global env "max_live")))
    end

  let get_max_live_size env =
    G.i (GlobalGet (nr (E.get_global env "max_live")))


  (* Page allocation. Ensures that the memory up to the given unskewed pointer is allocated. *)
  let grow_memory env =
    Func.share_code1 env "grow_memory" ("ptr", I32Type) [] (fun env get_ptr ->
      let (set_pages_needed, get_pages_needed) = new_local env "pages_needed" in
      get_ptr ^^ compile_divU_const page_size ^^
      compile_add_const 1l ^^
      G.i MemorySize ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
      set_pages_needed ^^

      (* Check that the new heap pointer is within the memory *)
      get_pages_needed ^^
      compile_unboxed_zero ^^
      G.i (Compare (Wasm.Values.I32 I32Op.GtS)) ^^
      G.if_ []
        ( get_pages_needed ^^
          G.i MemoryGrow ^^
          (* Check result *)
          compile_unboxed_zero ^^
          G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^
          E.then_trap_with env "Cannot grow memory."
        ) G.nop
      )

  let dyn_alloc_words env = G.i (Call (nr (E.built_in env "alloc_words")))
  let dyn_alloc_bytes env = G.i (Call (nr (E.built_in env "alloc_bytes")))

  let declare_alloc_functions env =
    (* Dynamic allocation *)
    Func.define_built_in env "alloc_words" [("n", I32Type)] [I32Type] (fun env ->
      (* expects the size (in words), returns the skewed pointer *)
      let get_n = G.i (LocalGet (nr 0l)) in
      (* return the current pointer (skewed) *)
      get_skewed_heap_ptr env ^^

      (* Count allocated bytes *)
      get_n ^^ compile_mul_const word_size ^^
      count_allocations env ^^

      (* Update heap pointer *)
      get_heap_ptr env ^^
      get_n ^^ compile_mul_const word_size ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      set_heap_ptr env ^^

      (* grow memory if needed *)
      get_heap_ptr env ^^ grow_memory env
    );
    Func.define_built_in env "alloc_bytes" [("n", I32Type)] [I32Type] (fun env ->
      let get_n = G.i (LocalGet (nr 0l)) in
      (* Round up to next multiple of the word size and convert to words *)
      get_n ^^
      compile_add_const 3l ^^
      compile_divU_const word_size ^^
      dyn_alloc_words env
    )

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
  let memcpy env = E.call_import env "rts" "as_memcpy"
  (* Comparing bytes (works on unskewed memory addresses) *)
  let memcmp env = E.call_import env "rts" "as_memcmp"

  (* Copying words (works on skewed memory addresses) *)
  let memcpy_words_skewed env =
    Func.share_code3 env "memcpy_words_skewed" (("to", I32Type), ("from", I32Type), ("n", I32Type)) [] (fun env get_to get_from get_n ->
      get_n ^^
      from_0_to_n env (fun get_i ->
          get_to ^^
          get_i ^^ compile_mul_const word_size ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^

          get_from ^^
          get_i ^^ compile_mul_const word_size ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          load_ptr ^^

          store_ptr
      )
    )

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

module ClosureTable = struct
  (* See rts/closure-table.c *)
  let remember env : G.t = E.call_import env "rts" "remember_closure"
  let recall env : G.t = E.call_import env "rts" "recall_closure"
  let count env : G.t = E.call_import env "rts" "closure_count"
  let size env : G.t = E.call_import env "rts" "closure_table_size"
  let root env : G.t = E.call_import env "rts" "closure_table_loc"
end (* ClosureTable *)

module Bool = struct
  (* Boolean literals are either 0 or 1,
     at StackRep UnboxedWord32
     They need to be shifted before put in the heap
  *)
  let vanilla_lit = function
    | false -> 0l
    | true -> 1l

  let lit b =
    compile_unboxed_const (vanilla_lit b)

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

     Summary:

       0b…11: A pointer
       0b…x0: A shifted scalar
       0b00: `false`
       0b01: `true`

     Note that Word16 and Word8 do not need to be explicitly bit-tagged:
     The bytes are stored in the _most_ significant byte(s) of the `i32`,
     thus lowest two bits are always 0.
     All arithmetic is implemented directly on that representation, see
     module TaggedSmallWord.
  *)
  let if_tagged_scalar env retty is1 is2 =
    compile_bitand_const 0x1l ^^
    G.if_ retty is2 is1

  (* With two bit-tagged pointers on the stack, decide
     whether both are scalars and invoke is1 (the fast path)
     if so, and otherwise is2 (the slow path).
  *)
  let if_both_tagged_scalar env retty is1 is2 =
    G.i (Binary (Wasm.Values.I32 I32Op.Or)) ^^
    compile_bitand_const 0x1l ^^
    G.if_ retty is2 is1

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
    G.if_ retty is1 is2

  let if_can_tag_u64 env retty is1 is2 =
    compile_shrU64_const 30L ^^
    G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
    G.if_ retty is1 is2

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
    G.if_ retty is2 is1 (* NB: swapped branches *)

  let if_can_tag_u32 env retty is1 is2 =
    compile_shrU_const 30l ^^
    G.if_ retty is2 is1 (* NB: swapped branches *)

  let tag_i32 = compile_shl_const 1l
  let untag_i32 env = compile_shrS_const 1l

end (* BitTagged *)

module Tagged = struct
  (* Tagged objects have, well, a tag to describe their runtime type.
     This tag is used to traverse the heap (serialization, GC), but also
     for objectification of arrays.

     The tag is a word at the beginning of the object.

     All tagged heap objects have a size of at least two words
     (important for GC, which replaces them with an Indirection).

     Attention: This mapping is duplicated in rts/rts.c, so update both!
   *)

  type tag =
    | Object
    | ObjInd (* The indirection used for object fields *)
    | Array (* Also a tuple *)
    | Bits64 (* Contains a 64 bit number *)
    | MutBox (* used for mutable heap-allocated variables *)
    | Closure
    | Some (* For opt *)
    | Variant
    | Blob
    | Indirection
    | Bits32 (* Contains a 32 bit unsigned number *)
    | BigInt
    | Concat (* String concatenation, used by rts/text.c *)
    | StableSeen (* Marker that we have seen this thing before *)

  (* Let's leave out tag 0 to trap earlier on invalid memory *)
  let int_of_tag = function
    | Object -> 1l
    | ObjInd -> 2l
    | Array -> 3l
    | Bits64 -> 5l
    | MutBox -> 6l
    | Closure -> 7l
    | Some -> 8l
    | Variant -> 9l
    | Blob -> 10l
    | Indirection -> 11l
    | Bits32 -> 12l
    | BigInt -> 13l
    | Concat -> 14l
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
        G.if_ retty code (go cases)
    in
    load ^^
    set_tag ^^
    go cases

  (* like branch_default but the tag is known statically *)
  let branch env retty = function
    | [] -> G.i Unreachable
    | [_, code] -> G.i Drop ^^ code
    | (_, code) :: cases -> branch_default env retty code cases

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
end


module Opt = struct
  (* The Option type. Not much interesting to see here. Structure for
     Some:

       ┌─────┬─────────┐
       │ tag │ payload │
       └─────┴─────────┘

    A None value is simply an unboxed scalar.

  *)

  let payload_field = Tagged.header_size

  (* This needs to be disjoint from all pointers, i.e. tagged as a scalar. *)
  let null_vanilla_lit = 5l
  let null_lit = compile_unboxed_const null_vanilla_lit

  let is_some env =
    null_lit ^^
    G.i (Compare (Wasm.Values.I32 I32Op.Ne))

  let inject env e = Tagged.obj env Tagged.Some [e]
  let project = Heap.load_field payload_field

end (* Opt *)

module Variant = struct
  (* The Variant type. We store the variant tag in a first word; we can later
     optimize and squeeze it in the Tagged tag. We can also later support unboxing
     variants with an argument of type ().

       ┌─────────┬────────────┬─────────┐
       │ heaptag │ varianttag │ payload │
       └─────────┴────────────┴─────────┘

  *)

  let tag_field = Tagged.header_size
  let payload_field = Int32.add Tagged.header_size 1l

  let hash_variant_label : Mo_types.Type.lab -> int32 =
    Mo_types.Hash.hash

  let inject env l e =
    Tagged.obj env Tagged.Variant [compile_unboxed_const (hash_variant_label l); e]

  let get_tag = Heap.load_field tag_field
  let project = Heap.load_field payload_field

  (* Test if the top of the stacks points to a variant with this label *)
  let test_is env l =
    get_tag ^^
    compile_eq_const (hash_variant_label l)

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
    let rec pow () = Func.share_code2 env "pow"
                       (("n", I64Type), ("exp", I64Type)) [I64Type]
                       Wasm.Values.(fun env get_n get_exp ->
         let one = compile_const_64 1L in
         let (set_res, get_res) = new_local64 env "res" in
         let square_recurse_with_shifted =
           get_n ^^ get_exp ^^ one ^^
           G.i (Binary (I64 I64Op.ShrU)) ^^
           pow () ^^ set_res ^^ get_res ^^ get_res ^^ G.i (Binary (Wasm.Values.I64 I64Op.Mul))
         in get_exp ^^ G.i (Test (I64 I64Op.Eqz)) ^^
            G.if_ [I64Type]
             one
             (get_exp ^^ one ^^ G.i (Binary (I64 I64Op.And)) ^^ G.i (Test (I64 I64Op.Eqz)) ^^
              G.if_ [I64Type]
                square_recurse_with_shifted
                (get_n ^^
                 square_recurse_with_shifted ^^
                 G.i (Binary (Wasm.Values.I64 I64Op.Mul)))))
    in pow ()

  let compile_eq env = G.i (Compare (Wasm.Values.I64 I64Op.Eq))
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
      G.if_ [I32Type]
        (get_n ^^ BitTagged.tag_i32)
        (compile_box env get_n)
    )

  let unbox env = Func.share_code1 env "unbox_i32" ("n", I32Type) [I32Type] (fun env get_n ->
      get_n ^^
      BitTagged.if_tagged_scalar env [I32Type]
        ( get_n ^^ BitTagged.untag_i32 env)
        ( get_n ^^ Heap.load_field payload_field)
    )

  let _lit env n = compile_unboxed_const n ^^ box env

end (* BoxedSmallWord *)

module TaggedSmallWord = struct
  (* While smaller-than-32bit words are treated as i32 from the WebAssembly perspective,
     there are certain differences that are type based. This module provides helpers to abstract
     over those. *)

  let bits_of_type = function
    | Type.(Int8|Nat8|Word8) -> 8
    | Type.(Int16|Nat16|Word16) -> 16
    | _ -> 32

  let shift_of_type ty = Int32.of_int (32 - bits_of_type ty)

  let bitwidth_mask_of_type = function
    | Type.Word8 -> 0b111l
    | Type.Word16 -> 0b1111l
    | p -> todo "bitwidth_mask_of_type" (Arrange_type.prim p) 0l

  let const_of_type ty n = Int32.(shift_left n (to_int (shift_of_type ty)))

  let padding_of_type ty = Int32.(sub (const_of_type ty 1l) one)

  let mask_of_type ty = Int32.lognot (padding_of_type ty)

  (* Makes sure that we only shift/rotate the maximum number of bits available in the word. *)
  let clamp_shift_amount = function
    | Type.Word32 -> G.nop
    | ty -> compile_bitand_const (bitwidth_mask_of_type ty)

  let shift_leftWordNtoI32 = compile_shl_const

  (* Makes sure that the word payload (e.g. shift/rotate amount) is in the LSB bits of the word. *)
  let lsb_adjust = function
    | Type.(Int32|Nat32|Word32) -> G.nop
    | Type.(Nat8|Word8|Nat16|Word16) as ty -> compile_shrU_const (shift_of_type ty)
    | Type.(Int8|Int16) as ty -> compile_shrS_const (shift_of_type ty)
    | _ -> assert false

  (* Makes sure that the word payload (e.g. operation result) is in the MSB bits of the word. *)
  let msb_adjust = function
    | Type.(Int32|Nat32|Word32) -> G.nop
    | ty -> shift_leftWordNtoI32 (shift_of_type ty)

  (* Makes sure that the word representation invariant is restored. *)
  let sanitize_word_result = function
    | Type.Word32 -> G.nop
    | ty -> compile_bitand_const (mask_of_type ty)

  (* Sets the number (according to the type's word invariant) of LSBs. *)
  let compile_word_padding = function
    | Type.Word32 -> G.nop
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
  let check_and_tag_codepoint env get_n =
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

  let vanilla_lit ty v =
    Int32.(shift_left (of_int v) (to_int (shift_of_type ty)))

  (* Wrapping implementation for multiplication and exponentiation. *)

  let compile_word_mul env ty =
    lsb_adjust ty ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Mul))

  let compile_word_power env ty =
    let rec pow () = Func.share_code2 env (prim_fun_name ty "pow")
                       (("n", I32Type), ("exp", I32Type)) [I32Type]
                       Wasm.Values.(fun env get_n get_exp ->
        let one = compile_unboxed_const (const_of_type ty 1l) in
        let (set_res, get_res) = new_local env "res" in
        let mul = compile_word_mul env ty in
        let square_recurse_with_shifted sanitize =
          get_n ^^ get_exp ^^ compile_shrU_const 1l ^^ sanitize ^^
          pow () ^^ set_res ^^ get_res ^^ get_res ^^ mul
        in get_exp ^^ G.i (Test (I32 I32Op.Eqz)) ^^
           G.if_ [I32Type]
             one
             (get_exp ^^ one ^^ G.i (Binary (I32 I32Op.And)) ^^ G.i (Test (I32 I32Op.Eqz)) ^^
              G.if_ [I32Type]
                (square_recurse_with_shifted G.nop)
                (get_n ^^
                 square_recurse_with_shifted (sanitize_word_result ty) ^^
                 mul)))
    in pow ()

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
    get_buf ^^ E.call_import env "rts" "read_u32_of_leb128"

  let read_sleb128 env get_buf =
    get_buf ^^ E.call_import env "rts" "read_i32_of_sleb128"

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
    get_x ^^ G.if_ [I32Type]
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
            G.if_ [I32Type]
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
        G.if_ [I32Type]
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
            G.if_ [I32Type]
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
        G.if_ [I32Type]
          (get_res ^^ Num.truncate_to_word32 env ^^ BitTagged.tag_i32)
          get_res
      end)

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
          G.if_ [I32Type]
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

  let compile_eq = try_comp_unbox2 "B_eq" Word64.compile_eq Num.compile_eq
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
        G.if_ [I32Type]
          begin
            get_a ^^
            (* -2^30 is small enough for compact representation, but 2^30 isn't *)
            compile_eq_const 0x80000000l ^^ (* i.e. -2^30 shifted *)
            G.if_ [I32Type]
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
    G.if_ [I32Type]
      (get_res ^^ Num.truncate_to_word32 env ^^ BitTagged.tag_i32)
      get_res

  let compile_store_to_data_buf_unsigned env =
    let set_x, get_x = new_local env "x" in
    let set_buf, get_buf = new_local env "buf" in
    set_x ^^ set_buf ^^
    get_x ^^
    try_unbox I32Type
      (fun env ->
        BitTagged.untag_i32 env ^^ set_x ^^
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
        BitTagged.untag_i32 env ^^ set_x ^^
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
        BitTagged.untag_i32 env ^^ set_x ^^
        I32Leb.compile_leb128_size get_x
      )
      (fun env -> Num.compile_data_size_unsigned env)
      env

  let compile_data_size_signed env =
    try_unbox I32Type
      (fun _ ->
        let set_x, get_x = new_local env "x" in
        BitTagged.untag_i32 env ^^ set_x ^^
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
      (get_a ^^ BitTagged.untag_i32 env)
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
      (get_a ^^ BitTagged.untag_i32 env)
      (get_a ^^ Num.to_word32 env)

  let to_word32_with env =
    let set_a, get_a = new_local env "a" in
    let set_err_msg, get_err_msg = new_local env "err_msg" in
    set_err_msg ^^ set_a ^^
    get_a ^^
    BitTagged.if_tagged_scalar env [I32Type]
      (get_a ^^ BitTagged.untag_i32 env)
      (get_a ^^ get_err_msg ^^ Num.to_word32_with env)
end

module BigNumLibtommath : BigNumType = struct

  let to_word32 env = E.call_import env "rts" "bigint_to_word32_trap"
  let to_word64 env = E.call_import env "rts" "bigint_to_word64_trap"
  let to_word32_with env = E.call_import env "rts" "bigint_to_word32_trap_with"

  let truncate_to_word32 env = E.call_import env "rts" "bigint_to_word32_wrap"
  let truncate_to_word64 env = E.call_import env "rts" "bigint_to_word64_wrap"

  let from_word32 env = E.call_import env "rts" "bigint_of_word32"
  let from_word64 env = E.call_import env "rts" "bigint_of_word64"
  let from_signed_word32 env = E.call_import env "rts" "bigint_of_word32_signed"
  let from_signed_word64 env = E.call_import env "rts" "bigint_of_word64_signed"

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

    (* copied from Blob *)
    let header_size = Int32.add Tagged.header_size 1l in
    let unskewed_payload_offset = Int32.(add ptr_unskew (mul Heap.word_size header_size)) in

    let limbs =
      (* see MP_DIGIT_BIT *)
      let twoto28 = Big_int.power_int_positive_int 2 28 in
      let rec go n =
        if Big_int.sign_big_int n = 0
        then []
        else
          let (a, b) = Big_int.quomod_big_int n twoto28 in
          [ Int32.of_int (Big_int.int_of_big_int b) ] @ go a
      in go n
    in
    (* how many 32 bit digits *)
    let size = Int32.of_int (List.length limbs) in

    let data_blob = E.add_static env StaticBytes.[
      I32 Tagged.(int_of_tag Blob);
      I32 Int32.(mul Heap.word_size size);
      i32s limbs
    ] in
    let data_ptr = Int32.(add data_blob unskewed_payload_offset) in

    (* cf. mp_int in tommath.h *)
    let ptr = E.add_static env StaticBytes.[
      I32 Tagged.(int_of_tag BigInt);
      I32 size;
      I32 size; (* alloc *)
      I32 sign;
      I32 data_ptr;
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
  (* The Word8 and Word16 bits sit in the MSBs of the i32, in this manner
     we can perform almost all operations, with the exception of
     - Mul (needs shr of one operand)
     - Shr (needs masking of result)
     - Rot (needs duplication into LSBs, masking of amount and masking of result)
     - ctz (needs shr of operand or sub from result)

     Both Word8/16 easily fit into the vanilla stackrep, so no boxing is necessary.
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
  let prim_shiftToWordN env b =
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
    fields need to be able to alias local mutal variables.

    We could alternatively switch to an allocate-first approach in the
    await-translation of objects, and get rid of this indirection -- if it were
    not for the implemenating of sharing of mutable stable values.
  *)

  let header_size = Int32.add Tagged.header_size 2l

  (* Number of object fields *)
  let size_field = Int32.add Tagged.header_size 0l
  let hash_ptr_field = Int32.add Tagged.header_size 1l

  module FieldEnv = Env.Make(String)

  (* This is for static objects *)
  let vanilla_lit env (fs : (string * int32) list) : int32 =
    let open List in
    let (hashes, ptrs) = fs
      |> map (fun (n, ptr) -> (Mo_types.Hash.hash n,ptr))
      |> sort compare
      |> split
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
      List.map (fun (n,_) -> (Mo_types.Hash.hash n, n)) |>
      List.sort compare |>
      List.mapi (fun i (_h,n) -> (n,Int32.of_int i)) |>
      List.fold_left (fun m (n,i) -> FieldEnv.add n i m) FieldEnv.empty in

    let sz = Int32.of_int (FieldEnv.cardinal name_pos_map) in

    (* Create hash array *)
    let hashes = fs |>
      List.map (fun (n,_) -> Mo_types.Hash.hash n) |>
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
  let idx_hash_raw env =
    Func.share_code2 env "obj_idx" (("x", I32Type), ("hash", I32Type)) [I32Type] (fun env get_x get_hash ->
      let (set_h_ptr, get_h_ptr) = new_local env "h_ptr" in

      get_x ^^ Heap.load_field hash_ptr_field ^^ set_h_ptr ^^

      get_x ^^ Heap.load_field size_field ^^
      (* Linearly scan through the fields (binary search can come later) *)
      from_0_to_n env (fun get_i ->
        get_i ^^
        compile_mul_const Heap.word_size  ^^
        get_h_ptr ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        Heap.load_field 0l ^^
        get_hash ^^
        G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
        G.if_ []
          ( get_i ^^
            compile_add_const header_size ^^
            compile_mul_const Heap.word_size ^^
            get_x ^^
            G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
            G.i Return
          ) G.nop
      ) ^^
      E.trap_with env "internal error: object field not found"
    )

  (* Returns a pointer to the object field (possibly following the indirection) *)
  let idx_hash env indirect =
    if indirect
    then Func.share_code2 env "obj_idx_ind" (("x", I32Type), ("hash", I32Type)) [I32Type] (fun env get_x get_hash ->
      get_x ^^ get_hash ^^
      idx_hash_raw env ^^
      load_ptr ^^ compile_add_const Heap.word_size
    )
    else idx_hash_raw env

  (* Determines whether the field is mutable (and thus needs an indirection) *)
  let is_mut_field env obj_type s =
    let _, fields = Type.as_obj_sub [s] obj_type in
    Type.is_mut (Type.lookup_val_field s fields)

  (* Returns a pointer to the object field (without following the indirection) *)
  let idx_raw env f =
    compile_unboxed_const (Mo_types.Hash.hash f) ^^
    idx_hash_raw env

  (* Returns a pointer to the object field (possibly following the indirection) *)
  let idx env obj_type f =
    compile_unboxed_const (Mo_types.Hash.hash f) ^^
    idx_hash env (is_mut_field env obj_type f)

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

  let alloc env = Func.share_code1 env "blob_alloc" ("len", I32Type) [I32Type] (fun env get_len ->
      let (set_x, get_x) = new_local env "x" in
      compile_unboxed_const (Int32.mul Heap.word_size header_size) ^^
      get_len ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      Heap.dyn_alloc_bytes env ^^
      set_x ^^

      get_x ^^ Tagged.(store Blob) ^^
      get_x ^^ get_len ^^ Heap.store_field len_field ^^
      get_x
   )

  let unskewed_payload_offset = Int32.(add ptr_unskew (mul Heap.word_size header_size))
  let payload_ptr_unskewed = compile_add_const unskewed_payload_offset

  let as_ptr_len env = Func.share_code1 env "as_ptr_size" ("x", I32Type) [I32Type; I32Type] (
    fun env get_x ->
      get_x ^^ payload_ptr_unskewed ^^
      get_x ^^ Heap.load_field len_field
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

  let of_size_copy env get_size_fun copy_fun offset =
    let (set_len, get_len) = new_local env "len" in
    let (set_blob, get_blob) = new_local env "blob" in
    get_size_fun env ^^ set_len ^^

    get_len ^^ alloc env ^^ set_blob ^^
    get_blob ^^ payload_ptr_unskewed ^^
    compile_unboxed_const offset ^^
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

        get_x ^^ Heap.load_field len_field ^^ set_len1 ^^
        get_y ^^ Heap.load_field len_field ^^ set_len2 ^^

        (* Find mininum length *)
        begin if op = EqOp then
          (* Early exit for equality *)
          get_len1 ^^ get_len2 ^^ G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
          G.if_ [] G.nop (Bool.lit false ^^ G.i Return) ^^

          get_len1 ^^ set_len
        else
          get_len1 ^^ get_len2 ^^ G.i (Compare (Wasm.Values.I32 I32Op.LeU)) ^^
          G.if_ []
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
          G.if_ [] G.nop (
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

  let len env =
    Heap.load_field len_field ^^ BigNum.from_word32 env
  let iter env =
    E.call_import env "rts" "blob_iter"
  let iter_done env =
    E.call_import env "rts" "blob_iter_done"
  let iter_next env =
    E.call_import env "rts" "blob_iter_next" ^^
    TaggedSmallWord.msb_adjust Type.Word8

  let dyn_alloc_scratch env = alloc env ^^ payload_ptr_unskewed

end (* Blob *)

module Text = struct
  (*
  Most of the heavy lifting around text values is in rts/text.c
  *)

  (* The layout of a concatenation node is

     ┌─────┬─────────┬───────┬───────┐
     │ tag │ n_bytes │ text1 │ text2 │
     └─────┴─────────┴───────┴───────┘

    This is internal to rts/text.c, with the exception of GC-related code.
  *)

  let concat_field1 = Int32.add Tagged.header_size 1l
  let concat_field2 = Int32.add Tagged.header_size 2l

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
  let alloc env =
    let (set_len, get_len) = new_local env "len" in
    let (set_r, get_r) = new_local env "r" in
    set_len ^^

    (* Check size (should not be larger than half the memory space) *)
    get_len ^^
    compile_unboxed_const Int32.(shift_left 1l (32-2-1)) ^^
    G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
    E.else_trap_with env "Array allocation too large" ^^

    (* Allocate *)
    get_len ^^
    compile_add_const header_size ^^
    Heap.dyn_alloc_words env ^^
    set_r ^^

    (* Write header *)
    get_r ^^
    Tagged.(store Array) ^^
    get_r ^^
    get_len ^^
    Heap.store_field len_field ^^

    get_r

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

end (* Array *)

module Tuple = struct
  (* Tuples use the same object representation (and same tag) as arrays.
     Even though we know the size statically, we still need the size
     information for the GC.

     One could introduce tags for small tuples, to save one word.
  *)

  (* We represent the boxed empty tuple as the unboxed scalar 0, i.e. simply as
     number (but really anything is fine, we never look at this) *)
  let unit_vanilla_lit = 1l
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
      G.block_ [] (
        let rec go = function
        | [] -> E.trap_with env
          ("internal error: unexpected state entering " ^ string_of_state new_state)
        | (s::ss) ->
          get env ^^ compile_eq_const (int_of_state s) ^^
          G.if_ [] (G.i (Br (nr 1l))) G.nop ^^
          go ss
        in go (pre_states new_state)
        ) ^^
      set env new_state
    )

end (* Lifecycle *)


module Dfinity = struct
  (* Dfinity-specific stuff: System imports, databufs etc. *)

  let i32s n = Lib.List.make n I32Type

  let import_ic0 env =
      E.add_func_import env "ic0" "call_simple" (i32s 10) [I32Type];
      E.add_func_import env "ic0" "canister_self_copy" (i32s 3) [];
      E.add_func_import env "ic0" "canister_self_size" [] [I32Type];
      E.add_func_import env "ic0" "debug_print" (i32s 2) [];
      E.add_func_import env "ic0" "msg_arg_data_copy" (i32s 3) [];
      E.add_func_import env "ic0" "msg_arg_data_size" [] [I32Type];
      E.add_func_import env "ic0" "msg_caller_copy" (i32s 3) [];
      E.add_func_import env "ic0" "msg_caller_size" [] [I32Type];
      E.add_func_import env "ic0" "msg_reject_code" [] [I32Type];
      E.add_func_import env "ic0" "msg_reject_msg_size" [] [I32Type];
      E.add_func_import env "ic0" "msg_reject_msg_copy" (i32s 3) [];
      E.add_func_import env "ic0" "msg_reject" (i32s 2) [];
      E.add_func_import env "ic0" "msg_reply_data_append" (i32s 2) [];
      E.add_func_import env "ic0" "msg_reply" [] [];
      E.add_func_import env "ic0" "trap" (i32s 2) [];
      E.add_func_import env "ic0" "stable_write" (i32s 3) [];
      E.add_func_import env "ic0" "stable_read" (i32s 3) [];
      E.add_func_import env "ic0" "stable_size" [] [I32Type];
      E.add_func_import env "ic0" "stable_grow" [I32Type] [I32Type];
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

  let print_ptr_len env =
    match E.mode env with
    | Flags.WasmMode -> G.i Drop ^^ G.i Drop
    | Flags.ICMode | Flags.RefMode -> system_call env "ic0" "debug_print"
    | Flags.WASIMode ->
      Func.share_code2 env "print_ptr" (("ptr", I32Type), ("len", I32Type)) [] (fun env get_ptr get_len ->
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
          G.i Drop
        )
      )

  let print_text env =
    Func.share_code1 env "print_text" ("str", I32Type) [] (fun env get_str ->
      let (set_blob, get_blob) = new_local env "blob" in
      get_str ^^ Text.to_blob env ^^ set_blob ^^
      get_blob ^^ Blob.payload_ptr_unskewed ^^
      get_blob ^^ Heap.load_field Blob.len_field ^^
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

  let _trap_text env  =
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
      (* Collect garbage *)
      G.i (Call (nr (E.built_in env "collect"))) ^^

      Lifecycle.trans env Lifecycle.Idle
    ) in
    let fi = E.add_fun env "canister_init" empty_f in
    E.add_export env (nr {
      name = Wasm.Utf8.decode "canister_init";
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

    let pre_upgrade_fi = E.add_fun env "pre_upgrade" (Func.of_body env [] [] (fun env ->
      Lifecycle.trans env Lifecycle.InPreUpgrade ^^
      G.i (Call (nr (E.built_in env "pre_exp"))) ^^
      Lifecycle.trans env Lifecycle.PostPreUpgrade
    )) in

    let post_upgrade_fi = E.add_fun env "post_upgrade" (Func.of_body env [] [] (fun env ->
      Lifecycle.trans env Lifecycle.InInit ^^
      G.i (Call (nr (E.built_in env "init"))) ^^
      Lifecycle.trans env Lifecycle.InPostUpgrade ^^
      G.i (Call (nr (E.built_in env "post_exp"))) ^^
      Lifecycle.trans env Lifecycle.Idle ^^
      G.i (Call (nr (E.built_in env "collect")))
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
          (fun env -> system_call env "ic0" "canister_self_copy") 0l
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
        (fun env -> system_call env "ic0" "msg_caller_copy") 0l
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
        G.if_ [I32Type]
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
        (fun env -> system_call env "ic0" "msg_reject_msg_copy") 0l
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
    match E.mode env with
    | Flags.(ICMode | RefMode) ->
      (* simply tuple canister name and function name *)
      Blob.lit env name ^^
      Tuple.from_stack env 2
    | Flags.WasmMode ->
      E.trap_with env (Printf.sprintf "cannot access actor with -no-system-api")
    | Flags.WASIMode ->
      E.trap_with env (Printf.sprintf "cannot access actor with -wasi-system-api")

  let fail_assert env at =
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

end (* Dfinity *)

module RTS_Exports = struct
  let system_exports env =
    Heap.declare_alloc_functions env;
    E.add_export env (nr {
      name = Wasm.Utf8.decode "alloc_bytes";
      edesc = nr (FuncExport (nr (E.built_in env "alloc_bytes")))
    });
    E.add_export env (nr {
      name = Wasm.Utf8.decode "alloc_words";
      edesc = nr (FuncExport (nr (E.built_in env "alloc_words")))
    });
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
        get_str ^^ get_len ^^ Dfinity.trap_ptr_len env
      )
    ) in
    E.add_export env (nr {
      name = Wasm.Utf8.decode "rts_trap";
      edesc = nr (FuncExport (nr rts_trap_fi))
    })

end (* RTS_Exports *)


module HeapTraversal = struct
  (* Returns the object size (in words) *)
  let object_size env =
    Func.share_code1 env "object_size" ("x", I32Type) [I32Type] (fun env get_x ->
      get_x ^^
      Tagged.branch env [I32Type]
        [ Tagged.Bits64,
          compile_unboxed_const 3l
        ; Tagged.Bits32,
          compile_unboxed_const 2l
        ; Tagged.BigInt,
          compile_unboxed_const 5l (* HeapTag + sizeof(mp_int) *)
        ; Tagged.Some,
          compile_unboxed_const 2l
        ; Tagged.Variant,
          compile_unboxed_const 3l
        ; Tagged.ObjInd,
          compile_unboxed_const 2l
        ; Tagged.MutBox,
          compile_unboxed_const 2l
        ; Tagged.Array,
          get_x ^^
          Heap.load_field Arr.len_field ^^
          compile_add_const Arr.header_size
        ; Tagged.Blob,
          get_x ^^
          Heap.load_field Blob.len_field ^^
          compile_add_const 3l ^^
          compile_divU_const Heap.word_size ^^
          compile_add_const Blob.header_size
        ; Tagged.Object,
          get_x ^^
          Heap.load_field Object.size_field ^^
          compile_add_const Object.header_size
        ; Tagged.Closure,
          get_x ^^
          Heap.load_field Closure.len_field ^^
          compile_add_const Closure.header_size
        ; Tagged.Concat,
          compile_unboxed_const 4l
        ]
        (* Indirections have unknown size. *)
    )

  let walk_heap_from_to env compile_from compile_to mk_code =
      let (set_x, get_x) = new_local env "x" in
      compile_from ^^ set_x ^^
      compile_while
        (* While we have not reached the end of the area *)
        ( get_x ^^
          compile_to ^^
          G.i (Compare (Wasm.Values.I32 I32Op.LtU))
        )
        ( mk_code get_x ^^
          get_x ^^
          get_x ^^ object_size env ^^ compile_mul_const Heap.word_size ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          set_x
        )

  let for_each_array_elem env get_array mk_code =
      get_array ^^
      Heap.load_field Arr.len_field ^^
      from_0_to_n env (fun get_i ->
        mk_code (
          get_array ^^
          get_i ^^
          Arr.idx env
        )
      )

  (* Calls mk_code for each pointer in the object pointed to by get_x,
     passing code get the address of the pointer,
     and code to get the offset of the pointer (for the BigInt payload field). *)
  let for_each_pointer env get_x mk_code mk_code_offset =
    let (set_ptr_loc, get_ptr_loc) = new_local env "ptr_loc" in
    let code = mk_code get_ptr_loc in
    let code_offset = mk_code_offset get_ptr_loc in
    get_x ^^
    Tagged.branch_default env [] G.nop
      [ Tagged.MutBox,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size MutBox.field) ^^
        set_ptr_loc ^^
        code
      ; Tagged.BigInt,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size 4l) ^^
        set_ptr_loc ^^
        code_offset Blob.unskewed_payload_offset
      ; Tagged.Some,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size Opt.payload_field) ^^
        set_ptr_loc ^^
        code
      ; Tagged.Variant,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size Variant.payload_field) ^^
        set_ptr_loc ^^
        code
      ; Tagged.ObjInd,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size 1l) ^^
        set_ptr_loc ^^
        code
      ; Tagged.Array,
        for_each_array_elem env get_x (fun get_elem_ptr ->
          get_elem_ptr ^^
          set_ptr_loc ^^
          code
        )
      ; Tagged.Object,
        get_x ^^
        Heap.load_field Object.size_field ^^

        from_0_to_n env (fun get_i ->
          get_i ^^
          compile_add_const Object.header_size ^^
          compile_mul_const Heap.word_size ^^
          get_x ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          set_ptr_loc ^^
          code
        )
      ; Tagged.Closure,
        get_x ^^
        Heap.load_field Closure.len_field ^^

        from_0_to_n env (fun get_i ->
          get_i ^^
          compile_add_const Closure.header_size ^^
          compile_mul_const Heap.word_size ^^
          get_x ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          set_ptr_loc ^^
          code
        )
      ; Tagged.Concat,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size Text.concat_field1) ^^
        set_ptr_loc ^^
        code ^^
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size Text.concat_field2) ^^
        set_ptr_loc ^^
        code
      ]

end (* HeapTraversal *)

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
    | Prim (Nat8|Word8) -> Some 5l
    | Prim (Nat16|Word16) -> Some 6l
    | Prim (Nat32|Word32|Char) -> Some 7l
    | Prim (Nat64|Word64) -> Some 8l
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
        add_typ Type.(Array (Prim Word8))
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
      let (set_data_size, get_data_size) = new_local env "data_size" in
      let (set_ref_size, get_ref_size) = new_local env "ref_size" in
      compile_unboxed_const 0l ^^ set_data_size ^^
      compile_unboxed_const 0l ^^ set_ref_size ^^

      let inc_data_size code =
        get_data_size ^^ code ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        set_data_size
      in

      let size_word env code =
        let (set_word, get_word) = new_local env "word" in
        code ^^ set_word ^^
        inc_data_size (I32Leb.compile_leb128_size get_word)
      in

      let size env t =
        buffer_size env t ^^
        get_ref_size ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^ set_ref_size ^^
        get_data_size ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^ set_data_size
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
        G.if_ [] begin
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
      | Prim (Int8|Nat8|Word8) -> inc_data_size (compile_unboxed_const 1l)
      | Prim (Int16|Nat16|Word16) -> inc_data_size (compile_unboxed_const 2l)
      | Prim (Int32|Nat32|Word32|Char) -> inc_data_size (compile_unboxed_const 4l)
      | Prim (Int64|Nat64|Word64|Float) -> inc_data_size (compile_unboxed_const 8l)
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
        get_x ^^ Heap.load_field Blob.len_field ^^ set_len ^^
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
        G.if_ [] (get_x ^^ Opt.project ^^ size env t) G.nop
      | Variant vs ->
        List.fold_right (fun (i, {lab = l; typ = t}) continue ->
            get_x ^^
            Variant.test_is env l ^^
            G.if_ []
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
      get_data_size ^^
      get_ref_size
    )

  (* Copies x to the data_buffer, storing references after ref_count entries in ref_base *)
  let rec serialize_go env t =
    let open Type in
    let t = Type.normalize t in
    let name = "@serialize_go<" ^ typ_hash t ^ ">" in
    Func.share_code3 env name (("x", I32Type), ("data_buffer", I32Type), ("ref_buffer", I32Type)) [I32Type; I32Type]
    (fun env get_x get_data_buf get_ref_buf ->
      let set_data_buf = G.i (LocalSet (nr 1l)) in
      let set_ref_buf = G.i (LocalSet (nr 2l)) in

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
        G.if_ []
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
          (* Write the office to the output buffer *)
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
      | Prim (Int64|Nat64|Word64) ->
        get_data_buf ^^
        get_x ^^ BoxedWord64.unbox env ^^
        G.i (Store {ty = I64Type; align = 0; offset = 0l; sz = None}) ^^
        compile_unboxed_const 8l ^^ advance_data_buf
      | Prim (Int32|Nat32|Word32) ->
        get_data_buf ^^
        get_x ^^ BoxedSmallWord.unbox env ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = None}) ^^
        compile_unboxed_const 4l ^^ advance_data_buf
      | Prim Char ->
        get_data_buf ^^
        get_x ^^ TaggedSmallWord.untag_codepoint ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = None}) ^^
        compile_unboxed_const 4l ^^ advance_data_buf
      | Prim (Int16|Nat16|Word16) ->
        get_data_buf ^^
        get_x ^^ TaggedSmallWord.lsb_adjust Word16 ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.Pack16}) ^^
        compile_unboxed_const 2l ^^ advance_data_buf
      | Prim (Int8|Nat8|Word8) ->
        get_data_buf ^^
        get_x ^^ TaggedSmallWord.lsb_adjust Word8 ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.Pack8}) ^^
        compile_unboxed_const 1l ^^ advance_data_buf
      | Prim Bool ->
        get_data_buf ^^
        get_x ^^
        BoxedSmallWord.unbox env (* essentially SR.adjust SR.Vanilla SR.bool *) ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Types.Pack8}) ^^
        compile_unboxed_const 1l ^^ advance_data_buf
      | Tup [] -> (* e(()) = null *)
        G.nop
      | Tup ts ->
        G.concat_mapi (fun i t ->
          get_x ^^ Tuple.load_n (Int32.of_int i) ^^
          write env t
        ) ts
      | Obj ((Object | Memory), fs) ->
        G.concat_map (fun (_h,f) ->
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
        G.if_ []
          ( write_byte (compile_unboxed_const 1l) ^^ get_x ^^ Opt.project ^^ write env t )
          ( write_byte (compile_unboxed_const 0l) )
      | Variant vs ->
        List.fold_right (fun (i, {lab = l; typ = t}) continue ->
            get_x ^^
            Variant.test_is env l ^^
            G.if_ []
              ( write_word (compile_unboxed_const (Int32.of_int i)) ^^
                get_x ^^ Variant.project ^^ write env t)
              continue
          )
          ( List.mapi (fun i (_h, f) -> (i,f)) (sort_by_hash vs) )
          ( E.trap_with env "serialize_go: unexpected variant" )
      | Prim Blob ->
        let (set_len, get_len) = new_local env "len" in
        get_x ^^ Heap.load_field Blob.len_field ^^ set_len ^^
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

  let rec deserialize_go env t =
    let open Type in
    let t = Type.normalize t in
    let name = "@deserialize_go<" ^ typ_hash t ^ ">" in
    Func.share_code4 env name
      (("data_buffer", I32Type),
       ("ref_buffer", I32Type),
       ("typtbl", I32Type),
       ("idltyp", I32Type)
      ) [I32Type]
    (fun env get_data_buf get_ref_buf get_typtbl get_idltyp ->

      let go env t =
        let (set_idlty, get_idlty) = new_local env "idl_ty" in
        set_idlty ^^
        get_data_buf ^^
        get_ref_buf ^^
        get_typtbl ^^
        get_idlty ^^
        deserialize_go env t
      in

      let check_prim_typ t =
        get_idltyp ^^
        compile_eq_const (Int32.neg (Option.get (to_idl_prim t)))
      in

      let assert_prim_typ t =
        check_prim_typ t ^^
        E.else_trap_with env ("IDL error: unexpected IDL type when parsing " ^ string_of_typ t)
      in

      let read_byte_tagged = function
        | [code0; code1] ->
          ReadBuf.read_byte env get_data_buf ^^
          let (set_b, get_b) = new_local env "b" in
          set_b ^^
          get_b ^^
          compile_eq_const 0l ^^
          G.if_ [I32Type]
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
          ; read_blob ()
          ]
      in

      (* checks that idltyp is positive, looks it up in the table,
         creates a fresh typ_buf pointing into the type description,
         reads the type constructor index and traps if it is the wrong one.
         and passes the typ_buf to a subcomputation to read the type arguments *)
      let with_composite_arg_typ get_arg_typ idl_tycon_id f =
        (* make sure index is not negative *)
        get_arg_typ ^^
        compile_unboxed_const 0l ^^ G.i (Compare (Wasm.Values.I32 I32Op.GeS)) ^^
        E.else_trap_with env ("IDL error: expected composite type when parsing " ^ string_of_typ t) ^^
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
          compile_eq_const idl_tycon_id ^^
          E.else_trap_with env ("IDL error: wrong composite type when parsing " ^ string_of_typ t) ^^
          (* to the work *)
          f get_typ_buf
        ) in

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

      let assert_blob_typ env =
        with_composite_typ idl_vec (fun get_typ_buf ->
          ReadBuf.read_sleb128 env get_typ_buf ^^
          compile_eq_const (-5l) (* Nat8 *) ^^
          E.else_trap_with env ("IDL error: blob not a vector of nat8")
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
        G.if_ [] begin
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
        G.if_ [] begin
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
        G.if_ [] (ReadBuf.set_ptr get_data_buf get_cur) G.nop ^^

        get_result
      in


      (* Now the actual deserialization *)
      begin match t with
      (* Primitive types *)
      | Prim Nat ->
        assert_prim_typ t ^^
        get_data_buf ^^
        BigNum.compile_load_from_data_buf env false
      | Prim Int ->
        (* Subtyping with nat *)
        check_prim_typ (Prim Nat) ^^
        G.if_ [I32Type]
          begin
            get_data_buf ^^
            BigNum.compile_load_from_data_buf env false
          end
          begin
            assert_prim_typ t ^^
            get_data_buf ^^
            BigNum.compile_load_from_data_buf env true
          end
      | Prim Float ->
        assert_prim_typ t ^^
        ReadBuf.read_float64 env get_data_buf ^^
        Float.box env
      | Prim (Int64|Nat64|Word64) ->
        assert_prim_typ t ^^
        ReadBuf.read_word64 env get_data_buf ^^
        BoxedWord64.box env
      | Prim (Int32|Nat32|Word32) ->
        assert_prim_typ t ^^
        ReadBuf.read_word32 env get_data_buf ^^
        BoxedSmallWord.box env
      | Prim Char ->
        let set_n, get_n = new_local env "len" in
        assert_prim_typ t ^^
        ReadBuf.read_word32 env get_data_buf ^^ set_n ^^
        TaggedSmallWord.check_and_tag_codepoint env get_n
      | Prim (Int16|Nat16|Word16) ->
        assert_prim_typ t ^^
        ReadBuf.read_word16 env get_data_buf ^^
        TaggedSmallWord.msb_adjust Word16
      | Prim (Int8|Nat8|Word8) ->
        assert_prim_typ t ^^
        ReadBuf.read_byte env get_data_buf ^^
        TaggedSmallWord.msb_adjust Word8
      | Prim Bool ->
        assert_prim_typ t ^^
        read_byte_tagged
          [ Bool.lit false
          ; Bool.lit true
          ] ^^
        BoxedSmallWord.box env (* essentially SR.adjust SR.bool SR.Vanilla *)
      | Prim Null ->
        assert_prim_typ t ^^
        Opt.null_lit
      | Any ->
        (* Skip values of any possible type *)
        get_data_buf ^^ get_typtbl ^^ get_idltyp ^^ compile_unboxed_const 0l ^^
        E.call_import env "rts" "skip_any" ^^

        (* Any vanilla value works here *)
        Opt.null_lit
      | Prim Blob ->
        assert_blob_typ env ^^
        read_blob ()
      | Prim Principal ->
        assert_prim_typ t ^^
        read_byte_tagged
          [ E.trap_with env "IDL error: unexpected principal reference"
            ; read_blob ()
          ]
      | Prim Text ->
        assert_prim_typ t ^^
        read_text ()
      | Tup [] -> (* e(()) = null *)
        assert_prim_typ t ^^
        Tuple.from_stack env 0
      (* Composite types *)
      | Tup ts ->
        with_record_typ (fun get_typ_buf get_n_ptr ->
          G.concat_mapi (fun i t ->
            (* skip all possible intermediate extra fields *)
            get_typ_buf ^^ get_data_buf ^^ get_typtbl ^^ compile_unboxed_const (Int32.of_int i) ^^ get_n_ptr ^^
            E.call_import env "rts" "find_field" ^^
            G.if_ [I32Type]
              begin
                ReadBuf.read_sleb128 env get_typ_buf ^^ go env t
              end
              begin
                E.trap_with env "IDL error: did not find tuple field in record"
              end
          ) ts ^^

          (* skip all possible trailing extra fields *)
          get_typ_buf ^^ get_data_buf ^^ get_typtbl ^^ get_n_ptr ^^
          E.call_import env "rts" "skip_fields" ^^

          Tuple.from_stack env (List.length ts)
        )
      | Obj ((Object | Memory) as sort, fs) ->
        with_record_typ (fun get_typ_buf get_n_ptr ->

          Object.lit_raw env (List.map (fun (h,f) ->
            f.Type.lab, fun () ->
              (* skip all possible intermediate extra fields *)
              get_typ_buf ^^ get_data_buf ^^ get_typtbl ^^ compile_unboxed_const (Lib.Uint32.to_int32 h) ^^ get_n_ptr ^^
              E.call_import env "rts" "find_field" ^^
              G.if_ [I32Type]
                begin
                  ReadBuf.read_sleb128 env get_typ_buf ^^ go env f.typ
                end
                begin
                  match sort with
                  | Object ->
                    E.trap_with env (Printf.sprintf "IDL error: did not find field %s in record" f.lab)
                  | Memory ->
                    assert (is_opt f.typ);
                    Opt.null_lit
                  | _ -> assert false
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
          let (set_arg_typ, get_arg_typ) = new_local env "arg_typ" in
          with_composite_arg_typ get_array_typ idl_vec (ReadBuf.read_sleb128 env) ^^ set_arg_typ ^^
          ReadBuf.read_leb128 env get_data_buf ^^ set_len ^^
          get_len ^^ Arr.alloc env ^^ set_x ^^
          on_alloc get_x ^^
          get_len ^^ from_0_to_n env (fun get_i ->
            get_x ^^ get_i ^^ Arr.idx env ^^
            get_arg_typ ^^ go env t ^^
            store_ptr
          )
        )
      | Array t ->
        let (set_len, get_len) = new_local env "len" in
        let (set_x, get_x) = new_local env "x" in
        let (set_arg_typ, get_arg_typ) = new_local env "arg_typ" in
        with_composite_typ idl_vec (ReadBuf.read_sleb128 env) ^^ set_arg_typ ^^
        ReadBuf.read_leb128 env get_data_buf ^^ set_len ^^
        get_len ^^ Arr.alloc env ^^ set_x ^^
        get_len ^^ from_0_to_n env (fun get_i ->
          get_x ^^ get_i ^^ Arr.idx env ^^
          get_arg_typ ^^ go env t ^^
          store_ptr
        ) ^^
        get_x
      | Opt t ->
        check_prim_typ (Prim Null) ^^
        G.if_ [I32Type]
          begin
            Opt.null_lit
          end
          begin
            let (set_arg_typ, get_arg_typ) = new_local env "arg_typ" in
            with_composite_typ idl_opt (ReadBuf.read_sleb128 env) ^^ set_arg_typ ^^
            read_byte_tagged
              [ Opt.null_lit
              ; Opt.inject env (get_arg_typ ^^ go env t)
              ]
          end
      | Variant vs ->
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

          List.fold_right (fun (h, {lab = l; typ = t}) continue ->
              get_tag ^^ compile_eq_const (Lib.Uint32.to_int32 h) ^^
              G.if_ [I32Type]
                ( Variant.inject env l (get_arg_typ ^^ go env t) )
                continue
            )
            ( sort_by_hash vs )
            ( E.trap_with env "IDL error: unexpected variant tag" )
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
      end
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

      compile_add_const tydesc_len  ^^
      set_data_size ^^

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

      get_blob ^^ Heap.load_field Blob.len_field ^^ set_data_size ^^
      get_blob ^^ Blob.payload_ptr_unskewed ^^ set_data_start ^^

      (* Allocate space for the reference buffer and copy it *)
      compile_unboxed_const 0l ^^ set_refs_size (* none yet *) ^^

      (* Allocate space for out parameters of parse_idl_header *)
      Stack.with_words env "get_typtbl_ptr" 1l (fun get_typtbl_ptr ->
      Stack.with_words env "get_maintyps_ptr" 1l (fun get_maintyps_ptr ->

      (* Set up read buffers *)
      ReadBuf.alloc env (fun get_data_buf -> ReadBuf.alloc env (fun get_ref_buf ->

      ReadBuf.set_ptr get_data_buf get_data_start ^^
      ReadBuf.set_size get_data_buf get_data_size ^^
      ReadBuf.set_ptr get_ref_buf get_refs_start ^^
      ReadBuf.set_size get_ref_buf (get_refs_size ^^ compile_mul_const Heap.word_size) ^^

      (* Go! *)
      Bool.lit extended ^^ get_data_buf ^^ get_typtbl_ptr ^^ get_maintyps_ptr ^^
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
          deserialize_go env t
        ) ts ^^

        get_arg_count ^^ compile_eq_const (Int32.of_int (List.length ts)) ^^
        G.if_ []
          begin
            ReadBuf.is_empty env get_data_buf ^^
            E.else_trap_with env ("IDL error: left-over bytes " ^ ts_name) ^^
            ReadBuf.is_empty env get_ref_buf ^^
            E.else_trap_with env ("IDL error: left-over references " ^ ts_name)
          end G.nop
      )))))
    )

  let deserialize env ts =
    Blob.of_size_copy env
      (fun env -> Dfinity.system_call env "ic0" "msg_arg_data_size")
      (fun env -> Dfinity.system_call env "ic0" "msg_arg_data_copy") 0l ^^
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


(* Stabilization (serialization to/from stable memory) *)

module Stabilization = struct

  let stabilize env t =
    let (set_dst, get_dst) = new_local env "dst" in
    let (set_len, get_len) = new_local env "len" in
    Serialization.serialize env [t] ^^
    set_len ^^
    set_dst ^^

    let (set_pages, get_pages) = new_local env "len" in
    get_len ^^
    compile_add_const 4l ^^  (* reserve one word for size *)
    compile_divU_const page_size ^^
    compile_add_const 1l ^^
    set_pages ^^

    (* grow stable memory if needed *)
    let (set_pages_needed, get_pages_needed) = new_local env "pages_needed" in
    get_pages ^^
    E.call_import env "ic0" "stable_size" ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
    set_pages_needed ^^

    get_pages_needed ^^
    compile_unboxed_zero ^^
    G.i (Compare (Wasm.Values.I32 I32Op.GtS)) ^^
    G.if_ []
      ( get_pages_needed ^^
        E.call_import env "ic0" "stable_grow" ^^
        (* Check result *)
        compile_unboxed_zero ^^
        G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^
        E.then_trap_with env "Cannot grow stable memory."
      ) G.nop
    ^^

    (* write len to initial word of stable memory*)
    Stack.with_words env "get_size_ptr" 1l (fun get_size_ptr ->

      get_size_ptr ^^ get_len ^^ store_unskewed_ptr ^^

      compile_unboxed_const 0l ^^
      get_size_ptr ^^ compile_unboxed_const 4l ^^
      Dfinity.system_call env "ic0" "stable_write") ^^

    (* copy data to following stable memory *)
    compile_unboxed_const 4l ^^
    get_dst ^^
    get_len ^^
    E.call_import env "ic0" "stable_write"


  (* return the initial i32 in stable memory recording the size of the following stable data *)
  let stable_data_size env =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      (* read size from initial word of (assumed non-empty) stable memory*)
      Stack.with_words env "get_size_ptr" 1l (fun get_size_ptr ->
        get_size_ptr ^^ compile_unboxed_const 0l ^^  compile_unboxed_const 4l ^^
        Dfinity.system_call env "ic0" "stable_read" ^^
        get_size_ptr ^^ load_unskewed_ptr)
    | _ -> assert false

  let destabilize env t =
    Blob.of_size_copy env stable_data_size
      (* copy the stable data from stable memory from offset 4 *)
      (fun env -> Dfinity.system_call env "ic0" "stable_read") 4l ^^
    Serialization.deserialize_from_blob true env [t]

end

module GC = struct
  (* This is a very simple GC:
     It copies everything live to the to-space beyond the bump pointer,
     then it memcpies it back, over the from-space (so that we still neatly use
     the beginning of memory).

     Roots are:
     * All objects in the static part of the memory.
     * the closure_table (see module ClosureTable)
  *)

  let gc_enabled = true

  (* If the pointer at ptr_loc points after begin_from_space, copy
     to after end_to_space, and replace it with a pointer, adjusted for where
     the object will be finally. *)
  (* Returns the new end of to_space *)
  (* Invariant: Must not be called on the same pointer twice. *)
  (* All pointers, including ptr_loc and space end markers, are skewed *)

  let evacuate_common env
        get_obj update_ptr
        get_begin_from_space get_begin_to_space get_end_to_space
        =

    let (set_len, get_len) = new_local env "len" in

    (* If this is static, ignore it *)
    get_obj ^^
    get_begin_from_space ^^
    G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
    G.if_ [] (get_end_to_space ^^ G.i Return) G.nop ^^

    (* If this is an indirection, just use that value *)
    get_obj ^^
    Tagged.branch_default env [] G.nop [
      Tagged.Indirection,
      update_ptr (get_obj ^^ Heap.load_field 1l) ^^
      get_end_to_space ^^ G.i Return
    ] ^^

    (* Get object size *)
    get_obj ^^ HeapTraversal.object_size env ^^ set_len ^^

    (* Grow memory if needed *)
    get_end_to_space ^^
    get_len ^^ compile_mul_const Heap.word_size ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
    Heap.grow_memory env ^^

    (* Copy the referenced object to to space *)
    get_obj ^^ HeapTraversal.object_size env ^^ set_len ^^

    get_end_to_space ^^ get_obj ^^ get_len ^^ Heap.memcpy_words_skewed env ^^

    let (set_new_ptr, get_new_ptr) = new_local env "new_ptr" in

    (* Calculate new pointer *)
    get_end_to_space ^^
    get_begin_to_space ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
    get_begin_from_space ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
    set_new_ptr ^^

    (* Set indirection *)
    get_obj ^^
    Tagged.(store Indirection) ^^
    get_obj ^^
    get_new_ptr ^^
    Heap.store_field 1l ^^

    (* Update pointer *)
    update_ptr get_new_ptr ^^

    (* Calculate new end of to space *)
    get_end_to_space ^^
    get_len ^^ compile_mul_const Heap.word_size ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Add))

  (* Used for normal skewed pointers *)
  let evacuate env = Func.share_code4 env "evacuate" (("begin_from_space", I32Type), ("begin_to_space", I32Type), ("end_to_space", I32Type), ("ptr_loc", I32Type)) [I32Type] (fun env get_begin_from_space get_begin_to_space get_end_to_space get_ptr_loc ->

    let get_obj = get_ptr_loc ^^ load_ptr in

    (* If this is an unboxed scalar, ignore it *)
    get_obj ^^
    BitTagged.if_tagged_scalar env [] (get_end_to_space ^^ G.i Return) G.nop ^^

    let update_ptr new_val_code =
      get_ptr_loc ^^ new_val_code ^^ store_ptr in

    evacuate_common env
        get_obj update_ptr
        get_begin_from_space get_begin_to_space get_end_to_space
  )

  (* A variant for pointers that point into the payload (used for the bignum objects).
     These are never scalars. *)
  let evacuate_offset env offset =
    let name = Printf.sprintf "evacuate_offset_%d" (Int32.to_int offset) in
    Func.share_code4 env name (("begin_from_space", I32Type), ("begin_to_space", I32Type), ("end_to_space", I32Type), ("ptr_loc", I32Type)) [I32Type] (fun env get_begin_from_space get_begin_to_space get_end_to_space get_ptr_loc ->
    let get_obj = get_ptr_loc ^^ load_ptr ^^ compile_sub_const offset in

    let update_ptr new_val_code =
      get_ptr_loc ^^ new_val_code ^^ compile_add_const offset ^^ store_ptr in

    evacuate_common env
        get_obj update_ptr
        get_begin_from_space get_begin_to_space get_end_to_space
  )

  let register env static_roots =
    Func.define_built_in env "get_heap_size" [] [I32Type] (fun env ->
      Heap.get_heap_ptr env ^^
      Heap.get_heap_base env ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Sub))
    );

    Func.define_built_in env "collect" [] [] (fun env ->
      if not gc_enabled then G.nop else

      (* Copy all roots. *)
      let (set_begin_from_space, get_begin_from_space) = new_local env "begin_from_space" in
      let (set_begin_to_space, get_begin_to_space) = new_local env "begin_to_space" in
      let (set_end_to_space, get_end_to_space) = new_local env "end_to_space" in

      Heap.get_heap_base env ^^ compile_add_const ptr_skew ^^ set_begin_from_space ^^
      let get_end_from_space = get_begin_to_space in
      Heap.get_skewed_heap_ptr env ^^ set_begin_to_space ^^
      Heap.get_skewed_heap_ptr env ^^ set_end_to_space ^^


      (* Common arguments for evacuate *)
      let evac get_ptr_loc =
          get_begin_from_space ^^
          get_begin_to_space ^^
          get_end_to_space ^^
          get_ptr_loc ^^
          evacuate env ^^
          set_end_to_space in

      let evac_offset get_ptr_loc offset =
          get_begin_from_space ^^
          get_begin_to_space ^^
          get_end_to_space ^^
          get_ptr_loc ^^
          evacuate_offset env offset ^^
          set_end_to_space in

      (* Go through the roots, and evacuate them *)
      HeapTraversal.for_each_array_elem env (compile_unboxed_const static_roots) (fun get_elem_ptr ->
        let (set_static, get_static) = new_local env "static_obj" in
        get_elem_ptr ^^ load_ptr ^^ set_static ^^
        HeapTraversal.for_each_pointer env get_static evac evac_offset
      ) ^^
      evac (ClosureTable.root env) ^^

      (* Go through the to-space, and evacuate that.
         Note that get_end_to_space changes as we go, but walk_heap_from_to can handle that.
       *)
      HeapTraversal.walk_heap_from_to env
        get_begin_to_space
        get_end_to_space
        (fun get_x -> HeapTraversal.for_each_pointer env get_x evac evac_offset) ^^

      (* Note some stats *)
      get_end_to_space ^^ get_begin_to_space ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
      Heap.note_live_size env ^^

      get_end_from_space ^^ get_begin_from_space ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
      get_end_to_space ^^ get_begin_to_space ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
      Heap.add_reclaimed env ^^

      (* Copy the to-space to the beginning of memory. *)
      get_begin_from_space ^^ compile_add_const ptr_unskew ^^
      get_begin_to_space ^^ compile_add_const ptr_unskew ^^
      get_end_to_space ^^ get_begin_to_space ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
      Heap.memcpy env ^^

      (* Reset the heap pointer *)
      get_begin_from_space ^^ compile_add_const ptr_unskew ^^
      get_end_to_space ^^ get_begin_to_space ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      Heap.set_heap_ptr env
  )

  let get_heap_size env =
    G.i (Call (nr (E.built_in env "get_heap_size")))

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
    | Prim (Nat64 | Int64 | Word64) -> UnboxedWord64
    | Prim (Nat32 | Int32 | Word32) -> UnboxedWord32
    | Prim (Nat8 | Nat16 | Int8 | Int16 | Word8 | Word16 | Char) -> Vanilla
    | Prim (Text | Blob | Principal) -> Vanilla
    | Prim Float -> UnboxedFloat64
    | p -> todo "StackRep.of_type" (Arrange_ir.typ p) Vanilla

  let to_block_type env = function
    | Vanilla -> [I32Type]
    | UnboxedWord64 -> [I64Type]
    | UnboxedWord32 -> [I32Type]
    | UnboxedFloat64 -> [F64Type]
    | UnboxedTuple 0 -> []
    | UnboxedTuple 1 -> [I32Type]
    | UnboxedTuple n ->
      assert false; (* not supported without muti_value *)
    | Const _ -> []
    | Unreachable -> []

  let to_string = function
    | Vanilla -> "Vanilla"
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
    | Vanilla | UnboxedWord64 | UnboxedWord32 | UnboxedFloat64 -> G.i Drop
    | UnboxedTuple n -> G.table n (fun _ -> G.i Drop)
    | Const _ | Unreachable -> G.nop

  (* Materializes a Const.lit: If necessary, puts
     bytes into static memory, and returns a vanilla value.
  *)
  let materialize_lit env (lit : Const.lit) : int32 =
    match lit with
      (* Booleans are directly in Vanilla representation *)
      | Const.Vanilla n  -> n
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

    | UnboxedWord64, Vanilla -> BoxedWord64.box env
    | Vanilla, UnboxedWord64 -> BoxedWord64.unbox env

    | UnboxedWord32, Vanilla -> BoxedSmallWord.box env
    | Vanilla, UnboxedWord32 -> BoxedSmallWord.unbox env

    | UnboxedFloat64, Vanilla -> Float.box env
    | Vanilla, UnboxedFloat64 -> Float.unbox env

    | Const c, Vanilla -> compile_unboxed_const (materialize_const_t env c)
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
       with an offset (in words) to value.
       Used for mutable captured data *)
    | HeapInd of int32 * int32
    (* A static mutable memory location (static address of a MutBox field) *)
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

  let reuse_local_with_offset (ae : t) name i off =
      { ae with vars = NameEnv.add name (HeapInd (i, off)) ae.vars }

  let add_local_with_offset env (ae : t) name off =
      let i = E.add_anon_local env I32Type in
      E.add_local_name env i name;
      (reuse_local_with_offset ae name i off, i)

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
  let rec add_argument_locals env (ae : t) = function
    | [] -> (ae, [])
    | (name :: names) ->
      let i = E.add_anon_local env I32Type in
      E.add_local_name env i name;
      let ae' = { ae with vars = NameEnv.add name (Local i) ae.vars } in
      let (ae_final, setters) = add_argument_locals env ae' names
      in (ae_final, G.i (LocalSet (nr i)) :: setters)

  let add_label (ae : t) name (d : G.depth) =
      { ae with labels = NameEnv.add name d ae.labels }

  let get_label_depth (ae : t) name : G.depth  =
    match NameEnv.find_opt name ae.labels with
      | Some d -> d
      | None   -> Printf.eprintf "Could not find %s\n" name; raise Not_found

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
    | Some (HeapInd (i, off)) ->
      let (set_new_val, get_new_val) = new_local env "new_val" in
      set_new_val ^^
      G.i (LocalGet (nr i)) ^^
      get_new_val ^^
      Heap.store_field off
    | Some (HeapStatic ptr) ->
      let (set_new_val, get_new_val) = new_local env "new_val" in
      set_new_val ^^
      compile_unboxed_const ptr ^^
      get_new_val ^^
      Heap.store_field 1l
    | Some (Const _) -> fatal "set_val: %s is const" var
    | Some (PublicMethod _) -> fatal "set_val: %s is PublicMethod" var
    | None   -> fatal "set_val: %s missing" var

  (* Returns the payload (optimized representation) *)
  let get_val (env : E.t) (ae : VarEnv.t) var = match VarEnv.lookup_var ae var with
    | Some (Local i) ->
      SR.Vanilla, G.i (LocalGet (nr i))
    | Some (HeapInd (i, off)) ->
      SR.Vanilla, G.i (LocalGet (nr i)) ^^ Heap.load_field off
    | Some (HeapStatic i) ->
      SR.Vanilla, compile_unboxed_const i ^^ Heap.load_field 1l
    | Some (Const c) ->
      SR.Const c, G.nop
    | Some (PublicMethod (_, name)) ->
      SR.Vanilla,
      Dfinity.get_self_reference env ^^
      Dfinity.actor_public_field env name
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
    | Some (HeapInd (i, off)) ->
      ( G.i (LocalGet (nr i))
      , fun new_env ae1 ->
        let ae2, j = VarEnv.add_local_with_offset new_env ae1 var off in
        let restore_code = G.i (LocalSet (nr j))
        in ae2, fun body -> restore_code ^^ body
      )
    | _ -> assert false

  (* Returns a pointer to a heap allocated box for this.
     (either a mutbox, if already mutable, or a freshly allocated box)
  *)
  let field_box env code =
    Tagged.obj env Tagged.ObjInd [ code ]

  let get_val_ptr env ae var = match VarEnv.lookup_var ae var with
    | Some (HeapInd (i, 1l)) -> G.i (LocalGet (nr i))
    | Some (HeapStatic _) -> assert false (* we never do this on the toplevel *)
    | _  -> field_box env (get_val_vanilla env ae var)

end (* Var *)

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
        G.i (Call (nr (E.built_in env "collect"))) ^^
        Lifecycle.trans env Lifecycle.Idle
      | Type.Shared Type.Query ->
        Lifecycle.trans env Lifecycle.PostQuery
      | _ -> assert false

  let compile_const_message outer_env outer_ae sort control args mk_body ret_tys at : E.func_with_names =
    let ae0 = VarEnv.mk_fun_ae outer_ae in
    Func.of_body outer_env [] [] (fun env -> G.with_region at (
      message_start env sort ^^
      (* reply early for a oneway *)
      (if control = Type.Returns
       then
         Tuple.compile_unit ^^
         Serialization.serialize env [] ^^
         Dfinity.reply_with_data env
       else G.nop) ^^
      (* Deserialize argument and add params to the environment *)
      let arg_names = List.map (fun a -> a.it) args in
      let arg_tys = List.map (fun a -> a.note) args in
      let (ae1, setters) = VarEnv.add_argument_locals env ae0 arg_names in
      Serialization.deserialize env arg_tys ^^
      G.concat (List.rev setters) ^^
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

  (* Returns the index of a saved closure *)
  let async_body env ae ts free_vars mk_body at =
    (* We compile this as a local, returning function, so set return type to [] *)
    let sr, code = lit env ae "anon_async" Type.Local Type.Returns free_vars [] mk_body [] at in
    code ^^
    StackRep.adjust env sr SR.Vanilla ^^
    ClosureTable.remember env

  (* Takes the reply and reject callbacks, tuples them up,
     add them to the closure table, and returns the two callbacks expected by
     call_simple.

     The tupling is necessary because we want to free _both_ closures when
     one is called.

     The reply callback function exists once per type (it has to do
     serialization); the reject callback function is unique.
  *)

  let closures_to_reply_reject_callbacks env ts =
    let reply_name = "@callback<" ^ Typ_hash.typ_hash (Type.Tup ts) ^ ">" in
    Func.define_built_in env reply_name ["env", I32Type] [] (fun env ->
        message_start env (Type.Shared Type.Write) ^^
        (* Look up closure *)
        let (set_closure, get_closure) = new_local env "closure" in
        G.i (LocalGet (nr 0l)) ^^
        ClosureTable.recall env ^^
        Arr.load_field 0l ^^ (* get the reply closure *)
        set_closure ^^
        get_closure ^^

        (* Deserialize arguments  *)
        Serialization.deserialize env ts ^^

        get_closure ^^
        Closure.call_closure env (List.length ts) 0 ^^

        message_cleanup env (Type.Shared Type.Write)
      );

    let reject_name = "@reject_callback" in
    Func.define_built_in env reject_name ["env", I32Type] [] (fun env ->
        message_start env (Type.Shared Type.Write) ^^
        (* Look up closure *)
        let (set_closure, get_closure) = new_local env "closure" in
        G.i (LocalGet (nr 0l)) ^^
        ClosureTable.recall env ^^
        Arr.load_field 1l ^^ (* get the reject closure *)
        set_closure ^^
        get_closure ^^

        (* Synthesize value of type `Text`, the error message
           (The error code is fetched via a prim)
        *)
        Dfinity.error_value env ^^

        get_closure ^^
        Closure.call_closure env 1 0 ^^

        message_cleanup env (Type.Shared Type.Write)
      );

    (* The upper half of this function must not depend on the get_k and get_r
       parameters, so hide them from above (cute trick) *)
    fun get_k get_r ->
      let (set_cb_index, get_cb_index) = new_local env "cb_index" in
      (* store the tuple away *)
      Arr.lit env [get_k; get_r] ^^
      ClosureTable.remember env ^^
      set_cb_index ^^

      (* return arguments for the ic.call *)
      compile_unboxed_const (E.add_fun_ptr env (E.built_in env reply_name)) ^^
      get_cb_index ^^
      compile_unboxed_const (E.add_fun_ptr env (E.built_in env reject_name)) ^^
      get_cb_index

  let ignoring_callback env =
    let name = "@ignore_callback" in
    Func.define_built_in env name ["env", I32Type] [] (fun env -> G.nop);
    compile_unboxed_const (E.add_fun_ptr env (E.built_in env name))

  let ic_call env ts1 ts2 get_meth_pair get_arg get_k get_r =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->

      (* The callee *)
      get_meth_pair ^^ Arr.load_field 0l ^^ Blob.as_ptr_len env ^^
      (* The method name *)
      get_meth_pair ^^ Arr.load_field 1l ^^ Blob.as_ptr_len env ^^
      (* The reply and reject callback *)
      closures_to_reply_reject_callbacks env ts2 get_k get_r ^^
      (* the data *)
      get_arg ^^ Serialization.serialize env ts1 ^^
      (* done! *)
      Dfinity.system_call env "ic0" "call_simple" ^^
      (* Check error code *)
      G.i (Test (Wasm.Values.I32 I32Op.Eqz)) ^^
      E.else_trap_with env "could not perform call"
    | _ ->
      E.trap_with env (Printf.sprintf "cannot perform remote call when running locally")

  let ic_call_one_shot env ts get_meth_pair get_arg =
    match E.mode env with
    | Flags.ICMode | Flags.RefMode ->

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
      (* the data *)
      get_arg ^^ Serialization.serialize env ts ^^
      (* done! *)
      Dfinity.system_call env "ic0" "call_simple" ^^
      (* This is a one-shot function: Ignore error code *)
      G.i Drop
    | _ -> assert false

  let export_async_method env =
    let name = Dfinity.async_method_name in
    begin match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Func.define_built_in env name [] [] (fun env ->
        let (set_closure, get_closure) = new_local env "closure" in

        message_start env (Type.Shared Type.Write) ^^

        (* Check that we are calling this *)
        Dfinity.assert_caller_self env ^^

        (* Deserialize and look up closure argument *)
        Serialization.deserialize env Type.[Prim Word32] ^^
        BoxedSmallWord.unbox env ^^
        ClosureTable.recall env ^^
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
          G.labeled_block_ [I32Type] inner_fail (is1 inner_fail_code ^^ Bool.lit true) ^^
          G.if_ [] G.nop (is2 fail_code)
        )
      | CannotFail is2 -> CannotFail (
          let inner_fail = G.new_depth_label () in
          let inner_fail_code = Bool.lit false ^^ G.branch_to_ inner_fail in
          G.labeled_block_ [I32Type] inner_fail (is1 inner_fail_code ^^ Bool.lit true) ^^
          G.if_ [] G.nop is2
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
  let how_of_ae ae : allocHow = M.map (fun l ->
    match l with
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
      let (ae1, i) = VarEnv.add_local_with_offset env ae name 1l in
      let alloc_code =
        Tagged.obj env Tagged.MutBox [ compile_unboxed_zero ] ^^
        G.i (LocalSet (nr i)) in
      (ae1, alloc_code)
    | StoreStatic ->
      let tag = bytes_of_int32 (Tagged.int_of_tag Tagged.MutBox) in
      let zero = bytes_of_int32 0l in
      let ptr = E.add_mutable_static_bytes env (tag ^ zero) in
      E.add_static_root env ptr;
      let ae1 = VarEnv.add_local_heap_static ae name ptr in
      (ae1, G.nop)

end (* AllocHow *)

(* The actual compiler code that looks at the AST *)

let nat64_to_int64 n =
  let open Big_int in
  let twoRaised63 = power_int_positive_int 2 63 in
  let q, r = quomod_big_int (Value.Nat64.to_big_int n) twoRaised63 in
  if sign_big_int q = 0 then r else sub_big_int r twoRaised63

let const_lit_of_lit : Ir.lit -> Const.lit = function
  | BoolLit false -> Const.Word32 0l
  | BoolLit true  -> Const.Word32 1l
  | IntLit n
  | NatLit n      -> Const.BigInt n
  | Word8Lit n    -> Const.Vanilla (Value.Word8.to_bits n) (* already Msb-aligned *)
  | Word16Lit n   -> Const.Vanilla (Value.Word16.to_bits n)
  | Word32Lit n   -> Const.Word32 n
  | Word64Lit n   -> Const.Word64 n
  | Int8Lit n     -> Const.Vanilla (TaggedSmallWord.vanilla_lit Type.Int8 (Value.Int_8.to_int n))
  | Nat8Lit n     -> Const.Vanilla (TaggedSmallWord.vanilla_lit Type.Nat8 (Value.Nat8.to_int n))
  | Int16Lit n    -> Const.Vanilla (TaggedSmallWord.vanilla_lit Type.Int16 (Value.Int_16.to_int n))
  | Nat16Lit n    -> Const.Vanilla (TaggedSmallWord.vanilla_lit Type.Nat16 (Value.Nat16.to_int n))
  | Int32Lit n    -> Const.Word32 (Int32.of_int (Value.Int_32.to_int n))
  | Nat32Lit n    -> Const.Word32 (Int32.of_int (Value.Nat32.to_int n))
  | Int64Lit n    -> Const.Word64 (Big_int.int64_of_big_int (Value.Int_64.to_big_int n))
  | Nat64Lit n    -> Const.Word64 (Big_int.int64_of_big_int (nat64_to_int64 n))
  | CharLit c     -> Const.Vanilla Int32.(shift_left (of_int c) 8)
  | NullLit       -> Const.Vanilla Opt.null_vanilla_lit
  | TextLit t
  | BlobLit t     -> Const.Blob t
  | FloatLit f    -> Const.Float64 f

let const_of_lit lit =
  Const.t_of_v (Const.Lit (const_lit_of_lit lit))

let compile_lit env lit =
  SR.Const (const_of_lit lit), G.nop

let compile_lit_as env sr_out lit =
  let sr_in, code = compile_lit env lit in
  code ^^ StackRep.adjust env sr_in sr_out

let prim_of_typ ty = match Type.normalize ty with
  | Type.Prim ty -> ty
  | _ -> assert false

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
  | NegOp, Type.(Prim Word64) ->
    SR.UnboxedWord64, SR.UnboxedWord64,
    Func.share_code1 env "neg" ("n", I64Type) [I64Type] (fun env get_n ->
      compile_const_64 0L ^^
      get_n ^^
      G.i (Binary (Wasm.Values.I64 I64Op.Sub))
    )
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
  | NegOp, Type.(Prim (Word8 | Word16 | Word32)) ->
    StackRep.of_type t, StackRep.of_type t,
    Func.share_code1 env "neg32" ("n", I32Type) [I32Type] (fun env get_n ->
      compile_unboxed_zero ^^
      get_n ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Sub))
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
  | NotOp, Type.(Prim Word64) ->
     SR.UnboxedWord64, SR.UnboxedWord64,
     compile_const_64 (-1L) ^^
     G.i (Binary (Wasm.Values.I64 I64Op.Xor))
  | NotOp, Type.(Prim (Word8 | Word16 | Word32 as ty)) ->
     StackRep.of_type t, StackRep.of_type t,
     compile_unboxed_const (TaggedSmallWord.mask_of_type ty) ^^
     G.i (Binary (Wasm.Values.I32 I32Op.Xor))
  | _ ->
    todo "compile_unop" (Arrange_ops.unop op)
      (SR.Vanilla, SR.Unreachable, E.trap_with env "TODO: compile_unop")

(* Logarithmic helpers for deciding whether we can carry out operations in constant bitwidth *)

(* Compiling Int/Nat64 ops by conversion to/from BigNum. This is currently
   consing a lot, but compact bignums will get back efficiency as soon as
   they are merged. *)

(* helper, traps with message *)
let else_arithmetic_overflow env =
  E.else_trap_with env "arithmetic overflow"

(* helpers to decide if Int64 arithmetic can be carried out on the fast path *)
let additiveInt64_shortcut fast env get_a get_b slow =
  get_a ^^ get_a ^^ compile_shl64_const 1L ^^ G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^ compile_shrU64_const 63L ^^
  get_b ^^ get_b ^^ compile_shl64_const 1L ^^ G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^ compile_shrU64_const 63L ^^
  G.i (Binary (Wasm.Values.I64 I64Op.Or)) ^^
  G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
  G.if_ [I64Type]
    (get_a ^^ get_b ^^ fast)
    slow

let mulInt64_shortcut fast env get_a get_b slow =
  get_a ^^ get_a ^^ compile_shl64_const 1L ^^ G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^
  get_b ^^ get_b ^^ compile_shl64_const 1L ^^ G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^
  G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
  compile_const_64 65L ^^ G.i (Compare (Wasm.Values.I64 I64Op.GeU)) ^^
  G.if_ [I64Type]
    (get_a ^^ get_b ^^ fast)
    slow

let powInt64_shortcut fast env get_a get_b slow =
  get_b ^^ G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
  G.if_ [I64Type]
    (compile_const_64 1L) (* ^0 *)
    begin (* ^(1+n) *)
      get_a ^^ compile_const_64 (-1L) ^^ G.i (Compare (Wasm.Values.I64 I64Op.Eq)) ^^
      G.if_ [I64Type]
        begin (* -1 ** (1+exp) == if even (1+exp) then 1 else -1 *)
          get_b ^^ compile_const_64 1L ^^
          G.i (Binary (Wasm.Values.I64 I64Op.And)) ^^ G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
          G.if_ [I64Type]
            (compile_const_64 1L)
            get_a
        end
        begin
          get_a ^^ compile_shrS64_const 1L ^^
          G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
          G.if_ [I64Type]
            get_a (* {0,1}^(1+n) *)
            begin
              get_b ^^ compile_const_64 64L ^^
              G.i (Compare (Wasm.Values.I64 I64Op.GeU)) ^^ then_arithmetic_overflow env ^^
              get_a ^^ get_a ^^ compile_shl64_const 1L ^^ G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^
              G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^ compile_sub64_const 63L ^^
              get_b ^^ G.i (Binary (Wasm.Values.I64 I64Op.Mul)) ^^
              compile_const_64 (-63L) ^^ G.i (Compare (Wasm.Values.I64 I64Op.GeS)) ^^
              G.if_ [I64Type]
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
  G.if_ [I64Type]
    (get_a ^^ get_b ^^ fast)
    slow

let mulNat64_shortcut fast env get_a get_b slow =
  get_a ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^
  get_b ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^
  G.i (Binary (Wasm.Values.I64 I64Op.Add)) ^^
  compile_const_64 64L ^^ G.i (Compare (Wasm.Values.I64 I64Op.GeU)) ^^
  G.if_ [I64Type]
    (get_a ^^ get_b ^^ fast)
    slow

let powNat64_shortcut fast env get_a get_b slow =
  get_b ^^ G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
  G.if_ [I64Type]
    (compile_const_64 1L) (* ^0 *)
    begin (* ^(1+n) *)
      get_a ^^ compile_shrU64_const 1L ^^
      G.i (Test (Wasm.Values.I64 I64Op.Eqz)) ^^
      G.if_ [I64Type]
        get_a (* {0,1}^(1+n) *)
        begin
          get_b ^^ compile_const_64 64L ^^ G.i (Compare (Wasm.Values.I64 I64Op.GeU)) ^^ then_arithmetic_overflow env ^^
          get_a ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz)) ^^ compile_sub64_const 64L ^^
          get_b ^^ G.i (Binary (Wasm.Values.I64 I64Op.Mul)) ^^ compile_const_64 (-64L) ^^ G.i (Compare (Wasm.Values.I64 I64Op.GeS)) ^^
          G.if_ [I64Type]
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
  | Type.(Prim Word64),                       AddOp -> G.i (Binary (Wasm.Values.I64 I64Op.Add))
  | Type.(Prim Int64),                        AddOp ->
    compile_Int64_kernel env "add" BigNum.compile_add
      (additiveInt64_shortcut (G.i (Binary (Wasm.Values.I64 I64Op.Add))))
  | Type.(Prim Nat64),                        AddOp ->
    compile_Nat64_kernel env "add" BigNum.compile_add
      (additiveNat64_shortcut (G.i (Binary (Wasm.Values.I64 I64Op.Add))))
  | Type.(Prim Nat),                          SubOp -> BigNum.compile_unsigned_sub env
  | Type.(Prim Int),                          SubOp -> BigNum.compile_signed_sub env
  | Type.(Prim (Nat | Int)),                  MulOp -> BigNum.compile_mul env
  | Type.(Prim Word64),                       MulOp -> G.i (Binary (Wasm.Values.I64 I64Op.Mul))
  | Type.(Prim Int64),                        MulOp ->
    compile_Int64_kernel env "mul" BigNum.compile_mul
      (mulInt64_shortcut (G.i (Binary (Wasm.Values.I64 I64Op.Mul))))
  | Type.(Prim Nat64),                        MulOp ->
    compile_Nat64_kernel env "mul" BigNum.compile_mul
      (mulNat64_shortcut (G.i (Binary (Wasm.Values.I64 I64Op.Mul))))
  | Type.(Prim (Nat64|Word64)),               DivOp -> G.i (Binary (Wasm.Values.I64 I64Op.DivU))
  | Type.(Prim (Nat64|Word64)),               ModOp -> G.i (Binary (Wasm.Values.I64 I64Op.RemU))
  | Type.(Prim Int64),                        DivOp -> G.i (Binary (Wasm.Values.I64 I64Op.DivS))
  | Type.(Prim Int64),                        ModOp -> G.i (Binary (Wasm.Values.I64 I64Op.RemS))
  | Type.(Prim Nat),                          DivOp -> BigNum.compile_unsigned_div env
  | Type.(Prim Nat),                          ModOp -> BigNum.compile_unsigned_rem env
  | Type.(Prim Word64),                       SubOp -> G.i (Binary (Wasm.Values.I64 I64Op.Sub))
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

  | Type.Prim Type.(Word8 | Word16 | Word32), AddOp -> G.i (Binary (Wasm.Values.I32 I32Op.Add))
  | Type.(Prim Int32),                        AddOp -> compile_Int32_kernel env "add" I64Op.Add
  | Type.Prim Type.(Int8 | Int16 as ty),      AddOp -> compile_smallInt_kernel env ty "add" I32Op.Add
  | Type.(Prim Nat32),                        AddOp -> compile_Nat32_kernel env "add" I64Op.Add
  | Type.Prim Type.(Nat8 | Nat16 as ty),      AddOp -> compile_smallNat_kernel env ty "add" I32Op.Add
  | Type.(Prim Float),                        AddOp -> G.i (Binary (Wasm.Values.F64 F64Op.Add))
  | Type.Prim Type.(Word8 | Word16 | Word32), SubOp -> G.i (Binary (Wasm.Values.I32 I32Op.Sub))
  | Type.(Prim Int32),                        SubOp -> compile_Int32_kernel env "sub" I64Op.Sub
  | Type.(Prim (Int8|Int16 as ty)),           SubOp -> compile_smallInt_kernel env ty "sub" I32Op.Sub
  | Type.(Prim Nat32),                        SubOp -> compile_Nat32_kernel env "sub" I64Op.Sub
  | Type.(Prim (Nat8|Nat16 as ty)),           SubOp -> compile_smallNat_kernel env ty "sub" I32Op.Sub
  | Type.(Prim Float),                        SubOp -> G.i (Binary (Wasm.Values.F64 F64Op.Sub))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  MulOp -> TaggedSmallWord.compile_word_mul env ty
  | Type.(Prim Int32),                        MulOp -> compile_Int32_kernel env "mul" I64Op.Mul
  | Type.(Prim Int16),                        MulOp -> compile_smallInt_kernel env Type.Int16 "mul" I32Op.Mul
  | Type.(Prim Int8),                         MulOp -> compile_smallInt_kernel' env Type.Int8 "mul"
                                                         (compile_shrS_const 8l ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)))
  | Type.(Prim Nat32),                        MulOp -> compile_Nat32_kernel env "mul" I64Op.Mul
  | Type.(Prim Nat16),                        MulOp -> compile_smallNat_kernel env Type.Nat16 "mul" I32Op.Mul
  | Type.(Prim Nat8),                         MulOp -> compile_smallNat_kernel' env Type.Nat8 "mul"
                                                         (compile_shrU_const 8l ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)))
  | Type.(Prim Float),                        MulOp -> G.i (Binary (Wasm.Values.F64 F64Op.Mul))
  | Type.(Prim (Nat8|Nat16|Nat32|Word8|Word16|Word32 as ty)), DivOp ->
    G.i (Binary (Wasm.Values.I32 I32Op.DivU)) ^^
    TaggedSmallWord.msb_adjust ty
  | Type.(Prim (Nat8|Nat16|Nat32|Word8|Word16|Word32)), ModOp -> G.i (Binary (Wasm.Values.I32 I32Op.RemU))
  | Type.(Prim Int32),                        DivOp -> G.i (Binary (Wasm.Values.I32 I32Op.DivS))
  | Type.(Prim (Int8|Int16 as ty)),           DivOp ->
    Func.share_code2 env (prim_fun_name ty "div")
      (("a", I32Type), ("b", I32Type)) [I32Type]
      (fun env get_a get_b ->
        let (set_res, get_res) = new_local env "res" in
        get_a ^^ get_b ^^ G.i (Binary (Wasm.Values.I32 I32Op.DivS)) ^^
        TaggedSmallWord.msb_adjust ty ^^ set_res ^^
        get_a ^^ compile_eq_const 0x80000000l ^^
        G.if_ (StackRep.to_block_type env SR.UnboxedWord32)
          begin
            get_b ^^ TaggedSmallWord.lsb_adjust ty ^^ compile_eq_const (-1l) ^^
            G.if_ (StackRep.to_block_type env SR.UnboxedWord32)
              (G.i Unreachable)
              get_res
          end
          get_res)
  | Type.(Prim Float),                        DivOp -> G.i (Binary (Wasm.Values.F64 F64Op.Div))
  | Type.(Prim Float),                        ModOp -> E.call_import env "rts" "float_rem"
  | Type.(Prim (Int8|Int16|Int32)),           ModOp -> G.i (Binary (Wasm.Values.I32 I32Op.RemS))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  PowOp -> TaggedSmallWord.compile_word_power env ty
  | Type.(Prim ((Nat8|Nat16) as ty)),         PowOp ->
    Func.share_code2 env (prim_fun_name ty "pow")
      (("n", I32Type), ("exp", I32Type)) [I32Type]
      (fun env get_n get_exp ->
        let (set_res, get_res) = new_local env "res" in
        let bits = TaggedSmallWord.bits_of_type ty in
        get_exp ^^
        G.if_ [I32Type]
          begin
            get_n ^^ compile_shrU_const Int32.(sub 33l (of_int bits)) ^^
            G.if_ [I32Type]
              begin
                unsigned_dynamics get_n ^^ compile_sub_const (Int32.of_int bits) ^^
                get_exp ^^ TaggedSmallWord.lsb_adjust ty ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)) ^^
                compile_unboxed_const (-30l) ^^
                G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^ then_arithmetic_overflow env ^^
                get_n ^^ TaggedSmallWord.lsb_adjust ty ^^
                get_exp ^^ TaggedSmallWord.lsb_adjust ty ^^
                TaggedSmallWord.compile_word_power env Type.Word32 ^^ set_res ^^
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
        G.if_ [I32Type]
          begin
            get_n ^^ compile_shrU_const 1l ^^
            G.if_ [I32Type]
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
        G.if_ [I32Type]
          begin
            get_n ^^ compile_shrS_const Int32.(sub 33l (of_int bits)) ^^
            G.if_ [I32Type]
              begin
                signed_dynamics get_n ^^ compile_sub_const (Int32.of_int (bits - 1)) ^^
                get_exp ^^ TaggedSmallWord.lsb_adjust ty ^^ G.i (Binary (Wasm.Values.I32 I32Op.Mul)) ^^
                compile_unboxed_const (-30l) ^^
                G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^ then_arithmetic_overflow env ^^
                get_n ^^ TaggedSmallWord.lsb_adjust ty ^^
                get_exp ^^ TaggedSmallWord.lsb_adjust ty ^^
                TaggedSmallWord.compile_word_power env Type.Word32 ^^
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
        G.if_ [I32Type]
          begin
            get_n ^^ compile_unboxed_one ^^ G.i (Compare (Wasm.Values.I32 I32Op.LeS)) ^^
            get_n ^^ compile_unboxed_const (-1l) ^^ G.i (Compare (Wasm.Values.I32 I32Op.GeS)) ^^
            G.i (Binary (Wasm.Values.I32 I32Op.And)) ^^
            G.if_ [I32Type]
              begin
                get_n ^^ compile_unboxed_zero ^^ G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^
                G.if_ [I32Type]
                  begin
                    (* -1 ** (1+exp) == if even (1+exp) then 1 else -1 *)
                    get_exp ^^ compile_unboxed_one ^^ G.i (Binary (Wasm.Values.I32 I32Op.And)) ^^
                    G.if_ [I32Type]
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
  | Type.(Prim Word64),                       PowOp -> Word64.compile_unsigned_pow env
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
  | Type.(Prim Nat64),                        PowOp ->
    compile_Nat64_kernel env "pow"
      BigNum.compile_unsigned_pow
      (powNat64_shortcut (Word64.compile_unsigned_pow env))
  | Type.(Prim Nat),                          PowOp -> BigNum.compile_unsigned_pow env
  | Type.(Prim Float),                        PowOp -> E.call_import env "rts" "float_pow"
  | Type.(Prim Word64),                       AndOp -> G.i (Binary (Wasm.Values.I64 I64Op.And))
  | Type.Prim Type.(Word8 | Word16 | Word32), AndOp -> G.i (Binary (Wasm.Values.I32 I32Op.And))
  | Type.(Prim Word64),                       OrOp  -> G.i (Binary (Wasm.Values.I64 I64Op.Or))
  | Type.Prim Type.(Word8 | Word16 | Word32), OrOp  -> G.i (Binary (Wasm.Values.I32 I32Op.Or))
  | Type.(Prim Word64),                       XorOp -> G.i (Binary (Wasm.Values.I64 I64Op.Xor))
  | Type.Prim Type.(Word8 | Word16 | Word32), XorOp -> G.i (Binary (Wasm.Values.I32 I32Op.Xor))
  | Type.(Prim Word64),                       ShLOp -> G.i (Binary (Wasm.Values.I64 I64Op.Shl))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  ShLOp -> TaggedSmallWord.(
     lsb_adjust ty ^^ clamp_shift_amount ty ^^
     G.i (Binary (Wasm.Values.I32 I32Op.Shl)))
  | Type.(Prim Word64),                       UShROp -> G.i (Binary (Wasm.Values.I64 I64Op.ShrU))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  UShROp -> TaggedSmallWord.(
     lsb_adjust ty ^^ clamp_shift_amount ty ^^
     G.i (Binary (Wasm.Values.I32 I32Op.ShrU)) ^^
     sanitize_word_result ty)
  | Type.(Prim Word64),                       SShROp -> G.i (Binary (Wasm.Values.I64 I64Op.ShrS))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  SShROp -> TaggedSmallWord.(
     lsb_adjust ty ^^ clamp_shift_amount ty ^^
     G.i (Binary (Wasm.Values.I32 I32Op.ShrS)) ^^
     sanitize_word_result ty)
  | Type.(Prim Word64),                       RotLOp -> G.i (Binary (Wasm.Values.I64 I64Op.Rotl))
  | Type.Prim Type.                  Word32,  RotLOp -> G.i (Binary (Wasm.Values.I32 I32Op.Rotl))
  | Type.Prim Type.(Word8 | Word16 as ty),    RotLOp -> TaggedSmallWord.(
     Func.share_code2 env (prim_fun_name ty "rotl") (("n", I32Type), ("by", I32Type)) [I32Type]
       Wasm.Values.(fun env get_n get_by ->
      let beside_adjust = compile_shrU_const (Int32.sub 32l (shift_of_type ty)) in
      get_n ^^ get_n ^^ beside_adjust ^^ G.i (Binary (I32 I32Op.Or)) ^^
      get_by ^^ lsb_adjust ty ^^ clamp_shift_amount ty ^^ G.i (Binary (I32 I32Op.Rotl)) ^^
      sanitize_word_result ty))
  | Type.(Prim Word64),                       RotROp -> G.i (Binary (Wasm.Values.I64 I64Op.Rotr))
  | Type.Prim Type.                  Word32,  RotROp -> G.i (Binary (Wasm.Values.I32 I32Op.Rotr))
  | Type.Prim Type.(Word8 | Word16 as ty),    RotROp -> TaggedSmallWord.(
     Func.share_code2 env (prim_fun_name ty "rotr") (("n", I32Type), ("by", I32Type)) [I32Type]
       Wasm.Values.(fun env get_n get_by ->
      get_n ^^ get_n ^^ lsb_adjust ty ^^ G.i (Binary (I32 I32Op.Or)) ^^
      get_by ^^ lsb_adjust ty ^^ clamp_shift_amount ty ^^ G.i (Binary (I32 I32Op.Rotr)) ^^
      sanitize_word_result ty))

  | Type.(Prim Text), CatOp -> Text.concat env
  | Type.Non, _ -> G.i Unreachable
  | _ -> todo_trap env "compile_binop" (Arrange_ops.binop op)
  )

let compile_eq env = function
  | Type.(Prim Text) -> Text.compare env Operator.EqOp
  | Type.(Prim (Blob|Principal)) -> Blob.compare env Operator.EqOp
  | Type.(Prim Bool) -> G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | Type.(Prim (Nat | Int)) -> BigNum.compile_eq env
  | Type.(Prim (Int64 | Nat64 | Word64)) -> G.i (Compare (Wasm.Values.I64 I64Op.Eq))
  | Type.(Prim (Int8 | Nat8 | Word8 | Int16 | Nat16 | Word16 | Int32 | Nat32 | Word32 | Char)) ->
    G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | Type.Non -> G.i Unreachable
  | Type.(Prim Float) -> G.i (Compare (Wasm.Values.F64 F64Op.Eq))
  | _ -> todo_trap env "compile_eq" (Arrange_ops.relop Operator.EqOp)

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
    | Nat64 | Word64 -> G.i (Compare (Wasm.Values.I64 u64op))
    | Nat8 | Word8 | Nat16 | Word16 | Nat32 | Word32 | Char -> G.i (Compare (Wasm.Values.I32 u32op))
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
  | Type.(Prim (Nat | Nat8 | Nat16 | Nat32 | Nat64 | Int | Int8 | Int16 | Int32 | Int64 | Word8 | Word16 | Word32 | Word64 | Char as t1)), op1 ->
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
          code1 ^^ StackRep.adjust env fun_sr SR.Vanilla ^^
          set_meth_pair ^^
          compile_exp_as env ae SR.Vanilla e2 ^^ set_arg ^^

          FuncDec.ic_call_one_shot env ts get_meth_pair get_arg
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
      Dfinity.actor_public_field env name

    | ArrayPrim (m, t), es ->
      SR.Vanilla,
      Arr.lit env (List.map (compile_exp_vanilla env ae) es)
    | IdxPrim, [e1; e2]  ->
      SR.Vanilla,
      compile_exp_vanilla env ae e1 ^^ (* offset to array *)
      compile_exp_vanilla env ae e2 ^^ (* idx *)
      Arr.idx_bigint env ^^
      load_ptr

    | BreakPrim name, [e] ->
      let d = VarEnv.get_label_depth ae name in
      SR.Unreachable,
      compile_exp_vanilla env ae e ^^
      G.branch_to_ d
    | AssertPrim, [e1] ->
      SR.unit,
      compile_exp_as env ae SR.bool e1 ^^
      G.if_ [] G.nop (Dfinity.fail_assert env exp.at)
    | RetPrim, [e] ->
      SR.Unreachable,
      compile_exp_as env ae (StackRep.of_arity (E.get_return_arity env)) e ^^
      FakeMultiVal.store env (Lib.List.make (E.get_return_arity env) I32Type) ^^
      G.i Return

    (* Numeric conversions *)
    | NumConvPrim (t1, t2), [e] -> begin
      let open Type in
      match t1, t2 with
      | (Nat|Int), (Word8|Word16) ->
        SR.Vanilla,
        compile_exp_vanilla env ae e ^^
        Prim.prim_shiftToWordN env (TaggedSmallWord.shift_of_type t2)

      | (Nat|Int), Word32 ->
        SR.UnboxedWord32,
        compile_exp_vanilla env ae e ^^
        Prim.prim_intToWord32 env

      | (Nat|Int), Word64 ->
        SR.UnboxedWord64,
        compile_exp_vanilla env ae e ^^
        BigNum.truncate_to_word64 env

      | Nat64, Word64
      | Int64, Word64
      | Word64, Nat64
      | Word64, Int64
      | Nat32, Word32
      | Int32, Word32
      | Word32, Nat32
      | Word32, Int32
      | Nat16, Word16
      | Int16, Word16
      | Word16, Nat16
      | Word16, Int16
      | Nat8, Word8
      | Int8, Word8
      | Word8, Nat8
      | Word8, Int8 ->
        compile_exp env ae e

      | Int, Int64 ->
        SR.UnboxedWord64,
        compile_exp_vanilla env ae e ^^
        Func.share_code1 env "Int->Int64" ("n", I32Type) [I64Type] (fun env get_n ->
          get_n ^^
          BigNum.fits_signed_bits env 64 ^^
          E.else_trap_with env "losing precision" ^^
          get_n ^^
          BigNum.truncate_to_word64 env)

      | Int, (Int8|Int16|Int32) ->
        let ty = exp.note.Note.typ in
        StackRep.of_type ty,
        let pty = prim_of_typ ty in
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

      | Nat, (Nat8|Nat16|Nat32) ->
        let ty = exp.note.Note.typ in
        StackRep.of_type ty,
        let pty = prim_of_typ ty in
        compile_exp_vanilla env ae e ^^
        Func.share_code1 env (prim_fun_name pty "Nat->") ("n", I32Type) [I32Type] (fun env get_n ->
          get_n ^^
          BigNum.fits_unsigned_bits env (TaggedSmallWord.bits_of_type pty) ^^
          E.else_trap_with env "losing precision" ^^
          get_n ^^
          BigNum.truncate_to_word32 env ^^
          TaggedSmallWord.msb_adjust pty)

      | Char, Word32 ->
        SR.UnboxedWord32,
        compile_exp_vanilla env ae e ^^
        TaggedSmallWord.untag_codepoint

      | (Nat8|Word8|Nat16|Word16), Nat ->
        SR.Vanilla,
        compile_exp_vanilla env ae e ^^
        Prim.prim_shiftWordNtoUnsigned env (TaggedSmallWord.shift_of_type t1)

      | (Int8|Int16), Int ->
        SR.Vanilla,
        compile_exp_vanilla env ae e ^^
        Prim.prim_shiftWordNtoSigned env (TaggedSmallWord.shift_of_type t1)

      | (Nat32|Word32), Nat ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedWord32 e ^^
        Prim.prim_word32toNat env

      | Int32, Int ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedWord32 e ^^
        Prim.prim_word32toInt env

      | (Nat64|Word64), Nat ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedWord64 e ^^
        BigNum.from_word64 env

      | Int64, Int ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedWord64 e ^^
        BigNum.from_signed_word64 env

      | Word32, Char ->
        SR.Vanilla,
        compile_exp_as env ae SR.UnboxedWord32 e ^^
        Func.share_code1 env "Word32->Char" ("n", I32Type) [I32Type]
        TaggedSmallWord.check_and_tag_codepoint

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

    (* Other prims, unary *)
    | SerializePrim ts, [e] ->
      SR.Vanilla,
      compile_exp_as env ae SR.Vanilla e ^^
      Serialization.serialize env ts ^^
      Blob.of_ptr_size env

    | DeserializePrim ts, [e] ->
      StackRep.of_arity (List.length ts),
      compile_exp_as env ae SR.Vanilla e ^^
      Serialization.deserialize_from_blob false env ts

    | OtherPrim "array_len", [e] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^
      Heap.load_field Arr.len_field ^^
      BigNum.from_word32 env

    | OtherPrim "text_len", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Text.len env
    | OtherPrim "text_iter", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Text.iter env
    | OtherPrim "text_iter_done", [e] ->
      SR.bool, compile_exp_vanilla env ae e ^^ Text.iter_done env
    | OtherPrim "text_iter_next", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Text.iter_next env

    | OtherPrim "blob_size", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Blob.len env
    | OtherPrim "blob_iter", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Blob.iter env
    | OtherPrim "blob_iter_done", [e] ->
      SR.bool, compile_exp_vanilla env ae e ^^ Blob.iter_done env
    | OtherPrim "blob_iter_next", [e] ->
      SR.Vanilla, compile_exp_vanilla env ae e ^^ Blob.iter_next env

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
      E.call_import env "rts" "float_sin"

    | OtherPrim "fcos", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "float_cos"

    | OtherPrim "ftan", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "float_tan"

    | OtherPrim "fasin", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "float_arcsin"

    | OtherPrim "facos", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "float_arccos"

    | OtherPrim "fatan", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "float_arctan"

    | OtherPrim "fatan2", [y; x] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 y ^^
      compile_exp_as env ae SR.UnboxedFloat64 x ^^
      E.call_import env "rts" "float_arctan2"

    | OtherPrim "fexp", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "float_exp"

    | OtherPrim "flog", [e] ->
      SR.UnboxedFloat64,
      compile_exp_as env ae SR.UnboxedFloat64 e ^^
      E.call_import env "rts" "float_log"

    (* Other prims, nullary *)

    | SystemTimePrim, [] ->
      SR.UnboxedWord64,
      Dfinity.get_system_time env

    | OtherPrim "rts_version", [] ->
      SR.Vanilla,
      E.call_import env "rts" "version"

    | OtherPrim "rts_heap_size", [] ->
      SR.Vanilla,
      GC.get_heap_size env ^^ Prim.prim_word32toNat env

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
      Heap.get_max_live_size env ^^ BigNum.from_word64 env

    | OtherPrim "rts_callback_table_count", [] ->
      SR.Vanilla,
      ClosureTable.count env ^^ Prim.prim_word32toNat env

    | OtherPrim "rts_callback_table_size", [] ->
      SR.Vanilla,
      ClosureTable.size env ^^ Prim.prim_word32toNat env

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
      TaggedSmallWord.msb_adjust Type.Word8
    | OtherPrim "popcnt16", [e] ->
      SR.Vanilla,
      compile_exp_vanilla env ae e ^^
      G.i (Unary (Wasm.Values.I32 I32Op.Popcnt)) ^^
      TaggedSmallWord.msb_adjust Type.Word16
    | OtherPrim "popcnt32", [e] ->
      SR.UnboxedWord32,
      compile_exp_as env ae SR.UnboxedWord32 e ^^
      G.i (Unary (Wasm.Values.I32 I32Op.Popcnt))
    | OtherPrim "popcnt64", [e] ->
      SR.UnboxedWord64,
      compile_exp_as env ae SR.UnboxedWord64 e ^^
      G.i (Unary (Wasm.Values.I64 I64Op.Popcnt))
    | OtherPrim "clz8", [e] -> SR.Vanilla, compile_exp_vanilla env ae e ^^ TaggedSmallWord.clz_kernel Type.Word8
    | OtherPrim "clz16", [e] -> SR.Vanilla, compile_exp_vanilla env ae e ^^ TaggedSmallWord.clz_kernel Type.Word16
    | OtherPrim "clz32", [e] -> SR.UnboxedWord32, compile_exp_as env ae SR.UnboxedWord32 e ^^ G.i (Unary (Wasm.Values.I32 I32Op.Clz))
    | OtherPrim "clz64", [e] -> SR.UnboxedWord64, compile_exp_as env ae SR.UnboxedWord64 e ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz))
    | OtherPrim "ctz8", [e] -> SR.Vanilla, compile_exp_vanilla env ae e ^^ TaggedSmallWord.ctz_kernel Type.Word8
    | OtherPrim "ctz16", [e] -> SR.Vanilla, compile_exp_vanilla env ae e ^^ TaggedSmallWord.ctz_kernel Type.Word16
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
      Dfinity.print_text env

    (* Other prims, binary*)
    | OtherPrim "Array.init", [_;_] ->
      const_sr SR.Vanilla (Arr.init env)
    | OtherPrim "Array.tabulate", [_;_] ->
      const_sr SR.Vanilla (Arr.tabulate env)
    | OtherPrim "btst8", [_;_] ->
      (* TODO: btstN returns Bool, not a small value *)
      const_sr SR.Vanilla (TaggedSmallWord.btst_kernel env Type.Word8)
    | OtherPrim "btst16", [_;_] ->
      const_sr SR.Vanilla (TaggedSmallWord.btst_kernel env Type.Word16)
    | OtherPrim "btst32", [_;_] ->
      const_sr SR.UnboxedWord32 (TaggedSmallWord.btst_kernel env Type.Word32)
    | OtherPrim "btst64", [_;_] ->
      const_sr SR.UnboxedWord64 (
        let (set_b, get_b) = new_local64 env "b" in
        set_b ^^ compile_const_64 1L ^^ get_b ^^ G.i (Binary (Wasm.Values.I64 I64Op.Shl)) ^^
        G.i (Binary (Wasm.Values.I64 I64Op.And))
      )

    (* Coercions for abstract types *)
    | CastPrim (_,_), [e] ->
      compile_exp env ae e

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
      Dfinity.get_self_reference env

    | ICReplyPrim ts, [e] ->
      SR.unit, begin match E.mode env with
      | Flags.ICMode | Flags.RefMode ->
        compile_exp_as env ae SR.Vanilla e ^^
        (* TODO: We can try to avoid the boxing and pass the arguments to
          serialize individually *)
        Serialization.serialize env ts ^^
        Dfinity.reply_with_data env
      | _ ->
        E.trap_with env (Printf.sprintf "cannot reply when running locally")
      end

    | ICRejectPrim, [e] ->
      SR.unit, Dfinity.reject env (compile_exp_vanilla env ae e)

    | ICCallerPrim, [] ->
      Dfinity.caller env

    | ICCallPrim, [f;e;k;r] ->
      SR.unit, begin
      (* TBR: Can we do better than using the notes? *)
      let _, _, _, ts1, _ = Type.as_func f.note.Note.typ in
      let _, _, _, ts2, _ = Type.as_func k.note.Note.typ in
      let (set_meth_pair, get_meth_pair) = new_local env "meth_pair" in
      let (set_arg, get_arg) = new_local env "arg" in
      let (set_k, get_k) = new_local env "k" in
      let (set_r, get_r) = new_local env "r" in
      compile_exp_as env ae SR.Vanilla f ^^ set_meth_pair ^^
      compile_exp_as env ae SR.Vanilla e ^^ set_arg ^^
      compile_exp_as env ae SR.Vanilla k ^^ set_k ^^
      compile_exp_as env ae SR.Vanilla r ^^ set_r ^^
      FuncDec.ic_call env ts1 ts2 get_meth_pair get_arg get_k get_r
        end

    | ICStableRead ty, [] ->
(*
      * On initial install:
           1. return record of nulls
      * On upgrade:
           1. deserialize stable store to v : ty,
           (TODO: inserting null values for missing fields.)
           2. return v
*)
      SR.Vanilla,
      E.call_import env "ic0" "stable_size" ^^
      G.if_ [I32Type]
        (Stabilization.destabilize env ty)
        (let (_, fs) = Type.as_obj ty in
         let fs' = List.map
           (fun f -> (f.Type.lab, fun () -> Opt.null_lit))
            fs in
         Object.lit_raw env fs')

    | ICStableWrite ty, [e] ->
      SR.unit,
      compile_exp_as env ae SR.Vanilla e ^^
      Stabilization.stabilize env ty

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
    G.if_
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
    G.block_ (StackRep.to_block_type env SR.Vanilla) (
      G.with_current_depth (fun depth ->
        let ae1 = VarEnv.add_label ae name depth in
        compile_exp_vanilla env ae1 e
      )
    )
  | LoopE e ->
    SR.Unreachable,
    let ae' = VarEnv.{ ae with lvl = NotTopLvl } in
    G.loop_ [] (compile_exp_unit env ae' e ^^ G.i (Br (nr 0l))
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
    let (ae1, i) = VarEnv.add_local_with_offset env ae name 1l in
    let sr, code = compile_exp env ae1 e in
    sr,
    Tagged.obj env Tagged.MutBox [ compile_unboxed_zero ] ^^
    G.i (LocalSet (nr i)) ^^
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
    let (set_closure_idx, get_closure_idx) = new_local env "closure_idx" in
    let (set_k, get_k) = new_local env "k" in
    let (set_r, get_r) = new_local env "r" in
    let mk_body env1 ae1 = compile_exp_as env1 ae1 SR.unit exp_f in
    let captured = Freevars.captured exp_f in
    FuncDec.async_body env ae ts captured mk_body exp.at ^^
    set_closure_idx ^^

    compile_exp_as env ae SR.Vanilla exp_k ^^ set_k ^^
    compile_exp_as env ae SR.Vanilla exp_r ^^ set_r ^^

    FuncDec.ic_call env Type.[Prim Word32] ts
      ( Dfinity.get_self_reference env ^^
        Dfinity.actor_public_field env (Dfinity.async_method_name))
      (get_closure_idx ^^ BoxedSmallWord.box env)
      get_k
      get_r
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
        if Object.is_mut_field env exp.note.Note.typ f.it.name
        then Var.get_val_ptr env ae f.it.var
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
  (if sr != SR.UnboxedWord32 then StackRep.adjust env sr SR.Vanilla else G.nop)

(* Compile a prim of type Char -> Char to a RTS call. *)
and compile_char_to_char_rts env ae exp rts_fn =
  SR.Vanilla,
  compile_exp_as env ae SR.Vanilla exp ^^
  TaggedSmallWord.untag_codepoint ^^
  E.call_import env "rts" rts_fn ^^
  TaggedSmallWord.tag_codepoint

(* Compile a prim of type Char -> Bool to a RTS call. The RTS function should
   have type int32_t -> int32_t where the return value is 0 for 'false' and 1
   for 'true'. *)
and compile_char_to_bool_rts (env : E.t) (ae : VarEnv.t) exp rts_fn =
  SR.bool,
  compile_exp_as env ae SR.Vanilla exp ^^
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
  | Word8Lit _ ->
    compile_lit_as env SR.Vanilla l ^^
    compile_eq env Type.(Prim Word8)
  | Word16Lit _ ->
    compile_lit_as env SR.Vanilla l ^^
    compile_eq env Type.(Prim Word16)
  | Word32Lit _ ->
    BoxedSmallWord.unbox env ^^
    compile_lit_as env SR.UnboxedWord32 l ^^
    compile_eq env Type.(Prim Word32)
  | CharLit _ ->
    compile_lit_as env SR.Vanilla l ^^
    compile_eq env Type.(Prim Char)
  | Word64Lit _ ->
    BoxedWord64.unbox env ^^
    compile_lit_as env SR.UnboxedWord64 l ^^
    compile_eq env Type.(Prim Word64)
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
        G.if_ []
          ( get_x ^^
            Opt.project ^^
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
        G.if_ []
          ( get_x ^^
            Variant.project ^^
            with_fail fail_code (fill_pat env ae p)
          )
          fail_code
      )
  | LitP l ->
      CanFail (fun fail_code ->
        compile_lit_pat env l ^^
        G.if_ [] G.nop fail_code)
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
  | LitE l -> Const.(t_of_v (Lit (const_lit_of_lit l))), (fun _ _ -> ())
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

and compile_init_func mod_env ((cu, _flavor) : Ir.prog) =
  match cu with
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

    (* Add any params to the environment *)
    let args = match as_opt with None -> [] | Some as_ -> as_ in
    let arg_names = List.map (fun a -> a.it) args in
    let arg_tys = List.map (fun a -> a.note) args in
    let (ae1, setters) = VarEnv.add_argument_locals env ae0 arg_names in

    (* Reverse the fs, to a map from variable to exported name *)
    let v2en = E.NameEnv.from_list (List.map (fun f -> (f.it.var, f.it.name)) fs) in

    (* Compile the declarations *)
    let ae2, decls_codeW = compile_decs_public env ae1 ds v2en
      (Freevars.captured_vars (Freevars.upgrade up))
    in

    (* Export the public functions *)
    List.iter (export_actor_field env ae2) fs;

    (* Export upgrade hooks *)
    Func.define_built_in env "pre_exp" [] [] (fun env ->
      compile_exp_as env ae2 SR.unit up.pre);
    Func.define_built_in env "post_exp" [] [] (fun env ->
      compile_exp_as env ae2 SR.unit up.post);
    Dfinity.export_upgrade_methods env;

    (* Deserialize any arguments *)
    (match as_opt with
     | Some [] ->
       (* Liberally accept empty as well as unit argument *)
       Dfinity.system_call env "ic0" "msg_arg_data_size" ^^
       G.if_ [] (Serialization.deserialize env arg_tys) G.nop
     | Some (_ :: _) ->
       Serialization.deserialize env arg_tys ^^
       G.concat (List.rev setters)
     | None ->
       (* Reject unexpected argument *)
       Dfinity.system_call env "ic0" "msg_arg_data_size" ^^
       E.then_trap_with env "unexpected installation argument" ^^
       G.nop) ^^

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

  GC.register env static_roots;

  set_heap_base (E.get_end_of_static_memory env);

  (* Wrap the start function with the RTS initialization *)
  let rts_start_fi = E.add_fun env "rts_start" (Func.of_body env [] [] (fun env1 ->
    Heap.get_heap_base env ^^ Heap.set_heap_ptr env ^^
    match start_fi_o with
    | Some fi ->
      G.i (Call fi)
    | None ->
      Lifecycle.set env Lifecycle.PreInit
  )) in

  Dfinity.default_exports env;

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
      name = {
        module_ = None;
        function_names =
            List.mapi (fun i (f,n,_) -> Int32.(add ni' (of_int i), n)) funcs;
        locals_names =
            List.mapi (fun i (f,_,ln) -> Int32.(add ni' (of_int i), ln)) funcs;
      };
    } in

  match E.get_rts env with
  | None -> emodule
  | Some rts -> Linking.LinkModule.link emodule "rts" rts

let compile mode module_name rts (prog : Ir.prog) : Wasm_exts.CustomModule.extended_module =
  let env = E.mk_global mode rts Dfinity.trap_with Lifecycle.end_ in

  Heap.register_globals env;
  Stack.register_globals env;

  Dfinity.system_imports env;
  RTS.system_imports env;
  RTS_Exports.system_exports env;

  compile_init_func env prog;
  let start_fi_o = match E.mode env with
    | Flags.ICMode | Flags.RefMode ->
      Dfinity.export_init env;
      None
    | Flags.WASIMode ->
      Dfinity.export_wasi_start env;
      None
    | Flags.WasmMode ->
      Some (nr (E.built_in env "init"))
  in

  conclude_module env start_fi_o
