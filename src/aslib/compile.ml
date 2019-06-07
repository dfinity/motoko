(*
This module is the backend of the ActorScript compiler. It takes a program in
the intermediate representation (ir.ml), and produces a WebAssembly module,
with DFINITY extensions (customModule.ml). An important helper module is
instrList.ml, which provides a more convenient way of assembling WebAssembly
instruction lists, as it takes care of (1) source locations and (2) labels.

This file is split up in a number of modules, purely for namespacing and
grouping. Every module has a high-level prose comment explaining the concept;
this keeps documentation close to the code (a lesson learned from Simon PJ).
*)


open Wasm.Ast
open Wasm.Types
open Source
(* Re-shadow Source.(@@), to get Pervasives.(@@) *)
let (@@) = Pervasives.(@@)

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

(* Helper functions to produce annotated terms (Wasm.AST) *)
let nr x = { Wasm.Source.it = x; Wasm.Source.at = Wasm.Source.no_region }

let todo fn se x = Printf.eprintf "%s: %s" fn (Wasm.Sexpr.to_string 80 se); x

module SR = struct
  (* This goes with the StackRep module, but we need the types earlier *)

  (* Statically known values: They are not put on the stack, but the
     “stack representation“ carries the static information.
  *)
  type static_thing =
    | StaticFun of int32
    | StaticMessage of int32

  (* Value representation on the stack:

     Compiling an expression means putting its value on the stack. But
     there are various ways of putting a value onto the stack -- unboxed,
     tupled etc.
   *)
  type t =
    | Vanilla
    | UnboxedTuple of int
    | UnboxedRefTuple of int
    | UnboxedWord64
    | UnboxedWord32
    | UnboxedReference
    | Unreachable
    | StaticThing of static_thing

  let unit = UnboxedTuple 0

  let bool = Vanilla

end (* SR *)

(*

** The compiler environment.

Of course, as we go through the code we have to track a few things; these are
put in the compiler environment, type `E.t`. Some fields are valid globally, some
only make sense locally, i.e. within a single function (but we still put them
in one big record, for convenience).

The fields fall into the following categories:

 1. Static global fields. Never change.
    Example: whether we are compiling with -no-dfinity-api; the prelude code

 2. Immutable global fields. Change in a well-scoped manner.
    Example: Mapping from ActorScript names to their location.

 3. Mutable global fields. Change only monotonously.
    These are used to register things like functions. This should be monotone
    in the sense that entries are only added, and that the order should not
    matter in a significant way. In some instances, the list contains futures
    so that we can reserve and know the _position_ of the thing before we have
    to actually fill it in.

 4. Static local fields. Never change within a function.
    Example: number of parameters and return values

 5. Immutable local fields. Change in a well-scoped manner.
    Example: Jump label depth

 6. Mutable local fields. See above
    Example: Name and type of locals.

**)

(* Before we can define the environment, we need some auxillary types *)

type mode = WasmMode | DfinityMode


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
  type local_names = (int32 * string) list (* For the debug section: Names of locals *)
  type func_with_names = func * local_names
  type lazy_built_in =
    | Declared of (int32 * (func_with_names -> unit))
    | Defined of int32
    | Pending of (unit -> func_with_names)
  type t = {
    (* Global fields *)
    (* Static *)
    mode : mode;
    prelude : Ir.prog; (* The prelude. Re-used when compiling actors *)
    rts : CustomModule.extended_module option; (* The rts. Re-used when compiling actors *)
    trap_with : t -> string -> G.t;
      (* Trap with message; in the env for dependency injection *)

    (* Immutable *)

    (* Mutable *)
    func_types : func_type list ref;
    func_imports : import list ref;
    other_imports : import list ref;
    exports : export list ref;
    dfinity_types : (int32 * CustomModule.type_ list) list ref; (* Dfinity types of exports *)
    funcs : (func * string * local_names) Lib.Promise.t list ref;
    built_in_funcs : lazy_built_in NameEnv.t ref;
    static_strings : int32 StringEnv.t ref;
    end_of_static_memory : int32 ref; (* End of statically allocated memory *)
    static_memory : (int32 * string) list ref; (* Content of static memory *)
    static_memory_frozen : bool ref;
      (* Sanity check: Nothing should bump end_of_static_memory once it has been read *)

    (* Local fields (only valid/used inside a function) *)
    (* Static *)
    n_param : int32; (* Number of parameters (to calculate indices of locals) *)
    n_res : int; (* Number of return values (for type of Return) *)

    (* Immutable *)

    (* Mutable *)
    locals : value_type list ref; (* Types of locals *)
    local_names : (int32 * string) list ref; (* Names of locals *)
  }


  (* The initial global environment *)
  let mk_global mode rts prelude trap_with dyn_mem : t = {
    mode;
    rts;
    prelude;
    trap_with;
    func_types = ref [];
    func_imports = ref [];
    other_imports = ref [];
    exports = ref [];
    dfinity_types = ref [];
    funcs = ref [];
    built_in_funcs = ref NameEnv.empty;
    static_strings = ref StringEnv.empty;
    end_of_static_memory = ref dyn_mem;
    static_memory = ref [];
    static_memory_frozen = ref false;
    (* Actually unused outside mk_fun_env: *)
    n_param = 0l;
    n_res = 0;
    locals = ref [];
    local_names = ref [];
  }


  let mk_fun_env env n_param n_res =
    { env with
      n_param;
      n_res;
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

  let add_dfinity_type (env : t) e =
    ignore (reg env.dfinity_types e)

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

  let built_in (env : t) name : int32 =
    match NameEnv.find_opt name !(env.built_in_funcs) with
    | None ->
        let (fi, fill) = reserve_fun env name in
        env.built_in_funcs := NameEnv.add name (Declared (fi, fill)) !(env.built_in_funcs);
        fi
    | Some (Declared (fi, _)) -> fi
    | Some (Defined fi) -> fi
    | Some (Pending mk_fun) ->
        let (fi, fill) = reserve_fun env name in
        env.built_in_funcs := NameEnv.add name (Defined fi) !(env.built_in_funcs);
        fill (mk_fun ());
        fi

  let define_built_in (env : t) name mk_fun : unit =
    match NameEnv.find_opt name !(env.built_in_funcs) with
    | None ->
        env.built_in_funcs := NameEnv.add name (Pending mk_fun) !(env.built_in_funcs);
    | Some (Declared (fi, fill)) ->
        env.built_in_funcs := NameEnv.add name (Defined fi) !(env.built_in_funcs);
        fill (mk_fun ());
    | Some (Defined fi) ->  ()
    | Some (Pending mk_fun) -> ()

  let get_n_res (env : t) = env.n_res

  let get_func_imports (env : t) = !(env.func_imports)
  let get_other_imports (env : t) = !(env.other_imports)
  let get_exports (env : t) = !(env.exports)
  let get_dfinity_types (env : t) = !(env.dfinity_types)
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
      let name = modname ^ "_" ^ funcname in
      assert (not (NameEnv.mem name !(env.built_in_funcs)));
      env.built_in_funcs := NameEnv.add name (Defined fi) !(env.built_in_funcs);
    else assert false (* "add all imports before all functions!" *)

  let call_import (env : t) modname funcname =
    let name = modname ^ "_" ^ funcname in
    let fi = match NameEnv.find_opt name !(env.built_in_funcs) with
      | Some (Defined fi) -> fi
      | _ ->
        Printf.eprintf "Function import not declared: %s.%s\n" modname funcname;
        assert false
    in
    G.i (Call (nr fi))

  let get_prelude (env : t) = env.prelude
  let get_rts (env : t) = env.rts

  let get_trap_with (env : t) = env.trap_with
  let trap_with env msg = env.trap_with env msg
  let then_trap_with env msg = G.if_ (ValBlockType None) (trap_with env msg) G.nop
  let else_trap_with env msg = G.if_ (ValBlockType None) G.nop (trap_with env msg)

  let reserve_static_memory (env : t) size : int32 =
    if !(env.static_memory_frozen) then assert false (* "Static memory frozen" *);
    let ptr = !(env.end_of_static_memory) in
    let aligned = Int32.logand (Int32.add size 3l) (Int32.lognot 3l) in
    env.end_of_static_memory := Int32.add ptr aligned;
    ptr


  let add_mutable_static_bytes (env : t) data : int32 =
    let ptr = reserve_static_memory env (Int32.of_int (String.length data)) in
    env.static_memory := !(env.static_memory) @ [ (ptr, data) ];
    Int32.(add ptr ptr_skew) (* Return a skewed pointer *)

  let add_static_bytes (env : t) data : int32 =
    match StringEnv.find_opt data !(env.static_strings)  with
    | Some ptr -> ptr
    | None ->
      let ptr = add_mutable_static_bytes env data  in
      env.static_strings := StringEnv.add data ptr !(env.static_strings);
      ptr

  let get_end_of_static_memory env : int32 =
    env.static_memory_frozen := true;
    !(env.end_of_static_memory)

  let get_static_memory env =
    !(env.static_memory)

  let mem_size env =
    Int32.(add (div (get_end_of_static_memory env) page_size) 1l)
end


(* General code generation functions:
   Rule of thumb: Here goes stuff that independent of the ActorScript AST.
*)

(* Function called compile_* return a list of instructions (and maybe other stuff) *)

let compile_unboxed_const i = G.i (Wasm.Ast.Const (nr (Wasm.Values.I32 i)))
let compile_const_64 i = G.i (Wasm.Ast.Const (nr (Wasm.Values.I64 i)))
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
let compile_shrU_const = function
  | 0l -> G.nop | n -> compile_op_const I32Op.ShrU n
let compile_shl_const = function
  | 0l -> G.nop | n -> compile_op_const I32Op.Shl n
let compile_bitand_const = compile_op_const I32Op.And
let compile_bitor_const = function
  | 0l -> G.nop | n -> compile_op_const I32Op.Or n
let compile_eq_const i =
  compile_unboxed_const i ^^
  G.i (Compare (Wasm.Values.I32 I32Op.Eq))

(* more random utilities *)

let bytes_of_int32 (i : int32) : string =
  let b = Buffer.create 4 in
  let i1 = Int32.to_int i land 0xff in
  let i2 = (Int32.to_int i lsr 8) land 0xff in
  let i3 = (Int32.to_int i lsr 16) land 0xff in
  let i4 = (Int32.to_int i lsr 24) land 0xff in
  Buffer.add_char b (Char.chr i1);
  Buffer.add_char b (Char.chr i2);
  Buffer.add_char b (Char.chr i3);
  Buffer.add_char b (Char.chr i4);
  Buffer.contents b

(* A common variant of todo *)

let todo_trap env fn se = todo fn se (E.trap_with env ("TODO: " ^ fn))
let todo_trap_SR env fn se = todo fn se (SR.Unreachable, E.trap_with env ("TODO: " ^ fn))

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

(* Some common code macros *)

(* Iterates while cond is true. *)
let compile_while cond body =
    G.loop_ (ValBlockType None) (
      cond ^^ G.if_ (ValBlockType None) (body ^^ G.i (Br (nr 1l))) G.nop
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

module Func = struct
  (* This module contains basic bookkeeping functionality to define functions,
     in particular creating the environment, and finally adding it to the environment.
  *)

  let of_body env params retty mk_body =
    let env1 = E.mk_fun_env env (Int32.of_int (List.length params)) (List.length retty) in
    List.iteri (fun i (n,_t) -> E.add_local_name env1 (Int32.of_int i) n) params;
    let ty = FuncType (List.map snd params, retty) in
    let body = G.to_instr_list (mk_body env1) in
    (nr { ftype = nr (E.func_type env ty);
          locals = E.get_locals env1;
          body }
    , E.get_local_names env1)

  let define_built_in env name params retty mk_body =
    E.define_built_in env name (fun () -> of_body env params retty mk_body)

  (* (Almost) transparently lift code into a function and call this function. *)
  let share_code env name params retty mk_body =
    define_built_in env name params retty mk_body;
    G.i (Call (nr (E.built_in env name)))

  (* Shorthands for various arities *)
  let _share_code0 env name retty mk_body =
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
    E.add_func_import env "rts" "version" [] [I32Type];
    E.add_func_import env "rts" "bigint_of_word32" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_word32_signed" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word32_wrap" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word32_trap" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word32_signed_trap" [I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_word64" [I64Type] [I32Type];
    E.add_func_import env "rts" "bigint_of_word64_signed" [I64Type] [I32Type];
    E.add_func_import env "rts" "bigint_to_word64_wrap" [I32Type] [I64Type];
    E.add_func_import env "rts" "bigint_to_word64_trap" [I32Type] [I64Type];
    E.add_func_import env "rts" "bigint_to_word64_signed_trap" [I32Type] [I64Type];
    E.add_func_import env "rts" "bigint_eq" [I32Type; I32Type] [I32Type];
    E.add_func_import env "rts" "bigint_isneg" [I32Type] [I32Type];
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
    E.add_func_import env "rts" "bigint_sleb128_decode" [I32Type] [I32Type]

  let system_exports env =
    E.add_export env (nr {
      name = Wasm.Utf8.decode "alloc_bytes";
      edesc = nr (FuncExport (nr (E.built_in env "alloc_bytes")))
    });
    let bigint_trap_fi = E.add_fun env "bigint_trap" (
      Func.of_body env [] [] (fun env ->
        E.trap_with env "bigint function error"
      )
    ) in
    E.add_export env (nr {
      name = Wasm.Utf8.decode "bigint_trap";
      edesc = nr (FuncExport (nr bigint_trap_fi))
    })

end (* RTS *)

module Heap = struct
  (* General heap object functionality (allocation, setting fields, reading fields) *)

  (* Memory addresses are 32 bit (I32Type). *)
  let word_size = 4l

  (* We keep track of the end of the used heap in this global, and bump it if
     we allocate stuff. This is the actual memory offset, not-skewed yet *)
  let base_global = 3l
  let heap_global = 4l
  let get_heap_base = G.i (GlobalGet (nr base_global))
  let get_heap_ptr = G.i (GlobalGet (nr heap_global))
  let set_heap_ptr = G.i (GlobalSet (nr heap_global))
  let get_skewed_heap_ptr = get_heap_ptr ^^ compile_add_const ptr_skew

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
      G.i (Compare (Wasm.Values.I32 I32Op.GtU)) ^^
      G.if_ (ValBlockType None)
        ( get_pages_needed ^^
          G.i MemoryGrow ^^
          (* Check result *)
          compile_unboxed_zero ^^
          G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^
          E.then_trap_with env "Cannot grow memory."
        ) G.nop
      )

  (* Dynamic allocation *)
  let dyn_alloc_words env =
    Func.share_code1 env "alloc_words" ("n", I32Type) [I32Type] (fun env get_n ->
      (* expects the size (in words), returns the pointer *)

      (* return the current pointer (skewed) *)
      get_skewed_heap_ptr ^^

      (* Update heap pointer *)
      get_heap_ptr ^^
      get_n ^^ compile_mul_const word_size ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      set_heap_ptr ^^
      get_heap_ptr ^^ grow_memory env
    )

  let dyn_alloc_bytes env =
    Func.share_code1 env "alloc_bytes" ("n", I32Type) [I32Type] (fun env get_n ->
      get_n ^^
      (* Round up to next multiple of the word size and convert to words *)
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

     We don’t use the stack space (yet), but we could easily start to use it for
     scratch space, as long as we don’t need more than 64k.
  *)

  let stack_global = 2l

  let end_of_stack = page_size (* 64k of stack *)

end (* Stack *)

module ElemHeap = struct
  (* The ElemHeap adds a level of indirection for references (elements, as in
     ElemRef). This way, the fake orthogonal persistence code can easily
     store all references an elembuf.

     This could be done differently (e.g. traversing the heap and looking for tagged references), but
     it predates the heap traversal code, and the whole thing goes away once we
     target orthogonal persistence anyways.
  *)

  let ref_counter_global = 5l
  let get_ref_ctr = G.i (GlobalGet (nr ref_counter_global))
  let set_ref_ctr = G.i (GlobalSet (nr ref_counter_global))

  (* For now, we allocate a fixed size range. This obviously cannot stay. *)
  let max_references = 1024l

  let ref_location = Stack.end_of_stack

  let table_end : int32 = Int32.(add ref_location (mul max_references Heap.word_size))

  (* Assumes a reference on the stack, and replaces it with an index into the
     reference table *)
  let remember_reference env : G.t =
    Func.share_code1 env "remember_reference" ("ref", I32Type) [I32Type] (fun env get_ref ->
      (* Check table space *)
      get_ref_ctr ^^
      compile_unboxed_const max_references ^^
      G.i (Compare (Wasm.Values.I32 I64Op.LtU)) ^^
      E.else_trap_with env "Reference table full" ^^

      (* Return index *)
      get_ref_ctr ^^

      (* Store reference *)
      get_ref_ctr ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const ref_location ^^
      get_ref ^^
      store_unskewed_ptr ^^

      (* Bump counter *)
      get_ref_ctr ^^
      compile_add_const 1l ^^
      set_ref_ctr
    )

  (* Assumes a index into the table on the stack, and replaces it with the reference *)
  let recall_reference env : G.t =
    Func.share_code1 env "recall_reference" ("ref_idx", I32Type) [I32Type] (fun env get_ref_idx ->
      get_ref_idx ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const ref_location ^^
      load_unskewed_ptr
    )

end (* ElemHeap *)

module ClosureTable = struct
  (*
  Another fixed-size table at the beginning of memory: When we create a closure
  that is bound to a funcref that we pass out, we need this level of indirection for
  two reasons:
  - we cannot just bind the address via i32.bind, because that is not stable, due
    to our moving GC, and
  - we need to remember that these closures are roots (and currenlty never freed!)

  Therefore we maintain a static table from closure index to address of the closure
  on the heap.
  *)

  let max_entries = 1024l
  let loc = ElemHeap.table_end
  let table_end = Int32.(add loc (mul max_entries Heap.word_size))

  (* For reasons I do not recall we use the first word of the table as the counter,
     and not a global.
  *)
  let get_counter = compile_unboxed_const loc ^^ load_unskewed_ptr

  (* Assumes a reference on the stack, and replaces it with an index into the
     reference table *)
  let remember_closure env : G.t =
    Func.share_code1 env "remember_closure" ("ptr", I32Type) [I32Type] (fun env get_ptr ->
      (* Check table space *)
      get_counter ^^
      compile_unboxed_const (Int32.sub max_entries 1l) ^^
      G.i (Compare (Wasm.Values.I32 I64Op.LtU)) ^^
      E.else_trap_with env "Closure table full" ^^

      (* Return index *)
      get_counter ^^
      compile_add_const 1l ^^

      (* Store reference *)
      get_counter ^^
      compile_add_const 1l ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const loc ^^
      get_ptr ^^
      store_unskewed_ptr ^^

      (* Bump counter *)
      compile_unboxed_const loc ^^
      get_counter ^^
      compile_add_const 1l ^^
      store_unskewed_ptr
    )

  (* Assumes a index into the table on the stack, and replaces it with a ptr to the closure *)
  let recall_closure env : G.t =
    Func.share_code1 env "recall_closure" ("closure_idx", I32Type) [I32Type] (fun env get_closure_idx ->
      get_closure_idx ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const loc ^^
      load_unskewed_ptr
    )

end (* ClosureTable *)

module Bool = struct
  (* Boolean literals are either 0 or 1
     Both are recognized as unboxed scalars anyways,
     This allows us to use the result of the WebAssembly comparison operators
     directly, and to use the booleans directly with WebAssembly’s If.
  *)
  let lit = function
    | false -> compile_unboxed_zero
    | true -> compile_unboxed_one

end (* Bool *)


module BitTagged = struct
  let scalar_shift = 2l

  (* This module takes care of pointer tagging:

     A pointer to an object at offset `i` on the heap is represented as
     `i-1`, so the low two bits of the pointer are always set. We call
     `i-1` a *skewed* pointer, in a feeble attempt to avoid the term shifted,
     which may sound like a logical shift.

     We use the constants ptr_skew and ptr_unskew to change a pointer as a
     signpost where we switch between raw pointers to skewed ones.

     This means we can store a small unboxed scalar x as (x << 2), and still
     tell it apart from a pointer.

     We actually use the *second* lowest bit to tell a pointer apart from a
     scalar.

     It means that 0 and 1 are also recognized as non-pointers, and we can use
     these for false and true, matching the result of WebAssembly’s comparison
     operators.
  *)
  let if_unboxed env retty is1 is2 =
    Func.share_code1 env "is_unboxed" ("x", I32Type) [I32Type] (fun env get_x ->
      (* Get bit *)
      get_x ^^
      compile_bitand_const 0x2l ^^
      (* Check bit *)
      G.i (Test (Wasm.Values.I32 I32Op.Eqz))
    ) ^^
    G.if_ retty is1 is2

  (* The untag_scalar and tag functions expect 64 bit numbers *)
  let untag_scalar env =
    compile_shrU_const scalar_shift ^^
    G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32))

  let tag =
    G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
    compile_unboxed_const scalar_shift ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Shl))

  (* The untag_i32 and tag_i32 functions expect 32 bit numbers *)
  let untag_i32 env =
    compile_shrU_const scalar_shift

  let tag_i32 =
    compile_unboxed_const scalar_shift ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Shl))

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
    | Reference (* Either arrayref or funcref, no need to distinguish here *)
    | Int (* Contains a 64 bit number *)
    | MutBox (* used for mutable heap-allocated variables *)
    | Closure
    | Some (* For opt *)
    | Variant
    | Text
    | Indirection
    | SmallWord (* Contains a 32 bit unsigned number *)
    | BigInt

  (* Let's leave out tag 0 to trap earlier on invalid memory *)
  let int_of_tag = function
    | Object -> 1l
    | ObjInd -> 2l
    | Array -> 3l
    | Reference -> 4l
    | Int -> 5l
    | MutBox -> 6l
    | Closure -> 7l
    | Some -> 8l
    | Variant -> 9l
    | Text -> 10l
    | Indirection -> 11l
    | SmallWord -> 12l
    | BigInt -> 13l

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
    | [] -> failwith "branch"
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
    | [] -> failwith "branch_with"
    | [_, code] -> code
    | (_, code) :: cases ->
       let (set_o, get_o) = new_local env "o" in
       let prep (t, code) = (t, get_o ^^ code)
       in set_o ^^ get_o ^^ branch_default env retty (get_o ^^ code) (List.map prep cases)

  (* Can a value of this type be represented by a heap object with this tag? *)
  (* Needs to be conservative, i.e. return `true` if unsure *)
  (* This function can also be used as assertions in a lint mode, e.g. in compile_exp *)
  let can_have_tag ty tag =
    let open Type in
    match (tag : tag) with
    | Array ->
      begin match normalize ty with
      | (Con _ | Shared | Any) -> true
      | (Array _ | Tup _ | Obj _) -> true
      | (Prim _ | Opt _ | Variant _ | Func _ | Serialized _ | Non) -> false
      | (Pre | Async _ | Mut _ | Var _ | Typ _) -> assert false
      end
    | Text ->
      begin match normalize ty with
      | (Con _ | Shared | Any) -> true
      | (Prim Text | Obj _) -> true
      | (Prim _ | Array _ | Tup _ | Opt _ | Variant _ | Func _ | Serialized _ | Non) -> false
      | (Pre | Async _ | Mut _ | Var _ | Typ _) -> assert false
      end
    | Object ->
      begin match normalize ty with
      | (Con _ | Shared | Any) -> true
      | (Obj _) -> true
      | (Prim _ | Array _ | Tup _ | Opt _ | Variant _ | Func _ | Serialized _ | Non) -> false
      | (Pre | Async _ | Mut _ | Var _ | Typ _) -> assert false
      end
    | _ -> true

  (* like branch_with but with type information to statically skip some branches *)
  let branch_typed_with env ty retty branches =
    branch_with env retty (List.filter (fun (tag,c) -> can_have_tag ty tag) branches)

  let obj env tag element_instructions : G.t =
    Heap.obj env @@
      compile_unboxed_const (int_of_tag tag) ::
      element_instructions

end (* Tagged *)

module MutBox = struct
  (* Mutable heap objects *)

  let field = Tagged.header_size
  let load = Heap.load_field field
  let store = Heap.store_field field
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
  let null = compile_unboxed_const 5l

  let is_some env =
    null ^^
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

  let hash_variant_label : Type.lab -> int32 = fun l ->
    Int32.of_int (Hashtbl.hash l)

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

  let get = G.i (LocalGet (nr 0l))
  let load_data i = Heap.load_field (Int32.add header_size i)
  let store_data i = Heap.store_field (Int32.add header_size i)

  (* Calculate the wasm type for a given calling convention.
     An extra first argument for the closure! *)
  let ty env cc =
    E.func_type env (FuncType (
      I32Type :: Lib.List.make cc.Value.n_args I32Type,
      Lib.List.make cc.Value.n_res I32Type))

  (* Expect on the stack
     * the function closure
     * and arguments (n-ary!)
     * the function closure again!
  *)
  let call_closure env cc =
    (* get the table index *)
    Heap.load_field funptr_field ^^
    (* All done: Call! *)
    G.i (CallIndirect (nr (ty env cc)))

  let fixed_closure env fi fields =
      Tagged.obj env Tagged.Closure
        ([ compile_unboxed_const fi
         ; compile_unboxed_const (Int32.of_int (List.length fields)) ] @
         fields)

end (* Closure *)


module BoxedWord = struct
  (* We store large nats and ints in immutable boxed 64bit heap objects.

     Small values (just <2^5 for now, so that both code paths are well-tested)
     are stored unboxed, tagged, see BitTagged.

     The heap layout of a BoxedWord is:

       ┌─────┬─────┬─────┐
       │ tag │    i64    │
       └─────┴─────┴─────┘

     Note, that due to the equivalence of in-memory and on-stack
     representations, the 64-bit word type is also represented in this
     way. As we get proper bigints, the memory representations should
     be disambiguated and stack representations adapted. (Renaming
     those will point out where the backend needs adjustments.)
  *)

  let payload_field = Tagged.header_size

  let compile_box env compile_elem : G.t =
    let (set_i, get_i) = new_local env "boxed_i64" in
    Heap.alloc env 3l ^^
    set_i ^^
    get_i ^^ Tagged.store Tagged.Int ^^
    get_i ^^ compile_elem ^^ Heap.store_field64 payload_field ^^
    get_i

  let box env = Func.share_code1 env "box_i64" ("n", I64Type) [I32Type] (fun env get_n ->
      get_n ^^ compile_const_64 (Int64.of_int (1 lsl 5)) ^^
      G.i (Compare (Wasm.Values.I64 I64Op.LtU)) ^^
      G.if_ (ValBlockType (Some I32Type))
        (get_n ^^ BitTagged.tag)
        (compile_box env get_n)
    )

  let unbox env = Func.share_code1 env "unbox_i64" ("n", I32Type) [I64Type] (fun env get_n ->
      get_n ^^
      BitTagged.if_unboxed env (ValBlockType (Some I64Type))
        ( get_n ^^ BitTagged.untag_scalar env)
        ( get_n ^^ Heap.load_field64 payload_field)
    )

  let _box32 env =
    G.i (Convert (Wasm.Values.I64 I64Op.ExtendSI32)) ^^ box env

  let _lit env n = compile_const_64 n ^^ box env

  (* from/to SR.UnboxedWord64 *)
  let to_word64 env = G.nop
  let from_word64 env = G.nop (* TODO trap if negative *)
  let from_signed_word64 env = G.nop
  let to_word32 env = G.i (Convert (Wasm.Values.I32 I32Op.WrapI64))
  let from_word32 env = G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32))
  let from_signed_word32 env = G.i (Convert (Wasm.Values.I64 I64Op.ExtendSI32))

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
            G.if_ (ValBlockType (Some I64Type))
             one
             (get_exp ^^ one ^^ G.i (Binary (I64 I64Op.And)) ^^ G.i (Test (I64 I64Op.Eqz)) ^^
              G.if_ (ValBlockType (Some I64Type))
                square_recurse_with_shifted
                (get_n ^^
                 square_recurse_with_shifted ^^
                 G.i (Binary (Wasm.Values.I64 I64Op.Mul)))))
    in pow ()

end (* BoxedWord *)


module BoxedSmallWord = struct
  (* We store proper 32bit Word32 in immutable boxed 32bit heap objects.

     Small values (just <2^10 for now, so that both code paths are well-tested)
     are stored unboxed, tagged, see BitTagged.

     The heap layout of a BoxedSmallWord is:

       ┌─────┬─────┐
       │ tag │ i32 │
       └─────┴─────┘

  *)

  let payload_field = Tagged.header_size

  let compile_box env compile_elem : G.t =
    let (set_i, get_i) = new_local env "boxed_i32" in
    Heap.alloc env 2l ^^
    set_i ^^
    get_i ^^ Tagged.store Tagged.SmallWord ^^
    get_i ^^ compile_elem ^^ Heap.store_field payload_field ^^
    get_i

  let box env = Func.share_code1 env "box_i32" ("n", I32Type) [I32Type] (fun env get_n ->
      get_n ^^ compile_unboxed_const (Int32.of_int (1 lsl 10)) ^^
      G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
      G.if_ (ValBlockType (Some I32Type))
        (get_n ^^ BitTagged.tag_i32)
        (compile_box env get_n)
    )

  let unbox env = Func.share_code1 env "unbox_i32" ("n", I32Type) [I32Type] (fun env get_n ->
      get_n ^^
      BitTagged.if_unboxed env (ValBlockType (Some I32Type))
        ( get_n ^^ BitTagged.untag_i32 env)
        ( get_n ^^ Heap.load_field payload_field)
    )

  let _lit env n = compile_unboxed_const n ^^ box env

end (* BoxedSmallWord *)

module UnboxedSmallWord = struct
  (* While smaller-than-32bit words are treated as i32 from the WebAssembly perspective,
     there are certain differences that are type based. This module provides helpers to abstract
     over those. *)

  let shift_of_type = function
    | Type.Word8 -> 24l
    | Type.Word16 -> 16l
    | _ -> 0l

  let bitwidth_mask_of_type = function
    | Type.Word8 -> 0b111l
    | Type.Word16 -> 0b1111l
    | p -> todo "bitwidth_mask_of_type" (Arrange_type.prim p) 0l

  let const_of_type ty n = Int32.(shift_left n (to_int (shift_of_type ty)))

  let padding_of_type ty = Int32.(sub (const_of_type ty 1l) one)

  let mask_of_type ty = Int32.lognot (padding_of_type ty)

  let name_of_type ty seed = match Arrange.prim ty with
    | Wasm.Sexpr.Atom s -> seed ^ "<" ^ s ^ ">"
    | wtf -> todo "name_of_type" wtf seed

  (* Makes sure that we only shift/rotate the maximum number of bits available in the word. *)
  let clamp_shift_amount = function
    | Type.Word32 -> G.nop
    | ty -> compile_bitand_const (bitwidth_mask_of_type ty)

  let shift_leftWordNtoI32 b =
    compile_unboxed_const b ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Shl))

  (* Makes sure that the word payload (e.g. shift/rotate amount) is in the LSB bits of the word. *)
  let lsb_adjust = function
    | Type.Word32 -> G.nop
    | ty -> compile_shrU_const (shift_of_type ty)

  (* Makes sure that the word payload (e.g. operation result) is in the MSB bits of the word. *)
  let msb_adjust = function
    | Type.Word32 -> G.nop
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
    compile_unboxed_const (shift_of_type ty) ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Rotr)) ^^
    G.i (Unary (Wasm.Values.I32 I32Op.Ctz)) ^^
    msb_adjust ty

  (* Kernel for testing a bit position, according to the word invariant. *)
  let btst_kernel env ty =
    let (set_b, get_b) = new_local env "b"
    in lsb_adjust ty ^^ set_b ^^ lsb_adjust ty ^^
       compile_unboxed_one ^^ get_b ^^ clamp_shift_amount ty ^^
       G.i (Binary (Wasm.Values.I32 I32Op.Shl)) ^^
       G.i (Binary (Wasm.Values.I32 I32Op.And))

  (* Code points occupy 21 bits, no alloc needed in vanilla SR. *)
  let unbox_codepoint = compile_shrU_const 8l
  let box_codepoint = compile_shl_const 8l

  (* Two utilities for dealing with utf-8 encoded bytes. *)
  let compile_load_byte get_ptr offset =
    get_ptr ^^ G.i (Load {ty = I32Type; align = 0; offset; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)})

  let compile_6bit_mask = compile_bitand_const 0b00111111l

  (* Examines the byte pointed to the address on the stack
   * and following bytes,
   * building an unboxed Unicode code point, and passing it to set_res.
   * and finally returning the number of bytes consumed on the stack.
   * Inspired by https://rosettacode.org/wiki/UTF-8_encode_and_decode#C
   *)
  let len_UTF8_head env set_res =
    let (set_ptr, get_ptr) = new_local env "ptr" in
    let (set_byte, get_byte) = new_local env "byte" in
    let if_under thres mk_then mk_else =
      get_byte ^^ compile_unboxed_const thres ^^ G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
      G.if_ (ValBlockType (Some I32Type)) mk_then mk_else in
    let or_follower offset =
      compile_shl_const 6l ^^
      compile_load_byte get_ptr offset ^^
      compile_6bit_mask ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Or)) in
    set_ptr ^^
    compile_load_byte get_ptr 0l ^^ set_byte ^^
    if_under 0x80l
      ( get_byte ^^
        set_res ^^
        compile_unboxed_const 1l)
      (if_under 0xe0l
         (get_byte ^^ compile_bitand_const 0b00011111l ^^
          or_follower 1l ^^
          set_res ^^
          compile_unboxed_const 2l)
         (if_under 0xf0l
            (get_byte ^^ compile_bitand_const 0b00001111l ^^
             or_follower 1l ^^
             or_follower 2l ^^
             set_res ^^
             compile_unboxed_const 3l)
            (get_byte ^^ compile_bitand_const 0b00000111l ^^
             or_follower 1l ^^
             or_follower 2l^^
             or_follower 3l ^^
             set_res ^^
             compile_unboxed_const 4l)))

end (* UnboxedSmallWord *)

type comparator = Lt | Le | Ge | Gt

module type BigNumType =
sig
  (* word from SR.Vanilla, trapping, unsigned semantics *)
  val to_word32 : E.t -> G.t
  val to_word64 : E.t -> G.t

  (* word from SR.Vanilla, lossy, raw bits *)
  val truncate_to_word32 : E.t -> G.t
  val _truncate_to_word64 : E.t -> G.t

  (* unsigned word to SR.Vanilla *)
  val from_word32 : E.t -> G.t
  val from_word64 : E.t -> G.t

  (* signed word to SR.Vanilla *)
  val from_signed_word64 : E.t -> G.t
  val from_signed_word32 : E.t -> G.t

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
  (* given a data buffer on stack, consume bytes from it,
     deserializing to a numeric object
     and leave two words on stack:
     - number of consumed bytes (i32, TOS)
     - numeric object (vanilla)
   *)
  val compile_load_from_data_buf_signed : E.t -> G.t
  val compile_load_from_data_buf_unsigned : E.t -> G.t

  (* literals *)
  val compile_lit : E.t -> Big_int.big_int -> G.t

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
     N must be 2..63
   *)
  val _fits_signed_bits : E.t -> int -> G.t
  (* given a numeric object on the stack as skewed pointer, check whether
     it can be faithfully stored in N unsigned bits
     leaves boolean result on the stack
     N must be 1..64
   *)
  val _fits_unsigned_bits : E.t -> int -> G.t
end

[@@@warning "-60"] (* Do not warn about unused module *)
module BigNum64 : BigNumType = struct
  include BoxedWord

  (* examine the skewed pointer and determine if the unsigned number
     it points to fits into N bits *)
  let _fits_unsigned_bits env = function
    | 64 -> G.i Drop ^^ compile_unboxed_one
    | n when n > 64 || n < 1 -> assert false
    | n ->
      unbox env ^^
      compile_const_64 Int64.(shift_left minus_one n) ^^
      G.i (Binary (Wasm.Values.I64 I64Op.And)) ^^
      G.i (Test (Wasm.Values.I64 I64Op.Eqz))

  (* examine the skewed pointer and determine if the signed number
     it points to fits into N bits *)
  let _fits_signed_bits env = function
    | n when n > 63 || n < 2 -> assert false
    | n ->
      let set_num, get_num = new_local64 env "num" in
      unbox env ^^ set_num ^^ get_num ^^ get_num ^^
      compile_const_64 1L ^^
      G.i (Binary (Wasm.Values.I64 I64Op.Shl)) ^^
      G.i (Binary (Wasm.Values.I64 I64Op.Xor)) ^^
      compile_const_64 Int64.(shift_left minus_one n) ^^
      G.i (Binary (Wasm.Values.I64 I64Op.And)) ^^
      G.i (Test (Wasm.Values.I64 I64Op.Eqz))

  let to_word32 env =
    let (set_num, get_num) = new_local env "num" in
    set_num ^^ get_num ^^
    _fits_unsigned_bits env 32 ^^
    E.else_trap_with env "Losing precision" ^^
    get_num ^^
    unbox env ^^
    BoxedWord.to_word32 env

  let from_word32 env = BoxedWord.from_word32 env ^^ box env
  let from_signed_word32 env = BoxedWord.from_signed_word32 env ^^ box env
  let to_word64 env = unbox env ^^ BoxedWord.to_word64 env
  let from_word64 env = BoxedWord.from_word64 env ^^ box env

  let truncate_to_word32 env = unbox env ^^ BoxedWord.to_word32 env
  let _truncate_to_word64 env = unbox env ^^ BoxedWord.to_word64 env

  let compile_lit env n = compile_const_64 (Big_int.int64_of_big_int n) ^^ box env

  let compile_data_size_signed env = G.i Drop ^^ compile_unboxed_const 8l (* 64 bit for now *)
  let compile_data_size_unsigned = compile_data_size_signed

  let compile_store_to_data_buf_unsigned env =
    unbox env ^^
    G.i (Store {ty = I64Type; align = 0; offset = 0l; sz = None}) ^^
    compile_unboxed_const 8l (* 64 bit for now *)
  let compile_store_to_data_buf_signed = compile_store_to_data_buf_unsigned

  let compile_load_from_data_buf_signed env =
    G.i (Load {ty = I64Type; align = 2; offset = 0l; sz = None}) ^^
    box env ^^
    compile_unboxed_const 8l (* 64 bit for now *)
  let compile_load_from_data_buf_unsigned = compile_load_from_data_buf_signed

  let compile_abs env =
    let (set_i, get_i) = new_local env "abs_param" in
    set_i ^^
    get_i ^^
    unbox env ^^
    compile_const_64 0L ^^
    G.i (Compare (Wasm.Values.I64 I64Op.LtS)) ^^
    G.if_ (ValBlockType (Some I32Type))
      ( compile_const_64 0L ^^
        get_i ^^
        unbox env ^^
        G.i (Binary (Wasm.Values.I64 I64Op.Sub)) ^^
        box env
      )
      ( get_i )

  let with_both_unboxed op env =
    let set_tmp, get_tmp = new_local64 env "top" in
    unbox env ^^ set_tmp ^^ unbox env ^^ get_tmp ^^ op ^^ box env

  let compile_add = with_both_unboxed (G.i (Binary (Wasm.Values.I64 I64Op.Add)))
  let compile_signed_sub = with_both_unboxed (G.i (Binary (Wasm.Values.I64 I64Op.Sub)))
  let compile_mul = with_both_unboxed (G.i (Binary (Wasm.Values.I64 I64Op.Mul)))
  let compile_signed_div = with_both_unboxed (G.i (Binary (Wasm.Values.I64 I64Op.DivS)))
  let compile_signed_mod = with_both_unboxed (G.i (Binary (Wasm.Values.I64 I64Op.RemS)))
  let compile_unsigned_div = with_both_unboxed (G.i (Binary (Wasm.Values.I64 I64Op.DivU)))
  let compile_unsigned_rem = with_both_unboxed (G.i (Binary (Wasm.Values.I64 I64Op.RemU)))
  let compile_unsigned_sub env = with_both_unboxed (BoxedWord.compile_unsigned_sub env) env
  let compile_unsigned_pow env = with_both_unboxed (BoxedWord.compile_unsigned_pow env) env

  let compile_neg env =
    Func.share_code1 env "negInt" ("n", I32Type) [I32Type] (fun env get_n ->
      compile_lit env (Big_int.big_int_of_int 0) ^^
      get_n ^^
      compile_signed_sub env
    )

  let with_comp_unboxed op env =
    let set_tmp, get_tmp = new_local64 env "top" in
    unbox env ^^ set_tmp ^^ unbox env ^^ get_tmp ^^ op

  let i64op_from_relop = function
    | Lt -> I64Op.LtS
    | Le -> I64Op.LeS
    | Ge -> I64Op.GeS
    | Gt -> I64Op.GtS

  let compile_eq = with_comp_unboxed (G.i (Compare (Wasm.Values.I64 I64Op.Eq)))
  let compile_relop env relop = with_comp_unboxed (G.i (Compare (Wasm.Values.I64 (i64op_from_relop relop)))) env
  let compile_is_negative env = unbox env ^^ compile_const_64 0L ^^ G.i (Compare (Wasm.Values.I64 I64Op.LtS))
end
[@@@warning "+60"]

module BigNumLibtommmath : BigNumType = struct

  let to_word32 env = E.call_import env "rts" "bigint_to_word32_trap"
  let to_word64 env = E.call_import env "rts" "bigint_to_word64_trap"

  let truncate_to_word32 env = E.call_import env "rts" "bigint_to_word32_wrap"
  let _truncate_to_word64 env = E.call_import env "rts" "bigint_to_word64_wrap"

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

  let compile_load_from_data_buf_unsigned env =
    E.call_import env "rts" "bigint_leb128_decode" ^^
    let (set_n, get_n) = new_local env "n" in
    set_n ^^
    get_n ^^
    get_n ^^ E.call_import env "rts" "bigint_leb128_size"
  let compile_load_from_data_buf_signed env =
    E.call_import env "rts" "bigint_sleb128_decode" ^^
    let (set_n, get_n) = new_local env "n" in
    set_n ^^
    get_n ^^
    get_n ^^ E.call_import env "rts" "bigint_sleb128_size"

  let compile_lit env n =
    let limb_size = 31 in
    let twoto = Big_int.power_int_positive_int 2 limb_size in

    compile_unboxed_const 0l ^^
    E.call_import env "rts" "bigint_of_word32" ^^

    let rec go n =
      if Big_int.sign_big_int n = 0
      then G.nop
      else
        let (a, b) = Big_int.quomod_big_int n twoto in
        go a ^^
        compile_unboxed_const (Int32.of_int limb_size) ^^
        E.call_import env "rts" "bigint_lsh" ^^
        compile_unboxed_const (Big_int.int32_of_big_int b) ^^
        E.call_import env "rts" "bigint_of_word32" ^^
        E.call_import env "rts" "bigint_add" in

    go (Big_int.abs_big_int n) ^^

    if Big_int.sign_big_int n < 0
      then E.call_import env "rts" "bigint_neg"
      else G.nop

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
  let compile_relop env op =
    let fn = match op with
      | Lt -> "rts_bigint_lt"
      | Le -> "rts_bigint_le"
      | Ge -> "rts_bigint_ge"
      | Gt -> "rts_bigint_gt" in
    G.i (Call (nr (E.built_in env fn)))

  let _fits_signed_bits env bits =
    G.i (Call (nr (E.built_in env ("rts_bigint_2complement_bits")))) ^^
    compile_unboxed_const (Int32.of_int (bits - 1)) ^^
    G.i (Compare (Wasm.Values.I32 I32Op.LeU))
  let _fits_unsigned_bits env bits =
    G.i (Call (nr (E.built_in env ("rts_bigint_count_bits")))) ^^
    compile_unboxed_const (Int32.of_int bits) ^^
    G.i (Compare (Wasm.Values.I32 I32Op.LeU))

end (* BigNumLibtommmath *)

module BigNum = BigNumLibtommmath

(* Primitive functions *)
module Prim = struct
  open Wasm.Values

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
    compile_unboxed_const b ^^
    G.i (Binary (I32 I32Op.ShrS)) ^^
    prim_word32toInt env
  let prim_intToWord32 env = BigNum.truncate_to_word32 env
  let prim_shiftToWordN env b =
    prim_intToWord32 env ^^
    UnboxedSmallWord.shift_leftWordNtoI32 b
end (* Prim *)

module Object = struct
  (* An object has the following heap layout:

    ┌─────┬──────────┬─────────────┬─────────────┬───┐
    │ tag │ n_fields │ field_hash1 │ field_data1 │ … │
    └─────┴──────────┴─────────────┴─────────────┴───┘

    The field_data for immutable fields simply point to the value.

    The field_data for mutable fields are pointers to either an ObjInd, or a
    MutBox (they have the same layout). This indirection is a consequence of
    how we compile object literals with `await` instructions, as these mutable
    fields need to be able to alias local mutal variables.

    We could alternatively switch to an allocate-first approach in the
    await-translation of objects, and get rid of this indirection.
  *)

  let header_size = Int32.add Tagged.header_size 1l

  (* Number of object fields *)
  let size_field = Int32.add Tagged.header_size 0l

  (* We use the same hashing function as Ocaml would *)
  let hash_field_name s =
    Int32.of_int (Hashtbl.hash s)

  module FieldEnv = Env.Make(String)

  (* This is for non-recursive objects, i.e. ObjNewE *)
  (* The instructions in the field already create the indirection if needed *)
  let lit_raw env fs =
    let name_pos_map =
      fs |>
      (* We could store only public fields in the object, but
         then we need to allocate separate boxes for the non-public ones:
         List.filter (fun (_, vis, f) -> vis.it = Public) |>
      *)
      List.map (fun (n,_) -> (hash_field_name n, n)) |>
      List.sort compare |>
      List.mapi (fun i (_h,n) -> (n,Int32.of_int i)) |>
      List.fold_left (fun m (n,i) -> FieldEnv.add n i m) FieldEnv.empty in

     let sz = Int32.of_int (FieldEnv.cardinal name_pos_map) in

     (* Allocate memory *)
     let (set_ri, get_ri, ri) = new_local_ env I32Type "obj" in
     Heap.alloc env (Int32.add header_size (Int32.mul 2l sz)) ^^
     set_ri ^^

     (* Set tag *)
     get_ri ^^
     Tagged.store Tagged.Object ^^

     (* Set size *)
     get_ri ^^
     compile_unboxed_const sz ^^
     Heap.store_field size_field ^^

     let hash_position env n =
         let i = FieldEnv.find n name_pos_map in
         Int32.add header_size (Int32.mul 2l i) in
     let field_position env n =
         let i = FieldEnv.find n name_pos_map in
         Int32.add header_size (Int32.add (Int32.mul 2l i) 1l) in

     (* Write all the fields *)
     let init_field (name, mk_is) : G.t =
       (* Write the hash *)
       get_ri ^^
       compile_unboxed_const (hash_field_name name) ^^
       Heap.store_field (hash_position env name) ^^
       (* Write the pointer to the indirection *)
       get_ri ^^
       mk_is () ^^
       Heap.store_field (field_position env name)
     in
     G.concat_map init_field fs ^^

     (* Return the pointer to the object *)
     get_ri

  (* Returns a pointer to the object field (without following the indirection) *)
  let idx_hash_raw env =
    Func.share_code2 env "obj_idx" (("x", I32Type), ("hash", I32Type)) [I32Type] (fun env get_x get_hash ->
      let (set_f, get_f) = new_local env "f" in
      let (set_r, get_r) = new_local env "r" in

      get_x ^^
      Heap.load_field size_field ^^
      (* Linearly scan through the fields (binary search can come later) *)
      from_0_to_n env (fun get_i ->
        get_i ^^
        compile_mul_const 2l ^^
        compile_add_const header_size ^^
        compile_mul_const Heap.word_size  ^^
        get_x ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        set_f ^^

        get_f ^^
        Heap.load_field 0l ^^ (* the hash field *)
        get_hash ^^
        G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
        G.if_ (ValBlockType None)
          ( get_f ^^
            compile_add_const Heap.word_size ^^
            set_r
          ) G.nop
      ) ^^
      get_r
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
    let _, fields = Type.as_obj_sub "" obj_type in
    Type.is_mut (Lib.Option.value (Type.lookup_val_field s fields))

  let idx env obj_type name =
    compile_unboxed_const (hash_field_name name) ^^
    idx_hash env (is_mut_field env obj_type name)

  let load_idx env obj_type f =
    idx env obj_type f ^^
    load_ptr

end (* Object *)


module Iterators = struct
  (*
    We have to synthesize iterators for various functions in Text and Array.
    This is the common code for that.
  *)

  (*
  Parameters:
    name: base name for this built-in function (needs to be unique)
    mk_stop get_x: counter value at which to stop (unboxed)
    mk_next env get_i get_x: pushes onto the stack:
     * how much to increase the counter (unboxed)
     * the thing to return, Vanilla stackrep.
    get_x: The thing to put in the closure, and pass to mk_next

  Return code that takes the object (array or text) on the stack and puts a
  closure onto the stack.
  *)
  let create outer_env name mk_stop mk_next =
    Func.share_code1 outer_env name ("x", I32Type) [I32Type] (fun env get_x ->
      (* Register functions as needed *)
      let next_funid = E.add_fun env (name ^ "_next") (
        Func.of_body env ["clos", I32Type] [I32Type] (fun env ->
          let (set_n, get_n) = new_local env "n" in
          let (set_x, get_x) = new_local env "x" in
          let (set_ret, get_ret) = new_local env "ret" in

          (* Get pointer to counter from closure *)
          Closure.get ^^ Closure.load_data 0l ^^
          MutBox.load ^^ BoxedSmallWord.unbox env ^^ set_n ^^

          (* Get pointer to object in closure *)
          Closure.get ^^ Closure.load_data 1l ^^ set_x ^^

          get_n ^^
          (* Get counter end *)
          mk_stop env get_x ^^
          G.i (Compare (Wasm.Values.I32 I32Op.GeU)) ^^
          G.if_ (ValBlockType (Some I32Type))
            (* Then *)
            Opt.null
            (* Else *)
            begin (* Return stuff *)
              Opt.inject env (
                (* Put address of conter on the stack, for the store *)
                Closure.get ^^ Closure.load_data 0l ^^
                (* Get value and increase *)
                mk_next env get_n get_x ^^
                set_ret ^^ (* put return value aside *)
                (* Advance counter *)
                get_n ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
                BoxedSmallWord.box env ^^ MutBox.store ^^
                (* Return new value *)
                get_ret)
            end
        )
      ) in

      let iter_funid = E.add_fun env (name ^ "_iter") (
        Func.of_body env ["clos", I32Type] [I32Type] (fun env ->
          (* closure for the function *)
          let (set_ni, get_ni) = new_local env "next" in
          Closure.fixed_closure env next_funid
            [ Tagged.obj env Tagged.MutBox [ compile_unboxed_zero ]
            ;  Closure.get ^^ Closure.load_data 0l
            ] ^^
          set_ni ^^

          Object.lit_raw env
            [ "next", fun _ -> get_ni ]
        )
      ) in

      (* Now build the closure *)
      Closure.fixed_closure env iter_funid [ get_x ]
    )

end (* Iterators *)

module Text = struct
  (* The layout of a text object is

     ┌─────┬─────────┬──────────────────┐
     │ tag │ n_bytes │ bytes (padded) … │
     └─────┴─────────┴──────────────────┘

     Note: The bytes are UTF-8 encoded code points from Unicode.
  *)

  let header_size = Int32.add Tagged.header_size 1l

  let len_field = Int32.add Tagged.header_size 0l

  let lit env s =
    let tag = bytes_of_int32 (Tagged.int_of_tag Tagged.Text) in
    let len = bytes_of_int32 (Int32.of_int (String.length s)) in
    let data = tag ^ len ^ s in
    let ptr = E.add_static_bytes env data in
    compile_unboxed_const ptr

  let alloc env = Func.share_code1 env "text_alloc" ("len", I32Type) [I32Type] (fun env get_len ->
      let (set_x, get_x) = new_local env "x" in
      compile_unboxed_const (Int32.mul Heap.word_size header_size) ^^
      get_len ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      Heap.dyn_alloc_bytes env ^^
      set_x ^^

      get_x ^^ Tagged.store Tagged.Text ^^
      get_x ^^ get_len ^^ Heap.store_field len_field ^^
      get_x
   )

  let unskewed_payload_offset = Int32.(add ptr_unskew (mul Heap.word_size header_size))
  let payload_ptr_unskewed =
    compile_add_const unskewed_payload_offset

  (* String concatentation. Expects two strings on stack *)
  let concat env = Func.share_code2 env "concat" (("x", I32Type), ("y", I32Type)) [I32Type] (fun env get_x get_y ->
      let (set_z, get_z) = new_local env "z" in
      let (set_len1, get_len1) = new_local env "len1" in
      let (set_len2, get_len2) = new_local env "len2" in

      get_x ^^ Heap.load_field len_field ^^ set_len1 ^^
      get_y ^^ Heap.load_field len_field ^^ set_len2 ^^

      (* allocate memory *)
      get_len1 ^^
      get_len2 ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      alloc env ^^
      set_z ^^

      (* Copy first string *)
      get_z ^^ payload_ptr_unskewed ^^
      get_x ^^ payload_ptr_unskewed ^^
      get_len1 ^^
      Heap.memcpy env ^^

      (* Copy second string *)
      get_z ^^ payload_ptr_unskewed ^^ get_len1 ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      get_y ^^ payload_ptr_unskewed ^^
      get_len2 ^^
      Heap.memcpy env ^^

      (* Done *)
      get_z
    )

  (* String comparison. Expects two strings on stack *)
  let compare env =
    Func.share_code2 env "Text.compare" (("x", I32Type), ("y", I32Type)) [I32Type] (fun env get_x get_y ->
      let (set_len1, get_len1) = new_local env "len1" in
      let (set_len2, get_len2) = new_local env "len2" in

      get_x ^^ Heap.load_field len_field ^^ set_len1 ^^
      get_y ^^ Heap.load_field len_field ^^ set_len2 ^^

      get_len1 ^^
      get_len2 ^^
      G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
      G.if_ (ValBlockType None) G.nop (Bool.lit false ^^ G.i Return) ^^

      (* We could do word-wise comparisons if we know that the trailing bytes
         are zeroed *)
      get_len1 ^^
      from_0_to_n env (fun get_i ->
        get_x ^^
        payload_ptr_unskewed ^^
        get_i ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^

        get_y ^^
        payload_ptr_unskewed ^^
        get_i ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^

        G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
        G.if_ (ValBlockType None) G.nop (Bool.lit false ^^ G.i Return)
      ) ^^
      Bool.lit true
  )

  let prim_decodeUTF8 env =
    Func.share_code1 env "decodeUTF8" ("string", I32Type)
      [I32Type; I32Type] (fun env get_string ->
        let (set_res, get_res) = new_local env "res" in
        get_string ^^ payload_ptr_unskewed ^^
        UnboxedSmallWord.len_UTF8_head env set_res ^^
        BoxedSmallWord.box env ^^
        get_res ^^ UnboxedSmallWord.box_codepoint
      )

  let text_chars env =
    Iterators.create env "text_chars"
      (fun env get_x -> get_x ^^ Heap.load_field len_field)
      (fun env get_i get_x ->
          let (set_char, get_char) = new_local env "char" in
          get_x ^^ payload_ptr_unskewed ^^
          get_i ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          UnboxedSmallWord.len_UTF8_head env set_char ^^
          get_char ^^ UnboxedSmallWord.box_codepoint
      )

  let partial_len env =
    Func.share_code1 env "text_len_partial" ("x", I32Type) [I32Type] (fun env get_x ->
      let funid = E.add_fun env "text_len" (Func.of_body env ["clos", I32Type] [I32Type] (fun env ->
        let get_text_object = Closure.get ^^ Closure.load_data 0l in
        let (set_max, get_max) = new_local env "max" in
        let (set_n, get_n) = new_local env "n" in
        let (set_len, get_len) = new_local env "len" in
        compile_unboxed_zero ^^ set_n ^^
        compile_unboxed_zero ^^ set_len ^^
        get_text_object ^^ Heap.load_field len_field ^^ set_max ^^
        compile_while
          (get_n ^^ get_max ^^ G.i (Compare (Wasm.Values.I32 I32Op.LtU)))
          begin
            get_text_object ^^ payload_ptr_unskewed ^^ get_n ^^
              G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
            UnboxedSmallWord.len_UTF8_head env (G.i Drop) ^^
            get_n ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^ set_n ^^
            get_len ^^ compile_add_const 1l ^^ set_len
          end ^^
        get_len ^^
        BigNum.from_word32 env
      )) in
      Closure.fixed_closure env funid [ get_x ]
    )

  let prim_showChar env =
    let (set_c, get_c) = new_local env "c" in
    let (set_utf8, get_utf8) = new_local env "utf8" in
    let storeLeader bitpat shift =
      get_c ^^ compile_shrU_const shift ^^ compile_bitor_const bitpat ^^
      G.i (Store {ty = I32Type; align = 0;
                  offset = unskewed_payload_offset;
                  sz = Some Wasm.Memory.Pack8}) in
    let storeFollower offset shift =
      get_c ^^ compile_shrU_const shift ^^ UnboxedSmallWord.compile_6bit_mask ^^
        compile_bitor_const 0b10000000l ^^
      G.i (Store {ty = I32Type; align = 0;
                  offset = Int32.add offset unskewed_payload_offset;
                  sz = Some Wasm.Memory.Pack8}) in
    let allocPayload n = compile_unboxed_const n ^^ alloc env ^^ set_utf8 ^^ get_utf8 in
    UnboxedSmallWord.unbox_codepoint ^^
    set_c ^^
    get_c ^^
    compile_unboxed_const 0x80l ^^
    G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
    G.if_ (ValBlockType None)
      (allocPayload 1l ^^ storeLeader 0b00000000l 0l)
      begin
        get_c ^^
        compile_unboxed_const 0x800l ^^
        G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
        G.if_ (ValBlockType None)
          begin
            allocPayload 2l ^^ storeFollower 1l 0l ^^
            get_utf8 ^^ storeLeader 0b11000000l 6l
          end
          begin
            get_c ^^
            compile_unboxed_const 0x10000l ^^
            G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
            G.if_ (ValBlockType None)
            begin
              allocPayload 3l ^^ storeFollower 2l 0l ^^
              get_utf8 ^^ storeFollower 1l 6l ^^
              get_utf8 ^^ storeLeader 0b11100000l 12l
            end
            begin
              allocPayload 4l ^^ storeFollower 3l 0l ^^
              get_utf8 ^^ storeFollower 2l 6l ^^
              get_utf8 ^^ storeFollower 1l 12l ^^
              get_utf8 ^^ storeLeader 0b11110000l 18l
            end
          end
      end ^^
    get_utf8

  (* We also use Text heap objects for byte arrays
     (really should rename Text to Bytes) *)
  let dyn_alloc_scratch env =
    compile_add_const (Int32.mul Heap.word_size header_size) ^^
    Heap.dyn_alloc_bytes env ^^
    payload_ptr_unskewed

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

  (* Dynamic array access. Returns the address (not the value) of the field.
     Does bounds checking *)
  let idx env =
    Func.share_code2 env "Array.idx" (("array", I32Type), ("idx", I32Type)) [I32Type] (fun env get_array get_idx ->
      (* No need to check the lower bound, we interpret is as unsigned *)
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

  let partial_get env =
    Func.share_code1 env "array_get_partial" ("x", I32Type) [I32Type] (fun env get_x ->
      let funid = E.add_fun env "array_get" (Func.of_body env ["clos", I32Type; "idx", I32Type] [I32Type] (fun env1 ->
        let get_idx = G.i (LocalGet (nr 1l)) in
        Closure.get ^^ Closure.load_data 0l ^^
        get_idx ^^ BigNum.to_word32 env1 ^^
        idx env1 ^^
        load_ptr
      )) in
      Closure.fixed_closure env funid [ get_x ]
    )

  let partial_set env =
    Func.share_code1 env "array_set_partial" ("x", I32Type) [I32Type] (fun env get_x ->
      let funid = E.add_fun env "array_set" (Func.of_body env ["clos", I32Type; "idx", I32Type; "val", I32Type] [] (fun env1 ->
        let get_idx = G.i (LocalGet (nr 1l)) in
        let get_val = G.i (LocalGet (nr 2l)) in
        Closure.get ^^ Closure.load_data 0l ^^
        get_idx ^^ BigNum.to_word32 env1 ^^
        idx env1 ^^
        get_val ^^
        store_ptr
      )) in
      Closure.fixed_closure env funid [ get_x ]
    )

  let partial_len env =
    Func.share_code1 env "array_len_partial" ("x", I32Type) [I32Type] (fun env get_x ->
      let funid = E.add_fun env "array_len" (Func.of_body env ["clos", I32Type] [I32Type] (fun env1 ->
        Closure.get ^^ Closure.load_data 0l ^^
        Heap.load_field len_field ^^
        BigNum.from_word32 env1
      )) in
      Closure.fixed_closure env funid [ get_x ]
    )

  (* Compile an array literal. *)
  let lit env element_instructions =
    Tagged.obj env Tagged.Array
     ([ compile_unboxed_const (Wasm.I32.of_int_u (List.length element_instructions))
      ] @ element_instructions)

  let keys_iter env =
    Iterators.create env "array_keys"
      (fun env get_x -> get_x ^^ Heap.load_field len_field)
      (fun env get_i get_x ->
        compile_unboxed_const 1l ^^ (* advance by one *)
        get_i ^^ BigNum.from_word32 env (* return the boxed index *)
      )

  let vals_iter env =
    Iterators.create env "array_vals"
      (fun env get_x -> get_x ^^ Heap.load_field len_field)
      (fun env get_i get_x ->
        compile_unboxed_const 1l ^^ (* advance by one *)
        get_x ^^ get_i ^^ idx env ^^ load_ptr (* return the element *)
      )

  (* Does not initialize the fields! *)
  let alloc env =
    let (set_len, get_len) = new_local env "len" in
    let (set_r, get_r) = new_local env "r" in
    set_len ^^

    (* Allocate *)
    get_len ^^
    compile_add_const header_size ^^
    Heap.dyn_alloc_words env ^^
    set_r ^^

    (* Write header *)
    get_r ^^
    Tagged.store Tagged.Array ^^
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
      Closure.call_closure env (Value.local_cc 1 1) ^^
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
  let compile_unit = compile_unboxed_one

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
    let name = Printf.sprintf "from_%i_tuple" n in
    let retty = Lib.List.make n I32Type in
    Func.share_code1 env name ("tup", I32Type) retty (fun env get_tup ->
      G.table n (fun i -> get_tup ^^ load_n (Int32.of_int i))
    )
end (* Tuple *)

module Dfinity = struct
  (* Dfinity-specific stuff: System imports, databufs etc. *)

  let system_imports env =
    begin
      E.add_func_import env "test" "print" [I32Type] [];
      E.add_func_import env "test" "show_i32" [I32Type] [I32Type];
      E.add_func_import env "data" "externalize" [I32Type; I32Type] [I32Type];
      E.add_func_import env "data" "internalize" [I32Type; I32Type; I32Type; I32Type] [];
      E.add_func_import env "data" "length" [I32Type] [I32Type];
      E.add_func_import env "elem" "externalize" [I32Type; I32Type] [I32Type];
      E.add_func_import env "elem" "internalize" [I32Type; I32Type; I32Type; I32Type] [];
      E.add_func_import env "elem" "length" [I32Type] [I32Type];
      E.add_func_import env "module" "new" [I32Type] [I32Type];
      E.add_func_import env "actor" "new" [I32Type] [I32Type];
      E.add_func_import env "actor" "self" [] [I32Type];
      E.add_func_import env "actor" "export" [I32Type; I32Type] [I32Type];
      E.add_func_import env "func" "internalize" [I32Type; I32Type] [];
      E.add_func_import env "func" "externalize" [I32Type] [I32Type];
      E.add_func_import env "func" "bind_i32" [I32Type; I32Type] [I32Type];
    end

  let system_call env name =
    if E.mode env = DfinityMode
    then G.i (Call (nr (E.built_in env name)))
    else G.i Unreachable

  let compile_databuf_of_text env  =
    Func.share_code1 env "databuf_of_text" ("string", I32Type) [I32Type] (fun env get_string ->
      (* Calculate the offset *)
      get_string ^^
      compile_add_const Int32.(add (mul Heap.word_size Text.header_size) ptr_unskew) ^^

      (* Calculate the length *)
      get_string ^^
      Heap.load_field (Text.len_field) ^^

      (* Externalize *)
      system_call env "data_externalize"
    )

  let compile_databuf_of_bytes env (bytes : string) =
    Text.lit env bytes ^^ compile_databuf_of_text env

  (* For debugging *)
  let compile_static_print env s =
      compile_databuf_of_bytes env s ^^
      system_call env "test_print"

  let _compile_println_int env =
      system_call env "test_show_i32" ^^
      system_call env "test_print" ^^
      compile_static_print env "\n"

  let trap_with env s =
    if E.mode env = DfinityMode
    then compile_static_print env (s ^ "\n") ^^ G.i Unreachable
    else G.i Unreachable

  let prim_print env =
    compile_databuf_of_text env ^^
    system_call env "test_print"

  let default_exports env =
    (* these exports seem to be wanted by the hypervisor/v8 *)
    E.add_export env (nr {
      name = Wasm.Utf8.decode "mem";
      edesc = nr (MemoryExport (nr 0l))
    });
    E.add_export env (nr {
      name = Wasm.Utf8.decode "table";
      edesc = nr (TableExport (nr 0l))
    })

  let export_start_stub env =
    (* Create an empty message *)
    let empty_f = Func.of_body env [] [] (fun env1 ->
      (* Set up memory *)
      G.i (Call (nr (E.built_in env1 "restore_mem"))) ^^
      (* Collect garbage *)
      G.i (Call (nr (E.built_in env1 "collect"))) ^^
      (* Save memory *)
      G.i (Call (nr (E.built_in env1 "save_mem")))
      ) in
    let fi = E.add_fun env "start_stub" empty_f in
    E.add_export env (nr {
      name = Wasm.Utf8.decode "start";
      edesc = nr (FuncExport (nr fi))
    });
    E.add_dfinity_type env (fi, [])

  let box_reference env =
    Func.share_code1 env "box_reference" ("ref", I32Type) [I32Type] (fun env get_ref ->
      Tagged.obj env Tagged.Reference [
        get_ref ^^
        ElemHeap.remember_reference env
      ]
    )

  let unbox_reference env =
    Heap.load_field 1l ^^
    ElemHeap.recall_reference env

  let get_self_reference env =
    system_call env "actor_self" ^^
    box_reference env

  let static_message_funcref env fi =
    compile_unboxed_const fi ^^
    system_call env "func_externalize"

end (* Dfinity *)

module OrthogonalPersistence = struct
  (* This module implements the code that fakes orthogonal persistence *)

  let mem_global = 0l
  let elem_global = 1l

  (* Strategy:
     * There is a persistent global databuf called `datastore`
     * Two helper functions are installed in each actor: restore_mem and save_mem.
       (The don’t actually have names, just numbers, of course).
     * Upon each message entry, call restore_mem. At the end, call save_mem.
     * restore_mem checks if memstore is defined.
       - If it is 0, then this is the first message ever received.
         Run the actor’s start function (e.g. to initialize globals).
       - If it is not 0, then load the databuf into memory, and set
         the global with the end-of-memory pointer to the length.
     * save_mem simply copies the whole dynamic memory (up to the end-of-memory
       pointer) to a new databuf and stores that in memstore.

    This does not persist references yet.
  *)

  let register env start_funid =
    E.add_export env (nr {
      name = Wasm.Utf8.decode "datastore";
      edesc = nr (GlobalExport (nr mem_global))
    });
    E.add_export env (nr {
      name = Wasm.Utf8.decode "elemstore";
      edesc = nr (GlobalExport (nr elem_global))
    });

    Func.define_built_in env "restore_mem" [] [] (fun env1 ->
       let (set_i, get_i) = new_local env1 "len" in
       G.i (GlobalGet (nr mem_global)) ^^
       Dfinity.system_call env1 "data_length" ^^
       set_i ^^

       get_i ^^
       compile_eq_const 0l ^^
       G.if_ (ValBlockType None)
         (* First run, call the start function *)
         ( G.i (Call (nr start_funid)) )

         (* Subsequent run *)
         ( (* Set heap pointer based on databuf length *)
           get_i ^^
           compile_add_const ElemHeap.table_end ^^
           Heap.set_heap_ptr ^^
           Heap.get_heap_ptr ^^ Heap.grow_memory env ^^

           (* Load memory *)
           compile_unboxed_const ElemHeap.table_end ^^
           get_i ^^
           G.i (GlobalGet (nr mem_global)) ^^
           compile_unboxed_zero ^^
           Dfinity.system_call env1 "data_internalize" ^^

           (* Load reference counter *)
           G.i (GlobalGet (nr elem_global)) ^^
           Dfinity.system_call env1 "elem_length" ^^
           ElemHeap.set_ref_ctr ^^

           (* Load references *)
           compile_unboxed_const ElemHeap.ref_location ^^
           ElemHeap.get_ref_ctr ^^
           G.i (GlobalGet (nr elem_global)) ^^
           compile_unboxed_zero ^^
           Dfinity.system_call env1 "elem_internalize"
        )
    );
    Func.define_built_in env "save_mem" [] [] (fun env1 ->
       (* Store memory *)
       compile_unboxed_const ElemHeap.table_end ^^
       Heap.get_heap_ptr ^^
       compile_unboxed_const ElemHeap.table_end ^^
       G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
       Dfinity.system_call env "data_externalize" ^^
       G.i (GlobalSet (nr mem_global)) ^^

       (* Store references *)
       compile_unboxed_const ElemHeap.ref_location ^^
       ElemHeap.get_ref_ctr ^^
       Dfinity.system_call env "elem_externalize" ^^
       G.i (GlobalSet (nr elem_global))
    )

  let save_mem env =
    if E.mode env = DfinityMode
    then G.i (Call (nr (E.built_in env "save_mem")))
    else G.i Unreachable

  let restore_mem env =
    if E.mode env = DfinityMode
    then G.i (Call (nr (E.built_in env "restore_mem")))
    else G.i Unreachable

end (* OrthogonalPersistence *)

module HeapTraversal = struct
  (* Returns the object size (in words) *)
  let object_size env =
    Func.share_code1 env "object_size" ("x", I32Type) [I32Type] (fun env get_x ->
      get_x ^^
      Tagged.branch env (ValBlockType (Some I32Type))
        [ Tagged.Int,
          compile_unboxed_const 3l
        ; Tagged.SmallWord,
          compile_unboxed_const 2l
        ; Tagged.BigInt,
          compile_unboxed_const 5l (* HeapTag + sizeof(mp_int) *)
        ; Tagged.Reference,
          compile_unboxed_const 2l
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
        ; Tagged.Text,
          get_x ^^
          Heap.load_field Text.len_field ^^
          compile_add_const 3l ^^
          compile_divU_const Heap.word_size ^^
          compile_add_const Text.header_size
        ; Tagged.Object,
          get_x ^^
          Heap.load_field Object.size_field ^^
          compile_mul_const 2l ^^
          compile_add_const Object.header_size
        ; Tagged.Closure,
          get_x ^^
          Heap.load_field Closure.len_field ^^
          compile_add_const Closure.header_size
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

  (* Calls mk_code for each pointer in the object pointed to by get_x,
     passing code get the address of the pointer,
     and code to get the offset of the pointer (for the BigInt payload field). *)
  let for_each_pointer env get_x mk_code mk_code_offset =
    let (set_ptr_loc, get_ptr_loc) = new_local env "ptr_loc" in
    let code = mk_code get_ptr_loc in
    let code_offset = mk_code_offset get_ptr_loc in
    get_x ^^
    Tagged.branch_default env (ValBlockType None) G.nop
      [ Tagged.MutBox,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size MutBox.field) ^^
        set_ptr_loc ^^
        code
      ; Tagged.BigInt,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size 4l) ^^
        set_ptr_loc ^^
        code_offset Text.unskewed_payload_offset
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
        get_x ^^
        Heap.load_field Arr.len_field ^^
        (* Adjust fields *)
        from_0_to_n env (fun get_i ->
          get_x ^^
          get_i ^^
          Arr.idx env ^^
          set_ptr_loc ^^
          code
        )
      ; Tagged.Object,
        get_x ^^
        Heap.load_field Object.size_field ^^

        from_0_to_n env (fun get_i ->
          get_i ^^
          compile_mul_const 2l ^^
          compile_add_const 1l ^^
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
      ]

end (* HeapTraversal *)

module Serialization = struct
  (*
    Also see (and update) `design/TmpWireFormat.md`, which documents the format
    in a “user-facing” way.

    We have a specific serialization strategy for `Text` and references for
    easier interop with the console and the nonce. This is a stop-gap measure
    until we have nailed down IDL and Bidirectional Messaging.

    The general serialization strategy is as follows:
    * We traverse the data to calculate the size needed for the data buffer and the
      reference buffer.
    * We allocate memory for the data buffer and the reference buffer
      (this memory area is not referenced, so will be dead with the next GC)
    * We traverse the data, in a type-driven way, and copy it to the scratch space.
      We thread through pointers to the current free space of the two scratch spaces.
      This is type driven, and we use the `share_code` machinery and names that
      properly encode the type to resolve loops in a convenient way.
    * We externalize all that new data space into a databuf, and add this reference
      to the reference space.
    * We externalize the reference space into a elembuf

    TODO: Cycles are not detected.

    The deserialization is analogous:
    * We allocate some scratch space, and internalize the elembuf into it.
    * We allocate some more scratch space, and internalize the databuf into it.
    * We parse the data, in a type-driven way, using normal construction and
      allocation.
    * At the end, the scratch space is a hole in the heap, and will be reclaimed
      by the next GC.
  *)

  (* A type identifier *)

  (*
    This needs to map types to some identifier with the following properties:
     - Its domain are normalized types that do not mention any type parameters
     - It needs to be injective wrt. type equality
     - It needs to terminate, even for recursive types
     - It may fail upon type parameters (i.e. no polymorphism)
    We can use string_of_typ here for now, it seems.
  *)
  let typ_id : Type.typ -> string = Type.string_of_typ


  (* IDL field hashes *)
  let lab_hash : Type.lab -> int32 = fun s ->
    let open Int32 in
    List.fold_left
      (fun s c -> add (mul s 223l) (of_int (Char.code c)))
      0l
      (* TODO: also unescape the string, once #465 is approved *)
      (Lib.String.explode s)

  let sort_by_hash fs =
    List.sort
      (fun (h1,_) (h2,_) -> compare h1 h2)
      (List.map (fun f -> (lab_hash f.Type.lab, f)) fs)

  (* Checks whether the serialization of a given type could contain references *)
  module TS = Set.Make (struct type t = Type.typ let compare = compare end)
  let has_no_references : Type.typ -> bool = fun t ->
    let open Type in
    let seen = ref TS.empty in (* break the cycles *)
    let rec go t =
      TS.mem t !seen ||
      begin
        seen := TS.add t !seen;
        match t with
        | Var _ -> assert false
        | (Prim _ | Any | Non | Shared | Pre) -> true
        | Con (c, ts) ->
          begin match Con.kind c with
          | Abs _ -> assert false
          | Def (tbs,t) -> go (open_ ts t) (* TBR this may fail to terminate *)
          end
        | Array t -> go t
        | Tup ts -> List.for_all go ts
        | Func (Sharable, c, tbs, ts1, ts2) -> false
        | Func (s, c, tbs, ts1, ts2) ->
          let ts = open_binds tbs in
          List.for_all go (List.map (open_ ts) ts1) &&
          List.for_all go (List.map (open_ ts) ts2)
        | Opt t -> go t
        | Variant fs -> List.for_all (fun f -> go f.typ) fs
        | Async t -> go t
        | Obj (Actor, fs) -> false
        | Obj (_, fs) -> List.for_all (fun f -> go f.typ) fs
        | Mut t -> go t
        | Serialized t -> go t
        | Typ _ -> false
      end
    in go t

  (* TODO (leb128 for i32):
     All leb128 currently goes through bignum. This is of course
     absurdly expensive and round-about, but I want _one_ implementation
     first that I can test, until we properly exercise this code (i.e. there
     is also the JS-side of things and we know it is bug-free).
     Writing a Wasm-native implementation of this will then be done separately.
  *)


  (* Returns data (in bytes) and reference buffer size (in entries) needed *)
  let rec buffer_size env t =
    let open Type in
    let t = normalize t in
    let name = "@buffer_size<" ^ typ_id t ^ ">" in
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
      let inc_ref_size i =
        get_ref_size ^^ compile_add_const i ^^ set_ref_size
      in

      (* See TODO (leb128 for i32) *)
      let size_word env code =
        inc_data_size (code ^^ BigNum.from_word32 env ^^ BigNum.compile_data_size_unsigned env)
      in

      let size env t =
        buffer_size env t ^^
        get_ref_size ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^ set_ref_size ^^
        get_data_size ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^ set_data_size
      in

      (* Now the actual type-dependent code *)
      begin match t with
      | Prim Nat -> inc_data_size (get_x ^^ BigNum.compile_data_size_unsigned env)
      | Prim Int -> inc_data_size (get_x ^^ BigNum.compile_data_size_signed env)
      | Prim Word64 -> inc_data_size (compile_unboxed_const 8l) (* 64 bit *)
      | Prim Word8 -> inc_data_size (compile_unboxed_const 1l)
      | Prim Word16 -> inc_data_size (compile_unboxed_const 2l)
      | Prim Word32 -> inc_data_size (compile_unboxed_const 4l)
      | Prim Bool -> inc_data_size (compile_unboxed_const 1l)
      | Tup ts ->
        G.concat_mapi (fun i t ->
          get_x ^^ Tuple.load_n (Int32.of_int i) ^^
          size env t
        ) ts
      | Obj (Object Sharable, fs) ->
        G.concat_map (fun (_h, f) ->
          get_x ^^ Object.load_idx env t f.Type.lab ^^
          size env f.typ
        ) (sort_by_hash fs)
      | Array t ->
        size_word env (get_x ^^ Heap.load_field Arr.len_field) ^^
        get_x ^^ Heap.load_field Arr.len_field ^^
        from_0_to_n env (fun get_i ->
          get_x ^^ get_i ^^ Arr.idx env ^^ load_ptr ^^
          size env t
        )
      | Prim Text ->
        size_word env (get_x ^^ Heap.load_field Text.len_field) ^^
        inc_data_size (get_x ^^ Heap.load_field Text.len_field)
      | (Prim Null | Shared) -> G.nop
      | Opt t ->
        inc_data_size (compile_unboxed_const 1l) ^^ (* one byte tag *)
        get_x ^^ Opt.is_some env ^^
        G.if_ (ValBlockType None) (get_x ^^ Opt.project ^^ size env t) G.nop
      | Variant vs ->
        List.fold_right (fun (i, {lab = l; typ = t}) continue ->
            get_x ^^
            Variant.test_is env l ^^
            G.if_ (ValBlockType None)
              ( size_word env (compile_unboxed_const (Int32.of_int i)) ^^
                get_x ^^ Variant.project ^^ size env t
              ) continue
          )
          ( List.mapi (fun i (_h, f) -> (i,f)) (sort_by_hash vs) )
          ( E.trap_with env "buffer_size: unexpected variant" )
      | (Func _ | Obj (Actor, _)) ->
        inc_ref_size 1l
      | Non ->
        E.trap_with env "buffer_size called on value of type None"
      | _ -> todo "buffer_size" (Arrange_ir.typ t) G.nop
      end ^^
      get_data_size ^^
      get_ref_size
    )

  (* Copies x to the data_buffer, storing references after ref_count entries in ref_base *)
  let rec serialize_go env t =
    let open Type in
    let t = normalize t in
    let name = "@serialize_go<" ^ typ_id t ^ ">" in
    Func.share_code3 env name (("x", I32Type), ("data_buffer", I32Type), ("ref_buffer", I32Type)) [I32Type; I32Type]
    (fun env get_x get_data_buf get_ref_buf ->
      let set_data_buf = G.i (LocalSet (nr 1l)) in
      let set_ref_buf = G.i (LocalSet (nr 2l)) in

      (* Some combinators for writing values *)

      let advance_data_buf =
        get_data_buf ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^ set_data_buf in
      let advance_ref_buf =
        get_ref_buf ^^ compile_add_const Heap.word_size ^^ set_ref_buf in

      let write_word code =
        (* See TODO (leb128 for i32) *)
        get_data_buf ^^
        code ^^ BigNum.from_word32 env ^^
        BigNum.compile_store_to_data_buf_unsigned env ^^
        advance_data_buf
      in

      let write_byte code =
        get_data_buf ^^ code ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Memory.Pack8}) ^^
        compile_unboxed_const 1l ^^ advance_data_buf
      in

      let write env t =
        get_data_buf ^^
        get_ref_buf ^^
        serialize_go env t ^^
        set_ref_buf ^^
        set_data_buf
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
      | Prim Word64 ->
        get_data_buf ^^
        get_x ^^ BoxedWord.unbox env ^^
        G.i (Store {ty = I64Type; align = 0; offset = 0l; sz = None}) ^^
        compile_unboxed_const 8l ^^ advance_data_buf
      | Prim Word32 ->
        get_data_buf ^^
        get_x ^^ BoxedSmallWord.unbox env ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = None}) ^^
        compile_unboxed_const 4l ^^ advance_data_buf
      | Prim Word16 ->
        get_data_buf ^^
        get_x ^^ UnboxedSmallWord.lsb_adjust Word16 ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Memory.Pack16}) ^^
        compile_unboxed_const 2l ^^ advance_data_buf
      | Prim Word8 ->
        get_data_buf ^^
        get_x ^^ UnboxedSmallWord.lsb_adjust Word16 ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Memory.Pack8}) ^^
        compile_unboxed_const 1l ^^ advance_data_buf
      | Prim Bool ->
        get_data_buf ^^
        get_x ^^
        G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Memory.Pack8}) ^^
        compile_unboxed_const 1l ^^ advance_data_buf
      | Tup ts ->
        G.concat_mapi (fun i t ->
          get_x ^^ Tuple.load_n (Int32.of_int i) ^^
          write env t
        ) ts
      | Obj (Object Sharable, fs) ->
        G.concat_map (fun (_h,f) ->
          get_x ^^ Object.load_idx env t f.Type.lab ^^
          write env f.typ
        ) (sort_by_hash fs)
      | Array t ->
        write_word (get_x ^^ Heap.load_field Arr.len_field) ^^
        get_x ^^ Heap.load_field Arr.len_field ^^
        from_0_to_n env (fun get_i ->
          get_x ^^ get_i ^^ Arr.idx env ^^ load_ptr ^^
          write env t
        )
      | (Prim Null | Shared) -> G.nop
      | Opt t ->
        get_x ^^
        Opt.is_some env ^^
        G.if_ (ValBlockType None)
          ( write_byte (compile_unboxed_const 1l) ^^ get_x ^^ Opt.project ^^ write env t )
          ( write_byte (compile_unboxed_const 0l) )
      | Variant vs ->
        List.fold_right (fun (i, {lab = l; typ = t}) continue ->
            get_x ^^
            Variant.test_is env l ^^
            G.if_ (ValBlockType None)
              ( write_word (compile_unboxed_const (Int32.of_int i)) ^^
                get_x ^^ Variant.project ^^ write env t)
              continue
          )
          ( List.mapi (fun i (_h, f) -> (i,f)) (sort_by_hash vs) )
          ( E.trap_with env "serialize_go: unexpected variant" )
      | Prim Text ->
        let (set_len, get_len) = new_local env "len" in
        get_x ^^ Heap.load_field Text.len_field ^^ set_len ^^

        write_word get_len ^^
        get_data_buf ^^
        get_x ^^ Text.payload_ptr_unskewed ^^
        get_len ^^
        Heap.memcpy env ^^
        get_len ^^ advance_data_buf
      | (Func _ | Obj (Actor, _)) ->
        get_ref_buf ^^
        get_x ^^ Dfinity.unbox_reference env ^^
        store_unskewed_ptr ^^
        advance_ref_buf
      | Non ->
        E.trap_with env "serializing value of type None"
      | _ -> todo "serialize" (Arrange_ir.typ t) G.nop
      end ^^
      get_data_buf ^^
      get_ref_buf
    )

  let rec deserialize_go env t =
    let open Type in
    let t = normalize t in
    let name = "@deserialize_go<" ^ typ_id t ^ ">" in
    Func.share_code2 env name (("data_buffer", I32Type), ("ref_buffer", I32Type)) [I32Type; I32Type; I32Type]
    (fun env get_data_buf get_ref_buf ->
      let set_data_buf = G.i (LocalSet (nr 0l)) in
      let set_ref_buf = G.i (LocalSet (nr 1l)) in

      (* Some combinators for reading values *)
      let advance_data_buf =
        get_data_buf ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^ set_data_buf in
      let advance_ref_buf =
        get_ref_buf ^^ compile_add_const Heap.word_size ^^ set_ref_buf in

      let read_byte =
        get_data_buf ^^
        G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^
        compile_unboxed_const 1l ^^ advance_data_buf
      in

      let read_word =
        (* See TODO (leb128 for i32) *)
        get_data_buf ^^
        BigNum.compile_load_from_data_buf_unsigned env ^^
        advance_data_buf ^^
        BigNum.to_word32 env
      in

      let read env t =
        get_data_buf ^^
        get_ref_buf ^^
        deserialize_go env t ^^
        set_ref_buf ^^
        set_data_buf
      in

      (* Now the actual deserialization *)
      begin match t with
      | Prim Nat ->
        get_data_buf ^^
        BigNum.compile_load_from_data_buf_unsigned env ^^
        advance_data_buf
      | Prim Int ->
        get_data_buf ^^
        BigNum.compile_load_from_data_buf_signed env ^^
        advance_data_buf
      | Prim Word64 ->
        get_data_buf ^^
        G.i (Load {ty = I64Type; align = 2; offset = 0l; sz = None}) ^^
        BoxedWord.box env ^^
        compile_unboxed_const 8l ^^ advance_data_buf (* 64 bit *)
      | Prim Word32 ->
        get_data_buf ^^
        G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = None}) ^^
        BoxedSmallWord.box env ^^
        compile_unboxed_const 4l ^^ advance_data_buf
      | Prim Word16 ->
        get_data_buf ^^
        G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack16, Wasm.Memory.ZX)}) ^^
        UnboxedSmallWord.msb_adjust Word16 ^^
        compile_unboxed_const 2l ^^ advance_data_buf
      | Prim Word8 ->
        get_data_buf ^^
        G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^
        UnboxedSmallWord.msb_adjust Word8 ^^
        compile_unboxed_const 1l ^^ advance_data_buf
      | Prim Bool ->
        get_data_buf ^^
        G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^
        compile_unboxed_const 1l ^^ advance_data_buf
      | Tup ts ->
        G.concat_map (fun t -> read env t) ts ^^
        Tuple.from_stack env (List.length ts)
      | Obj (Object Sharable, fs) ->
        Object.lit_raw env (List.map (fun (_h,f) ->
          f.Type.lab, fun () -> read env f.typ
        ) (sort_by_hash fs))
      | Array t ->
        let (set_len, get_len) = new_local env "len" in
        let (set_x, get_x) = new_local env "x" in

        read_word ^^ set_len ^^
        get_len ^^ Arr.alloc env ^^ set_x ^^
        get_len ^^ from_0_to_n env (fun get_i ->
          get_x ^^ get_i ^^ Arr.idx env ^^
          read env t ^^ store_ptr
        ) ^^
        get_x
      | (Prim Null | Shared) -> Opt.null
      | Opt t ->
        read_byte ^^
        compile_eq_const 0l ^^
        G.if_ (ValBlockType (Some I32Type))
          ( Opt.null )
          ( Opt.inject env (read env t) )
      | Variant vs ->
        let (set_tag, get_tag) = new_local env "tag" in
        read_word ^^ set_tag ^^
        List.fold_right (fun (i, {lab = l; typ = t}) continue ->
            get_tag ^^
            compile_eq_const (Int32.of_int i) ^^
            G.if_ (ValBlockType (Some I32Type))
              ( Variant.inject env l (read env t) )
              continue
          )
          ( List.mapi (fun i (_h, f) -> (i,f)) (sort_by_hash vs) )
          ( E.trap_with env "deserialize_go: unexpected variant tag" )
      | Prim Text ->
        let (set_len, get_len) = new_local env "len" in
        let (set_x, get_x) = new_local env "x" in
        read_word ^^ set_len ^^

        get_len ^^ Text.alloc env ^^ set_x ^^

        get_x ^^ Text.payload_ptr_unskewed ^^
        get_data_buf ^^
        get_len ^^
        Heap.memcpy env ^^

        get_len ^^ advance_data_buf ^^

        get_x
      | (Func _ | Obj (Actor, _)) ->
        get_ref_buf ^^
        load_unskewed_ptr ^^
        Dfinity.box_reference env ^^
        advance_ref_buf
      | Non ->
        E.trap_with env "deserializing value of type None"
      | _ -> todo_trap env "deserialize" (Arrange_ir.typ t)
      end ^^
      get_data_buf ^^
      get_ref_buf
    )

  let serialize env t =
    let name = "@serialize<" ^ typ_id t ^ ">" in
    Func.share_code1 env name ("x", I32Type) [I32Type] (fun env get_x ->
      match Type.normalize t with
      | Type.Prim Type.Text -> get_x ^^ Dfinity.compile_databuf_of_text env
      | Type.Obj (Type.Actor, _)
      | Type.Func (Type.Sharable, _, _, _, _) -> get_x ^^ Dfinity.unbox_reference env
      | _ ->
        let (set_data_size, get_data_size) = new_local env "data_size" in
        let (set_refs_size, get_refs_size) = new_local env "refs_size" in

        (* Get object sizes *)
        get_x ^^
        buffer_size env t ^^
        compile_add_const 1l ^^ (* Leave space for databuf *)
        compile_mul_const Heap.word_size ^^
        set_refs_size ^^
        set_data_size ^^

        let (set_data_start, get_data_start) = new_local env "data_start" in
        let (set_refs_start, get_refs_start) = new_local env "refs_start" in

        get_data_size ^^ Text.dyn_alloc_scratch env ^^ set_data_start ^^
        get_refs_size ^^ Text.dyn_alloc_scratch env ^^ set_refs_start ^^

        (* Serialize x into the buffer *)
        get_x ^^
        get_data_start ^^
        get_refs_start ^^ compile_add_const Heap.word_size ^^ (* Leave space for databuf *)
        serialize_go env t ^^

        (* Sanity check: Did we fill exactly the buffer *)
        get_refs_start ^^ get_refs_size ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
        E.else_trap_with env "reference buffer not filled " ^^

        get_data_start ^^ get_data_size ^^ G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
        E.else_trap_with env "data buffer not filled " ^^

        (* Create databuf, and store at beginning of ref area *)
        get_refs_start ^^
        get_data_start ^^
        get_data_size ^^
        Dfinity.system_call env "data_externalize" ^^
        store_unskewed_ptr  ^^

        if has_no_references t
        then
          (* Sanity check: Really no references *)
          get_refs_size ^^
          compile_unboxed_const Heap.word_size ^^
          G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
          E.else_trap_with env "has_no_references wrong" ^^
          (* If there are no references, just return the databuf *)
          get_refs_start ^^
          load_unskewed_ptr
        else
          (* Finally, create elembuf *)
          get_refs_start ^^
          get_refs_size ^^ compile_divU_const Heap.word_size ^^
          Dfinity.system_call env "elem_externalize"
    )

  let deserialize_text env get_databuf =
    let (set_data_size, get_data_size) = new_local env "data_size" in
    let (set_x, get_x) = new_local env "x" in

    get_databuf ^^
    Dfinity.system_call env "data_length" ^^
    set_data_size ^^

    get_data_size ^^
    Text.alloc env ^^
    set_x ^^

    get_x ^^ Text.payload_ptr_unskewed ^^
    get_data_size ^^
    get_databuf ^^
    compile_unboxed_const 0l ^^
    Dfinity.system_call env "data_internalize" ^^

    get_x


  let deserialize env t =
    let name = "@deserialize<" ^ typ_id t ^ ">" in
    Func.share_code1 env name ("elembuf", I32Type) [I32Type] (fun env get_elembuf ->
      match Type.normalize t with
      | Type.Prim Type.Text -> deserialize_text env get_elembuf
      | Type.Obj (Type.Actor, _)
      | Type.Func (Type.Sharable, _, _, _, _) -> get_elembuf ^^ Dfinity.box_reference env
      | _ ->
        let (set_data_size, get_data_size) = new_local env "data_size" in
        let (set_refs_size, get_refs_size) = new_local env "refs_size" in
        let (set_data_start, get_data_start) = new_local env "data_start" in
        let (set_refs_start, get_refs_start) = new_local env "refs_start" in
        let (set_databuf, get_databuf) = new_local env "databuf" in

        begin
        if has_no_references t
        then
          (* We have no elembuf wrapper, so the argument is the databuf *)
          compile_unboxed_const 0l ^^ set_refs_start ^^
          get_elembuf ^^ set_databuf
        else
          (* Allocate space for the elem buffer *)
          get_elembuf ^^
          Dfinity.system_call env "elem_length" ^^
          set_refs_size ^^

          get_refs_size ^^
          Arr.alloc env ^^
          compile_add_const Arr.header_size ^^
          compile_add_const ptr_unskew ^^
          set_refs_start ^^

          (* Copy elembuf *)
          get_refs_start ^^
          get_refs_size ^^
          get_elembuf ^^
          compile_unboxed_const 0l ^^
          Dfinity.system_call env "elem_internalize" ^^

          (* Get databuf *)
          get_refs_start ^^
          load_unskewed_ptr ^^
          set_databuf
        end ^^

        (* Allocate space for the data buffer *)
        get_databuf ^^
        Dfinity.system_call env "data_length" ^^
        set_data_size ^^

        get_data_size ^^
        compile_add_const 3l ^^
        compile_divU_const Heap.word_size ^^
        Arr.alloc env ^^
        compile_add_const Arr.header_size ^^
        compile_add_const ptr_unskew ^^
        set_data_start ^^

        (* Copy data *)
        get_data_start ^^
        get_data_size ^^
        get_databuf ^^
        compile_unboxed_const 0l ^^
        Dfinity.system_call env "data_internalize" ^^

        (* Go! *)
        get_data_start ^^
        get_refs_start ^^ compile_add_const Heap.word_size ^^
        deserialize_go env t ^^
        G.i Drop ^^
        G.i Drop
    )

    let dfinity_type t =
      let open Type in
      let open CustomModule in
      match normalize t with
      | Prim Text -> DataBuf
      | Obj (Actor, _) -> ActorRef
      | Func (Sharable, _, _, _, _) -> FuncRef
      | t' when has_no_references t' -> DataBuf
      | _ -> ElemBuf

end (* Serialization *)

module GC = struct
  (* This is a very simple GC:
     It copies everything live to the to-space beyond the bump pointer,
     then it memcpies it back, over the from-space (so that we still neatly use
     the beginning of memory).

     Roots are:
     * All objects in the static part of the memory.
     * all closures ever bound to a `funcref`.
       These therefore need to live in a separate area of memory
       (could be mutable array of pointers, similar to the reference table)
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
    G.if_ (ValBlockType None) (get_end_to_space ^^ G.i Return) G.nop ^^

    (* If this is an indirection, just use that value *)
    get_obj ^^
    Tagged.branch_default env (ValBlockType None) G.nop [
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
    Tagged.store Tagged.Indirection ^^
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
    BitTagged.if_unboxed env (ValBlockType None) (get_end_to_space ^^ G.i Return) G.nop ^^

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

  let register env (end_of_static_space : int32) = Func.define_built_in env "collect" [] [] (fun env ->
    if not gc_enabled then G.nop else

    (* Copy all roots. *)
    let (set_begin_from_space, get_begin_from_space) = new_local env "begin_from_space" in
    let (set_begin_to_space, get_begin_to_space) = new_local env "begin_to_space" in
    let (set_end_to_space, get_end_to_space) = new_local env "end_to_space" in

    Heap.get_heap_base ^^ compile_add_const ptr_skew ^^ set_begin_from_space ^^
    Heap.get_skewed_heap_ptr ^^ set_begin_to_space ^^
    Heap.get_skewed_heap_ptr ^^ set_end_to_space ^^


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
    ClosureTable.get_counter ^^
    from_0_to_n env (fun get_i -> evac (
      get_i ^^
      compile_add_const 1l ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const ClosureTable.loc ^^
      compile_add_const ptr_skew
    )) ^^
    HeapTraversal.walk_heap_from_to env
      (compile_unboxed_const Int32.(add ClosureTable.table_end ptr_skew))
      (compile_unboxed_const Int32.(add end_of_static_space ptr_skew))
      (fun get_x -> HeapTraversal.for_each_pointer env get_x evac evac_offset) ^^

    (* Go through the to-space, and evacuate that.
       Note that get_end_to_space changes as we go, but walk_heap_from_to can handle that.
     *)
    HeapTraversal.walk_heap_from_to env
      get_begin_to_space
      get_end_to_space
      (fun get_x -> HeapTraversal.for_each_pointer env get_x evac evac_offset) ^^

    (* Copy the to-space to the beginning of memory. *)
    get_begin_from_space ^^ compile_add_const ptr_unskew ^^
    get_begin_to_space ^^ compile_add_const ptr_unskew ^^
    get_end_to_space ^^ get_begin_to_space ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
    Heap.memcpy env ^^

    (* Reset the heap pointer *)
    get_begin_from_space ^^ compile_add_const ptr_unskew ^^
    get_end_to_space ^^ get_begin_to_space ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
    Heap.set_heap_ptr
  )

end (* GC *)

module VarLoc = struct
  (* Most names are stored in heap locations or in locals.
     But some are special (static functions, the current actor, static messages of
     the current actor). These have no real location (yet), but we still need to
     produce a value on demand:
   *)

  type deferred_loc =
    { stack_rep : SR.t
    ; materialize : E.t -> G.t
    ; is_local : bool (* Only valid within the current function *)
    }

  (* A type to record where ActorScript names are stored. *)
  type varloc =
    (* A Wasm Local of the current function, directly containing the value
       (note that most values are pointers, but not all)
       Used for immutable and mutable, non-captured data *)
    | Local of int32
    (* A Wasm Local of the current function, that points to memory location,
       with an offset (in words) to value.
       Used for mutable captured data *)
    | HeapInd of (int32 * int32)
    (* A static mutable memory location (static address of a MutBox field) *)
    | Static of int32
    (* Dynamic code to put the value on the heap.
       May be local to the current function or module (see is_local) *)
    | Deferred of deferred_loc

  let is_non_local : varloc -> bool = function
    | Local _ -> false
    | HeapInd _ -> false
    | Static _ -> true
    | Deferred d -> not d.is_local
end

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

  let refs_of_arity n =
    if n = 1 then UnboxedReference else UnboxedRefTuple n

  (* The stack rel of a primitive type, i.e. what the binary operators expect *)
  let of_type : Type.typ -> t = function
    | Type.Prim Type.Bool -> bool
    | Type.Prim Type.Nat
    | Type.Prim Type.Int -> Vanilla
    | Type.Prim Type.Word64 -> UnboxedWord64
    | Type.Prim Type.Word32 -> UnboxedWord32
    | Type.Prim Type.(Word8 | Word16 | Char) -> Vanilla
    | Type.Prim Type.Text -> Vanilla
    | p -> todo "of_type" (Arrange_ir.typ p) Vanilla

  let to_block_type env = function
    | Vanilla -> ValBlockType (Some I32Type)
    | UnboxedWord64 -> ValBlockType (Some I64Type)
    | UnboxedWord32 -> ValBlockType (Some I32Type)
    | UnboxedReference -> ValBlockType (Some I32Type)
    | UnboxedTuple 0 -> ValBlockType None
    | UnboxedTuple 1 -> ValBlockType (Some I32Type)
    | UnboxedTuple n -> VarBlockType (nr (E.func_type env (FuncType ([], Lib.List.make n I32Type))))
    | UnboxedRefTuple 0 -> ValBlockType None
    | UnboxedRefTuple 1 -> ValBlockType (Some I32Type)
    | UnboxedRefTuple n -> VarBlockType (nr (E.func_type env (FuncType ([], Lib.List.make n I32Type))))
    | StaticThing _ -> ValBlockType None
    | Unreachable -> ValBlockType None

  let to_string = function
    | Vanilla -> "Vanilla"
    | UnboxedWord64 -> "UnboxedWord64"
    | UnboxedWord32 -> "UnboxedWord32"
    | UnboxedReference -> "UnboxedReference"
    | UnboxedTuple n -> Printf.sprintf "UnboxedTuple %d" n
    | UnboxedRefTuple n -> Printf.sprintf "UnboxedRefTuple %d" n
    | Unreachable -> "Unreachable"
    | StaticThing _ -> "StaticThing"

  let join (sr1 : t) (sr2 : t) = match sr1, sr2 with
    | _, _ when sr1 = sr2 -> sr1
    | Unreachable, sr2 -> sr2
    | sr1, Unreachable -> sr1
    | UnboxedWord64, UnboxedWord64 -> UnboxedWord64
    | UnboxedReference, UnboxedReference -> UnboxedReference
    | UnboxedTuple n, UnboxedTuple m when n = m -> sr1
    | _, Vanilla -> Vanilla
    | Vanilla, _ -> Vanilla
    | StaticThing _, StaticThing _ -> Vanilla
    | _, _ ->
      Printf.eprintf "Invalid stack rep join (%s, %s)\n"
        (to_string sr1) (to_string sr2); sr1

  let drop env (sr_in : t) =
    match sr_in with
    | Vanilla -> G.i Drop
    | UnboxedWord64 -> G.i Drop
    | UnboxedWord32 -> G.i Drop
    | UnboxedReference -> G.i Drop
    | UnboxedTuple n -> G.table n (fun _ -> G.i Drop)
    | UnboxedRefTuple n -> G.table n (fun _ -> G.i Drop)
    | StaticThing _ -> G.nop
    | Unreachable -> G.nop

  let materialize_unboxed_ref env = function
    | StaticFun fi ->
      assert false
    | StaticMessage fi ->
      Dfinity.static_message_funcref env fi

  let materialize env = function
    | StaticFun fi ->
      (* When accessing a variable that is a static function, then we need to
         create a heap-allocated closure-like thing on the fly. *)
      Tagged.obj env Tagged.Closure [
        compile_unboxed_const fi;
        compile_unboxed_zero (* number of parameters: none *)
      ]
    | StaticMessage fi ->
      Dfinity.static_message_funcref env fi ^^
      Dfinity.box_reference env

  let unbox_reference_n env n = match n with
    | 0 -> G.nop
    | 1 -> Dfinity.unbox_reference env
    | _ ->
      let name = Printf.sprintf "unbox_reference_n %i" n in
      let args = Lib.List.table n (fun i -> Printf.sprintf "arg%i" i, I32Type) in
      let retty = Lib.List.make n I32Type in
      Func.share_code env name args retty (fun env ->
        G.table n (fun i ->
          G.i (LocalGet (nr (Int32.of_int i))) ^^ Dfinity.unbox_reference env
        )
      )

  let box_reference_n env n = match n with
    | 0 -> G.nop
    | 1 -> Dfinity.box_reference env
    | _ ->
      let name = Printf.sprintf "box_reference_n %i" n in
      let args = Lib.List.table n (fun i -> Printf.sprintf "arg%i" i, I32Type) in
      let retty = Lib.List.make n I32Type in
      Func.share_code env name args retty (fun env ->
        G.table n (fun i ->
          G.i (LocalGet (nr (Int32.of_int i))) ^^ Dfinity.box_reference env
        )
      )

  let rec adjust env (sr_in : t) sr_out =
    if sr_in = sr_out
    then G.nop
    else match sr_in, sr_out with
    | Unreachable, Unreachable -> G.nop
    | Unreachable, _ -> G.i Unreachable

    | UnboxedTuple n, Vanilla -> Tuple.from_stack env n
    | Vanilla, UnboxedTuple n -> Tuple.to_stack env n

    | UnboxedRefTuple n, UnboxedTuple m when n = m -> box_reference_n env n
    | UnboxedTuple n, UnboxedRefTuple m when n = m -> unbox_reference_n env n

    | UnboxedRefTuple n, sr ->
      box_reference_n env n ^^ adjust env (UnboxedTuple n) sr
    | sr,  UnboxedRefTuple n ->
      adjust env sr (UnboxedTuple n) ^^ unbox_reference_n env n


    | UnboxedWord64, Vanilla -> BoxedWord.box env
    | Vanilla, UnboxedWord64 -> BoxedWord.unbox env

    | UnboxedWord32, Vanilla -> BoxedSmallWord.box env
    | Vanilla, UnboxedWord32 -> BoxedSmallWord.unbox env

    | UnboxedReference, Vanilla -> Dfinity.box_reference env
    | Vanilla, UnboxedReference -> Dfinity.unbox_reference env

    | StaticThing s, Vanilla -> materialize env s
    | StaticThing s, UnboxedReference -> materialize_unboxed_ref env s
    | StaticThing s, UnboxedTuple 0 -> G.nop

    | _, _ ->
      Printf.eprintf "Unknown stack_rep conversion %s -> %s\n"
        (to_string sr_in) (to_string sr_out);
      G.nop

end (* StackRep *)

module ASEnv = struct
  (*
  The ActorScript specific environment:
  In scope variables and in-scope jump labels
  *)

  module NameEnv = Env.Make(String)
  type t = {
    vars : VarLoc.varloc NameEnv.t; (* variables ↦ their location *)
    labels : G.depth NameEnv.t; (* jump label ↦ their depth *)
  }

  let empty_ae = {
    vars = NameEnv.empty;
    labels = NameEnv.empty;
  }

  (* Creating a local environment, resetting the local fields,
     and removing bindings for local variables (unless they are at global locations)
  *)

  let mk_fun_ae ae = { ae with
    vars = NameEnv.filter (fun _ -> VarLoc.is_non_local) ae.vars;
  }
  let lookup_var ae var =
    match NameEnv.find_opt var ae.vars with
      | Some l -> Some l
      | None   -> Printf.eprintf "Could not find %s\n" var; None

  let needs_capture ae var = match lookup_var ae var with
    | Some l -> not (VarLoc.is_non_local l)
    | None -> assert false

  let reuse_local_with_offset (ae : t) name i off =
      { ae with vars = NameEnv.add name (VarLoc.HeapInd (i, off)) ae.vars }

  let add_local_with_offset env (ae : t) name off =
      let i = E.add_anon_local env I32Type in
      E.add_local_name env i name;
      (reuse_local_with_offset ae name i off, i)

  let add_local_static (ae : t) name ptr =
      { ae with vars = NameEnv.add name (VarLoc.Static ptr) ae.vars }

  let add_local_deferred (ae : t) name stack_rep materialize is_local =
      let open VarLoc in
      let d = {stack_rep; materialize; is_local} in
      { ae with vars = NameEnv.add name (VarLoc.Deferred d) ae.vars }

  let add_direct_local env (ae : t) name =
      let i = E.add_anon_local env I32Type in
      E.add_local_name env i name;
      ({ ae with vars = NameEnv.add name (VarLoc.Local i) ae.vars }, i)

  let in_scope_set (ae : t) =
    NameEnv.fold (fun k _ -> Freevars.S.add k) ae.vars Freevars.S.empty

  let add_label (ae : t) name (d : G.depth) =
      { ae with labels = NameEnv.add name.it d ae.labels }

  let get_label_depth (ae : t) name : G.depth  =
    match NameEnv.find_opt name.it ae.labels with
      | Some d -> d
      | None   -> Printf.eprintf "Could not find %s\n" name.it; raise Not_found

end (* ASEnv *)

module Var = struct
  (* This module is all about looking up ActorScript variables in the environment,
     and dealing with mutable variables *)

  open VarLoc

  (* Stores the payload (which is found on the stack) *)
  let set_val env ae var = match ASEnv.lookup_var ae var with
    | Some (Local i) ->
      G.i (LocalSet (nr i))
    | Some (HeapInd (i, off)) ->
      let (set_new_val, get_new_val) = new_local env "new_val" in
      set_new_val ^^
      G.i (LocalGet (nr i)) ^^
      get_new_val ^^
      Heap.store_field off
    | Some (Static ptr) ->
      let (set_new_val, get_new_val) = new_local env "new_val" in
      set_new_val ^^
      compile_unboxed_const ptr ^^
      get_new_val ^^
      Heap.store_field 1l
    | Some (Deferred d) -> assert false
    | None   -> assert false

  (* Returns the payload (optimized representation) *)
  let get_val (env : E.t) (ae : ASEnv.t) var = match ASEnv.lookup_var ae var with
    | Some (Local i) ->
      SR.Vanilla, G.i (LocalGet (nr i))
    | Some (HeapInd (i, off)) ->
      SR.Vanilla, G.i (LocalGet (nr i)) ^^ Heap.load_field off
    | Some (Static i) ->
      SR.Vanilla, compile_unboxed_const i ^^ Heap.load_field 1l
    | Some (Deferred d) ->
      d.stack_rep, d.materialize env
    | None -> assert false

  (* Returns the payload (vanilla representation) *)
  let get_val_vanilla (env : E.t) (ae : ASEnv.t) var =
    let sr, code = get_val env ae var in
    code ^^ StackRep.adjust env sr SR.Vanilla

  (* Returns the value to put in the closure,
     and code to restore it, including adding to the environment
  *)
  let capture old_env ae0 var : G.t * (E.t -> ASEnv.t -> (ASEnv.t * G.t)) =
    match ASEnv.lookup_var ae0 var with
    | Some (Local i) ->
      ( G.i (LocalGet (nr i))
      , fun new_env ae1 ->
        let (ae2, j) = ASEnv.add_direct_local new_env ae1 var in
        let restore_code = G.i (LocalSet (nr j))
        in (ae2, restore_code)
      )
    | Some (HeapInd (i, off)) ->
      ( G.i (LocalGet (nr i))
      , fun new_env ae1 ->
        let (ae2, j) = ASEnv.add_local_with_offset new_env ae1 var off in
        let restore_code = G.i (LocalSet (nr j))
        in (ae2, restore_code)
      )
    | Some (Deferred d) ->
      assert d.is_local;
      ( d.materialize old_env ^^
        StackRep.adjust old_env d.stack_rep SR.Vanilla
      , fun new_env ae1 ->
        let (ae2, j) = ASEnv.add_direct_local new_env ae1 var in
        let restore_code = G.i (LocalSet (nr j))
        in (ae2, restore_code)
      )
    | _ -> assert false

  (* Returns a pointer to a heap allocated box for this.
     (either a mutbox, if already mutable, or a freshly allocated box)
  *)
  let field_box env code =
    Tagged.obj env Tagged.ObjInd [ code ]

  let get_val_ptr env ae var = match ASEnv.lookup_var ae var with
    | Some (HeapInd (i, 1l)) -> G.i (LocalGet (nr i))
    | Some (Static _) -> assert false (* we never do this on the toplevel *)
    | _  -> field_box env (get_val_vanilla env ae var)

end (* Var *)

(* This comes late because it also deals with messages *)
module FuncDec = struct
  (* We use the first table slot for calls to funcrefs *)
  (* This does not clash with slots for our functions as long as there
     is at least one imported function (which we do not add to the table) *)
  let tmp_table_slot = 0l

  (* The type of messages *)
  let message_ty env cc =
    E.func_type env (FuncType (Lib.List.make cc.Value.n_args I32Type,[]))

  (* Expects all arguments on the stack, in serialized form. *)
  let call_funcref env cc get_ref =
    compile_unboxed_const tmp_table_slot ^^ (* slot number *)
    get_ref ^^ (* the unboxed funcref *)
    Dfinity.system_call env "func_internalize" ^^

    compile_unboxed_const tmp_table_slot ^^
    G.i (CallIndirect (nr (message_ty env cc)))

  let export_self_message env =
    Func.share_code1 env "export_self_message" ("name", I32Type) [I32Type] (fun env get_name ->
      Tagged.obj env Tagged.Reference [
        (* Create a funcref for the message *)
        Dfinity.system_call env "actor_self" ^^
        get_name ^^ (* the databuf with the message name *)
        Dfinity.system_call env "actor_export" ^^
        ElemHeap.remember_reference env
      ]
    )

  let _static_self_message_pointer env name =
    Dfinity.compile_databuf_of_bytes env name ^^
    export_self_message env

  let bind_args ae0 first_arg as_ bind_arg =
    let rec go i ae = function
    | [] -> ae
    | a::as_ ->
      let get = G.i (LocalGet (nr (Int32.of_int i))) in
      let ae' = bind_arg ae a get in
      go (i+1) ae' as_ in
    go first_arg ae0 as_

  (* Create a WebAssembly func from a pattern (for the argument) and the body.
   Parameter `captured` should contain the, well, captured local variables that
   the function will find in the closure. *)
  let compile_local_function outer_env outer_ae cc restore_env args mk_body at =
    let arg_names = List.map (fun a -> a.it, I32Type) args in
    let retty = Lib.List.make cc.Value.n_res I32Type in
    let ae0 = ASEnv.mk_fun_ae outer_ae in
    Func.of_body outer_env (["clos", I32Type] @ arg_names) retty (fun env -> G.with_region at (
      let get_closure = G.i (LocalGet (nr 0l)) in

      let (ae1, closure_code) = restore_env env ae0 get_closure in

      (* Add arguments to the environment *)
      let ae2 = bind_args ae1 1 args (fun env a get ->
        ASEnv.add_local_deferred env a.it SR.Vanilla (fun _ -> get) true
      ) in

      closure_code ^^
      mk_body env ae2
    ))

  (* Similar, but for shared functions aka messages. Differences are:
     - The closure is actually an index into the closure table
     - The arguments need to be deserialized.
     - The return value ought to be discarded
     - We need to register the type in the custom types section
     - Do GC at the end
     - Fake orthogonal persistence
  *)
  let compile_message outer_env outer_ae cc restore_env args mk_body at =
    let arg_names = List.map (fun a -> a.it, I32Type) args in
    assert (cc.Value.n_res = 0);
    let ae0 = ASEnv.mk_fun_ae outer_ae in
    Func.of_body outer_env (["clos", I32Type] @ arg_names) [] (fun env -> G.with_region at (
      (* Restore memory *)
      OrthogonalPersistence.restore_mem env ^^

      (* Look up closure *)
      let (set_closure, get_closure) = new_local env "closure" in
      G.i (LocalGet (nr 0l)) ^^
      ClosureTable.recall_closure env ^^
      set_closure ^^

      let (ae1, closure_code) = restore_env env ae0 get_closure in

      (* Add arguments to the environment, as unboxed references *)
      let ae2 = bind_args ae1 1 args (fun ae a get ->
        ASEnv.add_local_deferred ae a.it SR.UnboxedReference (fun _ -> get) true
      ) in

      closure_code ^^
      mk_body env ae2 ^^

      (* Collect garbage *)
      G.i (Call (nr (E.built_in env "collect"))) ^^

      (* Save memory *)
      OrthogonalPersistence.save_mem env
    ))

  let compile_static_message outer_env outer_ae cc args mk_body at : E.func_with_names =
    let arg_names = List.map (fun a -> a.it, I32Type) args in
    assert (cc.Value.n_res = 0);
    let ae0 = ASEnv.mk_fun_ae outer_ae in
    (* Messages take no closure, return nothing *)
    Func.of_body outer_env arg_names [] (fun env ->
      (* Set up memory *)
      OrthogonalPersistence.restore_mem env ^^

      (* Add arguments to the environment, as unboxed references *)
      let ae1 = bind_args ae0 0 args (fun ae a get ->
        ASEnv.add_local_deferred ae a.it SR.UnboxedReference (fun _ -> get) true
      ) in

      mk_body env ae1 ^^

      (* Collect garbage *)
      G.i (Call (nr (E.built_in env "collect"))) ^^

      (* Save memory *)
      OrthogonalPersistence.save_mem env
      )

  let declare_dfinity_type env has_closure fi args =
      E.add_dfinity_type env (fi,
        (if has_closure then [ CustomModule.I32 ] else []) @
        List.map (
          fun a -> Serialization.dfinity_type (Type.as_serialized a.note)
        ) args
      )

  (* Compile a closed function declaration (captures no local variables) *)
  let closed pre_env cc name args mk_body at =
    let (fi, fill) = E.reserve_fun pre_env name in
    if cc.Value.sort = Type.Sharable
    then begin
      declare_dfinity_type pre_env false fi args;
      ( SR.StaticMessage fi, fun env ae ->
        fill (compile_static_message env ae cc args mk_body at)
      )
    end else
      ( SR.StaticFun fi, fun env ae ->
        let restore_no_env _env ae _ = (ae, G.nop) in
        fill (compile_local_function env ae cc restore_no_env args mk_body at)
      )

  (* Compile a closure declaration (captures local variables) *)
  let closure env ae cc name captured args mk_body at =
      let is_local = cc.Value.sort <> Type.Sharable in

      let (set_clos, get_clos) = new_local env (name ^ "_clos") in

      let len = Wasm.I32.of_int_u (List.length captured) in
      let (store_env, restore_env) =
        let rec go i = function
          | [] -> (G.nop, fun _env ae1 _ -> (ae1, G.nop))
          | (v::vs) ->
              let (store_rest, restore_rest) = go (i+1) vs in
              let (store_this, restore_this) = Var.capture env ae v in
              let store_env =
                get_clos ^^
                store_this ^^
                Closure.store_data (Wasm.I32.of_int_u i) ^^
                store_rest in
              let restore_env env ae1 get_env =
                let (ae2, code) = restore_this env ae1 in
                let (ae3, code_rest) = restore_rest env ae2 get_env in
                (ae3,
                 get_env ^^
                 Closure.load_data (Wasm.I32.of_int_u i) ^^
                 code ^^
                 code_rest
                )
              in (store_env, restore_env) in
        go 0 captured in

      let f =
        if is_local
        then compile_local_function env ae cc restore_env args mk_body at
        else compile_message env ae cc restore_env args mk_body at in

      let fi = E.add_fun env name f in

      if not is_local then declare_dfinity_type env true fi args;

      let code =
        (* Allocate a heap object for the closure *)
        Heap.alloc env (Int32.add Closure.header_size len) ^^
        set_clos ^^

        (* Store the tag *)
        get_clos ^^
        Tagged.store Tagged.Closure ^^

        (* Store the function number: *)
        get_clos ^^
        compile_unboxed_const fi ^^
        Heap.store_field Closure.funptr_field ^^

        (* Store the length *)
        get_clos ^^
        compile_unboxed_const len ^^
        Heap.store_field Closure.len_field ^^

        (* Store all captured values *)
        store_env
      in

      (* Possibly turn into a funcref *)
      if is_local
      then
        SR.Vanilla,
        code ^^
        get_clos
      else
        SR.UnboxedReference,
        code ^^
        compile_unboxed_const fi ^^
        Dfinity.system_call env "func_externalize" ^^
        get_clos ^^
        ClosureTable.remember_closure env ^^
        Dfinity.system_call env "func_bind_i32"

  let lit env ae how name cc free_vars args mk_body at =
    let captured = List.filter (ASEnv.needs_capture ae) free_vars in

    if captured = []
    then
      let (st, fill) = closed env cc name args mk_body at in
      fill env ae;
      (SR.StaticThing st, G.nop)
    else closure env ae cc name captured args mk_body at

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
          G.labeled_block_ (ValBlockType (Some I32Type)) inner_fail (is1 inner_fail_code ^^ Bool.lit true) ^^
          G.if_ (ValBlockType None) G.nop (is2 fail_code)
        )
      | CannotFail is2 -> CannotFail (
          let inner_fail = G.new_depth_label () in
          let inner_fail_code = Bool.lit false ^^ G.branch_to_ inner_fail in
          G.labeled_block_ (ValBlockType (Some I32Type)) inner_fail (is1 inner_fail_code ^^ Bool.lit true) ^^
          G.if_ (ValBlockType None) G.nop is2
        )

  let orTrap env : patternCode -> G.t = function
    | CannotFail is -> is
    | CanFail is -> is (E.trap_with env "pattern failed")

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
  find out which names need to be heap-allocated, which local-allocated and which
  are simply static functions. The goal is to avoid dynamic allocation where
  possible (and use locals), and to avoid turning function references into closures.

  The rules for non-top-level-blocks are:
  - functions are static, unless they capture something that is not a static
    function or a static heap allocation.
  - everything that is captured before it is defined needs to be dynamically
    heap-allocated, unless it is a static function
  - everything that is mutable and captured needs to be dynamically heap-allocated
  - the rest can be local (immutable things can be put into closures by values)

  These rules require a fixed-point analysis.

  For the top-level blocks the rules are simpler
  - all functions are static
  - everything that is captured in a function is statically heap allocated
  - everything else is a local

  We represent this as a lattice as follows:
  *)

  module M = Freevars.M
  module S = Freevars.S

  type nonStatic = LocalImmut | LocalMut | StoreHeap | StoreStatic
  type allocHow = nonStatic M.t (* absent means static *)

  let join : allocHow -> allocHow -> allocHow =
    M.union (fun _ x y -> Some (match x, y with
      | StoreStatic, StoreHeap -> assert false
      | StoreHeap, StoreStatic -> assert false
      | _, StoreHeap -> StoreHeap
      | StoreHeap, _  -> StoreHeap
      | _, StoreStatic -> StoreStatic
      | StoreStatic, _  -> StoreStatic
      | LocalMut, _ -> LocalMut
      | _, LocalMut -> LocalMut
      | LocalImmut, LocalImmut -> LocalImmut
    ))

  type lvl = TopLvl | NotTopLvl

  let map_of_set x s = S.fold (fun v m -> M.add v x m) s M.empty
  let set_of_map m = M.fold (fun v _ m -> S.add v m) m S.empty

  let is_static ae how f =
    (* Does this capture nothing from outside? *)
    (S.is_empty (S.inter
      (Freevars.captured_vars f)
      (set_of_map (M.filter (fun _ x -> not (VarLoc.is_non_local x)) (ae.ASEnv.vars))))) &&
    (* Does this capture nothing non-static from here? *)
    (S.is_empty (S.inter
      (Freevars.captured_vars f)
      (set_of_map (M.filter (fun _ h -> h != StoreStatic) how))))

  let is_func_exp exp = match exp.it with
    | FuncE _ -> true
    | _ -> false

  let is_static_exp env how0 exp =
    (* Functions are static when they do not capture anything *)
    if is_func_exp exp
    then is_static env how0 (Freevars.exp exp)
    else false

  let is_local_mut _ = function
    | LocalMut -> true
    | _ -> false

  let dec_local env (seen, how0) dec =
    let (f,d) = Freevars.dec dec in
    let captured = Freevars.captured_vars f in

    (* Which allocation is required for the things defined here? *)
    let how1 = match dec.it with
      (* Mutable variables are, well, mutable *)
      | VarD _ ->
      map_of_set LocalMut d
      (* Static functions in an let-expression *)
      | LetD ({it = VarP _; _}, e) when is_static_exp env how0 e ->
      M.empty
      (* Everything else needs at least a local *)
      | _ ->
      map_of_set LocalImmut d in

    (* Do we capture anything unseen, but non-static?
       These need to be heap-allocated.
    *)
    let how2 =
      map_of_set StoreHeap
        (S.inter
          (set_of_map how0)
          (S.diff (Freevars.captured_vars f) seen)) in

    (* Do we capture anything else?
       For local blocks, mutable things must be heap allocated.
    *)
    let how3 =
      map_of_set StoreHeap
        (S.inter (set_of_map (M.filter is_local_mut how0)) captured) in

    let how = List.fold_left join M.empty [how0; how1; how2; how3] in
    let seen' = S.union seen d
    in (seen', how)

  let decs_local env decs captured_in_body : allocHow =
    let rec go how =
      let _seen, how1 = List.fold_left (dec_local env) (S.empty, how) decs in
      let how2 = map_of_set StoreHeap
        (S.inter (set_of_map (M.filter is_local_mut how1)) captured_in_body) in
      let how' = join how1 how2 in
      if M.equal (=) how how' then how else go how' in
    go M.empty

  let decs_top_lvl env decs captured_in_body : allocHow =
    let how0 = M.empty in
    (* All non-function are at least locals *)
    let how1 =
      let go how dec =
        let (f,d) = Freevars.dec dec in
        match dec.it with
          | LetD ({it = VarP _; _}, e) when is_func_exp e -> how
          | _ -> join how (map_of_set LocalMut d) in
      List.fold_left go how0 decs in
    (* All captured non-functions are heap allocated *)
    let how2 = join how1 (map_of_set StoreStatic (S.inter (set_of_map how1) captured_in_body)) in
    let how3 =
      let go how dec =
        let (f,d) = Freevars.dec dec in
        let captured = Freevars.captured_vars f in
        join how (map_of_set StoreStatic (S.inter (set_of_map how1) captured)) in
      List.fold_left go how2 decs in
    how3

  let decs env lvl decs captured_in_body : allocHow = match lvl with
    | TopLvl -> decs_top_lvl env decs captured_in_body
    | NotTopLvl -> decs_local env decs captured_in_body

  (* Functions to extend the environment (and possibly allocate memory)
     based on how we want to store them. *)
  let add_how env ae name : nonStatic option -> ASEnv.t * G.t = function
    | Some LocalImmut | Some LocalMut ->
      let (ae1, i) = ASEnv.add_direct_local env ae name in
      (ae1, G.nop)
    | Some StoreHeap ->
      let (ae1, i) = ASEnv.add_local_with_offset env ae name 1l in
      let alloc_code =
        Tagged.obj env Tagged.MutBox [ compile_unboxed_zero ] ^^
        G.i (LocalSet (nr i)) in
      (ae1, alloc_code)
    | Some StoreStatic ->
      let tag = bytes_of_int32 (Tagged.int_of_tag Tagged.MutBox) in
      let zero = bytes_of_int32 0l in
      let ptr = E.add_mutable_static_bytes env (tag ^ zero) in
      let ae1 = ASEnv.add_local_static ae name ptr in
      (ae1, G.nop)
    | None -> (ae, G.nop)

  let add_local env ae how name =
    add_how env ae name (M.find_opt name how)

end (* AllocHow *)

(* The actual compiler code that looks at the AST *)

let compile_lit env lit =
  try Syntax.(match lit with
    (* Booleans are directly in Vanilla representation *)
    | BoolLit false -> SR.bool, Bool.lit false
    | BoolLit true ->  SR.bool, Bool.lit true
    (* This maps int to int32, instead of a proper arbitrary precision library *)
    | IntLit n
    | NatLit n      -> SR.Vanilla, BigNum.compile_lit env n
    | Word8Lit n    -> SR.Vanilla, compile_unboxed_const (Value.Word8.to_bits n)
    | Word16Lit n   -> SR.Vanilla, compile_unboxed_const (Value.Word16.to_bits n)
    | Word32Lit n   -> SR.UnboxedWord32, compile_unboxed_const n
    | Word64Lit n   -> SR.UnboxedWord64, compile_const_64 n
    | CharLit c     -> SR.Vanilla, compile_unboxed_const Int32.(shift_left (of_int c) 8)
    | NullLit       -> SR.Vanilla, Opt.null
    | TextLit t     -> SR.Vanilla, Text.lit env t
    | _ -> todo_trap_SR env "compile_lit" (Arrange.lit lit)
    )
  with Failure _ ->
    Printf.eprintf "compile_lit: Overflow in literal %s\n" (Syntax.string_of_lit lit);
    SR.Unreachable, E.trap_with env "static literal overflow"

let compile_lit_as env sr_out lit =
  let sr_in, code = compile_lit env lit in
  code ^^ StackRep.adjust env sr_in sr_out

let compile_unop env t op =
  let open Syntax in
  match op, t with
  | NegOp, Type.(Prim Int) ->
      SR.Vanilla,
      BigNum.compile_neg env
  | NegOp, Type.(Prim Word64) ->
      SR.UnboxedWord64,
      Func.share_code1 env "neg" ("n", I64Type) [I64Type] (fun env get_n ->
        compile_const_64 0L ^^
        get_n ^^
        G.i (Binary (Wasm.Values.I64 I64Op.Sub))
      )
  | NegOp, Type.Prim Type.(Word8 | Word16 | Word32) ->
      StackRep.of_type t,
      Func.share_code1 env "neg32" ("n", I32Type) [I32Type] (fun env get_n ->
        compile_unboxed_zero ^^
        get_n ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Sub))
      )
  | NotOp, Type.(Prim Word64) ->
     SR.UnboxedWord64,
     compile_const_64 (-1L) ^^
     G.i (Binary (Wasm.Values.I64 I64Op.Xor))
  | NotOp, Type.Prim Type.(Word8 | Word16 | Word32 as ty) ->
      StackRep.of_type t, compile_unboxed_const (UnboxedSmallWord.mask_of_type ty) ^^
                          G.i (Binary (Wasm.Values.I32 I32Op.Xor))
  | _ ->
    (* NB: Must not use todo_trap_SR here, as the SR.t here is also passed to
       `compile_exp_as`, which does not take SR.Unreachable. *)
    todo "compile_unop" (Arrange.unop op) (SR.Vanilla, E.trap_with env "TODO: compile_unop")

(* This returns a single StackRep, to be used for both arguments and the
   result. One could imagine operators that require or produce different StackReps,
   but none of these do, so a single value is fine.
*)
let rec compile_binop env t op =
  StackRep.of_type t,
  Syntax.(match t, op with
  | Type.(Prim (Nat | Int)),                  AddOp -> BigNum.compile_add env
  | Type.(Prim Word64),                       AddOp -> G.i (Binary (Wasm.Values.I64 I64Op.Add))
  | Type.(Prim Nat),                          SubOp -> BigNum.compile_unsigned_sub env
  | Type.(Prim Int),                          SubOp -> BigNum.compile_signed_sub env
  | Type.(Prim (Nat | Int)),                  MulOp -> BigNum.compile_mul env
  | Type.(Prim Word64),                       MulOp -> G.i (Binary (Wasm.Values.I64 I64Op.Mul))
  | Type.(Prim Word64),                       DivOp -> G.i (Binary (Wasm.Values.I64 I64Op.DivU))
  | Type.(Prim Word64),                       ModOp -> G.i (Binary (Wasm.Values.I64 I64Op.RemU))
  | Type.(Prim Nat),                          DivOp -> BigNum.compile_unsigned_div env
  | Type.(Prim Nat),                          ModOp -> BigNum.compile_unsigned_rem env
  | Type.(Prim Word64),                       SubOp -> G.i (Binary (Wasm.Values.I64 I64Op.Sub))
  | Type.(Prim Int),                          DivOp -> BigNum.compile_signed_div env
  | Type.(Prim Int),                          ModOp -> BigNum.compile_signed_mod env

  | Type.Prim Type.(Word8 | Word16 | Word32), AddOp -> G.i (Binary (Wasm.Values.I32 I32Op.Add))
  | Type.Prim Type.(Word8 | Word16 | Word32), SubOp -> G.i (Binary (Wasm.Values.I32 I32Op.Sub))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  MulOp -> UnboxedSmallWord.lsb_adjust ty ^^
                                                       G.i (Binary (Wasm.Values.I32 I32Op.Mul))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  DivOp -> G.i (Binary (Wasm.Values.I32 I32Op.DivU)) ^^
                                                       UnboxedSmallWord.msb_adjust ty
  | Type.Prim Type.(Word8 | Word16 | Word32), ModOp -> G.i (Binary (Wasm.Values.I32 I32Op.RemU))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  PowOp ->
     let rec pow () = Func.share_code2 env (UnboxedSmallWord.name_of_type ty "pow")
                        (("n", I32Type), ("exp", I32Type)) [I32Type]
                        Wasm.Values.(fun env get_n get_exp ->
         let one = compile_unboxed_const (UnboxedSmallWord.const_of_type ty 1l) in
         let (set_res, get_res) = new_local env "res" in
         let mul = snd (compile_binop env t MulOp) in
         let square_recurse_with_shifted sanitize =
           get_n ^^ get_exp ^^ compile_shrU_const 1l ^^ sanitize ^^
           pow () ^^ set_res ^^ get_res ^^ get_res ^^ mul
         in get_exp ^^ G.i (Test (I32 I32Op.Eqz)) ^^
            G.if_ (StackRep.to_block_type env SR.UnboxedWord32)
             one
             (get_exp ^^ one ^^ G.i (Binary (I32 I32Op.And)) ^^ G.i (Test (I32 I32Op.Eqz)) ^^
              G.if_ (StackRep.to_block_type env SR.UnboxedWord32)
                (square_recurse_with_shifted G.nop)
                (get_n ^^
                 square_recurse_with_shifted (UnboxedSmallWord.sanitize_word_result ty) ^^
                 mul)))
     in pow ()
  | Type.(Prim Int),                          PowOp ->
     let pow = BigNum.compile_unsigned_pow env in
     let (set_n, get_n) = new_local env "n" in
     let (set_exp, get_exp) = new_local env "exp" in
     set_exp ^^ set_n ^^
     get_exp ^^ BigNum.compile_is_negative env ^^
     E.then_trap_with env "negative power" ^^
     get_n ^^ get_exp ^^ pow
  | Type.(Prim Word64),                       PowOp -> BoxedWord.compile_unsigned_pow env
  | Type.(Prim Nat),                          PowOp -> BigNum.compile_unsigned_pow env
  | Type.(Prim Word64),                       AndOp -> G.i (Binary (Wasm.Values.I64 I64Op.And))
  | Type.Prim Type.(Word8 | Word16 | Word32), AndOp -> G.i (Binary (Wasm.Values.I32 I32Op.And))
  | Type.(Prim Word64),                       OrOp  -> G.i (Binary (Wasm.Values.I64 I64Op.Or))
  | Type.Prim Type.(Word8 | Word16 | Word32), OrOp  -> G.i (Binary (Wasm.Values.I32 I32Op.Or))
  | Type.(Prim Word64),                       XorOp -> G.i (Binary (Wasm.Values.I64 I64Op.Xor))
  | Type.Prim Type.(Word8 | Word16 | Word32), XorOp -> G.i (Binary (Wasm.Values.I32 I32Op.Xor))
  | Type.(Prim Word64),                       ShLOp -> G.i (Binary (Wasm.Values.I64 I64Op.Shl))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  ShLOp -> UnboxedSmallWord.(
     lsb_adjust ty ^^ clamp_shift_amount ty ^^
     G.i (Binary (Wasm.Values.I32 I32Op.Shl)))
  | Type.(Prim Word64),                       UShROp -> G.i (Binary (Wasm.Values.I64 I64Op.ShrU))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  UShROp -> UnboxedSmallWord.(
     lsb_adjust ty ^^ clamp_shift_amount ty ^^
     G.i (Binary (Wasm.Values.I32 I32Op.ShrU)) ^^
     sanitize_word_result ty)
  | Type.(Prim Word64),                       SShROp -> G.i (Binary (Wasm.Values.I64 I64Op.ShrS))
  | Type.(Prim (Word8|Word16|Word32 as ty)),  SShROp -> UnboxedSmallWord.(
     lsb_adjust ty ^^ clamp_shift_amount ty ^^
     G.i (Binary (Wasm.Values.I32 I32Op.ShrS)) ^^
     sanitize_word_result ty)
  | Type.(Prim Word64),                       RotLOp -> G.i (Binary (Wasm.Values.I64 I64Op.Rotl))
  | Type.Prim Type.                  Word32,  RotLOp -> G.i (Binary (Wasm.Values.I32 I32Op.Rotl))
  | Type.Prim Type.(Word8 | Word16 as ty),    RotLOp -> UnboxedSmallWord.(
     Func.share_code2 env (name_of_type ty "rotl") (("n", I32Type), ("by", I32Type)) [I32Type]
       Wasm.Values.(fun env get_n get_by ->
      let beside_adjust = compile_shrU_const (Int32.sub 32l (shift_of_type ty)) in
      get_n ^^ get_n ^^ beside_adjust ^^ G.i (Binary (I32 I32Op.Or)) ^^
      get_by ^^ lsb_adjust ty ^^ clamp_shift_amount ty ^^ G.i (Binary (I32 I32Op.Rotl)) ^^
      sanitize_word_result ty))
  | Type.(Prim Word64),                       RotROp -> G.i (Binary (Wasm.Values.I64 I64Op.Rotr))
  | Type.Prim Type.                  Word32,  RotROp -> G.i (Binary (Wasm.Values.I32 I32Op.Rotr))
  | Type.Prim Type.(Word8 | Word16 as ty),    RotROp -> UnboxedSmallWord.(
     Func.share_code2 env (name_of_type ty "rotr") (("n", I32Type), ("by", I32Type)) [I32Type]
       Wasm.Values.(fun env get_n get_by ->
      get_n ^^ get_n ^^ lsb_adjust ty ^^ G.i (Binary (I32 I32Op.Or)) ^^
      get_by ^^ lsb_adjust ty ^^ clamp_shift_amount ty ^^ G.i (Binary (I32 I32Op.Rotr)) ^^
      sanitize_word_result ty))

  | Type.Prim Type.Text, CatOp -> Text.concat env
  | _ -> todo_trap env "compile_binop" (Arrange.binop op)
  )

let compile_eq env t = match t with
  | Type.Prim Type.Text -> Text.compare env
  | Type.Prim Type.Bool -> G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | Type.(Prim (Nat | Int)) -> BigNum.compile_eq env
  | Type.(Prim Word64) -> G.i (Compare (Wasm.Values.I64 I64Op.Eq))
  | Type.(Prim (Word8 | Word16 | Word32 | Char)) -> G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | _ -> todo_trap env "compile_eq" (Arrange.relop Syntax.EqOp)

let get_relops = Syntax.(function
  | GeOp -> Ge, I64Op.GeU, I32Op.GeU, I32Op.GeS
  | GtOp -> Gt, I64Op.GtU, I32Op.GtU, I32Op.GtS
  | LeOp -> Le, I64Op.LeU, I32Op.LeU, I32Op.LeS
  | LtOp -> Lt, I64Op.LtU, I32Op.LtU, I32Op.LtS
  | _ -> failwith "uncovered relop")

let compile_comparison env t op =
  let bigintop, u64op, u32op, s32op = get_relops op in
  let open Type in
  match t with
    | Nat | Int -> BigNum.compile_relop env bigintop
    | Word64 -> G.i (Compare (Wasm.Values.I64 u64op))
    | Word8 | Word16 | Word32 | Char -> G.i (Compare (Wasm.Values.I32 u32op))
    | _ -> todo_trap env "compile_comparison" (Arrange.prim t)

let compile_relop env t op =
  StackRep.of_type t,
  let open Syntax in
  match t, op with
  | _, EqOp -> compile_eq env t
  | _, NeqOp -> compile_eq env t ^^
     G.if_ (StackRep.to_block_type env SR.bool) (Bool.lit false) (Bool.lit true)
  | Type.Prim Type.(Nat | Int | Word8 | Word16 | Word32 | Word64 | Char as t1), op1 ->
     compile_comparison env t1 op1
  | _ -> todo_trap env "compile_relop" (Arrange.relop op)

(* compile_load_field implements the various “virtual fields”, which
   we currently have for arrays and text.
   It goes through branch_typed_with, which does a dynamic check of the
   heap object type *)
let compile_load_field env typ name =
  let branches =
    ( Tagged.Object, Object.load_idx env typ name ) ::
    match name with
    | "len" ->
      [ Tagged.Array, Arr.partial_len env
      ; Tagged.Text, Text.partial_len env ]
    | "get" ->
      [ Tagged.Array, Arr.partial_get env ]
    | "set" ->
      [ Tagged.Array, Arr.partial_set env ]
    | "keys" ->
      [ Tagged.Array, Arr.keys_iter env ]
    | "vals" ->
      [ Tagged.Array, Arr.vals_iter env ]
    | "chars" ->
      [ Tagged.Text, Text.text_chars env ]
    | _ -> []
    in
  Tagged.branch_typed_with env typ (ValBlockType (Some I32Type)) branches

(* compile_lexp is used for expressions on the left of an
assignment operator, produces some code (with side effect), and some pure code *)
let rec compile_lexp (env : E.t) ae exp =
  (fun (code,fill_code) -> (G.with_region exp.at code, G.with_region exp.at fill_code)) @@
  match exp.it with
  | VarE var ->
     G.nop,
     Var.set_val env ae var.it
  | IdxE (e1,e2) ->
     compile_exp_vanilla env ae e1 ^^ (* offset to array *)
     compile_exp_vanilla env ae e2 ^^ (* idx *)
     BigNum.to_word32 env ^^
     Arr.idx env,
     store_ptr
  | DotE (e, n) ->
     compile_exp_vanilla env ae e ^^
     (* Only real objects have mutable fields, no need to branch on the tag *)
     Object.idx env e.note.note_typ n,
     store_ptr
  | _ -> todo "compile_lexp" (Arrange_ir.exp exp) (E.trap_with env "TODO: compile_lexp", G.nop)

and compile_exp (env : E.t) ae exp =
  (fun (sr,code) -> (sr, G.with_region exp.at code)) @@
  match exp.it with
  | IdxE (e1, e2)  ->
    SR.Vanilla,
    compile_exp_vanilla env ae e1 ^^ (* offset to array *)
    compile_exp_vanilla env ae e2 ^^ (* idx *)
    BigNum.to_word32 env ^^
    Arr.idx env ^^
    load_ptr
  | DotE (e, name) ->
    SR.Vanilla,
    compile_exp_vanilla env ae e ^^
    compile_load_field env e.note.note_typ name
  | ActorDotE (e, name) ->
    SR.UnboxedReference,
    compile_exp_as env ae SR.UnboxedReference e ^^
    actor_fake_object_idx env name
  (* We only allow prims of certain shapes, as they occur in the prelude *)
  | CallE (_, ({ it = PrimE p; _} as pe), typ_args, e) ->
    begin
      (* First check for all unary prims. *)
      match p with
       | "@serialize" ->
         SR.UnboxedReference,
         let t = match typ_args with [t] -> t | _ -> assert false in
         compile_exp_vanilla env ae e ^^
         Serialization.serialize env t

       | "@deserialize" ->
         SR.Vanilla,
         let t = match typ_args with [t] -> t | _ -> assert false in
         compile_exp_as env ae SR.UnboxedReference e ^^
         Serialization.deserialize env t

       | "abs" ->
         SR.Vanilla,
         compile_exp_vanilla env ae e ^^
         BigNum.compile_abs env

       | "rts_version" ->
         SR.Vanilla,
         compile_exp_as env ae SR.unit e ^^
         E.call_import env "rts" "version"

       | "Nat->Word8"
       | "Int->Word8" ->
         SR.Vanilla,
         compile_exp_vanilla env ae e ^^
         Prim.prim_shiftToWordN env (UnboxedSmallWord.shift_of_type Type.Word8)

       | "Nat->Word16"
       | "Int->Word16" ->
         SR.Vanilla,
         compile_exp_vanilla env ae e ^^
         Prim.prim_shiftToWordN env (UnboxedSmallWord.shift_of_type Type.Word16)

       | "Nat->Word32"
       | "Int->Word32" ->
         SR.UnboxedWord32,
         compile_exp_vanilla env ae e ^^
         Prim.prim_intToWord32 env

       | "Nat->Word64"
       | "Int->Word64" ->
         SR.UnboxedWord64,
         compile_exp_vanilla env ae e ^^
         BigNum.to_word64 env

       | "Char->Word32" ->
         SR.UnboxedWord32,
         compile_exp_vanilla env ae e ^^
         UnboxedSmallWord.unbox_codepoint

       | "Word8->Nat" ->
         SR.Vanilla,
         compile_exp_vanilla env ae e ^^
         Prim.prim_shiftWordNtoUnsigned env (UnboxedSmallWord.shift_of_type Type.Word8)
       | "Word8->Int" ->
         SR.Vanilla,
         compile_exp_vanilla env ae e ^^
         Prim.prim_shiftWordNtoSigned env (UnboxedSmallWord.shift_of_type Type.Word8)

       | "Word16->Nat" ->
         SR.Vanilla,
         compile_exp_vanilla env ae e ^^
         Prim.prim_shiftWordNtoUnsigned env (UnboxedSmallWord.shift_of_type Type.Word16)
       | "Word16->Int" ->
         SR.Vanilla,
         compile_exp_vanilla env ae e ^^
         Prim.prim_shiftWordNtoSigned env (UnboxedSmallWord.shift_of_type Type.Word16)

       | "Word32->Nat" ->
         SR.Vanilla,
         compile_exp_as env ae SR.UnboxedWord32 e ^^
         Prim.prim_word32toNat env
       | "Word32->Int" ->
         SR.Vanilla,
         compile_exp_as env ae SR.UnboxedWord32 e ^^
         Prim.prim_word32toInt env

       | "Word64->Nat" ->
         SR.Vanilla,
         compile_exp_as env ae SR.UnboxedWord64 e ^^
         BigNum.from_word64 env

       | "Word64->Int" ->
         SR.Vanilla,
         compile_exp_as env ae SR.UnboxedWord64 e ^^
         BigNum.from_signed_word64 env

       | "Word32->Char" ->
         SR.Vanilla,
         compile_exp_as env ae SR.UnboxedWord32 e ^^
         UnboxedSmallWord.box_codepoint

       | "popcnt" ->
         SR.UnboxedWord32,
         compile_exp_as env ae SR.UnboxedWord32 e ^^
         G.i (Unary (Wasm.Values.I32 I32Op.Popcnt))
       | "popcnt8"
       | "popcnt16" ->
         SR.Vanilla,
         compile_exp_vanilla env ae e ^^
         G.i (Unary (Wasm.Values.I32 I32Op.Popcnt)) ^^
         UnboxedSmallWord.msb_adjust (match p with | "popcnt8" -> Type.Word8 | _ -> Type.Word16)
       | "popcnt64" ->
         SR.UnboxedWord64,
         compile_exp_as env ae SR.UnboxedWord64 e ^^
         G.i (Unary (Wasm.Values.I64 I64Op.Popcnt))
       | "clz" -> SR.UnboxedWord32, compile_exp_as env ae SR.UnboxedWord32 e ^^ G.i (Unary (Wasm.Values.I32 I32Op.Clz))
       | "clz8" -> SR.Vanilla, compile_exp_vanilla env ae e ^^ UnboxedSmallWord.clz_kernel Type.Word8
       | "clz16" -> SR.Vanilla, compile_exp_vanilla env ae e ^^ UnboxedSmallWord.clz_kernel Type.Word16
       | "clz64" -> SR.UnboxedWord64, compile_exp_as env ae SR.UnboxedWord64 e ^^ G.i (Unary (Wasm.Values.I64 I64Op.Clz))
       | "ctz" -> SR.UnboxedWord32, compile_exp_as env ae SR.UnboxedWord32 e ^^ G.i (Unary (Wasm.Values.I32 I32Op.Ctz))
       | "ctz8" -> SR.Vanilla, compile_exp_vanilla env ae e ^^ UnboxedSmallWord.ctz_kernel Type.Word8
       | "ctz16" -> SR.Vanilla, compile_exp_vanilla env ae e ^^ UnboxedSmallWord.ctz_kernel Type.Word16
       | "ctz64" -> SR.UnboxedWord64, compile_exp_as env ae SR.UnboxedWord64 e ^^ G.i (Unary (Wasm.Values.I64 I64Op.Ctz))

       | "Char->Text" ->
         SR.Vanilla,
         compile_exp_vanilla env ae e ^^
         Text.prim_showChar env

       | "print" ->
         SR.unit,
         compile_exp_vanilla env ae e ^^
         Dfinity.prim_print env
       | "decodeUTF8" ->
         SR.UnboxedTuple 2,
         compile_exp_vanilla env ae e ^^
         Text.prim_decodeUTF8 env
       | _ ->
        (* Now try the binary prims, expecting a manifest tuple argument *)
        begin match e.it with
        | TupE [e1;e2] ->
          begin
           let compile_kernel_as sr inst = sr, compile_exp_as env ae sr e1 ^^ compile_exp_as env ae sr e2 ^^ inst
           in match p with
             | "Array.init" -> compile_kernel_as SR.Vanilla (Arr.init env)
             | "Array.tabulate" -> compile_kernel_as SR.Vanilla (Arr.tabulate env)
             | "btst8" -> compile_kernel_as SR.Vanilla (UnboxedSmallWord.btst_kernel env Type.Word8)
             | "btst16" -> compile_kernel_as SR.Vanilla (UnboxedSmallWord.btst_kernel env Type.Word16)
             | "btst" -> compile_kernel_as SR.UnboxedWord32 (UnboxedSmallWord.btst_kernel env Type.Word32)
             | "btst64" -> compile_kernel_as SR.UnboxedWord64 (
                               let (set_b, get_b) = new_local64 env "b"
                               in set_b ^^ compile_const_64 1L ^^ get_b ^^ G.i (Binary (Wasm.Values.I64 I64Op.Shl)) ^^
                                  G.i (Binary (Wasm.Values.I64 I64Op.And)))

             | _ -> SR.Unreachable, todo_trap env "compile_exp" (Arrange_ir.exp pe)
          end
        | _ -> SR.Unreachable, todo_trap env "compile_exp" (Arrange_ir.exp pe)
        end
    end
  | VarE var ->
    Var.get_val env ae var.it
  | AssignE (e1,e2) ->
    SR.unit,
    let (prepare_code, store_code) = compile_lexp env ae e1 in
    prepare_code ^^
    compile_exp_vanilla env ae e2 ^^
    store_code
  | LitE l ->
    compile_lit env l
  | AssertE e1 ->
    SR.unit,
    compile_exp_as env ae SR.bool e1 ^^
    G.if_ (ValBlockType None) G.nop (G.i Unreachable)
  | UnE (_, Syntax.PosOp, e1) -> compile_exp env ae e1
  | UnE (t, op, e1) ->
    let sr, code = compile_unop env t op in
    sr,
    compile_exp_as env ae sr e1 ^^
    code
  | BinE (t, e1, op, e2) ->
    let sr, code = compile_binop env t op in
    sr,
    compile_exp_as env ae sr e1 ^^
    compile_exp_as env ae sr e2 ^^
    code
  | RelE (t, e1, op, e2) ->
    let sr, code = compile_relop env t op in
    SR.bool,
    compile_exp_as env ae sr e1 ^^
    compile_exp_as env ae sr e2 ^^
    code
  | IfE (scrut, e1, e2) ->
    let code_scrut = compile_exp_as env ae SR.bool scrut in
    let sr1, code1 = compile_exp env ae e1 in
    let sr2, code2 = compile_exp env ae e2 in
    let sr = StackRep.join sr1 sr2 in
    sr,
    code_scrut ^^ G.if_
      (StackRep.to_block_type env sr)
      (code1 ^^ StackRep.adjust env sr1 sr)
      (code2 ^^ StackRep.adjust env sr2 sr)
  | BlockE (decs, exp) ->
    let captured = Freevars.captured_vars (Freevars.exp exp) in
    let (ae', code1) = compile_decs env ae AllocHow.NotTopLvl decs captured in
    let (sr, code2) = compile_exp env ae' exp in
    (sr, code1 ^^ code2)
  | LabelE (name, _ty, e) ->
    (* The value here can come from many places -- the expression,
       or any of the nested returns. Hard to tell which is the best
       stack representation here.
       So let’s go with Vanilla. *)
    SR.Vanilla,
    G.block_ (StackRep.to_block_type env SR.Vanilla) (
      G.with_current_depth (fun depth ->
        let ae1 = ASEnv.add_label ae name depth in
        compile_exp_vanilla env ae1 e
      )
    )
  | BreakE (name, e) ->
    let d = ASEnv.get_label_depth ae name in
    SR.Unreachable,
    compile_exp_vanilla env ae e ^^
    G.branch_to_ d
  | LoopE e ->
    SR.Unreachable,
    G.loop_ (ValBlockType None) (compile_exp_unit env ae e ^^ G.i (Br (nr 0l))
    )
    ^^
   G.i Unreachable
  | RetE e ->
    SR.Unreachable,
    compile_exp_as env ae (StackRep.of_arity (E.get_n_res env)) e ^^
    G.i Return
  | OptE e ->
    SR.Vanilla,
    Opt.inject env (compile_exp_vanilla env ae e)
  | TagE (l, e) ->
    SR.Vanilla,
    Variant.inject env l.it (compile_exp_vanilla env ae e)
  | TupE es ->
    SR.UnboxedTuple (List.length es),
    G.concat_map (compile_exp_vanilla env ae) es
  | ProjE (e1,n) ->
    SR.Vanilla,
    compile_exp_vanilla env ae e1 ^^ (* offset to tuple (an array) *)
    Tuple.load_n (Int32.of_int n)
  | ArrayE (m, t, es) ->
    SR.Vanilla, Arr.lit env (List.map (compile_exp_vanilla env ae) es)
  | CallE (cc, e1, _, e2) ->
    StackRep.of_arity (cc.Value.n_res),
    let fun_sr, code1 = compile_exp env ae e1 in
    begin match fun_sr, cc.Value.sort with
     | SR.StaticThing (SR.StaticFun fi), _ ->
        code1 ^^
        compile_unboxed_zero ^^ (* A dummy closure *)
        compile_exp_as env ae (StackRep.of_arity cc.Value.n_args) e2 ^^ (* the args *)
        G.i (Call (nr fi))
     | _, Type.Local ->
        let (set_clos, get_clos) = new_local env "clos" in
        code1 ^^ StackRep.adjust env fun_sr SR.Vanilla ^^
        set_clos ^^
        get_clos ^^
        compile_exp_as env ae (StackRep.of_arity cc.Value.n_args) e2 ^^
        get_clos ^^
        Closure.call_closure env cc
     | _, Type.Sharable ->
        let (set_funcref, get_funcref) = new_local env "funcref" in
        code1 ^^ StackRep.adjust env fun_sr SR.UnboxedReference ^^
        set_funcref ^^
        compile_exp_as env ae (StackRep.refs_of_arity cc.Value.n_args) e2 ^^
        FuncDec.call_funcref env cc get_funcref
    end
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
    let (ae1, i) = ASEnv.add_local_with_offset env ae name.it 1l in
    let sr, code = compile_exp env ae1 e in
    sr,
    Tagged.obj env Tagged.MutBox [ compile_unboxed_zero ] ^^
    G.i (LocalSet (nr i)) ^^
    code
  | DefineE (name, _, e) ->
    SR.unit,
    compile_exp_vanilla env ae e ^^
    Var.set_val env ae name.it
  | FuncE (x, cc, typ_binds, args, _rt, e) ->
    let captured = Freevars.captured exp in
    let mk_body env1 ae1 = compile_exp_as env1 ae1 (StackRep.of_arity cc.Value.n_res) e in
    FuncDec.lit env ae typ_binds x cc captured args mk_body exp.at
  | ActorE (i, ds, fs, _) ->
    SR.UnboxedReference,
    let captured = Freevars.exp exp in
    let prelude_names = find_prelude_names env in
    if Freevars.M.is_empty (Freevars.diff captured prelude_names)
    then actor_lit env i ds fs exp.at
    else todo_trap env "non-closed actor" (Arrange_ir.exp exp)
  | NewObjE ((Type.Object _ (*sharing*) | Type.Module), fs, _) ->
    SR.Vanilla,
    let fs' = fs |> List.map
      (fun (f : Ir.field) -> (f.it.name, fun () ->
        if Object.is_mut_field env exp.note.note_typ f.it.name
        then Var.get_val_ptr env ae f.it.var.it
        else Var.get_val_vanilla env ae f.it.var.it)) in
    Object.lit_raw env fs'
  | _ -> SR.unit, todo_trap env "compile_exp" (Arrange_ir.exp exp)

and compile_exp_as env ae sr_out e =
  G.with_region e.at (
    match sr_out, e.it with
    (* Some optimizations for certain sr_out and expressions *)
    | SR.UnboxedRefTuple n, TupE es when n = List.length es ->
      G.concat_map (fun e ->
        compile_exp_as env ae SR.UnboxedReference e
      ) es
    | _ , BlockE (decs, exp) ->
      let captured = Freevars.captured_vars (Freevars.exp exp) in
      let (ae', code1) = compile_decs env ae AllocHow.NotTopLvl decs captured in
      let code2 = compile_exp_as env ae' sr_out exp in
      code1 ^^ code2
    (* Fallback to whatever stackrep compile_exp chooses *)
    | _ ->
      let sr_in, code = compile_exp env ae e in
      code ^^ StackRep.adjust env sr_in sr_out
  )

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
  | Syntax.NullLit ->
    compile_lit_as env SR.Vanilla l ^^
    G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | Syntax.BoolLit true ->
    G.nop
  | Syntax.BoolLit false ->
    Bool.lit false ^^
    G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | Syntax.(NatLit _ | IntLit _) ->
    compile_lit_as env SR.Vanilla l ^^
    BigNum.compile_eq env
  | Syntax.(TextLit t) ->
    Text.lit env t ^^
    Text.compare env
  | _ -> todo_trap env "compile_lit_pat" (Arrange.lit l)

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
        G.if_ (ValBlockType None)
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
        Variant.test_is env l.it ^^
        G.if_ (ValBlockType None)
          ( get_x ^^
            Variant.project ^^
            with_fail fail_code (fill_pat env ae p)
          )
          fail_code
      )
  | LitP l ->
      CanFail (fun fail_code ->
        compile_lit_pat env l ^^
        G.if_ (ValBlockType None) G.nop fail_code)
  | VarP name ->
      CannotFail (Var.set_val env ae name.it)
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
  let (_,d) = Freevars.pat pat in
  AllocHow.S.fold (fun v ae ->
    let (ae1, _i) = ASEnv.add_direct_local env ae v
    in ae1
  ) d ae

and alloc_pat env ae how pat : ASEnv.t * G.t  =
  (fun (ae,code) -> (ae, G.with_region pat.at code)) @@
  let (_,d) = Freevars.pat pat in
  AllocHow.S.fold (fun v (ae,code0) ->
    let (ae1, code1) = AllocHow.add_local env ae how v
    in (ae1, code0 ^^ code1)
  ) d (ae, G.nop)

and compile_pat_local env ae pat : ASEnv.t * patternCode =
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
    (fun (sr,code) -> (sr, G.with_region pat.at code)) @@
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

and compile_dec env pre_ae how dec : ASEnv.t * G.t * (ASEnv.t -> G.t) =
  (fun (pre_ae,alloc_code,mk_code) ->
       (pre_ae, G.with_region dec.at alloc_code, fun ae ->
         G.with_region dec.at (mk_code ae))) @@
  match dec.it with
  | TypD _ ->
    (pre_ae, G.nop, fun _ -> G.nop)
  (* A special case for static expressions *)
  | LetD ({it = VarP v; _}, e) when not (AllocHow.M.mem v.it how) ->
    let (static_thing, fill) = compile_static_exp env pre_ae how e in
    let pre_ae1 = ASEnv.add_local_deferred pre_ae v.it
      (SR.StaticThing static_thing) (fun _ -> G.nop) false in
    ( pre_ae1, G.nop, fun ae -> fill env ae; G.nop)
  | LetD (p, e) ->
    let (pre_ae1, alloc_code, pat_arity, fill_code) = compile_n_ary_pat env pre_ae how p in
    ( pre_ae1, alloc_code, fun ae ->
      compile_exp_as_opt env ae pat_arity e ^^
      fill_code
    )
  | VarD (name, e) ->
      assert (AllocHow.M.find_opt name.it how = Some AllocHow.LocalMut ||
              AllocHow.M.find_opt name.it how = Some AllocHow.StoreHeap ||
              AllocHow.M.find_opt name.it how = Some AllocHow.StoreStatic);
      let (pre_ae1, alloc_code) = AllocHow.add_local env pre_ae how name.it in

      ( pre_ae1, alloc_code, fun ae ->
        compile_exp_vanilla env ae e ^^
        Var.set_val env ae name.it
      )

and compile_decs env ae lvl decs captured_in_body : ASEnv.t * G.t =
  let how = AllocHow.decs ae lvl decs captured_in_body in
  let rec go pre_ae decs = match decs with
    | []          -> (pre_ae, G.nop, fun _ -> G.nop)
    | [dec]       -> compile_dec env pre_ae how dec
    | (dec::decs) ->
        let (pre_ae1, alloc_code1, mk_code1) = compile_dec env pre_ae how dec in
        let (pre_ae2, alloc_code2, mk_code2) = go              pre_ae1 decs in
        ( pre_ae2,
          alloc_code1 ^^ alloc_code2,
          fun env -> let code1 = mk_code1 env in
                     let code2 = mk_code2 env in
                     code1 ^^ code2
        ) in
  let (ae1, alloc_code, mk_code) = go ae decs in
  let code = mk_code ae1 in
  (ae1, alloc_code ^^ code)

and compile_top_lvl_expr env ae e = match e.it with
  | BlockE (decs, exp) ->
    let captured = Freevars.captured_vars (Freevars.exp e) in
    let (ae', code1) = compile_decs env ae AllocHow.TopLvl decs captured in
    let code2 = compile_top_lvl_expr env ae' exp in
    code1 ^^ code2
  | _ ->
    let (sr, code) = compile_exp env ae e in
    code ^^ StackRep.drop env sr

and compile_prog env ae (ds, e) =
    let captured = Freevars.captured_vars (Freevars.exp e) in
    let (ae', code1) = compile_decs env ae AllocHow.TopLvl ds captured in
    let code2 = compile_top_lvl_expr env ae' e in
    (ae', code1 ^^ code2)

and compile_static_exp env pre_ae how exp = match exp.it with
  | FuncE (name, cc, typ_binds, args, _rt, e) ->
      let mk_body env ae =
        assert begin (* Is this really closed? *)
          List.for_all (fun v -> ASEnv.NameEnv.mem v ae.ASEnv.vars)
            (Freevars.M.keys (Freevars.exp e))
        end;
        compile_exp_as env ae (StackRep.of_arity cc.Value.n_res) e in
      FuncDec.closed env cc name args mk_body exp.at
  | _ -> assert false

and compile_prelude env ae =
  (* Allocate the primitive functions *)
  let (decs, _flavor) = E.get_prelude env in
  let (ae1, code) = compile_prog env ae decs in
  (ae1, code)

(*
This is a horrible hack
When determining whether an actor is closed, we disregard the prelude, because
every actor is compiled with the prelude.
This breaks with shadowing.
This function compiles the prelude, just to find out the bound names.
*)
and find_prelude_names env =
  (* Create a throw-away environment *)
  let env0 = E.mk_global (E.mode env) None (E.get_prelude env) (fun _ _ -> G.i Unreachable) 0l in
  RTS.system_imports env0;
  let env1 = E.mk_fun_env env0 0l 0 in
  let ae = ASEnv.empty_ae in
  let (env2, _) = compile_prelude env1 ae in
  ASEnv.in_scope_set env2


and compile_start_func mod_env (progs : Ir.prog list) : E.func_with_names =
  let find_last_expr ds e =
    if ds = [] then [], e.it else
    match Lib.List.split_last ds, e.it with
    | (ds1', {it = LetD ({it = VarP i1; _}, e'); _}), TupE [] ->
      ds1', e'.it
    | (ds1', {it = LetD ({it = VarP i1; _}, e'); _}), VarE i2 when i1 = i2 ->
      ds1', e'.it
    | _ -> ds, e.it in

  let find_last_actor (ds,e) = match find_last_expr ds e with
    | ds1, ActorE (i, ds2, fs, _) ->
      Some (i, ds1 @ ds2, fs)
    | ds1, FuncE (_name, _cc, [], [], _, {it = ActorE (i, ds2, fs, _);_}) ->
      Some (i, ds1 @ ds2, fs)
    | _, _ ->
      None
  in

  Func.of_body mod_env [] [] (fun env ->
    let rec go ae = function
      | [] -> G.nop
      (* If the last program ends with an actor, then consider this the current actor  *)
      | [(prog, _flavor)] ->
        begin match find_last_actor prog with
        | Some (i, ds, fs) -> main_actor env ae i ds fs
        | None ->
          let (_ae, code) = compile_prog env ae prog in
          code
        end
      | ((prog, _flavor) :: progs) ->
        let (ae1, code1) = compile_prog env ae prog in
        let code2 = go ae1 progs in
        code1 ^^ code2 in
    go ASEnv.empty_ae progs
    )

and export_actor_field env  ae (f : Ir.field) =
  let sr, code = Var.get_val env ae f.it.var.it in
  (* A public actor field is guaranteed to be compiled as a StaticMessage *)
  let fi = match sr with
    | SR.StaticThing (SR.StaticMessage fi) -> fi
    | _ -> assert false in
  (* There should be no code associated with this *)
  assert (G.is_nop code);
  let _, _, _, ts, _ = Type.as_func f.note in
  E.add_dfinity_type env (fi,
    List.map (
      fun t -> Serialization.dfinity_type (Type.as_serialized t)
    ) ts
  );
  E.add_export env (nr {
    name = Wasm.Utf8.decode f.it.name;
    edesc = nr (FuncExport (nr fi))
  })

(* Local actor *)
and actor_lit outer_env this ds fs at =
  let wasm_binary =
    let mod_env = E.mk_global
      (E.mode outer_env)
      (E.get_rts outer_env)
      (E.get_prelude outer_env)
      (E.get_trap_with outer_env)
      ClosureTable.table_end in

    if E.mode mod_env = DfinityMode then Dfinity.system_imports mod_env;
    RTS.system_imports mod_env;
    RTS.system_exports mod_env;

    let start_fun = Func.of_body mod_env [] [] (fun env -> G.with_region at @@
      let ae0 = ASEnv.empty_ae in

      (* Compile the prelude *)
      let (ae1, prelude_code) = compile_prelude env ae0 in

      (* Add this pointer *)
      let ae2 = ASEnv.add_local_deferred ae1 this.it SR.Vanilla Dfinity.get_self_reference false in

      (* Compile the declarations *)
      let (ae3, decls_code) = compile_decs env ae2 AllocHow.TopLvl ds Freevars.S.empty in

      (* Export the public functions *)
      List.iter (export_actor_field env ae3) fs;

      prelude_code ^^ decls_code) in
    let start_fi = E.add_fun mod_env "start" start_fun in

    OrthogonalPersistence.register mod_env start_fi;

    let m = conclude_module mod_env this.it None in
    let (_map, wasm_binary) = CustomModuleEncode.encode m in
    wasm_binary in

    Dfinity.compile_databuf_of_bytes outer_env wasm_binary ^^
    (* Create actorref *)
    Dfinity.system_call outer_env "module_new" ^^
    Dfinity.system_call outer_env "actor_new"

(* Main actor: Just return the initialization code, and export functions as needed *)
and main_actor env ae1 this ds fs =
  (* Add this pointer *)
  let ae2 = ASEnv.add_local_deferred ae1 this.it SR.Vanilla Dfinity.get_self_reference false in

  (* Compile the declarations *)
  let (ae3, decls_code) = compile_decs env ae2 AllocHow.TopLvl ds Freevars.S.empty in

  (* Export the public functions *)
  List.iter (export_actor_field env ae3) fs;

  decls_code

and actor_fake_object_idx env name =
    Dfinity.compile_databuf_of_bytes env name ^^
    Dfinity.system_call env "actor_export"

and conclude_module env module_name start_fi_o =

  (* Wrap the start function with the RTS initialization *)
  let rts_start_fi = E.add_fun env "rts_start" (Func.of_body env [] [] (fun env1 ->
    Heap.get_heap_base ^^
    Heap.set_heap_ptr ^^
    match start_fi_o with
    | Some fi -> G.i (Call fi)
    | None -> G.nop
  )) in

  Dfinity.default_exports env;
  GC.register env (E.get_end_of_static_memory env);

  let func_imports = E.get_func_imports env in
  let ni = List.length func_imports in
  let ni' = Int32.of_int ni in

  let other_imports = E.get_other_imports env in

  let funcs = E.get_funcs env in
  let nf = List.length funcs in
  let nf' = Wasm.I32.of_int_u nf in

  let table_sz = Int32.add nf' ni' in

  let memories = [nr {mtype = MemoryType {min = E.mem_size env; max = None}} ] in

  (* We want to put all persistent globals first:
     The index in the persist annotation refers to the index in the
     list of *exported* globals, not all globals (at least with v8) *)
  let globals = [
      (* persistent databuf for memory *)
      nr { gtype = GlobalType (I32Type, Mutable);
        value = nr (G.to_instr_list compile_unboxed_zero)
      };
      (* persistent elembuf for references *)
      nr { gtype = GlobalType (I32Type, Mutable);
        value = nr (G.to_instr_list compile_unboxed_zero)
      };
      (* stack pointer *)
      nr { gtype = GlobalType (I32Type, Mutable);
        value = nr (G.to_instr_list (compile_unboxed_const Stack.end_of_stack))
      };
      (* beginning-of-heap pointer, may be changed by linker *)
      nr { gtype = GlobalType (I32Type, Immutable);
        value = nr (G.to_instr_list (compile_unboxed_const (E.get_end_of_static_memory env)))
      };
      (* end-of-heap pointer, initialized to __heap_base upon start *)
      nr { gtype = GlobalType (I32Type, Mutable);
        value = nr (G.to_instr_list (compile_unboxed_const 0xDEAFBEEFl))
      };
      (* reference counter *)
      nr { gtype = GlobalType (I32Type, Mutable);
        value = nr (G.to_instr_list compile_unboxed_zero)
      };
      ] in
  E.add_export env (nr {
    name = Wasm.Utf8.decode "__stack_pointer";
    edesc = nr (GlobalExport (nr Stack.stack_global))
  });
  E.add_export env (nr {
    name = Wasm.Utf8.decode "__heap_base";
    edesc = nr (GlobalExport (nr Heap.base_global))
  });

  let data = List.map (fun (offset, init) -> nr {
    index = nr 0l;
    offset = nr (G.to_instr_list (compile_unboxed_const offset));
    init;
    }) (E.get_static_memory env) in

  let module_ = {
      types = List.map nr (E.get_types env);
      funcs = List.map (fun (f,_,_) -> f) funcs;
      tables = [ nr { ttype = TableType ({min = table_sz; max = Some table_sz}, FuncRefType) } ];
      elems = [ nr {
        index = nr 0l;
        offset = nr (G.to_instr_list (compile_unboxed_const ni'));
        init = List.mapi (fun i _ -> nr (Wasm.I32.of_int_u (ni + i))) funcs } ];
      start = Some (nr rts_start_fi);
      globals = globals;
      memories = memories;
      imports = func_imports @ other_imports;
      exports = E.get_exports env;
      data
    } in

  let emodule =
    let open CustomModule in
    { module_;
      dylink = None;
      name = {
        module_ = Some module_name;
        function_names =
            List.mapi (fun i (f,n,_) -> Int32.(add ni' (of_int i), n)) funcs;
        locals_names =
            List.mapi (fun i (f,_,ln) -> Int32.(add ni' (of_int i), ln)) funcs;
      };
      types = E.get_dfinity_types env;
      persist =
             [ (OrthogonalPersistence.mem_global, CustomModule.DataBuf)
             ; (OrthogonalPersistence.elem_global, CustomModule.ElemBuf)
             ];
    } in

  match E.get_rts env with
  | None -> emodule
  | Some rts -> LinkModule.link emodule "rts" rts

let compile mode module_name rts (prelude : Ir.prog) (progs : Ir.prog list) : CustomModule.extended_module =
  let env = E.mk_global mode rts prelude Dfinity.trap_with ClosureTable.table_end in

  if E.mode env = DfinityMode then Dfinity.system_imports env;
  RTS.system_imports env;
  RTS.system_exports env;

  let start_fun = compile_start_func env (prelude :: progs) in
  let start_fi = E.add_fun env "start" start_fun in
  let start_fi_o =
    if E.mode env = DfinityMode
    then begin
      OrthogonalPersistence.register env start_fi;
      Dfinity.export_start_stub env;
      None
    end else Some (nr start_fi) in

  conclude_module env module_name start_fi_o
