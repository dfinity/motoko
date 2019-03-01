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
open Ir
open CustomModule
(* Re-shadow Source.(@@), to get Pervasives.(@@) *)
let (@@) = Pervasives.(@@)

module G = InstrList
let (^^) = G.(^^) (* is this how we import a single operator from a module that we otherwise use qualified? *)

(* WebAssembly pages are 64kb. *)
let page_size = Int32.of_int (64*1024)

(* Helper functions to produce annotated terms (Wasm.AST) *)
let nr x = { Wasm.Source.it = x; Wasm.Source.at = Wasm.Source.no_region }
(* Dito, for the Source AST  *)
let nr_ x = { it = x; at = no_region; note = () }

let todo fn se x = Printf.eprintf "%s: %s" fn (Wasm.Sexpr.to_string 80 se); x

module SR = struct
  (* This goes with the StackRep module, but we need the types earlier *)

  (* Statically known values: They are not put on the stack, but the
     “stack representation“ carries the static information.
  *)
  type static_thing =
    | StaticFun of int32

  (* Value representation on the stack:

     Compiling an expression means putting its value on the stack. But
     there are various ways of putting a value onto the stack -- unboxed,
     tupled etc.
   *)
  type t =
    | Vanilla
    | UnboxedTuple of int
    | UnboxedInt64
    | UnboxedInt32 (*UnboxedWord32?*)
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
    Example: whether we are compiling with --dfinity; the prelude code

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

(* A type to record where ActorScript names are stored. *)
type 'env varloc =
  (* A Wasm Local of the current function, directly containing the value
     (note that most values are pointers, but not all)
     Used for immutable and mutable, non-captured data *)
  | Local of int32
  (* A Wasm Local of the current function, that points to memory location,
     with an offset (in words) to value.
     Used for mutable captured data *)
  | HeapInd of (int32 * int32)
  (* A static memory location in the current module *)
  | Static of int32
  (* Dynamic code to allocate the expression, valid in the current module
     (need not be captured) *)
  | Deferred of 'env deferred_loc

(* Most names are stored in heap locations or in locals.
   But some are special (static funcions, the current actor, static messages of
   the current actor). These have no real location (yet), but we still need to
   produce a value on demand:
 *)
and 'env deferred_loc =
  { materialize : 'env -> (SR.t * G.t)
  ; materialize_vanilla : 'env -> G.t
  }

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
    prelude : prog; (* The prelude. Re-used when compiling actors *)

    (* Immutable *)
    local_vars_env : t varloc NameEnv.t; (* variables ↦ their location *)

    (* Mutable *)
    func_types : func_type list ref;
    func_imports : import list ref;
    other_imports : import list ref;
    exports : export list ref;
    dfinity_types : (int32 * CustomSections.type_ list) list ref; (* Dfinity types of exports *)
    funcs : (func * string * local_names) Lib.Promise.t list ref;
    built_in_funcs : lazy_built_in NameEnv.t ref;
    end_of_static_memory : int32 ref; (* End of statically allocated memory *)
    static_memory : (int32 * string) list ref; (* Content of static memory *)
    static_memory_frozen : bool ref;
      (* Sanity check: Nothing should bump end_of_static_memory once it has been read *)

    (* Local fields (only valid/used inside a function) *)
    (* Static *)
    n_param : int32; (* Number of parameters (to calculate indices of locals) *)
    n_res : int; (* Number of return values (for type of Return) *)

    (* Immutable *)
    ld : G.depth NameEnv.t; (* jump label ↦ their depth *)

    (* Mutable *)
    locals : value_type list ref; (* Types of locals *)
    local_names : (int32 * string) list ref; (* Names of locals *)
  }

  (* The initial global environment *)
  let mk_global mode prelude dyn_mem : t = {
    mode;
    prelude;
    local_vars_env = NameEnv.empty;
    func_types = ref [];
    func_imports = ref [];
    other_imports = ref [];
    exports = ref [];
    dfinity_types = ref [];
    funcs = ref [];
    built_in_funcs = ref NameEnv.empty;
    end_of_static_memory = ref dyn_mem;
    static_memory = ref [];
    static_memory_frozen = ref false;
    (* Actually unused outside mk_fun_env: *)
    n_param = 0l;
    n_res = 0;
    ld = NameEnv.empty;
    locals = ref [];
    local_names = ref [];
  }

  (* Creating a local environment, resetting the local fields,
     and removing bindings for local variables (unless they are at global locations)
  *)
  let is_non_local : 'env varloc -> bool = function
    | Local _ -> false
    | HeapInd _ -> false
    | Static _ -> true
    | Deferred _ -> true
  let mk_fun_env env n_param n_res =
    { env with
      n_param;
      n_res;
      ld = NameEnv.empty;
      locals = ref [];
      local_names = ref [];
      (* We keep all local vars that are bound to known functions or globals *)
      local_vars_env = NameEnv.filter (fun _ -> is_non_local) env.local_vars_env;
      }

  (* We avoid accessing the fields of t directly from outside of E, so here are a
     bunch of accessors. *)

  let mode (env : t) = env.mode

  let lookup_var env var =
    match NameEnv.find_opt var env.local_vars_env with
      | Some l -> Some l
      | None   -> Printf.eprintf "Could not find %s\n" var; None

  let _needs_capture env var = match lookup_var env var with
    | Some l -> not (is_non_local l)
    | None -> false

  let add_anon_local (env : t) ty =
      let i = reg env.locals ty in
      Wasm.I32.add env.n_param i

  let add_local_name (env : t) li name =
      let _ = reg env.local_names (li, name) in ()

  let reuse_local_with_offset (env : t) name i off =
      { env with local_vars_env = NameEnv.add name (HeapInd (i, off)) env.local_vars_env }

  let add_local_with_offset (env : t) name off =
      let i = add_anon_local env I32Type in
      add_local_name env i name;
      (reuse_local_with_offset env name i off, i)

  let add_local_static (env : t) name ptr =
      { env with local_vars_env = NameEnv.add name (Static ptr) env.local_vars_env }

  let add_local_deferred (env : t) name d =
      { env with local_vars_env = NameEnv.add name (Deferred d) env.local_vars_env }

  let add_local_deferred_vanilla (env : t) name materialize =
      let d = {
        materialize = (fun env -> (SR.Vanilla, materialize env));
        materialize_vanilla = (fun env -> materialize env) } in
      { env with local_vars_env = NameEnv.add name (Deferred d) env.local_vars_env }

  let add_direct_local (env : t) name =
      let i = add_anon_local env I32Type in
      add_local_name env i name;
      ({ env with local_vars_env = NameEnv.add name (Local i) env.local_vars_env }, i)

  let get_locals (env : t) = !(env.locals)
  let get_local_names (env : t) : (int32 * string) list = !(env.local_names)

  let in_scope_set (env : t) =
    let l = env.local_vars_env in
    NameEnv.fold (fun k _ -> Freevars.S.add k) l Freevars.S.empty

  let add_func_import (env : t) f =
    if !(env.funcs) = []
    then reg env.func_imports f
    else assert false (* "add all imports before all functions!" *)

  let _add_other_import (env : t) m =
    let _ = reg env.other_imports m in ()

  let add_export (env : t) e = let _ = reg env.exports e in ()

  let add_dfinity_type (env : t) e = let _ = reg env.dfinity_types e in ()

  let reserve_fun (env : t) name =
    let (j, fill) = reserve_promise env.funcs name in
    let n = Wasm.I32.of_int_u (List.length !(env.func_imports)) in
    let fi = Int32.add j n in
    let fill_ (f, local_names) = fill (f, name, local_names) in
    (fi, fill_)

  let add_fun (env : t) (f, local_names) name =
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

  let add_label (env : t) name (d : G.depth) =
      { env with ld = NameEnv.add name.it d env.ld }

  let get_label_depth (env : t) name : G.depth  =
    match NameEnv.find_opt name.it env.ld with
      | Some d -> d
      | None   -> Printf.eprintf "Could not find %s\n" name.it; raise Not_found

  let get_prelude (env : t) = env.prelude

  let reserve_static_memory (env : t) size : int32 =
    if !(env.static_memory_frozen) then assert false (* "Static memory frozen" *);
    let ptr = !(env.end_of_static_memory) in
    let aligned = Int32.logand (Int32.add size 3l) (Int32.lognot 3l) in
    env.end_of_static_memory := Int32.add ptr aligned;
    ptr

  let add_static_bytes (env : t) data : int32 =
    let ptr = reserve_static_memory env (Int32.of_int (String.length data)) in
    env.static_memory := !(env.static_memory) @ [ (ptr, data) ];
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
(*let compile_unboxed_one = compile_unboxed_const 1l LATER*)

(* Some common arithmetic, used for pointer and index arithmetic *)
let compile_op_const op i =
    compile_unboxed_const i ^^
    G.i (Binary (Wasm.Values.I32 op))
let compile_add_const = compile_op_const I32Op.Add
let compile_sub_const = compile_op_const I32Op.Sub
let compile_mul_const = compile_op_const I32Op.Mul
let compile_divU_const = compile_op_const I32Op.DivU

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

let _new_local64 env name =
  let (set_i, get_i, _) = new_local_ env I64Type name
  in (set_i, get_i)

(* Some common code macros *)

(* expects a number on the stack. Iterates from zero t below that number *)
let compile_while cond body =
    G.loop_ (ValBlockType None) (
      cond ^^ G.if_ (ValBlockType None) (body ^^ G.i (Br (nr 1l))) G.nop
    )

let from_0_to_n env mk_body =
    let (set_n, get_n) = new_local env "n" in
    let (set_i, get_i) = new_local env "i" in
    set_n ^^
    compile_unboxed_const 0l ^^
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

let load_ptr : G.t =
  G.i (Load {ty = I32Type; align = 2; offset = 0l; sz = None})

let store_ptr : G.t =
  G.i (Store {ty = I32Type; align = 2; offset = 0l; sz = None})

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

end (* Func *)

module Heap = struct
  (* General heap object functionalty (allocation, setting fields, reading fields) *)

  (* Memory addresses are 32 bit (I32Type). *)
  let word_size = 4l

  (* We keep track of the end of the used heap in this global, and bump it if
     we allocate stuff.  *)
  let heap_global = 2l
  let get_heap_ptr = G.i (GlobalGet (nr heap_global))
  let set_heap_ptr = G.i (GlobalSet (nr heap_global))

  (* Page allocation. Ensures that the memory up to the heap pointer is allocated. *)
  let grow_memory env =
    Func.share_code env "grow_memory" [] [] (fun env ->
      let (set_pages_needed, get_pages_needed) = new_local env "pages_needed" in
      get_heap_ptr ^^ compile_divU_const page_size ^^
      compile_add_const 1l ^^
      G.i MemorySize ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
      set_pages_needed ^^

      (* Check that the new heap pointer is within the memory *)
      get_pages_needed ^^
      compile_unboxed_const 0l ^^
      G.i (Compare (Wasm.Values.I32 I32Op.GtU)) ^^
      G.if_ (ValBlockType None)
        ( get_pages_needed ^^
          G.i MemoryGrow ^^
          (* Check result *)
          compile_unboxed_const 0l ^^
          G.i (Compare (Wasm.Values.I32 I32Op.LtS)) ^^
          G.if_ (ValBlockType None) (G.i Unreachable) G.nop
        ) G.nop
      )

  (* Dynamic allocation *)
  let dyn_alloc_words env =
    Func.share_code env "alloc_words" ["n", I32Type] [I32Type] (fun env ->
      let get_n = G.i (LocalGet (nr 0l)) in

      (* expect the size (in words), returns the pointer *)
      get_heap_ptr ^^

      (* Update heap pointer *)
      get_heap_ptr ^^
      get_n ^^ compile_mul_const word_size ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      set_heap_ptr ^^
      grow_memory env
    )

  let dyn_alloc_bytes env =
    Func.share_code env "alloc_bytes" ["n", I32Type] [I32Type] (fun env ->
      let get_n = G.i (LocalGet (nr 0l)) in

      get_n ^^
      (* Round up to next multiple of the word size and convert to words *)
      compile_add_const 3l ^^
      compile_divU_const word_size ^^
      dyn_alloc_words env
    )

  (* Static allocation (always words)
     (uses dynamic allocation for smaller and more readable code *)
  let alloc env (n : int32) : G.t =
    compile_unboxed_const n  ^^
    dyn_alloc_words env

  (* Heap objects *)

  (* At this level of abstactions, heap objects are just flat arrays of words *)

  let load_field (i : int32) : G.t =
    G.i (Load {ty = I32Type; align = 2; offset = Wasm.I32.mul word_size i; sz = None})

  let store_field (i : int32) : G.t =
    G.i (Store {ty = I32Type; align = 2; offset = Wasm.I32.mul word_size i; sz = None})

  (* Although we occationally want to treat to of them as a 64 bit number *)

  let load_field64 (i : int32) : G.t =
    G.i (Load {ty = I64Type; align = 2; offset = Wasm.I32.mul word_size i; sz = None})

  let store_field64 (i : int32) : G.t =
    G.i (Store {ty = I64Type; align = 2; offset = Wasm.I32.mul word_size i; sz = None})

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
  let memcpy env =
    Func.share_code env "memcpy" ["from", I32Type; "two", I32Type; "n", I32Type] [] (fun env ->
      let get_from = G.i (LocalGet (nr 0l)) in
      let get_to = G.i (LocalGet (nr 1l)) in
      let get_n = G.i (LocalGet (nr 2l)) in
      get_n ^^
      from_0_to_n env (fun get_i ->
          get_to ^^
          get_i ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^

          get_from ^^
          get_i ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^

          G.i (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Memory.Pack8})
      )
    )

end (* Heap *)

module ElemHeap = struct
  (* The ElemHeap adds a level of indirection for references (elements, as in
     ElemRef). This way, the fake orthogonal persistence code can easily
     store all references an elembuf.

     This could be done differently (e.g. traversing the heap and looking for tagged references), but
     it predates the heap traversal code, and the whole thing goes away once we
     target orthogonal persistence anyways.
  *)

  let ref_counter_global = 3l
  let get_ref_ctr = G.i (GlobalGet (nr ref_counter_global))
  let set_ref_ctr = G.i (GlobalSet (nr ref_counter_global))

  (* For now, we allocate a fixed size range. This obviously cannot stay. *)
  let max_references = 1024l

  (* By placing the ElemHeap at memory location 0, we incidentally make sure that
     the 0l pointer is never a valid pointer.
  *)
  let ref_location = 0l

  let table_end : int32 = Int32.(add ref_location (mul max_references Heap.word_size))

  (* Assumes a reference on the stack, and replaces it with an index into the
     reference table *)
  let remember_reference env : G.t =
    Func.share_code env "remember_reference" ["ref", I32Type] [I32Type] (fun env ->
      let get_ref = G.i (LocalGet (nr 0l)) in

      (* Return index *)
      get_ref_ctr ^^

      (* Store reference *)
      get_ref_ctr ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const ref_location ^^
      get_ref ^^
      store_ptr ^^

      (* Bump counter *)
      get_ref_ctr ^^
      compile_add_const 1l ^^
      set_ref_ctr
    )

  (* Assumes a index into the table on the stack, and replaces it with the reference *)
  let recall_reference env : G.t =
    Func.share_code env "recall_reference" ["ref_idx", I32Type] [I32Type] (fun env ->
      let get_ref_idx = G.i (LocalGet (nr 0l)) in
      get_ref_idx ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const ref_location ^^
      load_ptr
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
  let get_counter = compile_unboxed_const loc ^^ load_ptr

  (* Assumes a reference on the stack, and replaces it with an index into the
     reference table *)
  let remember_closure env : G.t =
    Func.share_code env "remember_closure" ["ptr", I32Type] [I32Type] (fun env ->
      let get_ptr = G.i (LocalGet (nr 0l)) in

      (* Return index *)
      get_counter ^^
      compile_add_const 1l ^^

      (* Store reference *)
      get_counter ^^
      compile_add_const 1l ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const loc ^^
      get_ptr ^^
      store_ptr ^^

      (* Bump counter *)
      compile_unboxed_const loc ^^
      get_counter ^^
      compile_add_const 1l ^^
      store_ptr
    )

  (* Assumes a index into the table on the stack, and replaces it with a ptr to the closure *)
  let recall_closure env : G.t =
    Func.share_code env "recall_closure" ["closure_idx", I32Type] [I32Type] (fun env ->
      let get_closure_idx = G.i (LocalGet (nr 0l)) in
      get_closure_idx ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const loc ^^
      load_ptr
    )

end (* ClosureTable *)

module Bool = struct
  (* Boolean literals are either 0 or 1
     The 1 is recognized as a unboxed scalar anyways,
     while the 0 is special (see below).
     This allows us to use the result of the WebAssembly comparison operators
     directly, and to use the booleans directly with WebAssembly’s If.
  *)
  let lit = function
    | false -> compile_unboxed_const 0l
    | true -> compile_unboxed_const 1l

end (* Bool *)


module BitTagged = struct
  (* This module takes care of pointer tagging: Pointer are always aligned, so they
     have their LSB bit unset. We use that and store an unboxed scalar x
     as (x << 1 | 1).
     Special case: The zero pointer is considered a scalar.
  *)
  let if_unboxed env retty is1 is2 =
    Func.share_code env "is_unboxed" ["x", I32Type] [I32Type] (fun env ->
      let get_x = G.i (LocalGet (nr 0l)) in
      (* Get bit *)
      get_x ^^
      compile_unboxed_const 1l ^^
      G.i (Binary (Wasm.Values.I32 I32Op.And)) ^^
      (* Check bit *)
      compile_unboxed_const 1l ^^
      G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
      G.if_ (ValBlockType None)
        (Bool.lit true ^^ G.i Return) G.nop ^^
      (* Also check if it is the null-pointer *)
      get_x ^^
      compile_unboxed_const 0l ^^
      G.i (Compare (Wasm.Values.I32 I32Op.Eq))
    ) ^^
    G.if_ retty is1 is2

  (* The untag_scalar and tag functions expect 64 bit numbers *)
  let untag_scalar env =
    compile_unboxed_const 1l ^^
    G.i (Binary (Wasm.Values.I32 I32Op.ShrU)) ^^
    G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32))

  let tag =
    G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
    compile_unboxed_const 1l ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Shl)) ^^
    compile_unboxed_const 1l ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Or))

  (* The untag_small and tag_small functions expect 32 bit numbers *)
  let untag_small env =
    compile_unboxed_const 1l ^^
    G.i (Binary (Wasm.Values.I32 I32Op.ShrU))

  let tag_small =
    compile_unboxed_const 1l ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Shl)) ^^
    compile_unboxed_const 1l ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Or))

end (* BitTagged *)

module Tagged = struct
  (* Tagged objects have, well, a tag to describe their runtime type.
     This tag is used to traverse the heap (serialization, GC), but also
     for objectification of arrays.

     The tag is a word at the beginning of the object.

     All tagged heap objects have a size of at least two words
     (important for GC, which replaces them with an Indirection).
   *)

  type tag =
    | Object
    | ObjInd (* The indirection used for object fields *)
    | Array (* Also a tuple *)
    | Reference (* Either arrayref or funcref, no need to distinguish here *)
    | Int (* Contains a 64 bit number *)
    | MutBox (* used for local variables *)
    | Closure
    | Some (* For opt *)
    | Text
    | Indirection
    | SmallInt (* Word? Contains a 32 bit unsigned number *)

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
    | Text -> 9l
    | Indirection -> 10l
    | SmallInt -> 11l

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
        compile_unboxed_const (int_of_tag tag) ^^
        G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
        G.if_ retty code (go cases)
    in
    load ^^
    set_tag ^^
    go cases

  let branch env retty (cases : (tag * G.t) list) : G.t =
    branch_default env retty (G.i Unreachable) cases

  let obj env tag element_instructions : G.t =
    Heap.obj env @@
      compile_unboxed_const (int_of_tag tag) ::
      element_instructions

end (* Tagged *)


module Var = struct
  (* This module is all about looking up ActorScript variables in the environment,
     and dealing with mutable variables *)

  (* When accessing a variable that is a static function, then we need to create a
     heap-allocated closure-like thing on the fly. *)
  let static_fun_pointer env fi =
    Tagged.obj env Tagged.Closure [
      compile_unboxed_const fi;
      compile_unboxed_const 0l (* number of parameters: none *)
    ]

  (* Local variables may in general be mutable (or at least late-defined).
     So we need to add an indirection through the heap.
     We tag this indirection using Tagged.MutBox.
  *)
  let mutbox_field = Tagged.header_size
  let load = Heap.load_field mutbox_field
  let store = Heap.store_field mutbox_field

  let add_local env name =
    E.add_local_with_offset env name mutbox_field

  (* Stores the payload (which is found on the stack) *)
  let set_val env var = match E.lookup_var env var with
    | Some (Local i) ->
      G.i (LocalSet (nr i))
    | Some (HeapInd (i, off)) ->
      let (set_new_val, get_new_val) = new_local env "new_val" in
      set_new_val ^^
      G.i (LocalGet (nr i)) ^^
      get_new_val ^^
      Heap.store_field off
    | Some (Static i) ->
      let (set_new_val, get_new_val) = new_local env "new_val" in
      set_new_val ^^
      compile_unboxed_const i ^^
      get_new_val ^^
      store_ptr
    | Some (Deferred d) -> G.i Unreachable
    | None   -> G.i Unreachable

  (* Returns the payload (vanialla representation) *)
  let get_val_vanilla env var = match E.lookup_var env var with
    | Some (Local i) -> G.i (LocalGet (nr i))
    | Some (HeapInd (i, off)) -> G.i (LocalGet (nr i)) ^^ Heap.load_field off
    | Some (Static i) -> compile_unboxed_const i ^^ load_ptr
    | Some (Deferred d) -> d.materialize_vanilla env
    | None -> G.i Unreachable

  (* Returns the payload (optimized representation) *)
  let get_val env var = match E.lookup_var env var with
    | Some (Deferred d) -> d.materialize env
    | _ -> SR.Vanilla, get_val_vanilla env var

  (* Returns the value to put in the closure,
     and code to restore it, including adding to the environment
     This currently reserves an unused word in the closure even for static stuff,
     could be improved at some point.
  *)
  let capture env var : G.t * (E.t -> (E.t * G.t)) = match E.lookup_var env var with
    | Some (Local i) ->
      ( G.i (LocalGet (nr i))
      , fun env1 ->
        let (env2, j) = E.add_direct_local env1 var in
        let restore_code = G.i (LocalSet (nr j))
        in (env2, restore_code)
      )
    | Some (HeapInd (i, off)) ->
      ( G.i (LocalGet (nr i))
      , fun env1 ->
        let (env2, j) = E.add_local_with_offset env1 var off in
        let restore_code = G.i (LocalSet (nr j))
        in (env2, restore_code)
      )
    | Some (Static i) ->
      ( compile_unboxed_zero, fun env1 -> (E.add_local_static env1 var i, G.i Drop))
    | Some (Deferred d) ->
      ( compile_unboxed_zero, fun env1 -> (E.add_local_deferred env1 var d, G.i Drop))
    | None -> (G.i Unreachable, fun env1 -> (env1, G.i Unreachable))

  (* Returns a pointer to a heap allocated box for this.
     (either a mutbox, if already mutable, or a freshly allocated box)
  *)
  let field_box env code =
    Tagged.obj env Tagged.ObjInd [ code ]

  let get_val_ptr env var = match E.lookup_var env var with
    | Some (HeapInd (i, 1l)) -> G.i (LocalGet (nr i))
    | _  -> field_box env (get_val_vanilla env var)

end (* Var *)

module Opt = struct
  (* The Option type. Not much intereting to see here *)

  let payload_field = Tagged.header_size

  (* This needs to be disjoint from all pointers, i.e. tagged as a scalar. *)
  let null = compile_unboxed_const 3l

  let inject env e = Tagged.obj env Tagged.Some [e]
  let project = Heap.load_field Tagged.header_size

end (* Opt *)

(* This is a bit oddly placed, but needed by module Closure *)
module AllocHow = struct

  (*
  When compiling a (recursive) block, we need to do a dependency analysis, to
  find out which names need to be heap-allocated, which local-allocated and which
  are simply static functions. The rules are:
  - functions are static, unless they capture something that is not a static function
  - everything that is captured before it is defined needs to be heap-allocated,
    unless it is a static function
  - everything that is mutable and captured needs to be heap-allocated
  - the rest can be local

  Immutable things are always pointers or unboxed scalars, and can be put into
  closures as such.

  We represent this as a lattice as follows:
  *)

  module M = Freevars.M
  module S = Freevars.S

  type nonStatic = LocalImmut | LocalMut | StoreHeap
  type allocHow = nonStatic M.t (* absent means static *)

  let join : allocHow -> allocHow -> allocHow =
    M.union (fun _ x y -> Some (match x, y with
      | _, StoreHeap -> StoreHeap
      | StoreHeap, _  -> StoreHeap
      | LocalMut, _ -> LocalMut
      | _, LocalMut -> LocalMut
      | LocalImmut, LocalImmut -> LocalImmut
    ))

  (* We need to do a fixed-point analysis, starting with everything being static.
  *)

  let map_of_set x s = S.fold (fun v m -> M.add v x m) s M.empty
  let set_of_map m = M.fold (fun v _ m -> S.add v m) m S.empty

  let is_static env how f =
    (* Does this capture nothing from outside? *)
    (S.is_empty (S.inter
      (Freevars.captured_vars f)
      (set_of_map (M.filter (fun _ x -> not (E.is_non_local x)) (env.E.local_vars_env))))) &&
    (* Does this capture nothing non-static from here? *)
    (S.is_empty (S.inter
      (Freevars.captured_vars f)
      (set_of_map how)))

  let is_static_exp env how0 exp = match exp.it with
    | BlockE [{ it = FuncD _; _} as dec] ->
      let f = Freevars.close (Freevars.dec dec) in
      is_static env how0 f
    | _ -> false

  let dec env (seen, how0) dec =
    let (f,d) = Freevars.dec dec in

    (* What allocation is required for the things defined here? *)
    let how1 = match dec.it with
      (* Mutable variables are, well, mutable *)
      | VarD _ ->
      map_of_set LocalMut d
      (* Messages cannot be static *)
      | FuncD (cc, _, _, _, _, _) when cc.Value.sort = Type.Sharable ->
      map_of_set LocalImmut d
      (* Static functions *)
      | FuncD _ when is_static env how0 f ->
      M.empty
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

    (* Do we capture anything mutable?
       These also need to be heap-allocated.
    *)
    let how3 =
      map_of_set StoreHeap
        (S.inter
          (set_of_map (M.filter (fun _ h -> h = LocalMut) how0))
          (Freevars.captured_vars f)) in

    let how = List.fold_left join M.empty [how0; how1; how2; how3] in
    let seen' = S.union seen d
    in (seen', how)

  let decs env decs : allocHow =
    let step how = snd (List.fold_left (dec env) (S.empty, how) decs) in
    let rec go how =
      let how1 = step how in
      if M.equal (=) how how1 then how else go how1 in
    go M.empty


  (* Functions to extend the environment (and possibly allocate memory)
     based on how we want to store them. *)
  let add_how env name = function
    | Some LocalImmut | Some LocalMut ->
      let (env1, i) = E.add_direct_local env name in
      (env1, G.nop)
    | Some StoreHeap ->
      let (env1, i) = E.add_local_with_offset env name 1l in
      let alloc_code =
        Tagged.obj env Tagged.MutBox [ compile_unboxed_zero ] ^^
        G.i (LocalSet (nr i)) in
      (env1, alloc_code)
    | None -> (env, G.nop)

  let add_local env how name =
    add_how env name (M.find_opt name how)

end (* AllocHow *)


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


module BoxedInt = struct
  (* We store large nats and ints in immutable boxed 64bit heap objects.
     Eventually, this should contain the bigint implementation.

     Small values (just <2^5 for now, so that both code paths are well-tested)
     are stored unboxed, tagged, see BitTagged.
  *)

  let payload_field = Int32.add Tagged.header_size 0l

  let compile_box env compile_elem : G.t =
    let (set_i, get_i) = new_local env "boxed_int" in
    Heap.alloc env 3l ^^
    set_i ^^
    get_i ^^ Tagged.store Tagged.Int ^^
    get_i ^^ compile_elem ^^ Heap.store_field64 1l ^^
    get_i

  let box env = Func.share_code env "box_int" ["n", I64Type] [I32Type] (fun env ->
      let get_n = G.i (LocalGet (nr 0l)) in
      get_n ^^ compile_const_64 (Int64.of_int (1 lsl 5)) ^^
      G.i (Compare (Wasm.Values.I64 I64Op.LtU)) ^^
      G.if_ (ValBlockType (Some I32Type))
        (get_n ^^ BitTagged.tag)
        (compile_box env get_n)
    )

  let unbox env = Func.share_code env "unbox_int" ["n", I32Type] [I64Type] (fun env ->
      let get_n = G.i (LocalGet (nr 0l)) in
      get_n ^^
      BitTagged.if_unboxed env (ValBlockType (Some I64Type))
        ( get_n ^^ BitTagged.untag_scalar env)
        ( get_n ^^ Heap.load_field64 payload_field)
    )

  let lit env n = compile_const_64 n ^^ box env

end (* BoxedInt *)

module BoxedSmallInt = struct
  (* We store proper 32bit Word32 in immutable boxed 32bit heap objects.

     Small values (just <2^10 for now, so that both code paths are well-tested)
     are stored unboxed, tagged, see BitTagged.
  *)

  let payload_field = Int32.add Tagged.header_size 0l

  let compile_box env compile_elem : G.t =
    let (set_i, get_i) = new_local env "boxed_int" in
    Heap.alloc env 3l ^^
    set_i ^^
    get_i ^^ Tagged.store Tagged.SmallInt ^^
    get_i ^^ compile_elem ^^ Heap.store_field 1l ^^
    get_i

  let box env = Func.share_code env "box_int32" ["n", I32Type] [I32Type] (fun env ->
      let get_n = G.i (LocalGet (nr 0l)) in
      get_n ^^ compile_unboxed_const (Int32.of_int (1 lsl 10)) ^^
      G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
      G.if_ (ValBlockType (Some I32Type))
        (get_n ^^ BitTagged.tag_small)
        (compile_box env get_n)
    )

  let unbox env = Func.share_code env "unbox_int32" ["n", I32Type] [I32Type] (fun env ->
      let get_n = G.i (LocalGet (nr 0l)) in
      get_n ^^
      BitTagged.if_unboxed env (ValBlockType (Some I32Type))
        ( get_n ^^ BitTagged.untag_small env)
        ( get_n ^^ Heap.load_field payload_field)
    )

  (*let lit env n = compile_unboxed_const n ^^ box env*)

end (* BoxedSmallInt *)

(* Primitive functions *)
module Prim = struct

  let prim_abs env =
    let (set_i, get_i) = new_local env "abs_param" in
    set_i ^^
    get_i ^^
    BoxedInt.unbox env ^^
    compile_const_64 0L ^^
    G.i (Compare (Wasm.Values.I64 I64Op.LtS)) ^^
    G.if_ (ValBlockType (Some I32Type))
      ( compile_const_64 0L ^^
        get_i ^^
        BoxedInt.unbox env ^^
        G.i (Binary (Wasm.Values.I64 I64Op.Sub)) ^^
        BoxedInt.box env
      )
      ( get_i )

  let prim_nat32toWord32 env =
    G.i (Convert (Wasm.Values.I32 I32Op.WrapI64))
  let prim_word32toNat32 env =
    G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32))
  let prim_int32toWord32 env =
    G.i (Convert (Wasm.Values.I32 I32Op.WrapI64))
  let prim_word32toInt32 env =
    G.i (Convert (Wasm.Values.I64 I64Op.ExtendSI32))

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
  let hash_field_name ({it = Name s; _}) =
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
      List.map (fun ({it = Name s;_} as n,_) -> (hash_field_name n, s)) |>
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

     let hash_position env {it = Name n; _} =
         let i = FieldEnv.find n name_pos_map in
         Int32.add header_size (Int32.mul 2l i) in
     let field_position env {it = Name n; _} =
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
       mk_is env ^^
       Heap.store_field (field_position env name)
     in
     G.concat_map init_field fs ^^

     (* Return the pointer to the object *)
     get_ri

  (* Returns a pointer to the object field (without following the indirection) *)
  let idx_hash_raw env =
    Func.share_code env "obj_idx" ["x", I32Type; "hash", I32Type] [I32Type] (fun env ->
      let get_x = G.i (LocalGet (nr 0l)) in
      let get_hash = G.i (LocalGet (nr 1l)) in
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
    then Func.share_code env "obj_idx_ind" ["x", I32Type; "hash", I32Type] [I32Type] (fun env ->
      let get_x = G.i (LocalGet (nr 0l)) in
      let get_hash = G.i (LocalGet (nr 1l)) in
      get_x ^^ get_hash ^^
      idx_hash_raw env ^^
      load_ptr ^^ compile_add_const Heap.word_size
    )
    else idx_hash_raw env

  (* Determines whether the field is mutable (and thus needs an indirection) *)
  let is_mut_field env obj_type ({it = Name s; _}) =
    let _, fields = Type.as_obj_sub "" obj_type in
    let field_typ = Type.lookup_field s fields in
    let mut = Type.is_mut field_typ in
    mut

  let idx env obj_type name =
    compile_unboxed_const (hash_field_name name) ^^
    idx_hash env (is_mut_field env obj_type name)

  let load_idx env obj_type f =
    idx env obj_type f ^^
    load_ptr

  let load_idx_immut env name =
    compile_unboxed_const (hash_field_name name) ^^
    idx_hash env false ^^
    load_ptr

end (* Object *)

module Text = struct
  (* The layout of a text object is

     ┌─────┬─────────┬──────────────────┐
     │ tag │ n_bytes │ bytes (padded) … │
     └─────┴─────────┴──────────────────┘
  *)

  let header_size = Int32.add Tagged.header_size 1l

  let len_field = Int32.add Tagged.header_size 0l

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

  let lit env s =
    let tag = bytes_of_int32 (Tagged.int_of_tag Tagged.Text) in
    let len = bytes_of_int32 (Int32.of_int (String.length s)) in
    let data = tag ^ len ^ s in
    let ptr = E.add_static_bytes env data in
    compile_unboxed_const ptr

  (* String concatentation. Expects two strings on stack *)
  let concat env = Func.share_code env "concat" ["x", I32Type; "y", I32Type] [I32Type] (fun env ->
      let get_x = G.i (LocalGet (nr 0l)) in
      let get_y = G.i (LocalGet (nr 1l)) in
      let (set_z, get_z) = new_local env "z" in
      let (set_len1, get_len1) = new_local env "len1" in
      let (set_len2, get_len2) = new_local env "len2" in

      get_x ^^ Heap.load_field len_field ^^ set_len1 ^^
      get_y ^^ Heap.load_field len_field ^^ set_len2 ^^

      (* allocate memory *)
      compile_unboxed_const (Int32.mul Heap.word_size header_size) ^^
      get_len1 ^^
      get_len2 ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      Heap.dyn_alloc_bytes env ^^
      set_z ^^

      (* Set tag *)
      get_z ^^ Tagged.store Tagged.Text ^^

      (* Set length *)
      get_z ^^
      get_len1 ^^
      get_len2 ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      Heap.store_field len_field ^^

      (* Copy first string *)
      get_x ^^
      compile_add_const (Int32.mul Heap.word_size header_size) ^^

      get_z ^^
      compile_add_const (Int32.mul Heap.word_size header_size) ^^

      get_len1 ^^

      Heap.memcpy env ^^

      (* Copy second string *)
      get_y ^^
      compile_add_const (Int32.mul Heap.word_size header_size) ^^

      get_z ^^
      compile_add_const (Int32.mul Heap.word_size header_size) ^^
      get_len1 ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^

      get_len2 ^^

      Heap.memcpy env ^^

      (* Done *)
      get_z
    )

  (* String comparison. Expects two strings on stack *)
  let compare env = Func.share_code env "Text.compare" ["x", I32Type; "y", I32Type] [I32Type] (fun env ->
      let get_x = G.i (LocalGet (nr 0l)) in
      let get_y = G.i (LocalGet (nr 1l)) in
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
        compile_add_const (Int32.mul Heap.word_size header_size) ^^
        get_i ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^

        get_y ^^
        compile_add_const (Int32.mul Heap.word_size header_size) ^^
        get_i ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
        G.i (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^

        G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
        G.if_ (ValBlockType None) G.nop (Bool.lit false ^^ G.i Return)
      ) ^^
      Bool.lit true
  )

end (* String *)

module Array = struct
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
  let idx env = Func.share_code env "Array.idx" ["array", I32Type; "idx", I32Type] [I32Type] (fun env ->
      let get_array = G.i (LocalGet (nr 0l)) in
      let get_idx = G.i (LocalGet (nr 1l)) in

      (* No need to check the lower bound, we interpret is as unsigned *)
      (* Check the upper bound *)
      get_idx ^^
      get_array ^^
      Heap.load_field len_field ^^
      G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
      G.if_ (ValBlockType None) G.nop (G.i Unreachable) ^^

      get_idx ^^
      compile_add_const header_size ^^
      compile_mul_const element_size ^^
      get_array ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add))
    )

  let common_funcs env =
    let get_array_object = Closure.get ^^ Closure.load_data 0l in
    let get_first_arg = G.i (LocalGet (nr 1l)) in
    let get_second_arg = G.i (LocalGet (nr 2l)) in

    E.define_built_in env "array_get"
      (fun () -> Func.of_body env ["clos", I32Type; "idx", I32Type] [I32Type] (fun env1 ->
            get_array_object ^^
            get_first_arg ^^ (* the index *)
            BoxedInt.unbox env1 ^^
            G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
            idx env ^^
            load_ptr
       ));
    E.define_built_in env "array_set"
      (fun () -> Func.of_body env ["clos", I32Type; "idx", I32Type; "val", I32Type] [] (fun env1 ->
            get_array_object ^^
            get_first_arg ^^ (* the index *)
            BoxedInt.unbox env1 ^^
            G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
            idx env ^^
            get_second_arg ^^ (* the value *)
            store_ptr
       ));
    E.define_built_in env "array_len"
      (fun () -> Func.of_body env ["clos", I32Type] [I32Type] (fun env1 ->
            get_array_object ^^
            Heap.load_field len_field ^^
            G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
            BoxedInt.box env1
      ));

    let mk_next_fun mk_code : E.func_with_names = Func.of_body env ["clos", I32Type] [I32Type] (fun env1 ->
            let (set_boxed_i, get_boxed_i) = new_local env1 "boxed_n" in
            let (set_i, get_i) = new_local env1 "n" in
            (* Get pointer to counter from closure *)
            Closure.get ^^ Closure.load_data 0l ^^
            (* Get current counter (boxed) *)
            Var.load ^^
            set_boxed_i ^^

            (* Get current counter (unboxed) *)
            get_boxed_i ^^
            BoxedInt.unbox env1 ^^
            G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
            set_i ^^

            get_i ^^
            (* Get length *)
            Closure.get ^^ Closure.load_data 1l ^^ Heap.load_field len_field ^^
            G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
            G.if_ (ValBlockType (Some I32Type))
              (* Then *)
              Opt.null
              (* Else *)
              ( (* Get point to counter from closure *)
                Closure.get ^^ Closure.load_data 0l ^^
                (* Store increased counter *)
                get_i ^^
                compile_add_const 1l ^^
                G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
                BoxedInt.box env1 ^^
                Var.store ^^
                (* Return stuff *)
                Opt.inject env1 (
                  mk_code env (Closure.get ^^ Closure.load_data 1l) get_boxed_i get_i
                )
              )
       ) in
    let mk_iterator next_funid = Func.of_body env ["clos", I32Type] [I32Type] (fun env1 ->
            (* next function *)
            let (set_ni, get_ni) = new_local env1 "next" in
            Closure.fixed_closure env1 next_funid
              [ Tagged.obj env1 Tagged.MutBox [ BoxedInt.lit env1 0L ]
              ; get_array_object
              ] ^^
            set_ni ^^

            Object.lit_raw env1
              [ nr_ (Name "next"), fun _ -> get_ni ]
       ) in

    E.define_built_in env "array_keys_next"
      (fun () -> mk_next_fun (fun env1 get_array get_boxed_i get_i ->
              get_boxed_i
       ));
    E.define_built_in env "array_keys"
      (fun () -> mk_iterator (E.built_in env "array_keys_next"));

    E.define_built_in env "array_vals_next"
      (fun () -> mk_next_fun (fun env1 get_array get_boxed_i get_i ->
              get_array ^^
              get_i ^^
              idx env1 ^^
              load_ptr
      ));
    E.define_built_in env "array_vals"
      (fun () -> mk_iterator (E.built_in env "array_vals_next"))

  (* Compile an array literal. *)
  let lit env element_instructions =
    Tagged.obj env Tagged.Array
     ([ compile_unboxed_const (Wasm.I32.of_int_u (List.length element_instructions))
      ] @ element_instructions)

  let fake_object_idx_option env built_in_name =
    let (set_i, get_i) = new_local env "array" in
    set_i ^^
    Closure.fixed_closure env (E.built_in env built_in_name) [ get_i ]

  let fake_object_idx env = function
      | "get" -> Some (fake_object_idx_option env "array_get")
      | "set" -> Some (fake_object_idx_option env "array_set")
      | "len" -> Some (fake_object_idx_option env "array_len")
      | "keys" -> Some (fake_object_idx_option env "array_keys")
      | "vals" -> Some (fake_object_idx_option env "array_vals")
      | _ -> None

  (* The primitive operations *)
  (* No need to wrap them in RTS functions: They occur only once, in the prelude. *)
  let init env =
    let (set_len, get_len) = new_local env "len" in
    let (set_x, get_x) = new_local env "x" in
    let (set_r, get_r) = new_local env "r" in
    set_x ^^
    BoxedInt.unbox env ^^
    G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
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
    BoxedInt.unbox env ^^
    G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
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

    (* Write fields *)
    get_len ^^
    from_0_to_n env (fun get_i ->
      (* The closure *)
      get_r ^^ get_i ^^ idx env ^^
      (* The arg *)
      get_f ^^
      get_i ^^
      G.i (Convert (Wasm.Values.I64 I64Op.ExtendUI32)) ^^
      BoxedInt.box env ^^
      (* The closure again *)
      get_r ^^ get_i ^^ idx env ^^
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
  let compile_unit = compile_unboxed_const 1l

  (* Expects on the stack the pointer to the array. *)
  let load_n n = Heap.load_field (Int32.add Array.header_size n)

  (* Takes n elements of the stack and produces an argument tuple *)
  let from_stack env n =
    if n = 0 then compile_unit
    else
      let name = Printf.sprintf "to_%i_tuple" n in
      let args = Lib.List.table n (fun i -> Printf.sprintf "arg%i" i, I32Type) in
      Func.share_code env name args [I32Type] (fun env ->
        Array.lit env (Lib.List.table n (fun i -> G.i (LocalGet (nr (Int32.of_int i)))))
      )

  (* Takes an argument tuple and puts the elements on the stack: *)
  let to_stack env n =
    if n = 0 then G.i Drop else
    let name = Printf.sprintf "from_%i_tuple" n in
    let retty = Lib.List.make n I32Type in
    Func.share_code env name ["tup", I32Type] retty (fun env ->
      let get_tup = G.i (LocalGet (nr 0l)) in
      G.table n (fun i -> get_tup ^^ load_n (Int32.of_int i))
    )
end (* Tuple *)

module Dfinity = struct
  (* Dfinity-specific stuff: System imports, databufs etc. *)

  (* function ids for imported stuff *)
  let test_print_i env = 0l
  let test_show_i32_i env = 1l
  let data_externalize_i env = 2l
  let data_internalize_i env = 3l
  let data_length_i env = 4l
  let elem_externalize_i env = 5l
  let elem_internalize_i env = 6l
  let elem_length_i env = 7l
  let module_new_i env = 8l
  let actor_new_i env = 9l
  let actor_self_i env = 10l
  let actor_export_i env = 11l
  let func_internalize_i env = 12l
  let func_externalize_i env = 13l
  let func_bind_i env = 14l

  (* Based on http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings *)
  (* Ok to use as long as everything is ASCII *)
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (Char.code s.[i] :: l) in
    exp (String.length s - 1) []

  let system_imports env =
    let i = E.add_func_import env (nr {
      module_name = explode "test";
      item_name = explode "print";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (test_print_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "test";
      item_name = explode "show_i32";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (test_show_i32_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "data";
      item_name = explode "externalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (data_externalize_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "data";
      item_name = explode "internalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type; I32Type; I32Type],[])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (data_internalize_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "data";
      item_name = explode "length";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (data_length_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "elem";
      item_name = explode "externalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (elem_externalize_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "elem";
      item_name = explode "internalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type; I32Type; I32Type],[])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (elem_internalize_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "elem";
      item_name = explode "length";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (elem_length_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "module";
      item_name = explode "new";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (module_new_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "actor";
      item_name = explode "new";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (actor_new_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "actor";
      item_name = explode "self";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (actor_self_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "actor";
      item_name = explode "export";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (actor_export_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "func";
      item_name = explode "internalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type],[])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (func_internalize_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "func";
      item_name = explode "externalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type], [I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (func_externalize_i env));

    let i = E.add_func_import env (nr {
      module_name = explode "func";
      item_name = explode "bind_i32";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (func_bind_i env))


  let compile_databuf_of_text env  =
    Func.share_code env "databuf_of_text" ["string", I32Type] [I32Type] (fun env ->
      let get_i = G.i (LocalGet (nr 0l)) in

      (* Calculate the offset *)
      get_i ^^
      compile_add_const (Int32.mul Heap.word_size Text.header_size) ^^
      (* Calculate the length *)
      get_i ^^
      Heap.load_field (Text.len_field) ^^

      (* Externalize *)
      G.i (Call (nr (data_externalize_i env)))
    )

  let compile_databuf_of_bytes env (bytes : string) =
    Text.lit env bytes ^^ compile_databuf_of_text env

  (* For debugging *)
  let _compile_static_print env s =
      compile_databuf_of_bytes env s ^^
      G.i (Call (nr (test_print_i env)))
  let _compile_print_int env =
      G.i (Call (nr (test_show_i32_i env))) ^^
      G.i (Call (nr (test_print_i env))) ^^
      _compile_static_print env "\n"

  let prim_printInt env =
    if E.mode env = DfinityMode
    then
      BoxedInt.unbox env ^^
      G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
      G.i (Call (nr (test_show_i32_i env))) ^^
      G.i (Call (nr (test_print_i env)))
    else
      G.i Unreachable

  let prim_print env =
    if E.mode env = DfinityMode
    then
      compile_databuf_of_text env ^^
      (* Call print *)
      G.i (Call (nr (test_print_i env)))
    else
      G.i Unreachable

  let default_exports env =
    (* these export seems to be wanted by the hypervisor/v8 *)
    E.add_export env (nr {
      name = explode "mem";
      edesc = nr (MemoryExport (nr 0l))
    });
    E.add_export env (nr {
      name = explode "table";
      edesc = nr (TableExport (nr 0l))
    })

  let export_start_stub env =
    (* Create an empty message *)
    let empty_f = Func.of_body env [] [] (fun env1 ->
      (* Set up memory *)
      G.i (Call (nr (E.built_in env "restore_mem"))) ^^
      (* Collect garbage *)
      G.i (Call (nr (E.built_in env "collect"))) ^^
      (* Save memory *)
      G.i (Call (nr (E.built_in env "save_mem")))
      ) in
    let fi = E.add_fun env empty_f "start_stub" in
    E.add_export env (nr {
      name = explode "start";
      edesc = nr (FuncExport (nr fi))
    })

  let box_reference env =
    Func.share_code env "box_reference" ["ref", I32Type] [I32Type] (fun env ->
      let get_ref = G.i (LocalGet (nr 0l)) in
      Tagged.obj env Tagged.Reference [
        get_ref ^^
        ElemHeap.remember_reference env
      ]
    )

  let unbox_reference env =
    Heap.load_field 1l ^^
    ElemHeap.recall_reference env

  let get_self_reference env =
    G.i (Call (nr (actor_self_i env))) ^^
    box_reference env

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
      name = Dfinity.explode "datastore";
      edesc = nr (GlobalExport (nr mem_global))
    });
    E.add_export env (nr {
      name = Dfinity.explode "elemstore";
      edesc = nr (GlobalExport (nr elem_global))
    });

    Func.define_built_in env "restore_mem" [] [] (fun env1 ->
       let (set_i, get_i) = new_local env1 "len" in
       G.i (GlobalGet (nr mem_global)) ^^
       G.i (Call (nr (Dfinity.data_length_i env1))) ^^
       set_i ^^

       get_i ^^
       compile_unboxed_const 0l ^^
       G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
       G.if_ (ValBlockType None)
         (* First run, call the start function *)
         ( G.i (Call (nr start_funid)) )

         (* Subsequent run *)
         ( (* Set heap pointer based on databuf length *)
           get_i ^^
           compile_add_const ElemHeap.table_end ^^
           Heap.set_heap_ptr ^^
           Heap.grow_memory env ^^

           (* Load memory *)
           compile_unboxed_const ElemHeap.table_end ^^
           get_i ^^
           G.i (GlobalGet (nr mem_global)) ^^
           compile_unboxed_zero ^^
           G.i (Call (nr (Dfinity.data_internalize_i env1))) ^^

           (* Load reference counter *)
           G.i (GlobalGet (nr elem_global)) ^^
           G.i (Call (nr (Dfinity.elem_length_i env1))) ^^
           ElemHeap.set_ref_ctr ^^

           (* Load references *)
           compile_unboxed_const ElemHeap.ref_location ^^
           ElemHeap.get_ref_ctr ^^
           G.i (GlobalGet (nr elem_global)) ^^
           compile_unboxed_zero ^^
           G.i (Call (nr (Dfinity.elem_internalize_i env1)))
        )
    );
    Func.define_built_in env "save_mem" [] [] (fun env1 ->
       (* Store memory *)
       compile_unboxed_const ElemHeap.table_end ^^
       Heap.get_heap_ptr ^^
       compile_unboxed_const ElemHeap.table_end ^^
       G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
       G.i (Call (nr (Dfinity.data_externalize_i env))) ^^
       G.i (GlobalSet (nr mem_global)) ^^

       (* Store references *)
       compile_unboxed_const ElemHeap.ref_location ^^
       ElemHeap.get_ref_ctr ^^
       G.i (Call (nr (Dfinity.elem_externalize_i env))) ^^
       G.i (GlobalSet (nr elem_global))
    )

  let save_mem env = G.i (Call (nr (E.built_in env "save_mem")))
  let restore_mem env = G.i (Call (nr (E.built_in env "restore_mem")))

end (* OrthogonalPersistence *)

module Serialization = struct
  (*
    The serialization strategy is as follows:
    * We remember the current heap pointer and reference table pointer
    * We deeply and compactly copy the arguments into the space beyond the heap
      pointer.
    * Special handling for closures: These are turned into funcrefs.
    * We traverse this space and make all pointers relative to the beginning of
      the space. Same for indices into the reference table.
    * We externalize all that new data space into a databuf, and add it to the
      reference table
    * We externalize all that new table space into a elembuf
    * We reset the heap pointer and table pointer, to garbage collect the scratch space.

    TODO: Cycles are not detected yet.

    We separate code for copying and the code for pointer adjustment because
    the latter can be used again in the deseriazliation code.

    The deserialization is analogous:
    * We internalize the elembuf into the table, bumping the table reference
      pointer.
    * The last entry of the table is the dataref from above. Since we don't
      need it after this, we decrement the table reference pointer by one.
    * We internalize this databuf intot the heap space, bumping the heap
      pointer.
    * We traverse this space and adjust all pointers.
      Same for indices into the reference table.
  *)

  let serialize_go env =
    Func.share_code env "serialize_go" ["x", I32Type] [I32Type] (fun env ->
      let get_x = G.i (LocalGet (nr 0l)) in
      let (set_copy, get_copy) = new_local env "x" in

      Heap.get_heap_ptr ^^
      set_copy ^^

      get_x ^^
      BitTagged.if_unboxed env (ValBlockType (Some I32Type))
        ( get_x )
        ( get_x ^^ Tagged.branch env (ValBlockType (Some I32Type))
          [ Tagged.Int,
            get_x ^^
            Heap.alloc env 2l ^^
            compile_unboxed_const (Int32.mul 2l Heap.word_size) ^^
            Heap.memcpy env ^^
            get_copy
          ; Tagged.Reference,
            get_x ^^
            Heap.alloc env 2l ^^
            compile_unboxed_const (Int32.mul 2l Heap.word_size) ^^
            Heap.memcpy env ^^
            get_copy
          ; Tagged.Some,
            Opt.inject env (
              get_x ^^ Opt.project ^^
              G.i (Call (nr (E.built_in env "serialize_go")))
            )
          ; Tagged.ObjInd,
            Tagged.obj env Tagged.ObjInd [
              get_x ^^ Heap.load_field 1l ^^
              G.i (Call (nr (E.built_in env "serialize_go")))
            ]
          ; Tagged.Array,
            begin
              let (set_len, get_len) = new_local env "len" in
              get_x ^^
              Heap.load_field Array.len_field ^^
              set_len ^^

              get_len ^^
              compile_add_const Array.header_size ^^
              Heap.dyn_alloc_words env ^^
              G.i Drop ^^

              (* Copy header *)
              get_x ^^
              get_copy ^^
              compile_unboxed_const (Int32.mul Heap.word_size Array.header_size) ^^
              Heap.memcpy env ^^

              (* Copy fields *)
              get_len ^^
              from_0_to_n env (fun get_i ->
                get_copy ^^
                get_i ^^
                Array.idx env ^^

                get_x ^^
                get_i ^^
                Array.idx env ^^
                load_ptr ^^
                G.i (Call (nr (E.built_in env "serialize_go"))) ^^
                store_ptr
              ) ^^
              get_copy
            end
          ; Tagged.Text,
            begin
              let (set_len, get_len) = new_local env "len" in
              get_x ^^
              Heap.load_field Text.len_field ^^
              (* get length in words *)
              compile_add_const 3l ^^
              compile_divU_const Heap.word_size ^^
              compile_add_const Text.header_size ^^
              set_len ^^

              get_len ^^
              Heap.dyn_alloc_words env ^^
              G.i Drop ^^

              (* Copy header and data *)
              get_x ^^
              get_copy ^^
              get_len ^^
              compile_mul_const Heap.word_size ^^
              Heap.memcpy env ^^

              get_copy
            end
          ; Tagged.Object,
            begin
              let (set_len, get_len) = new_local env "len" in
              get_x ^^
              Heap.load_field Object.size_field ^^
              set_len ^^

              get_len ^^
              compile_mul_const 2l ^^
              compile_add_const Object.header_size ^^
              Heap.dyn_alloc_words env ^^
              G.i Drop ^^

              (* Copy header *)
              get_x ^^
              get_copy ^^
              compile_unboxed_const (Int32.mul Heap.word_size Object.header_size) ^^
              Heap.memcpy env ^^

              (* Copy fields *)
              get_len ^^
              from_0_to_n env (fun get_i ->
                (* Copy hash *)
                get_i ^^
                compile_mul_const 2l ^^
                compile_add_const Object.header_size ^^
                compile_mul_const Heap.word_size ^^
                get_copy ^^
                G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^

                get_i ^^
                compile_mul_const 2l ^^
                compile_add_const Object.header_size ^^
                compile_mul_const Heap.word_size ^^
                get_x ^^
                G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^


                load_ptr ^^
                store_ptr ^^

                (* Copy data *)

                get_i ^^
                compile_mul_const 2l ^^
                compile_add_const Object.header_size ^^
                compile_mul_const Heap.word_size ^^
                get_copy ^^
                G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
                compile_add_const Heap.word_size ^^

                get_i ^^
                compile_mul_const 2l ^^
                compile_add_const Object.header_size ^^
                compile_mul_const Heap.word_size ^^
                get_x ^^
                G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
                compile_add_const Heap.word_size ^^

                load_ptr ^^
                G.i (Call (nr (E.built_in env "serialize_go"))) ^^
                store_ptr
              ) ^^
              get_copy
            end
          ]
        )
    )

  let shift_pointer_at env =
    Func.share_code env "shift_pointer_at" ["loc", I32Type;  "ptr_offset", I32Type] [] (fun env ->
      let get_loc = G.i (LocalGet (nr 0l)) in
      let get_ptr_offset = G.i (LocalGet (nr 1l)) in
      let (set_ptr, get_ptr) = new_local env "ptr" in
      get_loc ^^
      load_ptr ^^
      set_ptr ^^
      get_ptr ^^
      BitTagged.if_unboxed env (ValBlockType None)
        (* nothing to do *)
        ( G.nop )
        ( get_loc ^^
          get_ptr ^^
          get_ptr_offset ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          store_ptr
        )
    )

  (* Returns the object size (in bytes) *)
  let object_size env =
    Func.share_code env "object_size" ["x", I32Type] [I32Type] (fun env ->
      let get_x = G.i (LocalGet (nr 0l)) in
      get_x ^^
      Tagged.branch env (ValBlockType (Some I32Type))
        [ Tagged.Int,
          compile_unboxed_const (Int32.mul 3l Heap.word_size)
        ; Tagged.Reference,
          compile_unboxed_const (Int32.mul 2l Heap.word_size)
        ; Tagged.Some,
          compile_unboxed_const (Int32.mul 2l Heap.word_size)
        ; Tagged.ObjInd,
          compile_unboxed_const (Int32.mul 2l Heap.word_size)
        ; Tagged.MutBox,
          compile_unboxed_const (Int32.mul 2l Heap.word_size)
        ; Tagged.Array,
          get_x ^^
          Heap.load_field Array.len_field ^^
          compile_add_const Array.header_size ^^
          compile_mul_const Heap.word_size
        ; Tagged.Text,
          get_x ^^
          Heap.load_field Text.len_field ^^
          compile_add_const 3l ^^
          compile_divU_const Heap.word_size ^^
          compile_add_const Text.header_size ^^
          compile_mul_const Heap.word_size
        ; Tagged.Object,
          get_x ^^
          Heap.load_field Object.size_field ^^
          compile_mul_const 2l ^^
          compile_add_const Object.header_size ^^
          compile_mul_const Heap.word_size
        ; Tagged.Closure,
          get_x ^^
          Heap.load_field Closure.len_field ^^
          compile_add_const Closure.header_size ^^
          compile_mul_const Heap.word_size
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
          get_x ^^ object_size env ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          set_x
        )

  (* Calls mk_code for each pointer in the object pointed to by get_x,
     passing code get the address of the pointer. *)
  let for_each_pointer env get_x mk_code =
    let (set_ptr_loc, get_ptr_loc) = new_local env "ptr_loc" in
    get_x ^^
    Tagged.branch_default env (ValBlockType None) G.nop
      [ Tagged.MutBox,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size Var.mutbox_field) ^^
        set_ptr_loc ^^
        mk_code get_ptr_loc
      ; Tagged.Some,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size Opt.payload_field) ^^
        set_ptr_loc ^^
        mk_code get_ptr_loc
      ; Tagged.ObjInd,
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size 1l) ^^
        set_ptr_loc ^^
        mk_code get_ptr_loc
      ; Tagged.Array,
        get_x ^^
        Heap.load_field Array.len_field ^^
        (* Adjust fields *)
        from_0_to_n env (fun get_i ->
          get_x ^^
          get_i ^^
          Array.idx env ^^
          set_ptr_loc ^^
          mk_code get_ptr_loc
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
          mk_code get_ptr_loc
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
          mk_code get_ptr_loc
        )
      ]

  let shift_pointers env =
    Func.share_code env "shift_pointers" ["start", I32Type; "to", I32Type; "ptr_offset", I32Type] [] (fun env ->
      let get_start = G.i (LocalGet (nr 0l)) in
      let get_to = G.i (LocalGet (nr 1l)) in
      let get_ptr_offset = G.i (LocalGet (nr 2l)) in

      walk_heap_from_to env get_start get_to (fun get_x ->
        for_each_pointer env get_x (fun get_ptr_loc ->
          get_ptr_loc ^^
          get_ptr_offset ^^
          shift_pointer_at env
        )
      )
    )

  let extract_references env =
    Func.share_code env "extract_references" ["start", I32Type; "to", I32Type; "tbl_area", I32Type] [I32Type] (fun env ->
      let get_start = G.i (LocalGet (nr 0l)) in
      let get_to = G.i (LocalGet (nr 1l)) in
      let get_tbl_area = G.i (LocalGet (nr 2l)) in
      let (set_i, get_i) = new_local env "i" in

      compile_unboxed_const 0l ^^ set_i ^^

      walk_heap_from_to env get_start get_to (fun get_x ->
        get_x ^^
        Tagged.branch_default env (ValBlockType None) G.nop
          [ Tagged.Reference,
            (* Adjust reference *)
            get_tbl_area ^^
            get_i ^^ compile_mul_const Heap.word_size ^^
            G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
            get_x ^^
            Dfinity.unbox_reference env ^^
            store_ptr ^^

            get_x ^^
            get_i ^^
            Heap.store_field 1l ^^

            get_i ^^
            compile_add_const 1l ^^
            set_i
          ]
      ) ^^
      get_i
    )

  let intract_references env =
    Func.share_code env "intract_references" ["start", I32Type; "to", I32Type; "tbl_area", I32Type] [] (fun env ->
      let get_start = G.i (LocalGet (nr 0l)) in
      let get_to = G.i (LocalGet (nr 1l)) in
      let get_tbl_area = G.i (LocalGet (nr 2l)) in

      walk_heap_from_to env get_start get_to (fun get_x ->
        get_x ^^
        Tagged.branch_default env (ValBlockType None) G.nop
          [ Tagged.Reference,
            get_x ^^
            (* Adjust reference *)
            get_x ^^
            Heap.load_field 1l ^^
            compile_mul_const Heap.word_size ^^
            get_tbl_area ^^
            G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
            load_ptr ^^
            ElemHeap.remember_reference env ^^
            Heap.store_field 1l
          ]
      )
    )

  let serialize env =
    if E.mode env <> DfinityMode
    then Func.share_code env "serialize" ["x", I32Type] [I32Type] (fun env -> G.i Unreachable)
    else Func.share_code env "serialize" ["x", I32Type] [I32Type] (fun env ->
      let get_x = G.i (LocalGet (nr 0l)) in

      let (set_start, get_start) = new_local env "old_heap" in
      let (set_end, get_end) = new_local env "end" in
      let (set_tbl_size, get_tbl_size) = new_local env "tbl_size" in
      let (set_databuf, get_databuf) = new_local env "databuf" in

      (* Remember where we start to copy to *)
      Heap.get_heap_ptr ^^
      set_start ^^

      (* Copy data *)
      get_x ^^
      BitTagged.if_unboxed env (ValBlockType None)
        (* We have a bit-tagged raw value. Put this into a singleton databuf,
           which will be recognized as such by its size.
        *)
        ( Heap.alloc env 1l ^^
          get_x ^^
          store_ptr ^^

          (* Remember the end *)
          Heap.get_heap_ptr ^^
          set_end ^^

          (* Empty table of references *)
          compile_unboxed_const 0l ^^ set_tbl_size
        )
        (* We have real data on the heap. Copy.  *)
        ( get_x ^^
          serialize_go env ^^
          G.i Drop ^^

          (* Remember the end *)
          Heap.get_heap_ptr ^^
          set_end ^^

          (* Adjust pointers *)
          get_start ^^
          get_end ^^
          compile_unboxed_zero ^^ get_start ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
          shift_pointers env ^^

          (* Extract references, and remember how many there were *)
          get_start ^^
          get_end ^^
          get_end ^^
          extract_references env ^^
          set_tbl_size
        ) ^^

      (* Create databuf *)
      get_start ^^
      get_end ^^ get_start ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
      G.i (Call (nr (Dfinity.data_externalize_i env))) ^^
      set_databuf ^^

      (* Append this reference at the end of the extracted references *)
      get_end ^^
      get_tbl_size ^^ compile_mul_const Heap.word_size ^^
      G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
      get_databuf ^^
      store_ptr ^^
      (* And bump table end *)
      get_tbl_size ^^ compile_add_const 1l ^^ set_tbl_size ^^

      (* Reset the heap counter, to free some space *)
      get_start ^^
      Heap.set_heap_ptr ^^
      Heap.grow_memory env ^^

      (* Finally, create elembuf *)
      get_end ^^
      get_tbl_size ^^
      G.i (Call (nr (Dfinity.elem_externalize_i env)))
    )

  let serialize_n env n = match n with
    | 0 -> G.nop
    | 1 -> serialize env
    | _ ->
      let name = Printf.sprintf "serialize_%i" n in
      let args = Lib.List.table n (fun i -> Printf.sprintf "arg%i" i, I32Type) in
      let retty = Lib.List.make n I32Type in
      Func.share_code env name args retty (fun env ->
        G.table n (fun i ->
          G.i (LocalGet (nr (Int32.of_int i))) ^^ serialize env
        )
      )

  let deserialize env =
    Func.share_code env "deserialize" ["ref", I32Type] [I32Type] (fun env ->
      let get_elembuf = G.i (LocalGet (nr 0l)) in
      let (set_databuf, get_databuf) = new_local env "databuf" in
      let (set_start, get_start) = new_local env "start" in
      let (set_data_len, get_data_len) = new_local env "data_len" in
      let (set_tbl_size, get_tbl_size) = new_local env "tbl_size" in

      (* new positions *)
      Heap.get_heap_ptr ^^
      set_start ^^

      get_elembuf ^^ G.i (Call (nr (Dfinity.elem_length_i env))) ^^
      set_tbl_size ^^

      (* Get scratch space (one word) *)
      Heap.alloc env 1l ^^ G.i Drop ^^
      get_start ^^ Heap.set_heap_ptr ^^

      (* First load databuf reference (last entry) at the heap position somehow *)
      (* now load the databuf *)
      get_start ^^
      compile_unboxed_const 1l ^^
      get_elembuf ^^
      get_tbl_size ^^ compile_sub_const 1l ^^
      G.i (Call (nr (Dfinity.elem_internalize_i env))) ^^
      get_start ^^ load_ptr ^^
      set_databuf ^^

      get_databuf ^^ G.i (Call (nr (Dfinity.data_length_i env)))  ^^
      set_data_len ^^

      (* Get some scratch space *)
      get_data_len ^^ Heap.dyn_alloc_bytes env ^^ G.i Drop ^^
      get_start ^^ Heap.set_heap_ptr ^^

      (* Load data from databuf *)
      get_start ^^
      get_data_len ^^
      get_databuf ^^
      compile_unboxed_const 0l ^^
      G.i (Call (nr (Dfinity.data_internalize_i env))) ^^

      (* Check if we got something unboxed (data buf size 1 word) *)
      get_data_len ^^
      compile_unboxed_const Heap.word_size ^^
      G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
      G.if_ (ValBlockType (Some I32Type))
        (* Yes, we got something unboxed. Return it, and do _not_ bump the heap pointer *)
        ( get_start ^^ load_ptr )
        (* No, it is actual heap-data *)
        ( (* update heap pointer *)
          get_start ^^
          get_data_len ^^
          G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
          Heap.set_heap_ptr ^^
          Heap.grow_memory env ^^

          (* Fix pointers *)
          get_start ^^
          Heap.get_heap_ptr ^^
          get_start ^^
          shift_pointers env ^^

          (* Load references *)
          Heap.get_heap_ptr ^^
          get_tbl_size ^^ compile_sub_const 1l ^^
          get_elembuf ^^
          compile_unboxed_const 0l ^^
          G.i (Call (nr (Dfinity.elem_internalize_i env))) ^^

          (* Fix references *)
          (* Extract references *)
          get_start ^^
          Heap.get_heap_ptr ^^
          Heap.get_heap_ptr ^^
          intract_references env ^^

          (* return allocated thing *)
          get_start
        )
    )


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

  (* If the pointer at ptr_loc points after begin_from_space, copy
     to after end_to_space, and replace it with a pointer, adjusted for where
     the object will be finally. *)
  (* Invariant: Must not be called on the same pointer twice. *)
  let evacuate env = Func.share_code env "evaucate" ["begin_from_space", I32Type; "begin_to_space", I32Type; "end_to_space", I32Type; "ptr_loc", I32Type] [I32Type] (fun env ->
    let get_begin_from_space = G.i (LocalGet (nr 0l)) in
    let get_begin_to_space = G.i (LocalGet (nr 1l)) in
    let get_end_to_space = G.i (LocalGet (nr 2l)) in
    let get_ptr_loc = G.i (LocalGet (nr 3l)) in
    let (set_len, get_len) = new_local env "len" in
    let (set_new_ptr, get_new_ptr) = new_local env "new_ptr" in

    let get_obj = get_ptr_loc ^^ load_ptr in

    get_obj ^^
    (* If this is an unboxed scalar, ignore it *)
    BitTagged.if_unboxed env (ValBlockType None) (get_end_to_space ^^ G.i Return) G.nop ^^

    (* If this is static, ignore it *)
    get_obj ^^
    get_begin_from_space ^^
    G.i (Compare (Wasm.Values.I32 I32Op.LtU)) ^^
    G.if_ (ValBlockType None) (get_end_to_space ^^ G.i Return) G.nop ^^

    (* If this is an indirection, just use that value *)
    get_obj ^^
    Tagged.branch_default env (ValBlockType None) G.nop [
      Tagged.Indirection,
      (* Update pointer *)
      get_ptr_loc ^^
      get_ptr_loc ^^ load_ptr ^^ Heap.load_field 1l ^^
      store_ptr ^^

      get_end_to_space ^^
      G.i Return
    ] ^^

    (* Copy the referenced object to to space *)
    get_obj ^^ Serialization.object_size env ^^ set_len ^^

    get_obj ^^ get_end_to_space ^^ get_len ^^ Heap.memcpy env ^^

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
    get_ptr_loc ^^
    get_new_ptr ^^
    store_ptr ^^

    (* Calculate new end of to space *)
    get_end_to_space ^^
    get_len ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Add))
  )

  let register env (end_of_static_space : int32) = Func.define_built_in env "collect" [] [] (fun env ->
    (* Copy all roots. *)
    let (set_begin_from_space, get_begin_from_space) = new_local env "begin_from_space" in
    let (set_begin_to_space, get_begin_to_space) = new_local env "begin_to_space" in
    let (set_end_to_space, get_end_to_space) = new_local env "end_to_space" in

    compile_unboxed_const end_of_static_space ^^ set_begin_from_space ^^
    Heap.get_heap_ptr ^^ set_begin_to_space ^^
    Heap.get_heap_ptr ^^ set_end_to_space ^^


    (* Common arguments for evalcuate *)
    let evac get_ptr_loc =
        get_begin_from_space ^^
        get_begin_to_space ^^
        get_end_to_space ^^
        get_ptr_loc ^^
        evacuate env ^^
        set_end_to_space in

    (* Go through the roots, and evacaute them *)
    ClosureTable.get_counter ^^
    from_0_to_n env (fun get_i -> evac (
      get_i ^^
      compile_add_const 1l ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const ClosureTable.loc
    )) ^^
    Serialization.walk_heap_from_to env
      (compile_unboxed_const ClosureTable.table_end)
      (compile_unboxed_const end_of_static_space)
      (fun get_x -> Serialization.for_each_pointer env get_x evac) ^^

    (* Go through the to-space, and evacuate that.
       Note that get_end_to_space changes as we go, but walk_heap_from_to can handle that.
     *)
    Serialization.walk_heap_from_to env
      get_begin_to_space
      get_end_to_space
      (fun get_x -> Serialization.for_each_pointer env get_x evac) ^^

    (* Copy the to-space to the beginning of memory. *)
    get_begin_to_space ^^
    get_begin_from_space ^^
    get_end_to_space ^^ get_begin_to_space ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
    Heap.memcpy env ^^

    (* Reset the heap pointer *)
    get_begin_from_space ^^
    get_end_to_space ^^ get_begin_to_space ^^ G.i (Binary (Wasm.Values.I32 I32Op.Sub)) ^^
    G.i (Binary (Wasm.Values.I32 I32Op.Add)) ^^
    Heap.set_heap_ptr
  )


end (* GC *)


module StackRep = struct
  open SR

  (*
     Most expression have a “preferred”, most optimal, form. Hence,
     compile_exp put them on the stack in that form, and also returns
     the form it chose.

     But the users of compile_exp usually want a specific form as well.
     So they use compile_exp_as, indicating the form they expect.
     compile_exp_as then does the necessary coercions.
   *)

  let of_arity n =
    if n = 1 then Vanilla else UnboxedTuple n

  (* The stack rel of a primitive type, i.e. what the binary operators expect *)
  let of_type : Type.typ -> t = function
    | Type.Prim Type.Bool -> bool
    | Type.Prim Type.Nat -> UnboxedInt64
    | Type.Prim Type.Int -> UnboxedInt64
    | Type.Prim Type.Word32 -> UnboxedInt32
    | Type.Prim Type.Text -> Vanilla
    | p -> todo "of_type" (Arrange_ir.typ p) Vanilla

  let to_block_type env = function
    | Vanilla -> ValBlockType (Some I32Type)
    | UnboxedInt64 -> ValBlockType (Some I64Type)
    | UnboxedInt32 -> ValBlockType (Some I32Type)
    | UnboxedReference -> ValBlockType (Some I32Type)
    | UnboxedTuple 0 -> ValBlockType None
    | UnboxedTuple 1 -> ValBlockType (Some I32Type)
    | UnboxedTuple n -> VarBlockType (nr (E.func_type env (FuncType ([], Lib.List.make n I32Type))))
    | StaticThing _ -> ValBlockType None
    | Unreachable -> ValBlockType None

  let to_string = function
    | Vanilla -> "Vanilla"
    | UnboxedInt64 -> "UnboxedInt64"
    | UnboxedInt32 -> "UnboxedInt32"
    | UnboxedReference -> "UnboxedReference"
    | UnboxedTuple n -> Printf.sprintf "UnboxedTuple %d" n
    | Unreachable -> "Unreachable"
    | StaticThing _ -> "StaticThing"

  let join (sr1 : t) (sr2 : t) = match sr1, sr2 with
    | Unreachable, sr2 -> sr2
    | sr1, Unreachable -> sr1
    | UnboxedInt64, UnboxedInt64 -> UnboxedInt64
    | UnboxedReference, UnboxedReference -> UnboxedReference
    | UnboxedTuple n, UnboxedTuple m when n = m -> sr1
    | _, Vanilla -> Vanilla
    | Vanilla, _ -> Vanilla
    | _, _ ->
      Printf.eprintf "Invalid stack rep join (%s, %s)\n"
        (to_string sr1) (to_string sr2); sr1

  let drop env (sr_in : t) =
    match sr_in with
    | Vanilla -> G.i Drop
    | UnboxedInt64 -> G.i Drop
    | UnboxedInt32 -> G.i Drop
    | UnboxedReference -> G.i Drop
    | UnboxedTuple n -> G.table n (fun _ -> G.i Drop)
    | StaticThing _ -> G.nop
    | Unreachable -> G.nop

  let materialize env = function
    | StaticFun fi -> Var.static_fun_pointer env fi

  let deferred_of_static_think env s =
    { materialize = (fun env -> (StaticThing s, G.nop))
    ; materialize_vanilla = (fun env -> materialize env s)
    }

  let adjust env (sr_in : t) sr_out =
    if sr_in = sr_out
    then G.nop
    else match sr_in, sr_out with
    | Unreachable, Unreachable -> G.nop
    | Unreachable, _ -> G.i Unreachable

    | UnboxedTuple n, Vanilla -> Tuple.from_stack env n
    | Vanilla, UnboxedTuple n -> Tuple.to_stack env n

    | UnboxedInt64, Vanilla -> BoxedInt.box env
    | Vanilla, UnboxedInt64 -> BoxedInt.unbox env

    | UnboxedInt32, Vanilla -> BoxedSmallInt.box env
    | Vanilla, UnboxedInt32 -> BoxedSmallInt.unbox env

    | UnboxedReference, Vanilla -> Dfinity.box_reference env
    | Vanilla, UnboxedReference -> Dfinity.unbox_reference env

    | StaticThing s, Vanilla -> materialize env s
    | StaticThing s, UnboxedTuple 0 -> G.nop

    | _, _ ->
      Printf.eprintf "Unknown stack_rep conversion %s -> %s\n"
        (to_string sr_in) (to_string sr_out);
      G.nop

end (* StackRep *)


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
    if E.mode env <> DfinityMode then G.i Unreachable else
      compile_unboxed_const tmp_table_slot ^^ (* slot number *)
      get_ref ^^ (* the unboxed funcref *)
      G.i (Call (nr (Dfinity.func_internalize_i env))) ^^

      compile_unboxed_const tmp_table_slot ^^
      G.i (CallIndirect (nr (message_ty env cc)))

  let export_self_message env =
    Func.share_code env "export_self_message" ["name", I32Type] [I32Type] (fun env ->
      let get_name = G.i (LocalGet (nr 0l)) in

      Tagged.obj env Tagged.Reference [
        (* Create a funcref for the message *)
        G.i (Call (nr (Dfinity.actor_self_i env))) ^^
        get_name ^^ (* the databuf with the message name *)
        G.i (Call (nr (Dfinity.actor_export_i env))) ^^
        ElemHeap.remember_reference env
      ]
    )

  let static_self_message_pointer env name =
    Dfinity.compile_databuf_of_bytes env name.it ^^
    export_self_message env

  (* Create a WebAssembly func from a pattern (for the argument) and the body.
   Parameter `captured` should contain the, well, captured local variables that
   the function will find in the closure. *)
  let compile_local_function env cc restore_env mk_pat mk_body at =
    let args = Lib.List.table cc.Value.n_args (fun i -> Printf.sprintf "arg%i" i, I32Type) in
    let retty = Lib.List.make cc.Value.n_res I32Type in
    Func.of_body env (["clos", I32Type] @ args) retty (fun env1 -> G.with_region at (
      let get_closure = G.i (LocalGet (nr 0l)) in

      let (env2, closure_code) = restore_env env1 get_closure in

      (* Destruct the argument *)
      let (env3, destruct_args_code) = mk_pat env2  in

      closure_code ^^
      let get i = G.i (LocalGet (nr (Int32.(add 1l (of_int i))))) in
      destruct_args_code get ^^
      mk_body env3
    ))

  (* Similar, but for shared functions aka messages. Differences are:
     - The closure is actually an index into the closure table
     - The arguments need to be deserialized.
     - The return value ought to be discarded
     - We need to register the type in the custom types section
     - Do GC at the end
     - Fake orthogonal persistence
  *)
  let compile_message env cc restore_env mk_pat mk_body at =
    let args = Lib.List.table cc.Value.n_args (fun i -> Printf.sprintf "arg%i" i, I32Type) in
    assert (cc.Value.n_res = 0);
    Func.of_body env (["clos", I32Type] @ args) [] (fun env1 -> G.with_region at (
      (* Restore memory *)
      OrthogonalPersistence.restore_mem env1 ^^

      (* Look up closure *)
      let (set_closure, get_closure) = new_local env1 "closure" in
      G.i (LocalGet (nr 0l)) ^^
      ClosureTable.recall_closure env1 ^^
      set_closure ^^

      let (env2, closure_code) = restore_env env1 get_closure in

      (* Destruct the argument *)
      let (env3, destruct_args_code) = mk_pat env2  in

      closure_code ^^
      let get i =
        G.i (LocalGet (nr (Int32.(add 1l (of_int i))))) ^^
        Serialization.deserialize env in
      destruct_args_code get ^^
      mk_body env3 ^^

      (* Collect garbage *)
      G.i (Call (nr (E.built_in env3 "collect"))) ^^

      (* Save memory *)
      OrthogonalPersistence.save_mem env1
    ))

  (* A static message, from a public actor field *)
  (* Like compile__message, but no closure *)
  let compile_static_message env cc mk_pat mk_body at : E.func_with_names =
    let args = Lib.List.table cc.Value.n_args (fun i -> Printf.sprintf "arg%i" i, I32Type) in
    assert (cc.Value.n_res = 0);
    (* Messages take no closure, return nothing*)
    Func.of_body env args [] (fun env1 ->
      (* Set up memory *)
      OrthogonalPersistence.restore_mem env ^^

      (* Destruct the argument *)
      let (env2, destruct_args_code) = mk_pat env1  in

      let get i =
        G.i (LocalGet (nr (Int32.(add 0l (of_int i))))) ^^
        Serialization.deserialize env in
      destruct_args_code get ^^
      mk_body env2 ^^

      (* Collect memory *)
      G.i (Call (nr (E.built_in env "collect"))) ^^

      (* Save memory *)
      OrthogonalPersistence.save_mem env
      )

  (* Compile a closed function declaration (has no free variables) *)
  let dec_closed pre_env cc name mk_pat mk_body at =
      let (fi, fill) = E.reserve_fun pre_env name.it in
      let d = StackRep.deferred_of_static_think pre_env (SR.StaticFun fi) in
      let pre_env1 = E.add_local_deferred pre_env name.it d in
      ( pre_env1, fun env ->
        let restore_no_env env1 _ = (env1, G.nop) in
        let f = compile_local_function env cc restore_no_env mk_pat mk_body at in
        fill f
      )

  (* Compile a closure declaration (has free variables) *)
  let dec_closure pre_env cc h name captured mk_pat mk_body at =
      let is_local = cc.Value.sort <> Type.Sharable in

      let (set_li, get_li) = new_local pre_env (name.it ^ "_clos") in
      let (pre_env1, alloc_code0) = AllocHow.add_how pre_env name.it (Some h) in

      let len = Wasm.I32.of_int_u (List.length captured) in
      let alloc_code =
        (* Allocate a heap object for the closure *)
        Heap.alloc pre_env (Int32.add Closure.header_size len) ^^
        set_li ^^

        (* Alloc space for the name of the function *)
        alloc_code0
      in

      ( pre_env1, alloc_code, fun env ->

        let (store_env, restore_env) =
          let rec go i = function
            | [] -> (G.nop, fun env1 _ -> (env1, G.nop))
            | (v::vs) ->
                let (store_rest, restore_rest) = go (i+1) vs in
                let (store_this, restore_this) = Var.capture env v in
                let store_env =
                  get_li ^^
                  store_this ^^
                  Closure.store_data (Wasm.I32.of_int_u i) ^^
                  store_rest in
                let restore_env env1 get_env =
                  let (env2, code) = restore_this env1 in
                  let (env3, code_rest) = restore_rest env2 get_env in
                  (env3,
                   get_env ^^
                   Closure.load_data (Wasm.I32.of_int_u i) ^^
                   code ^^
                   code_rest
                  )
                in (store_env, restore_env) in
          go 0 captured in

        let fi =
          if is_local
          then
            let f = compile_local_function env cc restore_env mk_pat mk_body at in
            E.add_fun env f name.it
          else
            let f = compile_message env cc restore_env mk_pat mk_body at in
            let fi = E.add_fun env f name.it in
            E.add_dfinity_type env (fi,
              CustomSections.(I32 :: Lib.List.make cc.Value.n_args ElemBuf)
            );
            fi
          in

        (* Store the tag *)
        get_li ^^
        Tagged.store Tagged.Closure ^^

        (* Store the function number: *)
        get_li ^^
        compile_unboxed_const fi ^^
        Heap.store_field Closure.funptr_field ^^

        (* Store the length *)
        get_li ^^
        compile_unboxed_const len ^^
        Heap.store_field Closure.len_field ^^

        (* Store all captured values *)
        store_env ^^

        (* Possibly turn into a funcref *)
        begin
          if is_local
          then get_li
          else
            Tagged.obj env Tagged.Reference [
              compile_unboxed_const fi ^^
              G.i (Call (nr (Dfinity.func_externalize_i env))) ^^
              get_li ^^
              ClosureTable.remember_closure env ^^
              G.i (Call (nr (Dfinity.func_bind_i env))) ^^
              ElemHeap.remember_reference env
            ]
        end ^^

        (* Store it *)
        Var.set_val env name.it)

  let dec pre_env how name cc captured mk_pat mk_body at =
    let is_local = cc.Value.sort <> Type.Sharable in

    if not is_local && E.mode pre_env <> DfinityMode
    then
      let (pre_env1, _) = Var.add_local pre_env name.it in
      ( pre_env1, G.i Unreachable, fun env -> G.i Unreachable)
    else match AllocHow.M.find_opt name.it how with
      | None ->
        assert is_local;
        let (pre_env1, fill) = dec_closed pre_env cc name mk_pat mk_body at in
        (pre_env1, G.nop, fun env -> fill env; G.nop)
      | Some h ->
        dec_closure pre_env cc h name captured mk_pat mk_body at

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

  let orTrap : patternCode -> G.t = function
    | CannotFail is -> is
    | CanFail is -> is (G.i Unreachable)

  let with_region at = function
    | CannotFail is -> CannotFail (G.with_region at is)
    | CanFail is -> CanFail (fun k -> G.with_region at (is k))

end (* PatCode *)
open PatCode

(* The actual compiler code that looks at the AST *)

let compile_lit env lit = Syntax.(match lit with
  (* Booleans are directly in Vanilla representation *)
  | BoolLit false -> SR.bool, Bool.lit false
  | BoolLit true ->  SR.bool, Bool.lit true
  (* This maps int to int32, instead of a proper arbitrary precision library *)
  | IntLit n      -> SR.UnboxedInt64,
    (try compile_const_64 (Big_int.int64_of_big_int n)
    with Failure _ -> Printf.eprintf "compile_lit: Overflow in literal %s\n" (Big_int.string_of_big_int n); G.i Unreachable)
  | NatLit n      -> SR.UnboxedInt64,
    (try compile_const_64 (Big_int.int64_of_big_int n)
    with Failure _ -> Printf.eprintf "compile_lit: Overflow in literal %s\n" (Big_int.string_of_big_int n); G.i Unreachable)
  | Word32Lit n   -> SR.UnboxedInt32,
    (try compile_unboxed_const n
    with Failure _ -> Printf.eprintf "compile_lit: Overflow in literal %d\n" (Int32.to_int n); G.i Unreachable) (* TODO: check we are 64 bit *)
  | NullLit       -> SR.Vanilla, Opt.null
  | TextLit t     -> SR.Vanilla, Text.lit env t
  | _ -> todo "compile_lit" (Arrange.lit lit) (SR.Vanilla, G.i Unreachable)
  )

let compile_lit_as env sr_out lit =
  let sr_in, code = compile_lit env lit in
  code ^^ StackRep.adjust env sr_in sr_out

let compile_unop env t op = Syntax.(match op, t with
  | NegOp, Type.Prim Type.Int ->
      SR.UnboxedInt64,
      Func.share_code env "neg" ["n", I64Type] [I64Type] (fun env ->
        let get_n = G.i (LocalGet (nr 0l)) in
        compile_const_64 0L ^^
        get_n ^^
        G.i (Binary (Wasm.Values.I64 I64Op.Sub))
      )
  | NegOp, Type.Prim Type.Word32 ->
      SR.UnboxedInt32,
      Func.share_code env "neg32" ["n", I32Type] [I32Type] (fun env ->
        let get_n = G.i (LocalGet (nr 0l)) in
        compile_unboxed_zero ^^
        get_n ^^
        G.i (Binary (Wasm.Values.I32 I32Op.Sub))
      )
  | PosOp, Type.Prim (Type.Int | Type.Nat) ->
      SR.UnboxedInt64,
      G.nop
  | PosOp, Type.Prim Type.Word32 ->
      SR.UnboxedInt32,
      G.nop
  | _ -> todo "compile_unop" (Arrange.unop op) (SR.Vanilla, G.i Unreachable)
  )

(* This returns a single StackRep, to be used for both arguments and the
   result. One could imagine operators that require or produce different StackReps,
   but none of these do, so a single value is fine.
*)
let compile_binop env t op =
  StackRep.of_type t,
  Syntax.(match t, op with
  | Type.Prim Type.Nat, AddOp -> G.i (Binary (Wasm.Values.I64 I64Op.Add))
  | Type.Prim Type.Nat, SubOp ->
    Func.share_code env "nat_sub" ["n1", I64Type; "n2", I64Type] [I64Type] (fun env ->
      let get_n1 = G.i (LocalGet (nr 0l)) in
      let get_n2 = G.i (LocalGet (nr 1l)) in
      get_n1 ^^ get_n2 ^^ G.i (Compare (Wasm.Values.I64 I64Op.LtU)) ^^
      G.if_ (StackRep.to_block_type env SR.UnboxedInt64)
        (G.i Unreachable)
        (get_n1 ^^ get_n2 ^^ G.i (Binary (Wasm.Values.I64 I64Op.Sub)))
    )
  | Type.Prim Type.Nat, MulOp -> G.i (Binary (Wasm.Values.I64 I64Op.Mul))
  | Type.Prim Type.Nat, DivOp -> G.i (Binary (Wasm.Values.I64 I64Op.DivU))
  | Type.Prim Type.Nat, ModOp -> G.i (Binary (Wasm.Values.I64 I64Op.RemU))
  | Type.Prim Type.Int, AddOp -> G.i (Binary (Wasm.Values.I64 I64Op.Add))
  | Type.Prim Type.Int, SubOp -> G.i (Binary (Wasm.Values.I64 I64Op.Sub))
  | Type.Prim Type.Int, MulOp -> G.i (Binary (Wasm.Values.I64 I64Op.Mul))
  | Type.Prim Type.Int, DivOp -> G.i (Binary (Wasm.Values.I64 I64Op.DivU))
  | Type.Prim Type.Int, ModOp -> G.i (Binary (Wasm.Values.I64 I64Op.RemU))
  | Type.Prim Type.Text, CatOp -> Text.concat env
  | _ -> todo "compile_binop" (Arrange.binop op) (G.i Unreachable)
  )

let compile_eq env t = match t with
  | Type.Prim Type.Text -> Text.compare env
  | Type.Prim Type.Bool -> G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | Type.Prim (Type.Nat | Type.Int) -> G.i (Compare (Wasm.Values.I64 I64Op.Eq))
  | Type.Prim Type.Word32 -> G.i (Compare (Wasm.Values.I32 I32Op.Eq))
  | _ -> todo "compile_eq" (Arrange.relop Syntax.EqOp) (G.i Unreachable)

let compile_relop env t op =
  StackRep.of_type t,
  Syntax.(match t, op with
  | _, EqOp -> compile_eq env t
  | _, NeqOp -> compile_eq env t ^^
             G.if_ (StackRep.to_block_type env SR.bool)
                   (Bool.lit false) (Bool.lit true)
  | Type.Prim Type.Nat, GeOp -> G.i (Compare (Wasm.Values.I64 I64Op.GeU))
  | Type.Prim Type.Nat, GtOp -> G.i (Compare (Wasm.Values.I64 I64Op.GtU))
  | Type.Prim Type.Nat, LeOp -> G.i (Compare (Wasm.Values.I64 I64Op.LeU))
  | Type.Prim Type.Nat, LtOp -> G.i (Compare (Wasm.Values.I64 I64Op.LtU))
  | Type.Prim Type.Int, GeOp -> G.i (Compare (Wasm.Values.I64 I64Op.GeS))
  | Type.Prim Type.Int, GtOp -> G.i (Compare (Wasm.Values.I64 I64Op.GtS))
  | Type.Prim Type.Int, LeOp -> G.i (Compare (Wasm.Values.I64 I64Op.LeS))
  | Type.Prim Type.Int, LtOp -> G.i (Compare (Wasm.Values.I64 I64Op.LtS))
  | _ -> todo "compile_relop" (Arrange.relop op) (G.i Unreachable)
  )


(* compile_lexp is used for expressions on the left of an
assignment operator, produces some code (with sideffect), and some pure code *)
let rec compile_lexp (env : E.t) exp =
  (fun (sr,code) -> (sr, G.with_region exp.at code)) @@
  match exp.it with
  | VarE var ->
     G.nop,
     Var.set_val env var.it
  | IdxE (e1,e2) ->
     compile_exp_vanilla env e1 ^^ (* offset to array *)
     compile_exp_as env SR.UnboxedInt64 e2 ^^ (* idx *)
     G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
     Array.idx env,
     store_ptr
  | DotE (e, n) ->
     compile_exp_vanilla env e ^^
     (* Only real objects have mutable fields, no need to branch on the tag *)
     Object.idx env e.note.note_typ n,
     store_ptr
  | _ -> todo "compile_lexp" (Arrange_ir.exp exp) (G.i Unreachable, G.nop)

and compile_exp (env : E.t) exp =
  (fun (sr,code) -> (sr, G.with_region exp.at code)) @@
  match exp.it with
  | IdxE (e1, e2)  ->
    SR.Vanilla,
    compile_exp_vanilla env e1 ^^ (* offset to array *)
    compile_exp_as env SR.UnboxedInt64 e2 ^^ (* idx *)
    G.i (Convert (Wasm.Values.I32 I32Op.WrapI64)) ^^
    Array.idx env ^^
    load_ptr
  | DotE (e, ({it = Name n;_} as name)) ->
    SR.Vanilla,
    compile_exp_vanilla env e ^^
    begin match Array.fake_object_idx env n with
    | None -> Object.load_idx env e.note.note_typ name
    | Some array_code ->
      let (set_o, get_o) = new_local env "o" in
      set_o ^^
      get_o ^^
      Tagged.branch env (ValBlockType (Some I32Type)) (
        [ Tagged.Object, get_o ^^ Object.load_idx env e.note.note_typ name
        ; Tagged.Array, get_o ^^ array_code ]
       )
    end
  | ActorDotE (e, ({it = Name n;_} as name)) ->
    SR.UnboxedReference,
    if E.mode env <> DfinityMode then G.i Unreachable else
    compile_exp_as env SR.UnboxedReference e ^^
    actor_fake_object_idx env {name with it = n}
  (* We only allow prims of certain shapes, as they occur in the prelude *)
  (* Binary prims *)
  | CallE (_, ({ it = PrimE p; _} as pe), _, { it = TupE [e1;e2]; _}) ->
    SR.Vanilla,
    begin
     compile_exp_vanilla env e1 ^^
     compile_exp_vanilla env e2 ^^
     match p with
      | "Array.init" -> Array.init env
      | "Array.tabulate" -> Array.tabulate env
      | _ -> todo "compile_exp" (Arrange_ir.exp pe) (G.i Unreachable)
    end
  (* Unary prims *)
  | CallE (_, ({ it = PrimE p; _} as pe), _, e) ->
    begin
      match p with
       | "abs" ->
         SR.Vanilla,
         compile_exp_vanilla env e ^^
         Prim.prim_abs env
       | "Nat->Word32" ->
         SR.UnboxedInt32,
         compile_exp_as env SR.UnboxedInt64 e ^^
         Prim.prim_nat32toWord32 env
       | "Word32->Nat" ->
         SR.UnboxedInt64,
         compile_exp_as env SR.UnboxedInt32 e ^^
         Prim.prim_word32toNat32 env
       | "Int->Word32" ->
         SR.UnboxedInt32,
         compile_exp_as env SR.UnboxedInt64 e ^^
         Prim.prim_int32toWord32 env
       | "Word32->Int" ->
         SR.UnboxedInt64,
         compile_exp_as env SR.UnboxedInt32 e ^^
         Prim.prim_word32toInt32 env
       | "printInt" ->
         SR.unit,
         compile_exp_vanilla env e ^^
         Dfinity.prim_printInt env
       | "print" ->
         SR.unit,
         compile_exp_vanilla env e ^^
         Dfinity.prim_print env
       | _ ->
         SR.Unreachable,
         todo "compile_exp" (Arrange_ir.exp pe) (G.i Unreachable)
    end
  | VarE var ->
    Var.get_val env var.it
  | AssignE (e1,e2) ->
    SR.unit,
    let (prepare_code, store_code) = compile_lexp env e1  in
    prepare_code ^^
    compile_exp_vanilla env e2 ^^
    store_code
  | LitE l ->
    compile_lit env l
  | AssertE e1 ->
    SR.unit,
    compile_exp_as env SR.bool e1 ^^
    G.if_ (ValBlockType None) G.nop (G.i Unreachable)
  | UnE (t, op, e1) ->
    let sr, code = compile_unop env t op in
    sr,
    compile_exp_as env sr e1 ^^
    code
  | BinE (t, e1, op, e2) ->
    let sr, code = compile_binop env t op in
    sr,
    compile_exp_as env sr e1 ^^
    compile_exp_as env sr e2 ^^
    code
  | RelE (t, e1, op, e2) ->
    let sr, code = compile_relop env t op in
    SR.bool,
    compile_exp_as env sr e1 ^^
    compile_exp_as env sr e2 ^^
    code
  | IfE (scrut, e1, e2) ->
    let code_scrut = compile_exp_as env SR.bool scrut in
    let sr1, code1 = compile_exp env e1 in
    let sr2, code2 = compile_exp env e2 in
    let sr = StackRep.join sr1 sr2 in
    sr,
    code_scrut ^^ G.if_
      (StackRep.to_block_type env sr)
      (code1 ^^ StackRep.adjust env sr1 sr)
      (code2 ^^ StackRep.adjust env sr2 sr)
  | BlockE decs ->
    compile_decs env decs
  | LabelE (name, _ty, e) ->
    (* The value here can come from many places -- the expression,
       or any of the nested returns. Hard to tell which is the best
       stack representation here.
       So let’s go with Vanialla. *)
    SR.Vanilla,
    G.block_ (StackRep.to_block_type env SR.Vanilla) (
      G.with_current_depth (fun depth ->
        let env1 = E.add_label env name depth in
        compile_exp_vanilla env1 e
      )
    )
  | BreakE (name, e) ->
    let d = E.get_label_depth env name in
    SR.Unreachable,
    compile_exp_vanilla env e ^^
    G.branch_to_ d
  | LoopE (e, None) ->
    SR.Unreachable,
    G.loop_ (ValBlockType None) (compile_exp_unit env e ^^ G.i (Br (nr 0l))
    )
    ^^
   G.i Unreachable
  | LoopE (e1, Some e2) ->
    SR.unit,
    G.loop_ (ValBlockType None) (
      compile_exp_unit env e1 ^^
      compile_exp_as env SR.bool e2 ^^
      G.if_ (ValBlockType None) (G.i (Br (nr 1l))) G.nop
    )
  | WhileE (e1, e2) ->
    SR.unit,
    G.loop_ (ValBlockType None) (
      compile_exp_as env SR.bool e1 ^^
      G.if_ (ValBlockType None) (
        compile_exp_unit env e2 ^^
        G.i (Br (nr 1l))
      ) G.nop
    )
  | RetE e ->
    SR.Unreachable,
    compile_exp_as env (StackRep.of_arity (E.get_n_res env)) e ^^
    G.i Return
  | OptE e ->
    SR.Vanilla,
    Opt.inject env (compile_exp_vanilla env e)
  | TupE es ->
    SR.UnboxedTuple (List.length es),
    G.concat_map (compile_exp_vanilla env) es
  | ProjE (e1,n) ->
    SR.Vanilla,
    compile_exp_vanilla env e1 ^^ (* offset to tuple (an array) *)
    Tuple.load_n (Int32.of_int n)
  | ArrayE (m, t, es) ->
    SR.Vanilla, Array.lit env (List.map (compile_exp_vanilla env) es)
  | ActorE (name, fs, _) ->
    SR.UnboxedReference,
    let captured = Freevars.exp exp in
    let prelude_names = find_prelude_names env in
    if Freevars.M.is_empty (Freevars.diff captured prelude_names)
    then actor_lit env name fs exp.at
    else todo "non-closed actor" (Arrange_ir.exp exp) G.i Unreachable
  | CallE (cc, e1, _, e2) ->
    StackRep.of_arity (cc.Value.n_res),
    let fun_sr, code1 = compile_exp env e1 in
    begin match fun_sr, cc.Value.sort with
     | SR.StaticThing (SR.StaticFun fi), _ ->
        code1 ^^
        compile_unboxed_zero ^^ (* A dummy closure *)
        compile_exp_as env (StackRep.of_arity cc.Value.n_args) e2 ^^ (* the args *)
        G.i (Call (nr fi))
     | _, Type.Local ->
        let (set_clos, get_clos) = new_local env "clos" in
        code1 ^^ StackRep.adjust env fun_sr SR.Vanilla ^^
        set_clos ^^
        get_clos ^^
        compile_exp_as env (StackRep.of_arity cc.Value.n_args) e2 ^^
        get_clos ^^
        Closure.call_closure env cc
     | _, Type.Sharable ->
        let (set_funcref, get_funcref) = new_local env "funcref" in
        code1 ^^ StackRep.adjust env fun_sr SR.UnboxedReference ^^
        set_funcref ^^
        compile_exp_as env (StackRep.of_arity cc.Value.n_args) e2 ^^
        Serialization.serialize_n env cc.Value.n_args ^^
        FuncDec.call_funcref env cc get_funcref
    end
  | SwitchE (e, cs) ->
    SR.Vanilla,
    let code1 = compile_exp_vanilla env e in
    let (set_i, get_i) = new_local env "switch_in" in
    let (set_j, get_j) = new_local env "switch_out" in

    let rec go env cs = match cs with
      | [] -> CanFail (fun k -> k)
      | (c::cs) ->
          let pat = c.it.pat in
          let e = c.it.exp in
          let (env1, code) = compile_pat_local env pat in
          orElse ( CannotFail get_i ^^^ code ^^^
                   CannotFail (compile_exp_vanilla env1 e) ^^^ CannotFail set_j)
                 (go env cs)
          in
      let code2 = go env cs in
      code1 ^^ set_i ^^ orTrap code2 ^^ get_j
  | ForE (p, e1, e2) ->
    SR.unit,
    let code1 = compile_exp_vanilla env e1 in
    let (env1, code2) = compile_mono_pat env p in
    let code3 = compile_exp_unit env1 e2 in

    let (set_i, get_i) = new_local env "iter" in
    (* Store the iterator *)
    code1 ^^
    set_i ^^

    G.loop_ (ValBlockType None) (
      get_i ^^
      Object.load_idx_immut env1 (nr_ (Name "next")) ^^
      get_i ^^
      Object.load_idx_immut env1 (nr_ (Name "next")) ^^
      Closure.call_closure env1 (Value.local_cc 0 1) ^^
      let (set_oi, get_oi) = new_local env "opt" in
      set_oi ^^

      (* Check for null *)
      get_oi ^^
      Opt.null ^^
      G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
      G.if_ (ValBlockType None)
        G.nop
        ( get_oi ^^ Opt.project ^^
          code2 ^^ code3 ^^ G.i (Br (nr 1l))
        )
    )
  (* Async-wait lowering support features *)
  | DeclareE (name, _, e) ->
    let (env1, i) = E.add_local_with_offset env name.it 1l in
    let sr, code = compile_exp env1 e in
    sr,
    Tagged.obj env Tagged.MutBox [ compile_unboxed_const 0l ] ^^
    G.i (LocalSet (nr i)) ^^
    code
  | DefineE (name, _, e) ->
    SR.unit,
    compile_exp_vanilla env e ^^
    Var.set_val env name.it
  | NewObjE (Type.Object _ (*sharing*), fs, _) ->
    SR.Vanilla,
    let fs' = fs |> List.map
      (fun (name, id) -> (name, fun env ->
        if Object.is_mut_field env exp.note.note_typ name
        then Var.get_val_ptr env id.it
        else Var.get_val_vanilla env id.it)) in
    Object.lit_raw env fs'
  | _ -> SR.unit, todo "compile_exp" (Arrange_ir.exp exp) (G.i Unreachable)

and compile_exp_as env sr_out e =
  G.with_region e.at (
    let sr_in, code = compile_exp env e in
    code ^^ StackRep.adjust env sr_in sr_out
  )

and compile_exp_as_opt env sr_out_o e =
  let sr_in, code = compile_exp env e in
  G.with_region e.at (
    code ^^
    match sr_out_o with
    | None -> StackRep.drop env sr_in
    | Some sr_out -> StackRep.adjust env sr_in sr_out
  )

and compile_exp_vanilla (env : E.t) exp =
  compile_exp_as env SR.Vanilla exp

and compile_exp_unit (env : E.t) exp =
  compile_exp_as env SR.unit exp


(*
The compilation of declarations (and patterns!) needs to handle mutual recursion.
This requires conceptually thre passes:
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
    BoxedInt.unbox env ^^
    compile_lit_as env SR.UnboxedInt64 l ^^
    compile_eq env (Type.Prim Type.Nat)
  | Syntax.(TextLit t) ->
    Text.lit env t ^^
    Text.compare env
  | _ -> todo "compile_lit_pat" (Arrange.lit l) (G.i Unreachable)

and fill_pat env pat : patternCode =
  PatCode.with_region pat.at @@
  match pat.it with
  | WildP -> CannotFail (G.i Drop)
  | OptP p ->
      let code1 = fill_pat env p in
      let (set_i, get_i) = new_local env "opt_scrut" in
      CanFail (fun fail_code ->
        set_i ^^
        get_i ^^
        Opt.null ^^
        G.i (Compare (Wasm.Values.I32 I32Op.Eq)) ^^
        G.if_ (ValBlockType None) fail_code
          ( get_i ^^
            Opt.project ^^
            with_fail fail_code code1
          )
      )
  | LitP l ->
      CanFail (fun fail_code ->
        compile_lit_pat env l ^^
        G.if_ (ValBlockType None) G.nop fail_code)
  | VarP name ->
      CannotFail (Var.set_val env name.it)
  | TupP ps ->
      let (set_i, get_i) = new_local env "tup_scrut" in
      let rec go i ps env = match ps with
        | [] -> CannotFail G.nop
        | (p::ps) ->
          let code1 = fill_pat env p in
          let code2 = go (i+1) ps env in
          ( CannotFail (get_i ^^ Tuple.load_n (Int32.of_int i)) ^^^
            code1 ^^^
            code2 ) in
      CannotFail set_i ^^^ go 0 ps env
  | AltP (p1, p2) ->
      let code1 = fill_pat env p1 in
      let code2 = fill_pat env p2 in
      let (set_i, get_i) = new_local env "alt_scrut" in
      CannotFail set_i ^^^
      orElse (CannotFail get_i ^^^ code1)
             (CannotFail get_i ^^^ code2)

and alloc_pat_local env pat =
  let (_,d) = Freevars.pat pat in
  AllocHow.S.fold (fun v env ->
    let (env1, _i) = E.add_direct_local env  v
    in env1
  ) d env

and alloc_pat env how pat =
  (fun (env,code) -> (env, G.with_region pat.at code)) @@
  let (_,d) = Freevars.pat pat in
  AllocHow.S.fold (fun v (env,code0) ->
    let (env1, code1) = AllocHow.add_local env how v
    in (env1, code0 ^^ code1)
  ) d (env, G.nop)

and compile_pat_local env pat : E.t * patternCode =
  (* It returns:
     - the extended environment
     - the code to do the pattern matching.
       This expects the  undestructed value is on top of the stack,
       consumes it, and fills the heap
       If the pattern does not match, it branches to the depth at fail_depth.
  *)
  let env1 = alloc_pat_local env pat in
  let fill_code = fill_pat env1 pat in
  (env1, fill_code)

(* Used for mono patterns (ForE) *)
and compile_mono_pat env pat =
  let (env1, fill_code) = compile_pat_local env pat in
  (env1, orTrap fill_code)

(* Used for let patterns: If the patterns is an n-ary tuple pattern,
   we want to compile the expression accordingly, to avoid the reboxing.
*)
and compile_n_ary_pat env how pat =
  (* It returns:
     - the extended environment
     - the code to allocate memory
     - the arity
     - the code to do the pattern matching.
       This expects the  undestructed value is on top of the stack,
       consumes it, and fills the heap
       If the pattern does not match, it branches to the depth at fail_depth.
  *)
  let (env1, alloc_code) = alloc_pat env how pat in
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
      G.concat_mapi (fun i p -> orTrap (fill_pat env1 p)) (List.rev ps)
    (* The general case: Create a single value, match that. *)
    | _ ->
      Some SR.Vanilla,
      orTrap (fill_pat env1 pat)
  in (env1, alloc_code, arity, fill_code)

(* Used for function patterns
   The complication is that functions are n-ary, and we get the elements
   separately.
   If the function is unary, that’s great.
   If the pattern is a tuple pattern, that is great as well.
   But if not, we need to construct the tuple first.
*)
and compile_func_pat env cc pat =
  let env1 = alloc_pat_local env pat in
  let fill_code get =
    G.with_region pat.at @@
    if cc.Value.n_args = 1
    then
      (* Easy case: unary *)
      get 0 ^^ orTrap (fill_pat env1 pat)
    else
      match pat.it with
      (* Another easy case: Nothing to match *)
      | WildP -> G.nop
      (* The good case: We have a tuple pattern *)
      | TupP ps ->
        assert (List.length ps = cc.Value.n_args);
        G.concat_mapi (fun i p -> get i ^^ orTrap (fill_pat env1 p)) ps
      (* The general case: Construct the tuple, and apply the full pattern *)
      | _ ->
        Array.lit env (Lib.List.table cc.Value.n_args (fun i -> get i)) ^^
        orTrap (fill_pat env1 pat) in
  (env1, fill_code)

and compile_dec pre_env how dec : E.t * G.t * (E.t -> (SR.t * G.t)) =
  (fun (pre_env,alloc_code,mk_code) ->
       (pre_env, G.with_region dec.at alloc_code, fun env ->
         (fun (sr, code) -> (sr, G.with_region dec.at code)) (mk_code env))) @@
  match dec.it with
  | TypD _ ->
    (pre_env, G.nop, fun _ -> SR.unit, G.nop)
  | ExpD e ->(pre_env, G.nop, fun env -> compile_exp env e)

  (* A special case for static expressions *)
  | LetD ({it = VarP v; _}, e) when not (AllocHow.M.mem v.it how) ->
    let (static_thing, fill) = compile_static_exp pre_env how e in
    let d = StackRep.deferred_of_static_think pre_env static_thing in
    let pre_env1 = E.add_local_deferred pre_env v.it d in
    ( pre_env1, G.nop, fun env -> fill env; (SR.unit, G.nop))
  | LetD (p, e) ->
    let (pre_env1, alloc_code, pat_arity, fill_code) = compile_n_ary_pat pre_env how p in
    ( pre_env1, alloc_code, fun env ->
      SR.unit,
      compile_exp_as_opt env pat_arity e ^^
      fill_code
    )
  | VarD (name, e) ->
      assert (AllocHow.M.find_opt name.it how = Some AllocHow.LocalMut ||
              AllocHow.M.find_opt name.it how = Some AllocHow.StoreHeap);
      let (pre_env1, alloc_code) = AllocHow.add_local pre_env how name.it in

      ( pre_env1, alloc_code, fun env ->
        SR.unit,
        compile_exp_vanilla env e ^^
        Var.set_val env name.it
      )
  | FuncD (cc, name, typ_binds, p, _rt, e) ->
      (* Get captured variables *)
      let captured = Freevars.captured p e in
      let mk_pat env1 = compile_func_pat env1 cc p in
      let mk_body env1 = compile_exp_as env1 (StackRep.of_arity cc.Value.n_res) e in
      let (pre_env1, alloc_code, mk_code) = FuncDec.dec pre_env how name cc captured mk_pat mk_body dec.at in
      (pre_env1, alloc_code, fun env ->
        (* Bring type parameters into scope *)
        let sr, code = Var.get_val env name.it in
        sr, mk_code env ^^ code
      )

and compile_decs env decs : SR.t * G.t =
  snd (compile_decs_block env decs)

and compile_decs_block env decs : (E.t * (SR.t * G.t)) =
  let how = AllocHow.decs env decs in
  let rec go pre_env decs = match decs with
    | []          -> (pre_env, G.nop, fun _ -> (SR.unit, G.nop))
    | [dec]       -> compile_dec pre_env how dec
    | (dec::decs) ->
        let (pre_env1, alloc_code1, mk_code1) = compile_dec pre_env how dec in
        let (pre_env2, alloc_code2, mk_code2) = go          pre_env1 decs in
        ( pre_env2,
          alloc_code1 ^^ alloc_code2,
          fun env ->
            let (sr1, code1) = mk_code1 env in
            let (sr2, code2) = mk_code2 env in
            (sr2, code1 ^^ StackRep.drop env sr1 ^^ code2)
         ) in
  let (env1, alloc_code, mk_code) = go env decs in
  let (sr, code) = mk_code env1 in
  (env1, (sr, alloc_code ^^ code))

and compile_static_exp env how exp = match exp.it with
  | BlockE [{ it = FuncD (cc, name, typ_binds, p, _rt, e); _}] ->
      (* Get captured variables *)
      let mk_pat env1 = compile_func_pat env1 cc p in
      let mk_body env1 = compile_exp_as env1 (StackRep.of_arity cc.Value.n_res) e in
      let (env1, fill) = FuncDec.dec_closed env cc name mk_pat mk_body exp.at in
      begin match Var.get_val env1 name.it with
      | (SR.StaticThing st, _) -> (st, fill)
      | _ -> assert false
      end
  | _ -> assert false

and compile_prelude env =
  (* Allocate the primitive functions *)
  let (decs, _flavor) = E.get_prelude env in
  let (env1, (sr, code)) = compile_decs_block env decs in
  (env1, code ^^ StackRep.drop env sr)

(* Is this a hack? When determining whether an actor is closed,
we should disregard the prelude, because every actor is compiled with the
prelude. So this function compiles the prelude, just to find out the bound names.
*)
and find_prelude_names env =
  (* Create a throw-away environment *)
  let env1 = E.mk_fun_env (E.mk_global (E.mode env) (E.get_prelude env) 0l) 0l 0 in
  let (env2, _) = compile_prelude env1 in
  E.in_scope_set env2


and compile_start_func env (progs : Ir.prog list) : E.func_with_names =
  Func.of_body env [] [] (fun env1 ->
    let rec go env = function
      | [] -> G.nop
      | ((decs, _flavor) :: progs) ->
        let (env1, (sr, code1)) = compile_decs_block env decs in
        let code2 = go env1 progs in
        code1 ^^ StackRep.drop env1 sr ^^ code2 in
    go env1 progs
    )

and compile_private_actor_field pre_env (f : Ir.exp_field)  =
  let ptr = E.reserve_static_memory pre_env (Int32.mul 2l Heap.word_size) in
  let pre_env1 = E.add_local_static pre_env f.it.id.it (Int32.add Heap.word_size ptr) in
  ( pre_env1, fun env -> G.with_region f.at @@
    compile_unboxed_const ptr ^^
    Tagged.store Tagged.MutBox ^^

    compile_unboxed_const ptr ^^
    compile_exp_vanilla env f.it.exp ^^
    Var.store
  )

and compile_public_actor_field pre_env (f : Ir.exp_field) =
  let (cc, name, _, pat, _rt, exp) =
    let find_func exp = match exp.it with
    | BlockE [{it = FuncD (cc, name, ty_args, pat, rt, exp); _ }] ->
      (cc, name, ty_args, pat, rt, exp)
    | _ -> assert false (* "public actor field not a function" *)
    in find_func f.it.exp in

  let (fi, fill) = E.reserve_fun pre_env name.it in
  E.add_dfinity_type pre_env (fi, Lib.List.make cc.Value.n_args CustomSections.ElemBuf);
  E.add_export pre_env (nr {
    name = Dfinity.explode name.it;
    edesc = nr (FuncExport (nr fi))
  });
  let pre_env1 = E.add_local_deferred_vanilla pre_env name.it
    (fun env -> FuncDec.static_self_message_pointer env name) in

  ( pre_env1, fun env -> G.with_region f.at @@
    let mk_pat inner_env = compile_func_pat inner_env cc pat in
    let mk_body inner_env = compile_exp_as inner_env (StackRep.of_arity cc.Value.n_res) exp in
    let f = FuncDec.compile_static_message env cc mk_pat mk_body f.at in
    fill f;
    G.nop
  )

and compile_actor_field pre_env (f : Ir.exp_field) =
  if f.it.vis.it = Syntax.Private
  then compile_private_actor_field pre_env f
  else compile_public_actor_field pre_env f

and compile_actor_fields env fs =
  (* We need to tie the knot about the enrivonment *)
  let rec go env = function
    | []          -> (env, fun _ -> G.nop)
    | (f::fs) ->
        let (env1, mk_code1) = compile_actor_field env f in
        let (env2, mk_code2) = go env1 fs in
        (env2, fun env -> mk_code1 env ^^ mk_code2 env) in
  let (env1, mk_code2) = go env fs in
  (env1, mk_code2 env1)

and actor_lit outer_env name fs at =
  if E.mode outer_env <> DfinityMode then G.i Unreachable else

  let wasm_binary =
    let env = E.mk_global (E.mode outer_env) (E.get_prelude outer_env) ClosureTable.table_end in

    if E.mode env = DfinityMode then Dfinity.system_imports env;
    Array.common_funcs env;

    let start_fun = Func.of_body env [] [] (fun env3 -> G.with_region at @@
      (* Compile stuff here *)
      let (env4, prelude_code) = compile_prelude env3 in

      (* Bind the self pointer *)
      let env5 = E.add_local_deferred_vanilla env4 name.it Dfinity.get_self_reference in
      let (_env6, init_code)  = compile_actor_fields env5 fs in

      prelude_code ^^ init_code) in
    let start_fi = E.add_fun env start_fun "start" in

    OrthogonalPersistence.register env start_fi;

    let m = conclude_module env name.it None in
    let (_map, wasm_binary) = CustomModule.encode m in
    wasm_binary in

    Dfinity.compile_databuf_of_bytes outer_env wasm_binary ^^
    (* Create actorref *)
    G.i (Call (nr (Dfinity.module_new_i outer_env))) ^^
    G.i (Call (nr (Dfinity.actor_new_i outer_env)))

and actor_fake_object_idx env name =
    Dfinity.compile_databuf_of_bytes env (name.it) ^^
    G.i (Call (nr (Dfinity.actor_export_i env)))

and conclude_module env module_name start_fi_o =

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
      (* persistent elembuf for memory *)
      nr { gtype = GlobalType (I32Type, Mutable);
        value = nr (G.to_instr_list compile_unboxed_zero)
      };
      (* end-of-heap pointer *)
      nr { gtype = GlobalType (I32Type, Mutable);
        value = nr (G.to_instr_list (compile_unboxed_const (E.get_end_of_static_memory env)))
      };
      (* reference counter *)
      nr { gtype = GlobalType (I32Type, Mutable);
        value = nr (G.to_instr_list compile_unboxed_zero)
      };
      ] in

  let data = List.map (fun (offset, init) -> nr {
    index = nr 0l;
    offset = nr (G.to_instr_list (compile_unboxed_const offset));
    init;
    }) (E.get_static_memory env) in

  { module_ = nr {
      types = List.map nr (E.get_types env);
      funcs = List.map (fun (f,_,_) -> f) funcs;
      tables = [ nr { ttype = TableType ({min = table_sz; max = Some table_sz}, FuncRefType) } ];
      elems = [ nr {
        index = nr 0l;
        offset = nr (G.to_instr_list (compile_unboxed_const ni'));
        init = List.mapi (fun i _ -> nr (Wasm.I32.of_int_u (ni + i))) funcs } ];
      start = start_fi_o;
      globals = globals;
      memories = memories;
      imports = func_imports @ other_imports;
      exports = E.get_exports env;
      data
    };
    types = E.get_dfinity_types env;
    persist =
           [ (OrthogonalPersistence.mem_global, CustomSections.DataBuf)
           ; (OrthogonalPersistence.elem_global, CustomSections.ElemBuf)
           ];
    module_name;
    function_names =
	List.mapi (fun i (f,n,_) -> Int32.(add ni' (of_int i), n)) funcs;
    locals_names =
	List.mapi (fun i (f,_,ln) -> Int32.(add ni' (of_int i), ln)) funcs;
  }

let compile mode module_name (prelude : Ir.prog) (progs : Ir.prog list) : extended_module =
  let env = E.mk_global mode prelude ClosureTable.table_end in

  if E.mode env = DfinityMode then Dfinity.system_imports env;
  Array.common_funcs env;

  let start_fun = compile_start_func env (prelude :: progs) in
  let start_fi = E.add_fun env start_fun "start" in
  let start_fi_o =
    if E.mode env = DfinityMode
    then begin
      OrthogonalPersistence.register env start_fi;
      Dfinity.export_start_stub env;
      None
    end else Some (nr start_fi) in

  conclude_module env module_name start_fi_o
