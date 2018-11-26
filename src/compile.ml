open Wasm.Ast
open Wasm.Types

open Source
open Syntax

open CustomModule

module G = InstrList
let (^^) = G.(^^) (* is this how we do that? *)


(* Helper functions to produce annotated terms *)
let nr x = { Wasm.Source.it = x; Wasm.Source.at = Wasm.Source.no_region }
let (@@) x at =
  let left = { Wasm.Source.file = at.left.file;
    Wasm.Source.line = at.left.line;
    Wasm.Source.column = at.left.column } in
  let right = { Wasm.Source.file = at.right.file;
    Wasm.Source.line = at.right.line;
    Wasm.Source.column = at.right.column } in
  let at = { Wasm.Source.left = left; Wasm.Source.right = right } in
  { Wasm.Source.it = x; Wasm.Source.at = at }
let nr_ x = { it = x; at = no_region; note = () }
let nr__ x = { it = x; at = no_region; note = {note_typ = Type.Any; note_eff = Type.Triv } }


let todo fn se x = Printf.eprintf "%s: %s" fn (Wasm.Sexpr.to_string 80 se); x


(* The compiler environment.

It is almost immutable..

The mutable parts (`ref`) are used to register things like locals and
functions.  This should be monotone in the sense that entries are only added,
and that the order should not matter in a significant way.

*)

type mode = WasmMode | DfinityMode

(* Names can be referring to one of these things: *)
(* Most names are stored in heap locations stored in Locals.
   But some are special (static funcions, static messages of the current actor).
   These have no location (yet), but we need to generate one on demand.
 *)

type 'env deferred_loc =
  { allocate : 'env -> G.t
  ; is_direct_call : int32 option
    (* a little backdoor. coul be expanded into a general 'call' field *)
  }

type 'env varloc =
  (* A Wasm Local of the current function, that points to memory location,
     with an offset to the actual data. *)
  | Local of (int32 * int32)
  (* A static memory location in the current module *)
  | Static of int32
  (* Dynamic code to allocate the expression, valid in the current module
     (need not be captured) *)
  | Deferred of 'env deferred_loc

module E = struct

  (* Utilities, internal to E *)
  let reg (ref : 'a list ref) (x : 'a) : int32 =
      let i = Wasm.I32.of_int_u (List.length !ref) in
      ref := !ref @ [ x ];
      i

  let reserve_promise (ref : 'a Lib.Promise.t list ref) : (int32 * ('a -> unit)) =
      let p = Lib.Promise.make () in
      let i = Wasm.I32.of_int_u (List.length !ref) in
      ref := !ref @ [ p ];
      (i, Lib.Promise.fulfill p)


  (* The environment type *)
  module NameEnv = Env.Make(String)
  type local_names = (int32 * string) list
  type func_with_names = func * local_names
  type lazy_built_in =
    | Declared of (int32 * (func_with_names -> unit))
    | Defined of int32
    | Pending of (unit -> func_with_names)
  type t = {
    mode : mode;

    (* Imports defined *)
    imports : import list ref;
    (* Exports defined *)
    exports : export list ref;
    (* Function defined in this module *)
    funcs : func Lib.Promise.t list ref;
    func_names : (int32 * string) list ref;
    func_locals : (int32 * local_names) list ref;
    (* Function number and fill function for built-in functions *)
    built_in_funcs : lazy_built_in NameEnv.t ref;
    (* Types registered in this module *)
    func_types : func_type list ref;
    (* Number of parameters in the current function, to calculate indices of locals *)
    n_param : int32;
    (* Types of locals *)
    locals : value_type list ref;
    local_names : (int32 * string) list ref;
    (* A mapping from jump label to their depth *)
    ld : G.depth NameEnv.t;
    (* Mapping ActorScript variables to WebAssembly locals, globals or functions *)
    local_vars_env : t varloc NameEnv.t;
    (* The prelude. We need to re-use this when compiling actors *)
    prelude : prog;
    (* Exports that need a custom type for the hypervisor *)
    dfinity_types : (int32 * CustomSections.type_ list) list ref;
    (* Where does static memory end and dynamic memory begin? *)
    end_of_static_memory : int32 ref;
    (* Static memory defined so far *)
    static_memory : (int32 * string) list ref;
    (* Sanity check: Nothing should bump end_of_static_memory once it has been read *)
    static_memory_frozen : bool ref;
  }

  let mode (e : t) = e.mode

  (* Indices of local variables *)
  let tmp_local env : var = nr (env.n_param) (* first local after the params *)
  let unary_closure_local env : var = nr 0l (* first param *)
  let unary_param_local env : var = nr 1l   (* second param *)

  (* The initial global environment *)
  let mk_global mode prelude dyn_mem : t = {
    mode;
    imports = ref [];
    exports = ref [];
    funcs = ref [];
    func_names = ref [];
    func_locals = ref [];
    built_in_funcs = ref NameEnv.empty;
    func_types = ref [];
    dfinity_types = ref [];
    (* Actually unused outside mk_fun_env: *)
    locals = ref [];
    local_names = ref [];
    local_vars_env = NameEnv.empty;
    n_param = 0l;
    ld = NameEnv.empty;
    prelude;
    end_of_static_memory = ref dyn_mem;
    static_memory = ref [];
    static_memory_frozen = ref false;
  }


  let is_non_local = function
    | Local _ -> false
    | Static _ -> true
    | Deferred _ -> true

  (* Resetting the environment for a new function *)
  let mk_fun_env env n_param =
    { env with
      locals = ref [I32Type]; (* the first tmp local *)
      local_names = ref [ n_param , "tmp" ];
      n_param = n_param;
      (* We keep all local vars that are bound to known functions or globals *)
      local_vars_env = NameEnv.filter (fun _ -> is_non_local) env.local_vars_env;
      ld = NameEnv.empty;
      }

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
      { env with local_vars_env = NameEnv.add name (Local (i, off)) env.local_vars_env }

  let add_local_with_offset (env : t) name off =
      let i = add_anon_local env I32Type in
      add_local_name env i name;
      (reuse_local_with_offset env name i off, i)

  let add_local_static (env : t) name ptr =
      { env with local_vars_env = NameEnv.add name (Static ptr) env.local_vars_env }

  let add_local_deferred (env : t) name d =
      { env with local_vars_env = NameEnv.add name (Deferred d) env.local_vars_env }

  let get_locals (env : t) = !(env.locals)
  let get_local_names (env : t) : (int32 * string) list = !(env.local_names)

  let in_scope_set (env : t) =
    let l = env.local_vars_env in
    NameEnv.fold (fun k _ -> Freevars.S.add k) l Freevars.S.empty

  let add_import (env : t) i =
    if !(env.funcs) = []
    then reg env.imports i
    else raise (Invalid_argument "add all imports before all functions!")

  let add_export (env : t) e = let _ = reg env.exports e in ()

  let add_dfinity_type (env : t) e = let _ = reg env.dfinity_types e in ()

  let reserve_fun (env : t) =
    let (j, fill) = reserve_promise env.funcs in
    let n = Wasm.I32.of_int_u (List.length !(env.imports)) in
    let fi = Int32.add j n in
    let fill_ (f, local_names) =
      fill f;
      let _ = reg env.func_locals (fi, local_names) in () in
    (fi, fill_)

  let add_fun (env : t) (f, local_names) =
    let (fi, fill) = reserve_fun env in
    fill (f, local_names);
    fi

  let add_fun_name (env : t) fi name =
    let _ = reg env.func_names (fi, name) in ()

  let built_in (env : t) name : int32 =
    match NameEnv.find_opt name !(env.built_in_funcs) with
    | None ->
        let (fi, fill) = reserve_fun env in
        add_fun_name env fi name;
        env.built_in_funcs := NameEnv.add name (Declared (fi, fill)) !(env.built_in_funcs);
        fi
    | Some (Declared (fi, _)) -> fi
    | Some (Defined fi) -> fi
    | Some (Pending mk_fun) ->
        let (fi, fill) = reserve_fun env in
        add_fun_name env fi name;
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


  let get_imports (env : t) = !(env.imports)
  let get_exports (env : t) = !(env.exports)
  let get_dfinity_types (env : t) = !(env.dfinity_types)
  let get_funcs (env : t) = List.map Lib.Promise.value !(env.funcs)
  let get_func_names (env : t) = !(env.func_names)

  let get_func_local_names (env : t) = !(env.func_locals)

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
    if !(env.static_memory_frozen) then raise (Invalid_argument "Static memory frozen");
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
end


(* General code generation functions:
   Rule of thumb: Here goes stuff that independent of the ActorScript AST.
*)

(* Function called compile_* return a list of instructions (and maybe other stuff) *)

let compile_unboxed_const i = G.i_ (Wasm.Ast.Const (nr (Wasm.Values.I32 i)))
let compile_unboxed_true =    compile_unboxed_const 1l
let compile_unboxed_false =   compile_unboxed_const 0l
let compile_unboxed_zero =    compile_unboxed_const 0l
let compile_unit = compile_unboxed_const 1l
(* This needs to be disjoint from all pointers *)
let compile_null = compile_unboxed_const 3l

(* Some common arithmetic *)
let compile_op_const op i =
    compile_unboxed_const i ^^
    G.i_ (Binary (Wasm.Values.I32 op))
let compile_add_const = compile_op_const Wasm.Ast.I32Op.Add
let compile_sub_const = compile_op_const Wasm.Ast.I32Op.Sub
let compile_mul_const = compile_op_const Wasm.Ast.I32Op.Mul
let compile_divU_const = compile_op_const Wasm.Ast.I32Op.DivU

(* Locals *)

let set_tmp env = G.i_ (SetLocal (E.tmp_local env))
let get_tmp env = G.i_ (GetLocal (E.tmp_local env))

let new_local_ env name =
  let i = E.add_anon_local env I32Type in
  E.add_local_name env i name;
  ( G.i_ (SetLocal (nr i))
  , G.i_ (GetLocal (nr i))
  , i
  )

let new_local env name =
  let (set_i, get_i, _) = new_local_ env name
  in (set_i, get_i)

(* Stack utilities *)

(* duplicate top element *)
let dup env : G.t = set_tmp env ^^ get_tmp env ^^ get_tmp env

(* Some code combinators *)

(* expects a number on the stack. Iterates from zero t below that number *)
let compile_while cond body =
    G.loop_ [] ( cond ^^ G.if_ [] (body ^^ G.i_ (Br (nr 1l))) G.nop)

let from_0_to_n env mk_body =
    let (set_n, get_n) = new_local env "n" in
    let (set_i, get_i) = new_local env "i" in
    set_n ^^
    compile_unboxed_const 0l ^^
    set_i ^^

    compile_while
      ( get_i ^^
        get_n ^^
        G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LtS))
      ) (
        mk_body get_i ^^

        get_i ^^
        compile_add_const 1l ^^
        set_i
      )


(* Heap and allocations *)

let load_ptr : G.t =
  G.i_ (Load {ty = I32Type; align = 2; offset = 0l; sz = None})

let store_ptr : G.t =
  G.i_ (Store {ty = I32Type; align = 2; offset = 0l; sz = None})

module Func = struct

  let of_body env params retty mk_body =
    let env1 = E.mk_fun_env env (Int32.of_int (List.length params)) in
    List.iteri (fun i n -> E.add_local_name env1 (Int32.of_int i) n) params;
    let ty = FuncType (List.map (fun _ -> I32Type) params, retty) in
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
    G.i_ (Call (nr (E.built_in env name)))

end (* Func *)

module Heap = struct

  (* General heap object functionalty (allocation, setting fields, reading fields) *)

  (* Until we have GC, we simply keep track of the end of the used heap
     in this global, and bump it if we allocate stuff.
     Memory addresses are 32 bit (I32Type).
     *)
  let word_size = 4l

  let heap_ptr = 2l

  let alloc_bytes (n : int32) : G.t =
    (* returns the pointer to the allocate space on the heap *)
    G.i_ (GetGlobal (nr heap_ptr)) ^^
    G.i_ (GetGlobal (nr heap_ptr)) ^^
    let aligned = Int32.logand (Int32.add n 3l) (Int32.lognot 3l) in
    compile_add_const aligned  ^^
    G.i_ (SetGlobal (nr heap_ptr))

  let alloc (n : int32) : G.t =
    alloc_bytes (Wasm.I32.mul word_size n)

  let load_field (i : int32) : G.t =
    G.i_ (Load {ty = I32Type; align = 2; offset = Wasm.I32.mul word_size i; sz = None})

  let store_field (i : int32) : G.t =
    G.i_ (Store {ty = I32Type; align = 2; offset = Wasm.I32.mul word_size i; sz = None})

  (* Create a heap object with instructions that fill in each word *)
  let obj env element_instructions : G.t =
    let n = List.length element_instructions in

    let (set_i, get_i) = new_local env "heap_object" in
    alloc (Wasm.I32.of_int_u n) ^^
    set_i ^^

    let compile_self = get_i in

    let init_elem idx instrs : G.t =
      compile_self ^^
      instrs ^^
      store_field (Wasm.I32.of_int_u idx)
    in
    G.concat_mapi init_elem element_instructions ^^

    compile_self

end (* Heap *)

module ElemHeap = struct
  let ref_counter = 3l

  let max_references = 1024l
  let ref_location = 0l

  let table_end : int32 = Int32.(add ref_location (mul max_references Heap.word_size))

  (* Assumes a reference on the stack, and replaces it with an index into the
     reference table *)
  let remember_reference env : G.t =
    Func.share_code env "remember_reference" ["ref"] [I32Type] (fun env ->
      let get_ref = G.i_ (GetLocal (nr 0l)) in

      (* Return index *)
      G.i_ (GetGlobal (nr ref_counter)) ^^

      (* Store reference *)
      G.i_ (GetGlobal (nr ref_counter)) ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const ref_location ^^
      get_ref ^^
      store_ptr ^^

      (* Bump counter *)
      G.i_ (GetGlobal (nr ref_counter)) ^^
      compile_add_const 1l ^^
      G.i_ (SetGlobal (nr ref_counter))
    )

  (* Assumes a index into the table on the stack, and replaces it with the reference *)
  let recall_reference env : G.t =
    Func.share_code env "recall_reference" ["ref_idx"] [I32Type] (fun env ->
      let get_ref_idx = G.i_ (GetLocal (nr 0l)) in
      get_ref_idx ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const ref_location ^^
      load_ptr
    )

end (* ElemHeap *)

module ClosureTable = struct
  let max_entries = 1024l
  let loc = ElemHeap.table_end
  let table_end = Int32.(add loc (mul max_entries Heap.word_size))

  let get_counter = compile_unboxed_const loc ^^ load_ptr
  let set_counter env =
    let (set_i, get_i) = new_local env "new_counter" in
    set_i ^^
    compile_unboxed_const loc ^^
    get_i ^^
    store_ptr

  (* Assumes a reference on the stack, and replaces it with an index into the
     reference table *)
  let remember_closure env : G.t =
    Func.share_code env "remember_closure" ["ptr"] [I32Type] (fun env ->
      let get_ptr = G.i_ (GetLocal (nr 0l)) in

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
      get_counter ^^
      compile_add_const 1l ^^
      set_counter env
    )

  (* Assumes a index into the table on the stack, and replaces it with a ptr to the closure *)
  let recall_closure env : G.t =
    Func.share_code env "recall_closure" ["closure_idx"] [I32Type] (fun env ->
      let get_closure_idx = G.i_ (GetLocal (nr 0l)) in
      get_closure_idx ^^
      compile_mul_const Heap.word_size ^^
      compile_add_const loc ^^
      load_ptr
    )

end (* ClosureTable *)

module BitTagged = struct
  (* Raw values x are stored as ( x << 1 | 1), i.e. with the LSB set.
     Pointers are stored as is (and should be aligned to have a zero there).
  *)

  (* Expect a possibly bit-tagged pointer on the stack.
     If taged, untags it and executes the first sequest.
     Otherwise, leaves it on the stack and executes the second sequence.
  *)
  let if_unboxed env retty is1 is2 =
    let (set_i, get_i) = new_local env "bittagged" in
    set_i ^^
    (* Check bit *)
    get_i ^^
    compile_unboxed_const 1l ^^
    G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.And)) ^^
    compile_unboxed_const 1l ^^
    G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
    G.if_ retty
      ( get_i ^^
        compile_unboxed_const 1l ^^
        G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.ShrU)) ^^
        is1)
      ( get_i ^^ is2)

  let tag =
    compile_unboxed_const 1l ^^
    G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Shl)) ^^
    compile_unboxed_const 1l ^^
    G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Or))

end (* BitTagged *)

module Tagged = struct
  (* Tagged objects have, well, a tag to describe their runtime type.
     This tag is used to traverse the heap (serialization, GC), but also
     for objectification of arrays and actorrefs and the like.

     All tagged heap objects have a size of at least two words.
   *)

  type tag =
    | Object
    | Array (* Also a tuple *)
    | Reference (* Either arrayref or funcref, no need to distinguish here *)
    | Int
    | MutBox (* used for local variables *)
    | Closure
    | Some (* For opt *)
    | Text
    | Indirection

  (* Lets leave out tag 0 to trap earlier on invalid memory *)
  let int_of_tag = function
    | Object -> 1l
    | Array -> 2l
    | Reference -> 3l
    | Int -> 4l
    | MutBox -> 5l
    | Closure -> 6l
    | Some -> 7l
    | Text -> 8l
    | Indirection -> 9l

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
    let (set_i, get_i) = new_local env "tagged" in

    let rec go = function
      | [] -> def
      | ((tag, code) :: cases) ->
        get_i ^^
        load ^^
        compile_unboxed_const (int_of_tag tag) ^^
        G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
        G.if_ retty (get_i ^^ code) (go cases)
    in
    set_i ^^
    go cases

  let branch env retty (cases : (tag * G.t) list) : G.t =
    branch_default env retty (G.i_ Unreachable) cases

  let obj env tag element_instructions : G.t =
    Heap.obj env (compile_unboxed_const (int_of_tag tag) :: element_instructions)
end


module Var = struct

  (* When accessing a variable that is a static function, then we need to create a
     heap-allocated closure-like thing on the fly. *)
  let static_fun_pointer fi env =
    Tagged.obj env Tagged.Closure [
      compile_unboxed_const fi;
      compile_unboxed_const 0l (* number of parameters: none *)
    ]

  (* Local variables may in general be mutable (or at least late-defined).
     So we need to add an indirection through the heap.
     We tag this indirection using Tagged.MutBox.
     (Although I am not yet entirely sure that this needs to be tagged. Do these
     ever show up in GC or serialization? I guess as part of closures.)
  *)
  let mutbox_field = Tagged.header_size
  let load = Heap.load_field mutbox_field
  let store = Heap.store_field mutbox_field

  let add_local env name =
    E.add_local_with_offset env name mutbox_field

  (* Returns the payload *)
  let get_val env var = match E.lookup_var env var with
    | Some (Local (i, off))  -> G.i_ (GetLocal (nr i)) ^^ Heap.load_field off
    | Some (Static i) -> compile_unboxed_const i ^^ load_ptr
    | Some (Deferred d) -> d.allocate env
    | None   -> G.i_ Unreachable

  (* Returns a pointer to the payload *)
  let get_payload_loc env var = match E.lookup_var env var with
    | Some (Local (i, off)) ->
      G.i_ (GetLocal (nr i)) ^^
      compile_add_const (Int32.mul Heap.word_size off)
    | Some (Static i) -> compile_unboxed_const i
    | Some (Deferred _) -> raise (Invalid_argument "Should not write to a deferred thing")
    | None -> G.i_ Unreachable

  (* Returns a pointer to the box, and code to restore it,
     including adding to the environment *)
  let capture env var : G.t * (E.t -> (E.t * G.t)) = match E.lookup_var env var with
    | Some (Local (i, off)) ->
      ( G.i_ (GetLocal (nr i))
      , fun env1 ->
        let (env2, j) = E.add_local_with_offset env1 var off in
        let restore_code = G.i_ (SetLocal (nr j))
        in (env2, restore_code)
      )
    | Some (Static i) ->
      ( compile_null , fun env1 -> (E.add_local_static env1 var i, G.i_ Drop))
    | Some (Deferred d) ->
      ( compile_null , fun env1 -> (E.add_local_deferred env1 var d, G.i_ Drop))
    | None -> (G.i_ Unreachable, fun env1 -> (env1, G.i_ Unreachable))

end (* Var *)

module Opt = struct

let payload_field = Tagged.header_size

let inject env e = Tagged.obj env Tagged.Some [e]
let project = Heap.load_field Tagged.header_size

end (* Opt *)


module Closure = struct
  let header_size = Int32.add Tagged.header_size 2l

  let funptr_field = Tagged.header_size
  let len_field = Int32.add 1l Tagged.header_size

  let first_captured = header_size

  let load_the_closure = G.i_ (GetLocal (nr 0l))
  let load_closure i = load_the_closure ^^ Heap.load_field (Int32.add first_captured i)
  let load_argument  = G.i_ (GetLocal (nr 1l))

  (* First argument is a pointer to the closure *)
  let ty env = E.func_type env (FuncType ([I32Type; I32Type],[I32Type]))

  let static_function_id fi =
    (* should be different from any pointer *)
    Int32.add (Int32.mul fi Heap.word_size) 1l

  let unary_of_body env mk_body =
    Func.of_body env ["clos"; "param"] [I32Type] mk_body

  (* The argument on the stack *)
  let call_direct env fi at =
   (* Pop the argument *)
   let (set_i, get_i) = new_local env "param" in
   set_i ^^

   (* First arg: The (unused) closure pointer *)
   compile_null ^^

   (* Second arg: The argument *)
   get_i ^^

   (* All done: Call! *)
   G.i (Call (nr fi) @@ at)

  (* Expect the function closure and the argument on the stack *)
  let call_indirect env at =
    (* Pop the argument *)
    let (set_i, get_i) = new_local env "call_arg" in
    set_i ^^

    (* Pop the closure pointer *)
    let (set_fi, get_fi) = new_local env "callee" in
    set_fi ^^

    get_fi ^^
    Tagged.branch env [I32Type] (
      [ Tagged.Closure,
        (* First arg: The closure pointer *)
        (* already on the stack *)
        (* Second arg: The argument *)
        get_i ^^
        (* And now get the table index *)
        get_fi ^^
        Heap.load_field funptr_field ^^
        (* All done: Call! *)
        G.i (CallIndirect (nr (ty env)) @@ at)
      ] @ (
      if E.mode env = DfinityMode
      then [ Tagged.Reference, (* a funcref! *)
             get_i ^^
             G.i_ (Call (nr (E.built_in env "call_funcref"))) ^^
             compile_unit
           ]
      else [])
    )

   (* Create a WebAssembly func from a pattern (for the argument) and the body.
   Parameter `captured` should contain the, well, captured local variables that
   the function will find in the closure. *)

  let compile_func env restore_env mk_pat mk_body at =
    unary_of_body env (fun env1 ->
      let get_closure = G.i (GetLocal (E.unary_closure_local env1) @@ at) in

      let (env2, closure_code) = restore_env env1 get_closure in

      (* Destruct the argument *)
      let (env3, alloc_args_code, destruct_args_code) = mk_pat env2  in

      (* Compile the body *)
      let body_code = mk_body env3 in

      closure_code ^^
      alloc_args_code ^^
      G.i (GetLocal (E.unary_param_local env3) @@ at ) ^^
      destruct_args_code ^^
      body_code)

  (* Compile a closed function declaration (has no free variables) *)
  let dec_closed pre_env last name mk_pat mk_body at =
      let (fi, fill) = E.reserve_fun pre_env in
      E.add_fun_name pre_env fi name.it;
      let d = { allocate = Var.static_fun_pointer fi; is_direct_call = Some fi } in
      let pre_env1 = E.add_local_deferred pre_env name.it d in
      ( pre_env1, G.nop, fun env ->
        let mk_body' env = mk_body env (compile_unboxed_const (static_function_id fi)) in
        let f = compile_func env (fun env1 _ -> (env1, G.nop)) mk_pat mk_body' at in
        fill f;
        if last then d.allocate env else G.nop)

  (* Compile a closure declaration (has free variables) *)
  let dec_closure pre_env last name captured mk_pat mk_body at =
      let (set_li, get_li) = new_local pre_env "clos_ind" in
      let (pre_env1, vi) = Var.add_local pre_env name.it in

      let len = Wasm.I32.of_int_u (List.length captured) in
      let alloc_code =
        (* Allocate a heap object for the function *)
        Heap.alloc (Int32.add header_size len) ^^
        set_li ^^

        (* Allocate an extra indirection for the variable *)
        Tagged.obj pre_env1 Tagged.MutBox [ get_li ] ^^
        G.i ( SetLocal (vi @@ at) @@ at )
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
                  Heap.store_field (Int32.add first_captured (Wasm.I32.of_int_u i)) ^^
                  store_rest in
                let restore_env env1 get_env =
                  let (env2, code) = restore_this env1 in
                  let (env3, code_rest) = restore_rest env2 get_env in
                  (env3,
                   get_env ^^
                   Heap.load_field (Int32.add first_captured (Wasm.I32.of_int_u i)) ^^
                   code ^^
                   code_rest
                  )
                in (store_env, restore_env) in
          go 0 captured in

	(* All functions are unary for now (arguments passed as heap-allocated tuples)
           with the closure itself passed as a first argument *)
        let mk_body' env = mk_body env load_the_closure in
        let f = compile_func env restore_env mk_pat mk_body' at in
        let fi = E.add_fun env f in
        E.add_fun_name env fi name.it;

        (* Store the tag *)
        get_li ^^
        Tagged.store Tagged.Closure ^^

        (* Store the function number: *)
        get_li ^^
        compile_unboxed_const fi ^^
        Heap.store_field funptr_field ^^

        (* Store the length *)
        get_li ^^
        compile_unboxed_const len ^^
        Heap.store_field len_field ^^

        (* Store all captured values *)
        store_env ^^
        if last then get_li  else G.nop)

  let fixed_closure env fi fields =
      Tagged.obj env Tagged.Closure
        ([ compile_unboxed_const fi
         ; compile_unboxed_const (Int32.of_int (List.length fields)) ] @
         fields)

  let dec pre_env last name captured mk_pat mk_body at =
    (* This could be smarter: It is ok to capture closed functions,
       but then we would have to move the call to compile_func in dec_closed
       above into the continuation. *)
    if captured = []
    then dec_closed pre_env last name mk_pat mk_body at
    else dec_closure pre_env last name captured mk_pat mk_body at

end (* Closure *)

module RTS = struct
  let memcpy env =
    Func.share_code env "memcpy" ["from"; "two"; "n"] [] (fun env ->
      let get_from = G.i_ (GetLocal (nr 0l)) in
      let get_to = G.i_ (GetLocal (nr 1l)) in
      let get_n = G.i_ (GetLocal (nr 2l)) in
      get_n ^^
      from_0_to_n env (fun get_i ->
          get_to ^^
          get_i ^^
          G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^

          get_from ^^
          get_i ^^
          G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
          G.i_ (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^

          G.i_ (Store {ty = I32Type; align = 0; offset = 0l; sz = Some Wasm.Memory.Pack8})
      )
    )

  let alloc_bytes env =
    Func.share_code env "alloc_bytes" ["n"] [I32Type] (fun env ->
      let get_n = G.i_ (GetLocal (nr 0l)) in

      (* expect the size (in words), returns the pointer *)
      G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
      G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
      get_n ^^
      (* align *)
      compile_add_const 3l ^^
      compile_unboxed_const (-1l) ^^
      compile_unboxed_const 3l ^^
      G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Xor)) ^^
      G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.And)) ^^
      (* add to old heap value *)
      G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
      G.i_ (SetGlobal (nr Heap.heap_ptr))
    )

  let alloc_words env =
    Func.share_code env "alloc_words" ["n"] [I32Type] (fun env ->
      let get_n = G.i_ (GetLocal (nr 0l)) in

      get_n ^^
      compile_mul_const Heap.word_size ^^
      alloc_bytes env
    )


end (* RTS *)

module BoxedInt = struct
  (* We store large nats and ints in immutable boxed 32bit heap objects.
     Eventually, this should contain the bigint implementation.

     Small values (<2^5, so that both paths are tested) are stored unboxed,
     tagged, see BitTagged.
  *)

  let payload_field = Int32.add Tagged.header_size 0l

  let box env = Func.share_code env "box_int" ["n"] [I32Type] (fun env ->
      let get_n = G.i_ (GetLocal (nr 0l)) in
      get_n ^^ compile_unboxed_const (Int32.of_int (1 lsl 5)) ^^
      G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LtU)) ^^
      G.if_ [I32Type]
        (get_n ^^ BitTagged.tag)
        (Tagged.obj env Tagged.Int [ G.i_ (GetLocal (nr 0l)) ])
    )
  let unbox env = Func.share_code env "unbox_int" ["n"] [I32Type] (fun env ->
      let get_n = G.i_ (GetLocal (nr 0l)) in
      get_n ^^
      BitTagged.if_unboxed env [I32Type]
        G.nop
        (Heap.load_field payload_field)
    )

  let lit env n = compile_unboxed_const n ^^ box env

  let lit_false env = lit env 0l
  let lit_true env = lit env 1l

  let lift_unboxed_unary env op_is =
    (* unbox argument *)
    unbox env ^^
    (* apply operator *)
    op_is ^^
    (* box result *)
    box env

  let lift_unboxed_binary env op_is =
    let (set_i, get_i) = new_local env "n" in
    (* unbox both arguments *)
    set_i ^^ unbox env ^^
    get_i ^^ unbox env ^^
    (* apply operator *)
    op_is ^^
    (* box result *)
    box env

end (* BoxedInt *)

(* Primitive functions *)
module Prim = struct

  let prim_abs env =
    let (set_i, get_i) = new_local env "abs_param" in
    set_i ^^
    get_i ^^
    BoxedInt.unbox env ^^
    compile_unboxed_zero ^^
    G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LtS)) ^^
    G.if_ [I32Type]
      ( compile_unboxed_zero ^^
        get_i ^^
        BoxedInt.unbox env ^^
        G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ^^
        BoxedInt.box env
      )
      ( get_i )

end (* Prim *)

module Object = struct
  (* First word: Class pointer (0x1, an invalid pointer, when none) *)
  let header_size = Int32.add Tagged.header_size 2l

  let class_position = Int32.add Tagged.header_size 0l
  (* Number of object fields *)
  let size_field = Int32.add Tagged.header_size 1l

  let hash_field_name (name : string) = Int32.of_int (Hashtbl.hash name)

  module FieldEnv = Env.Make(String)
  let lit env this_name_opt class_option fs =
    let name_pos_map =
      fs |>
      (* We could store only public fields in the object, but
         then we need to allocate separate boxes for the non-public ones:
         List.filter (fun (_, priv, f) -> priv.it = Public) |>
      *)
      List.map (fun (id,priv,_) -> (hash_field_name (id.it), id.it)) |>
      List.sort compare |>
      List.mapi (fun i (_h,n) -> (n,Int32.of_int i)) |>
      List.fold_left (fun m (n,i) -> FieldEnv.add n i m) FieldEnv.empty in

     let sz = Int32.of_int (FieldEnv.cardinal name_pos_map) in

     (* Allocate memory *)
     let (set_ri, get_ri, ri) = new_local_ env "obj" in
     Heap.alloc (Int32.add header_size (Int32.mul 2l sz)) ^^
     set_ri ^^

     (* Set tag *)
     get_ri ^^
     Tagged.store Tagged.Object ^^

     (* Write the class field *)
     get_ri ^^
     (match class_option with
       | Some class_instrs -> class_instrs
       | None -> compile_unboxed_const 1l ) ^^
     Heap.store_field class_position ^^

     (* Set size *)
     get_ri ^^
     compile_unboxed_const sz ^^
     Heap.store_field size_field ^^

    let hash_position env f =
        let i = FieldEnv.find f.it name_pos_map in
        Int32.add header_size (Int32.mul 2l i) in
    let field_position env f =
        let i = FieldEnv.find f.it name_pos_map in
        Int32.add header_size (Int32.add (Int32.mul 2l i) 1l) in

     (* Bind the fields in the envrionment *)
     let mk_field_ptr env (id, _, _) =
      E.reuse_local_with_offset env id.it ri (field_position env id) in
     let env1 = List.fold_left mk_field_ptr env fs in

     (* An extra indirection for the 'this' pointer, if present *)
     let (env2, this_code) = match this_name_opt with
      | Some name -> let (env2, ti) = Var.add_local env1 name.it in
                     (env2, Tagged.obj env1 Tagged.MutBox [ get_ri ] ^^
                            G.i_ (SetLocal (nr ti)))
      | None -> (env1, G.nop) in
     this_code ^^

     (* Write all the fields *)
     let init_field (id, _, mk_is) : G.t =
        match FieldEnv.find_opt id.it name_pos_map with
        | None -> G.nop
        | Some i ->
          (* Write the hash *)
          get_ri ^^
          compile_unboxed_const (hash_field_name id.it) ^^
          Heap.store_field (hash_position env id) ^^
          (* Write the value *)
          get_ri ^^
          mk_is env2 ^^
          Heap.store_field (field_position env id)
     in
     G.concat_map init_field fs ^^

     (* Return the pointer to the object *)
     get_ri

  (* This is for non-recursive objects, i.e. ObjNewE *)
  (* TODO: Remove duplication with above *)
  let lit_raw env fs =
    let name_pos_map =
      fs |>
      (* We could store only public fields in the object, but
         then we need to allocate separate boxes for the non-public ones:
         List.filter (fun (_, priv, f) -> priv.it = Public) |>
      *)
      List.map (fun (id,_) -> (hash_field_name id, id)) |>
      List.sort compare |>
      List.mapi (fun i (_h,n) -> (n,Int32.of_int i)) |>
      List.fold_left (fun m (n,i) -> FieldEnv.add n i m) FieldEnv.empty in

     let sz = Int32.of_int (FieldEnv.cardinal name_pos_map) in

     (* Allocate memory *)
     let (set_ri, get_ri, ri) = new_local_ env "obj" in
     Heap.alloc (Int32.add header_size (Int32.mul 2l sz)) ^^
     set_ri ^^

     (* Set tag *)
     get_ri ^^
     Tagged.store Tagged.Object ^^

     (* Write the class field *)
     get_ri ^^
     compile_unboxed_const 1l ^^
     Heap.store_field class_position ^^

     (* Set size *)
     get_ri ^^
     compile_unboxed_const sz ^^
     Heap.store_field size_field ^^

    let hash_position env f =
        let i = FieldEnv.find f name_pos_map in
        Int32.add header_size (Int32.mul 2l i) in
    let field_position env f =
        let i = FieldEnv.find f name_pos_map in
        Int32.add header_size (Int32.add (Int32.mul 2l i) 1l) in

     (* Write all the fields *)
     let set_field (id, mk_is) : G.t =
        match FieldEnv.find_opt id name_pos_map with
        | None -> G.nop
        | Some i ->
          (* Write the hash *)
          get_ri ^^
          compile_unboxed_const (hash_field_name id) ^^
          Heap.store_field (hash_position env id) ^^
          (* Write the value *)
          get_ri ^^
          mk_is env ^^
          Heap.store_field (field_position env id)
     in
     G.concat_map set_field fs ^^

     (* Return the pointer to the object *)
     get_ri

  (* Returns a pointer to the object field *)
  let idx_hash env =
    Func.share_code env "obj_idx" ["x"; "hash"] [I32Type] (fun env ->
      let get_x = G.i_ (GetLocal (nr 0l)) in
      let get_hash = G.i_ (GetLocal (nr 1l)) in
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
        G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
        set_f ^^

        get_f ^^
        Heap.load_field 0l ^^ (* the hash field *)
        get_hash ^^
        G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
        G.if_ []
          ( get_f ^^
            compile_add_const Heap.word_size ^^
            set_r
          ) G.nop
      ) ^^
      get_r
    )

  let idx env (name : string) =
    compile_unboxed_const (hash_field_name name) ^^
    idx_hash env

  let load_idx env f = idx env f ^^ load_ptr

end (* Object *)

module Text = struct
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

  (* Two strings on stack *)
  let concat env = Func.share_code env "concat" ["x"; "y"] [I32Type] (fun env ->
      let get_x = G.i_ (GetLocal (nr 0l)) in
      let get_y = G.i_ (GetLocal (nr 1l)) in
      let (set_z, get_z) = new_local env "z" in
      let (set_len1, get_len1) = new_local env "len1" in
      let (set_len2, get_len2) = new_local env "len2" in

      get_x ^^ Heap.load_field len_field ^^ set_len1 ^^
      get_y ^^ Heap.load_field len_field ^^ set_len2 ^^

      (* allocate memory *)
      compile_unboxed_const (Int32.mul Heap.word_size header_size) ^^
      get_len1 ^^
      get_len2 ^^
      G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
      G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
      RTS.alloc_bytes env ^^
      set_z ^^

      (* Set tag *)
      get_z ^^ Tagged.store Tagged.Text ^^

      (* Set length *)
      get_z ^^
      get_len1 ^^
      get_len2 ^^
      G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
      Heap.store_field len_field ^^

      (* Copy first string *)
      get_x ^^
      compile_add_const (Int32.mul Heap.word_size header_size) ^^

      get_z ^^
      compile_add_const (Int32.mul Heap.word_size header_size) ^^

      get_len1 ^^

      RTS.memcpy env ^^

      (* Copy second string *)
      get_y ^^
      compile_add_const (Int32.mul Heap.word_size header_size) ^^

      get_z ^^
      compile_add_const (Int32.mul Heap.word_size header_size) ^^
      get_len1 ^^
      G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^

      get_len2 ^^

      RTS.memcpy env ^^

      (* Done *)
      get_z
    )

  (* Two strings on stack *)
  let compare env = Func.share_code env "Text.compare" ["x"; "y"] [I32Type] (fun env ->
      let get_x = G.i_ (GetLocal (nr 0l)) in
      let get_y = G.i_ (GetLocal (nr 1l)) in
      let (set_len1, get_len1) = new_local env "len1" in
      let (set_len2, get_len2) = new_local env "len2" in

      get_x ^^ Heap.load_field len_field ^^ set_len1 ^^
      get_y ^^ Heap.load_field len_field ^^ set_len2 ^^

      get_len1 ^^
      get_len2 ^^
      G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
      G.if_ [] G.nop (compile_unboxed_false ^^ G.i_ Return) ^^

      (* We could do word-wise comparisons if we know that the trailing bytes
         are zeroed *)
      get_len1 ^^
      from_0_to_n env (fun get_i ->
        get_x ^^
        compile_add_const (Int32.mul Heap.word_size header_size) ^^
        get_i ^^
        G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
        G.i_ (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^

        get_y ^^
        compile_add_const (Int32.mul Heap.word_size header_size) ^^
        get_i ^^
        G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
        G.i_ (Load {ty = I32Type; align = 0; offset = 0l; sz = Some (Wasm.Memory.Pack8, Wasm.Memory.ZX)}) ^^

        G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
        G.if_ [] G.nop (compile_unboxed_false ^^ G.i_ Return)
      ) ^^
      compile_unboxed_true
  )

end (* String *)

module Array = struct
  let header_size = Int32.add Tagged.header_size 1l
  let element_size = 4l
  let len_field = Int32.add Tagged.header_size 0l

  (* Calculates a static offset *)
  let field_of_idx n = Int32.add header_size n

  let idx env = Func.share_code env "Array.idx" ["array"; "idx"] [I32Type] (fun env ->
      let get_array = G.i_ (GetLocal (nr 0l)) in
      let get_idx = G.i_ (GetLocal (nr 1l)) in
      get_idx ^^
      compile_add_const header_size ^^
      compile_mul_const element_size ^^
      get_array ^^
      G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add))
    )

  (* Expects on the stack the pointer to the array. *)
  let load_n n = Heap.load_field (field_of_idx n)

  let common_funcs env =
    let get_array_object = Closure.load_closure 0l in
    let get_single_arg =   Closure.load_argument in
    let get_first_arg =    Closure.load_argument ^^ load_n 0l in
    let get_second_arg =   Closure.load_argument ^^ load_n 1l in

    E.define_built_in env "array_get"
      (fun () -> Closure.unary_of_body env (fun env1 ->
            get_array_object ^^
            get_single_arg ^^ (* the index *)
            BoxedInt.unbox env1 ^^
            idx env ^^
            load_ptr
       ));
    E.define_built_in env "array_set"
      (fun () -> Closure.unary_of_body env (fun env1 ->
            get_array_object ^^
            get_first_arg ^^ (* the index *)
            BoxedInt.unbox env1 ^^
            idx env ^^
            get_second_arg ^^ (* the value *)
            store_ptr ^^
            compile_unit
       ));
    E.define_built_in env "array_len"
      (fun () -> Closure.unary_of_body env (fun env1 ->
            get_array_object ^^
            Heap.load_field len_field ^^
            BoxedInt.box env1
      ));

    let mk_next_fun mk_code : E.func_with_names = Closure.unary_of_body env (fun env1 ->
            let (set_boxed_i, get_boxed_i) = new_local env1 "boxed_n" in
            let (set_i, get_i) = new_local env1 "n" in
            (* Get pointer to counter from closure *)
            Closure.load_closure 0l ^^
            (* Read pointer *)
            Var.load ^^
            set_boxed_i ^^
            get_boxed_i ^^
            BoxedInt.unbox env1 ^^
            set_i ^^

            get_i ^^
            (* Get pointer to array from closure *)
            Closure.load_closure 1l ^^
            (* Get length *)
            Heap.load_field len_field ^^
            G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
            G.if_ [I32Type]
              (* Then *)
              compile_null
              (* Else *)
              ( (* Get point to counter from closure *)
                Closure.load_closure 0l ^^
                (* Store increased counter *)
                get_i ^^
                compile_add_const 1l ^^
                BoxedInt.box env1 ^^
                Var.store ^^
                (* Return stuff *)
                Opt.inject env1 (
                  mk_code env (Closure.load_closure 1l) get_boxed_i get_i
                )
              )
       ) in
    let mk_iterator next_funid = Closure.unary_of_body env (fun env1 ->
            (* next function *)
            let (set_ni, get_ni) = new_local env1 "next" in
            Closure.fixed_closure env1 next_funid
              [ Tagged.obj env1 Tagged.MutBox [ BoxedInt.lit env1 0l ]
              ; get_array_object
              ] ^^
            set_ni ^^

            Object.lit env1 None None
              [ (nr_ "next", nr__ Public, fun _ -> get_ni) ]
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
              idx env ^^
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
  (* No need to wrap them in RTS functions: They occurr only once, in the prelude. *)
  let init env =
    let (set_len, get_len) = new_local env "len" in
    let (set_x, get_x) = new_local env "x" in
    let (set_r, get_r) = new_local env "r" in
    set_x ^^
    BoxedInt.unbox env ^^
    set_len ^^

    (* Allocate *)
    get_len ^^
    compile_add_const header_size ^^
    RTS.alloc_words env ^^
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
    set_len ^^

    (* Allocate *)
    get_len ^^
    compile_add_const header_size ^^
    RTS.alloc_words env ^^
    set_r ^^

    (* Write header *)
    get_r ^^
    Tagged.store Tagged.Array ^^
    get_r ^^
    get_len ^^
    Heap.store_field len_field ^^

    (* Write fields *)
    (* Copy fields *)
    get_len ^^
    from_0_to_n env (fun get_i ->
      get_r ^^
      get_i ^^
      idx env ^^
      get_f ^^
      get_i ^^
      BoxedInt.box env ^^
      Closure.call_indirect env no_region ^^
      store_ptr
    ) ^^
    get_r
end (* Array *)

module Dfinity = struct

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
    let i = E.add_import env (nr {
      module_name = explode "test";
      item_name = explode "print";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (test_print_i env));

    let i = E.add_import env (nr {
      module_name = explode "test";
      item_name = explode "show_i32";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (test_show_i32_i env));

    let i = E.add_import env (nr {
      module_name = explode "data";
      item_name = explode "externalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (data_externalize_i env));

    let i = E.add_import env (nr {
      module_name = explode "data";
      item_name = explode "internalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type; I32Type; I32Type],[])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (data_internalize_i env));

    let i = E.add_import env (nr {
      module_name = explode "data";
      item_name = explode "length";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (data_length_i env));

    let i = E.add_import env (nr {
      module_name = explode "elem";
      item_name = explode "externalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (elem_externalize_i env));

    let i = E.add_import env (nr {
      module_name = explode "elem";
      item_name = explode "internalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type; I32Type; I32Type],[])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (elem_internalize_i env));

    let i = E.add_import env (nr {
      module_name = explode "elem";
      item_name = explode "length";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (elem_length_i env));

    let i = E.add_import env (nr {
      module_name = explode "module";
      item_name = explode "new";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (module_new_i env));

    let i = E.add_import env (nr {
      module_name = explode "actor";
      item_name = explode "new";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (actor_new_i env));

    let i = E.add_import env (nr {
      module_name = explode "actor";
      item_name = explode "self";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (actor_self_i env));

    let i = E.add_import env (nr {
      module_name = explode "actor";
      item_name = explode "export";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (actor_export_i env));

    let i = E.add_import env (nr {
      module_name = explode "func";
      item_name = explode "internalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type],[])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (func_internalize_i env));

    let i = E.add_import env (nr {
      module_name = explode "func";
      item_name = explode "externalize";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type], [I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (func_externalize_i env));

    let i = E.add_import env (nr {
      module_name = explode "func";
      item_name = explode "bind_i32";
      idesc = nr (FuncImport (nr (E.func_type env (FuncType ([I32Type; I32Type],[I32Type])))))
    }) in
    assert (Int32.to_int i == Int32.to_int (func_bind_i env))


  let compile_databuf_of_text env  =
    Func.share_code env "databuf_of_text" ["string"] [I32Type] (fun env ->
      let get_i = G.i_ (GetLocal (nr 0l)) in

      (* Calculate the offset *)
      get_i ^^
      compile_add_const (Int32.mul Heap.word_size Text.header_size) ^^
      (* Calculate the length *)
      get_i ^^
      Heap.load_field (Text.len_field) ^^

      (* Externalize *)
      G.i_ (Call (nr (data_externalize_i env)))
    )

  let compile_databuf_of_bytes env (bytes : string) =
    Text.lit env bytes ^^ compile_databuf_of_text env

  (* For debugging *)
  let _compile_static_print env s =
      compile_databuf_of_bytes env s ^^
      G.i_ (Call (nr (test_print_i env)))
  let _compile_print_int env =
      G.i_ (Call (nr (test_show_i32_i env))) ^^
      G.i_ (Call (nr (test_print_i env))) ^^
      _compile_static_print env "\n"

  let static_self_message_pointer name env =
    compile_databuf_of_bytes env name.it ^^
    G.i_ (Call (nr (E.built_in env "export_self_message")))

  let prim_printInt env =
    if E.mode env = DfinityMode
    then
      BoxedInt.unbox env ^^
      G.i_ (Call (nr (test_show_i32_i env))) ^^
      G.i_ (Call (nr (test_print_i env))) ^^
      compile_unit
    else
      G.i_ Unreachable

  let prim_print env =
    if E.mode env = DfinityMode
    then
      compile_databuf_of_text env ^^
      (* Call print *)
      G.i_ (Call (nr (test_print_i env))) ^^
      compile_unit
    else
      G.i_ Unreachable

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
      G.i_ (Call (nr (E.built_in env "restore_mem"))) ^^
      (* Collect garbage *)
      G.i_ (Call (nr (E.built_in env "collect"))) ^^
      (* Save memory *)
      G.i_ (Call (nr (E.built_in env "save_mem")))
      ) in
    let fi = E.add_fun env empty_f in
    E.add_fun_name env fi "start_stub";
    E.add_export env (nr {
      name = explode "start";
      edesc = nr (FuncExport (nr fi))
    });

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
       G.i_ (GetGlobal (nr mem_global)) ^^
       G.i_ (Call (nr (Dfinity.data_length_i env1))) ^^
       set_i ^^

       get_i ^^
       compile_unboxed_const 0l ^^
       G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
       G.if_[]
         (* First run, call the start function *)
         ( G.i_ (Call (nr start_funid)) )

         (* Subsequent run *)
         ( (* Set heap pointer based on databuf length *)
           get_i ^^
           compile_add_const ElemHeap.table_end ^^
           G.i_ (SetGlobal (nr Heap.heap_ptr)) ^^

           (* Load memory *)
           compile_unboxed_const ElemHeap.table_end ^^
           get_i ^^
           G.i_ (GetGlobal (nr mem_global)) ^^
           compile_unboxed_zero ^^
           G.i_ (Call (nr (Dfinity.data_internalize_i env1))) ^^

           (* Load reference counter *)
           G.i_ (GetGlobal (nr elem_global)) ^^
           G.i_ (Call (nr (Dfinity.elem_length_i env1))) ^^
           G.i_ (SetGlobal (nr ElemHeap.ref_counter)) ^^

           (* Load references *)
           compile_unboxed_const ElemHeap.ref_location ^^
           G.i_ (GetGlobal (nr ElemHeap.ref_counter)) ^^
           G.i_ (GetGlobal (nr elem_global)) ^^
           compile_unboxed_zero ^^
           G.i_ (Call (nr (Dfinity.elem_internalize_i env1)))
        )
    );
    Func.define_built_in env "save_mem" [] [] (fun env1 ->
       (* Store memory *)
       compile_unboxed_const ElemHeap.table_end ^^
       G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
       compile_unboxed_const ElemHeap.table_end ^^
       G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ^^
       G.i_ (Call (nr (Dfinity.data_externalize_i env))) ^^
       G.i_ (SetGlobal (nr mem_global)) ^^

       (* Store references *)
       compile_unboxed_const ElemHeap.ref_location ^^
       G.i_ (GetGlobal (nr ElemHeap.ref_counter)) ^^
       G.i_ (Call (nr (Dfinity.elem_externalize_i env))) ^^
       G.i_ (SetGlobal (nr elem_global))
    )

  let save_mem env = G.i_ (Call (nr (E.built_in env "save_mem")))
  let restore_mem env = G.i_ (Call (nr (E.built_in env "restore_mem")))

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
    Func.share_code env "serialize_go" ["x"] [I32Type] (fun env ->
      let get_x = G.i_ (GetLocal (nr 0l)) in
      let (set_copy, get_copy) = new_local env "x" in

      G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
      set_copy ^^

      get_x ^^
      BitTagged.if_unboxed env [I32Type]
        ( (* Tagged unboxed value, can be left alone *)
          G.i_ Drop ^^ get_x
        )
        ( Tagged.branch env [I32Type]
          [ Tagged.Int,
            (* x still on the stack *)
            Heap.alloc 2l ^^
            compile_unboxed_const (Int32.mul 2l Heap.word_size) ^^
            RTS.memcpy env ^^
            get_copy
          ; Tagged.Reference,
            (* x still on the stack *)
            Heap.alloc 2l ^^
            compile_unboxed_const (Int32.mul 2l Heap.word_size) ^^
            RTS.memcpy env ^^
            get_copy
          ; Tagged.Some,
            G.i_ Drop ^^
            Opt.inject env (
              get_x ^^ Opt.project ^^
              G.i_ (Call (nr (E.built_in env "serialize_go")))
            )
          ; Tagged.Array,
            begin
              let (set_len, get_len) = new_local env "len" in
              Heap.load_field Array.len_field ^^
              set_len ^^

              get_len ^^
              compile_add_const Array.header_size ^^
              RTS.alloc_words env ^^
              G.i_ Drop ^^

              (* Copy header *)
              get_x ^^
              get_copy ^^
              compile_unboxed_const (Int32.mul Heap.word_size Array.header_size) ^^
              RTS.memcpy env ^^

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
                G.i_ (Call (nr (E.built_in env "serialize_go"))) ^^
                store_ptr
              ) ^^
              get_copy
            end
          ; Tagged.Text,
            begin
              let (set_len, get_len) = new_local env "len" in
              Heap.load_field Text.len_field ^^
              (* get length in words *)
              compile_add_const 3l ^^
              compile_divU_const Heap.word_size ^^
              compile_add_const Text.header_size ^^
              set_len ^^

              get_len ^^
              RTS.alloc_words env ^^
              G.i_ Drop ^^

              (* Copy header and data *)
              get_x ^^
              get_copy ^^
              get_len ^^
              compile_mul_const Heap.word_size ^^
              RTS.memcpy env ^^

              get_copy
            end
          ; Tagged.Object,
            begin
              let (set_len, get_len) = new_local env "len" in
              Heap.load_field Object.size_field ^^
              set_len ^^

              get_len ^^
              compile_mul_const 2l ^^
              compile_add_const Object.header_size ^^
              RTS.alloc_words env ^^
              G.i_ Drop ^^

              (* Copy header *)
              get_x ^^
              get_copy ^^
              compile_unboxed_const (Int32.mul Heap.word_size Object.header_size) ^^
              RTS.memcpy env ^^

              (* Copy fields *)
              get_len ^^
              from_0_to_n env (fun get_i ->
                (* Copy hash *)
                get_i ^^
                compile_mul_const 2l ^^
                compile_add_const Object.header_size ^^
                compile_mul_const Heap.word_size ^^
                get_copy ^^
                G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^

                get_i ^^
                compile_mul_const 2l ^^
                compile_add_const Object.header_size ^^
                compile_mul_const Heap.word_size ^^
                get_x ^^
                G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^


                load_ptr ^^
                store_ptr ^^

                (* Copy data *)

                get_i ^^
                compile_mul_const 2l ^^
                compile_add_const Object.header_size ^^
                compile_mul_const Heap.word_size ^^
                get_copy ^^
                G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
                compile_add_const Heap.word_size ^^

                get_i ^^
                compile_mul_const 2l ^^
                compile_add_const Object.header_size ^^
                compile_mul_const Heap.word_size ^^
                get_x ^^
                G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
                compile_add_const Heap.word_size ^^

                load_ptr ^^
                G.i_ (Call (nr (E.built_in env "serialize_go"))) ^^
                store_ptr
              ) ^^
              get_copy
            end
          ; Tagged.Closure,
            (* Closures are not copied. Instead, we create a funcref for them *)
            G.i_ Drop ^^
            Tagged.obj env Tagged.Reference [
              (* Funcref *)
              compile_unboxed_const (E.built_in env "invoke_closure") ^^
              G.i_ (Call (nr (Dfinity.func_externalize_i env))) ^^
              get_x ^^
              ClosureTable.remember_closure env ^^
              G.i_ (Call (nr (Dfinity.func_bind_i env))) ^^
              ElemHeap.remember_reference env
            ]
          ]
        )
    )

  let shift_pointer_at env =
    Func.share_code env "shift_pointer_at" ["loc";  "ptr_offset"] [] (fun env ->
      let get_loc = G.i_ (GetLocal (nr 0l)) in
      let get_ptr_offset = G.i_ (GetLocal (nr 1l)) in
      get_loc ^^
      load_ptr ^^
      BitTagged.if_unboxed env []
        (* nothing to do *)
        ( G.i_ Drop )
        ( set_tmp env ^^

          get_loc ^^
          get_tmp env ^^
          get_ptr_offset ^^
          G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
          store_ptr
        )
    )

  (* Returns the object size (in bytes) *)
  let object_size env =
    Func.share_code env "object_size" ["x"] [I32Type] (fun env ->
      let get_x = G.i_ (GetLocal (nr 0l)) in
      get_x ^^
      Tagged.branch env [I32Type]
        [ Tagged.Int,
          G.i_ Drop ^^
          compile_unboxed_const (Int32.mul 2l Heap.word_size)
        ; Tagged.Reference,
          G.i_ Drop ^^
          compile_unboxed_const (Int32.mul 2l Heap.word_size)
        ; Tagged.Some,
          G.i_ Drop ^^
          compile_unboxed_const (Int32.mul 2l Heap.word_size)
        ; Tagged.MutBox,
          G.i_ Drop ^^
          compile_unboxed_const (Int32.mul 2l Heap.word_size)
        ; Tagged.Array,
          (* x still on the stack *)
          Heap.load_field Array.len_field ^^
          compile_add_const Array.header_size ^^
          compile_mul_const Heap.word_size
        ; Tagged.Text,
          (* x still on the stack *)
          Heap.load_field Text.len_field ^^
          compile_add_const 3l ^^
          compile_divU_const Heap.word_size ^^
          compile_add_const Text.header_size ^^
          compile_mul_const Heap.word_size
        ; Tagged.Object,
          (* x still on the stack *)
          Heap.load_field Object.size_field ^^
          compile_mul_const 2l ^^
          compile_add_const Object.header_size ^^
          compile_mul_const Heap.word_size
        ; Tagged.Closure,
          (* x still on the stack *)
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
          G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LtS))
        )
        ( mk_code get_x ^^
          get_x ^^
          get_x ^^ object_size env ^^
          G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
          set_x
        )

  (* Calls mk_code for each pointer in the object pointed to by get_x,
     passing code get the address of the pointer. *)
  let for_each_pointer env get_x mk_code =
    let (set_ptr_loc, get_ptr_loc) = new_local env "ptr_loc" in
    get_x ^^
    Tagged.branch_default env [] G.nop
      [ Tagged.MutBox,
        (* Adust pointer *)
        compile_add_const (Int32.mul Heap.word_size Var.mutbox_field) ^^
        set_ptr_loc ^^
        mk_code get_ptr_loc
      ; Tagged.Some,
        (* Adust pointer *)
        compile_add_const (Int32.mul Heap.word_size Opt.payload_field) ^^
        set_ptr_loc ^^
        mk_code get_ptr_loc
      ; Tagged.Array,
        (* x still on the stack *)
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
        (* x still on the stack *)
        Heap.load_field Object.size_field ^^

        (* Adjust fields *)
        from_0_to_n env (fun get_i ->
          get_i ^^
          compile_mul_const 2l ^^
          compile_add_const 1l ^^
          compile_add_const Object.header_size ^^
          compile_mul_const Heap.word_size ^^
          get_x ^^
          G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
          set_ptr_loc ^^
          mk_code get_ptr_loc
        )
      ; Tagged.Closure,
        (* x still on the stack *)
        Heap.load_field Closure.len_field ^^
        (* Adjust fields *)
        from_0_to_n env (fun get_i ->
          get_i ^^
          compile_add_const Closure.header_size ^^
          compile_mul_const Heap.word_size ^^
          get_x ^^
          G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
          set_ptr_loc ^^
          mk_code get_ptr_loc
        )
      ]

  let shift_pointers env =
    Func.share_code env "shift_pointers" ["start"; "to"; "ptr_offset"] [] (fun env ->
      let get_start = G.i_ (GetLocal (nr 0l)) in
      let get_to = G.i_ (GetLocal (nr 1l)) in
      let get_ptr_offset = G.i_ (GetLocal (nr 2l)) in

      walk_heap_from_to env get_start get_to (fun get_x ->
        for_each_pointer env get_x (fun get_ptr_loc ->
          get_ptr_loc ^^
          get_ptr_offset ^^
          shift_pointer_at env
        )
      )
    )

  let extract_references env =
    Func.share_code env "extract_references" ["start"; "to"; "tbl_area"] [I32Type] (fun env ->
      let get_start = G.i_ (GetLocal (nr 0l)) in
      let get_to = G.i_ (GetLocal (nr 1l)) in
      let get_tbl_area = G.i_ (GetLocal (nr 2l)) in
      let (set_i, get_i) = new_local env "i" in

      compile_unboxed_const 0l ^^ set_i ^^

      walk_heap_from_to env get_start get_to (fun get_x ->
        get_x ^^
        Tagged.branch_default env [] (G.i_ Nop)
          [ Tagged.Reference,
            (* x still on the stack *)
            G.i_ Drop ^^

            (* Adjust reference *)
            get_tbl_area ^^
            get_i ^^ compile_mul_const Heap.word_size ^^
            G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
            get_x ^^
            Heap.load_field 1l ^^
            ElemHeap.recall_reference env ^^
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
    Func.share_code env "intract_references" ["start"; "to"; "tbl_area"] [] (fun env ->
      let get_start = G.i_ (GetLocal (nr 0l)) in
      let get_to = G.i_ (GetLocal (nr 1l)) in
      let get_tbl_area = G.i_ (GetLocal (nr 2l)) in

      walk_heap_from_to env get_start get_to (fun get_x ->
        get_x ^^
        Tagged.branch_default env [] (G.i_ Nop)
          [ Tagged.Reference,
            (* x still on the stack *)

            (* Adjust reference *)
            get_x ^^
            Heap.load_field 1l ^^
            compile_mul_const Heap.word_size ^^
            get_tbl_area ^^
            G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
            load_ptr ^^
            ElemHeap.remember_reference env ^^
            Heap.store_field 1l
          ]
      )
    )

  let serialize env =
    Func.share_code env "serialize" ["x"] [I32Type] (fun env ->
      let get_x = G.i_ (GetLocal (nr 0l)) in

      let (set_start, get_start) = new_local env "old_heap" in
      let (set_end, get_end) = new_local env "end" in
      let (set_tbl_size, get_tbl_size) = new_local env "tbl_size" in
      let (set_databuf, get_databuf) = new_local env "databuf" in

      (* Remember where we start to copy to *)
      G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
      set_start ^^

      (* Copy data *)
      get_x ^^
      BitTagged.if_unboxed env []
        (* We have a bit-tagged raw value. Put this into a singleton databuf,
           which will be recognized as such by its size.
        *)
        ( G.i_ Drop ^^
          Heap.alloc 1l ^^
          get_x ^^
          store_ptr ^^

          (* Remember the end *)
          G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
          set_end ^^

          (* Empty table of references *)
          compile_unboxed_const 0l ^^ set_tbl_size
        )
        (* We have real data on the heap. Copy.  *)
        ( serialize_go env ^^
          G.i_ Drop ^^

          (* Remember the end *)
          G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
          set_end ^^

          (* Adjust pointers *)
          get_start ^^
          get_end ^^
          compile_unboxed_zero ^^ get_start ^^ G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ^^
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
      get_end ^^ get_start ^^ G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ^^
      G.i_ (Call (nr (Dfinity.data_externalize_i env))) ^^
      set_databuf ^^

      (* Append this reference at the end of the extracted references *)
      get_end ^^
      get_tbl_size ^^ compile_mul_const Heap.word_size ^^
      G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
      get_databuf ^^
      store_ptr ^^
      (* And bump table end *)
      get_tbl_size ^^ compile_add_const 1l ^^ set_tbl_size ^^

      (* Reset the heap counter, to free some space *)
      get_start ^^
      G.i_ (SetGlobal (nr Heap.heap_ptr)) ^^

      (* Finally, create elembuf *)
      get_end ^^
      get_tbl_size ^^
      G.i_ (Call (nr (Dfinity.elem_externalize_i env)))
    )

  let deserialize env =
    Func.share_code env "deserialize" ["ref"] [I32Type] (fun env ->
      let get_elembuf = G.i_ (GetLocal (nr 0l)) in
      let (set_databuf, get_databuf) = new_local env "databuf" in
      let (set_start, get_start) = new_local env "start" in
      let (set_data_len, get_data_len) = new_local env "data_len" in
      let (set_tbl_size, get_tbl_size) = new_local env "tbl_size" in

      (* new positions *)
      G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
      set_start ^^

      get_elembuf ^^ G.i_ (Call (nr (Dfinity.elem_length_i env))) ^^
      set_tbl_size ^^

      (* First load databuf (last entry) at the heap position somehow *)
      get_start ^^
      compile_unboxed_const 1l ^^
      get_elembuf ^^
      get_tbl_size ^^ compile_sub_const 1l ^^
      G.i_ (Call (nr (Dfinity.elem_internalize_i env))) ^^
      get_start ^^ load_ptr ^^
      set_databuf ^^

      get_databuf ^^ G.i_ (Call (nr (Dfinity.data_length_i env)))  ^^
      set_data_len ^^

      (* Load data from databuf *)
      get_start ^^
      get_data_len ^^
      get_databuf ^^
      compile_unboxed_const 0l ^^
      G.i_ (Call (nr (Dfinity.data_internalize_i env))) ^^

      (* Check if we got something unboxed (data buf size 1 word) *)
      get_data_len ^^
      compile_unboxed_const Heap.word_size ^^
      G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
      G.if_ [I32Type]
        (* Yes, we got something unboxed. Return it, and do _not_ bump the heap pointer *)
        ( get_start ^^ load_ptr )
        (* No, it is actual heap-data *)
        ( (* update heap pointer *)
          get_start ^^
          get_data_len ^^
          G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
          G.i_ (SetGlobal (nr Heap.heap_ptr)) ^^

          (* Fix pointers *)
          get_start ^^
          G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
          get_start ^^
          G.i_ (Call (nr (E.built_in env "shift_pointers"))) ^^

          (* Load references *)
          G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
          get_tbl_size ^^ compile_sub_const 1l ^^
          get_elembuf ^^
          compile_unboxed_const 0l ^^
          G.i_ (Call (nr (Dfinity.elem_internalize_i env))) ^^

          (* Fix references *)
          (* Extract references *)
          get_start ^^
          G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
          G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^
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
  let evacuate env = Func.share_code env "evaucate" ["begin_from_space"; "begin_to_space"; "end_to_space"; "ptr_loc"] [I32Type] (fun env ->
    let get_begin_from_space = G.i_ (GetLocal (nr 0l)) in
    let get_begin_to_space = G.i_ (GetLocal (nr 1l)) in
    let get_end_to_space = G.i_ (GetLocal (nr 2l)) in
    let get_ptr_loc = G.i_ (GetLocal (nr 3l)) in
    let (set_len, get_len) = new_local env "len" in
    let (set_new_ptr, get_new_ptr) = new_local env "new_ptr" in

    let get_obj = get_ptr_loc ^^ load_ptr in

    get_obj ^^
    (* If this is an unboxed scalar, ignore it *)
    BitTagged.if_unboxed env [] (G.i_ Drop ^^ get_end_to_space ^^ G.i_ Return) (G.i_ Drop) ^^

    (* If this is static, ignore it *)
    get_obj ^^
    get_begin_from_space ^^
    G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LtU)) ^^
    G.if_ [] (get_end_to_space ^^ G.i_ Return) G.nop ^^

    (* If this is an indirection, just use that value *)
    get_obj ^^
    Tagged.branch_default env [] G.nop [
      Tagged.Indirection,
      G.i_ Drop ^^

      (* Update pointer *)
      get_ptr_loc ^^
      get_ptr_loc ^^ load_ptr ^^ Heap.load_field 1l ^^
      store_ptr ^^

      get_end_to_space ^^
      G.i_ Return
    ] ^^

    (* Copy the referenced object to to space *)
    get_obj ^^ Serialization.object_size env ^^ set_len ^^

    get_obj ^^ get_end_to_space ^^ get_len ^^ RTS.memcpy env ^^

    (* Calculate new pointer *)
    get_end_to_space ^^
    get_begin_to_space ^^
    G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ^^
    get_begin_from_space ^^
    G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
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
    G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add))
  )

  let register env (end_of_static_space : int32) = Func.define_built_in env "collect" [] [] (fun env ->
    (* Copy all roots. *)
    let (set_begin_from_space, get_begin_from_space) = new_local env "begin_from_space" in
    let (set_begin_to_space, get_begin_to_space) = new_local env "begin_to_space" in
    let (set_end_to_space, get_end_to_space) = new_local env "end_to_space" in

    compile_unboxed_const end_of_static_space ^^ set_begin_from_space ^^
    G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^ set_begin_to_space ^^
    G.i_ (GetGlobal (nr Heap.heap_ptr)) ^^ set_end_to_space ^^


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
    get_end_to_space ^^ get_begin_to_space ^^ G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ^^
    RTS.memcpy env ^^

    (* Reset the heap pointer *)
    get_begin_from_space ^^
    get_end_to_space ^^ get_begin_to_space ^^ G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ^^
    G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ^^
    G.i_ (SetGlobal (nr Heap.heap_ptr))
  )


end (* GC *)


module Message = struct
  (* We use the first table slot for calls to funcrefs *)
  (* This does not clash with slots for our functions as long as there
     is at least one imported function (which we do not add to the table) *)
  let tmp_table_slot = 0l

  (* The type of messages *)
  let message_ty env = E.func_type env (FuncType ([I32Type],[]))


  let system_funs mod_env =
    Func.define_built_in mod_env "invoke_closure" ["clos";"arg"] [] (fun env ->
      (* This is the entry point for external closure invocation.
         The first argument is the index of the closure in the ClosureTable
         (bound using `i32.bind`).
         The second a serialized message.
         This method must not be exported!
         We create a funcref internally and then bind the closure to it.
      *)
      OrthogonalPersistence.restore_mem env ^^

      (* Put closure on the stack *)
      G.i (nr (GetLocal (nr 0l))) ^^
      ClosureTable.recall_closure env ^^

      (* Put argument on the stack *)
      G.i (nr (GetLocal (nr 1l))) ^^
      Serialization.deserialize env ^^

      (* Invoke the call *)
      Closure.call_indirect env no_region ^^
      G.i_ Drop ^^

      (* Collect garbage *)
      G.i_ (Call (nr (E.built_in env "collect"))) ^^

      (* Save memory *)
      OrthogonalPersistence.save_mem env
    );
    E.add_dfinity_type mod_env
      (E.built_in mod_env "invoke_closure",
      [CustomSections.I32; CustomSections.ElemBuf]);

    Func.define_built_in mod_env "call_funcref" ["ref";"arg"] [] (fun env ->
      let get_ref = G.i_ (GetLocal (nr 0l)) in
      let get_arg = G.i_ (GetLocal (nr 1l)) in

      compile_unboxed_const tmp_table_slot ^^ (* slot number *)
      get_ref ^^ (* the boxed funcref table id *)
      Heap.load_field 1l ^^
      ElemHeap.recall_reference env ^^
      G.i_ (Call (nr (Dfinity.func_internalize_i env))) ^^

      get_arg ^^
      Serialization.serialize env ^^

      compile_unboxed_const tmp_table_slot ^^
      G.i_ (CallIndirect (nr (message_ty env)))
    );

    Func.define_built_in mod_env "export_self_message" ["name"] [I32Type] (fun env ->
      let get_name = G.i_ (GetLocal (nr 0l)) in

      Tagged.obj env Tagged.Reference [
        (* Create a funcref for the message *)
        G.i_ (Call (nr (Dfinity.actor_self_i env))) ^^
        get_name ^^ (* the databuf with the message name *)
        G.i_ (Call (nr (Dfinity.actor_export_i env))) ^^
        ElemHeap.remember_reference env
      ]
    )

  let compile env mk_pat mk_body at : E.func_with_names =
    (* Messages take no closure, return nothing*)
    Func.of_body env ["arg"] [] (fun env1 ->
      (* Set up memory *)
      OrthogonalPersistence.restore_mem env ^^

      (* Destruct the argument *)
      let (env2, alloc_args_code, destruct_args_code) = mk_pat env1  in

      (* Compile the body *)
      let body_code = mk_body env2 in

      alloc_args_code ^^
      G.i (GetLocal (nr 0l) @@ at) ^^
      Serialization.deserialize env ^^
      destruct_args_code ^^
      body_code ^^
      G.i_ Drop ^^

      (* Collect memory *)
      G.i_ (Call (nr (E.built_in env "collect"))) ^^

      (* Save memory *)
      OrthogonalPersistence.save_mem env
      )
end (* Message *)

module PatCode = struct
  (* Pattern failure code on demand.

  Patterns in general can fail, so we want a block around them with a jump-label
  for the fail case. But many patterns cannot fail, in particular not function
  arguments that are simple variables. In these cases, we do not want to create
  the block and the (unused) jump label. So we first generate the code, either as plain code
  (cannot fail) or as code with hole for code to fun in case of failure.
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
          let inner_fail_code = compile_unboxed_false ^^ G.branch_to_ inner_fail in
          G.labeled_block_ [I32Type] inner_fail (is1 inner_fail_code ^^ compile_unboxed_true) ^^
          G.if_ [] G.nop (is2 fail_code)
        )
      | CannotFail is2 -> CannotFail (
          let inner_fail = G.new_depth_label () in
          let inner_fail_code = compile_unboxed_false ^^ G.branch_to_ inner_fail in
          G.labeled_block_ [I32Type] inner_fail (is1 inner_fail_code ^^ compile_unboxed_true) ^^
          G.if_ [] G.nop is2
        )

  let orTrap : patternCode -> G.t = function
    | CannotFail is -> is
    | CanFail is -> is (G.i_ Unreachable)

end (* PatCode *)
open PatCode

(* The actual compiler code that looks at the AST *)

let compile_lit env lit = match lit with
  | BoolLit false -> BoxedInt.lit_false env
  | BoolLit true ->  BoxedInt.lit_true env
  (* This maps int to int32, instead of a proper arbitrary precision library *)
  | IntLit n      ->
    (try BoxedInt.lit env (Big_int.int32_of_big_int n)
    with Failure _ -> Printf.eprintf "compile_lit: Overflow in literal %s\n" (Big_int.string_of_big_int n); G.i_ Unreachable)
  | NatLit n      ->
    (try BoxedInt.lit env (Big_int.int32_of_big_int n)
    with Failure _ -> Printf.eprintf "compile_lit: Overflow in literal %s\n" (Big_int.string_of_big_int n); G.i_ Unreachable)
  | NullLit       -> compile_null
  | TextLit t     -> Text.lit env t
  | _ -> todo "compile_lit" (Arrange.lit lit) G.i_ Unreachable

let compile_unop env op = match op with
  | NegOp -> BoxedInt.lift_unboxed_unary env (
      set_tmp env ^^
      compile_unboxed_zero ^^
      get_tmp env ^^
      G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)))
  | PosOp -> G.nop
  | _ -> todo "compile_unop" (Arrange.unop op) G.i_ Unreachable

let compile_binop env op = match op with
  | AddOp -> BoxedInt.lift_unboxed_binary env (G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)))
  | SubOp -> BoxedInt.lift_unboxed_binary env (G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)))
  | MulOp -> BoxedInt.lift_unboxed_binary env (G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Mul)))
  | DivOp -> BoxedInt.lift_unboxed_binary env (G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.DivU)))
  | ModOp -> BoxedInt.lift_unboxed_binary env (G.i_ (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.RemU)))
  | CatOp -> Text.concat env
  | _ -> todo "compile_binop" (Arrange.binop op) G.i_ Unreachable

let compile_relop env op = BoxedInt.lift_unboxed_binary env (match op with
  | EqOp -> G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq))
  | NeqOp -> G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
             G.if_ [I32Type] compile_unboxed_false compile_unboxed_true
  | GeOp -> G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.GeS))
  | GtOp -> G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.GtS))
  | LeOp -> G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LeS))
  | LtOp -> G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LtS))
  )


(* compile_lexp is used for expressions on the left of an
assignment operator, and calculates (puts on the stack) the
memory location of such a thing. *)
let rec compile_lexp (env : E.t) exp = match exp.it with
  | VarE var ->
     Var.get_payload_loc env var.it
  | IdxE (e1,e2) ->
     compile_exp env e1 ^^ (* offset to array *)
     compile_exp env e2 ^^ (* idx *)
     BoxedInt.unbox env ^^
     Array.idx env
  | DotE (e, {it = Name n;_}) ->
     compile_exp env e ^^
     (* Only real objects have mutable fields, no need to branch on the tag *)
     Object.idx env n
  | _ -> todo "compile_lexp" (Arrange.exp exp) G.i_ Unreachable

(* compile_exp returns an *value*.
Currently, number (I32Type) are just repesented as such, but other
types may be point (e.g. for function, array, tuple, object).

Local variables (which maybe mutable, or have delayed initialisation)
are also points, but points to such values, and need to be read first.  *)
and compile_exp (env : E.t) exp = match exp.it with
  (* We can reuse the code in compile_lexp here *)
  | IdxE _  ->
     compile_lexp env exp ^^
     load_ptr
  | DotE (e, ({it = Name n;_} as id)) ->
     compile_exp env e ^^
     Tagged.branch env [I32Type]
      ( [ Tagged.Object, Object.load_idx env n ] @
        (if E.mode env = DfinityMode
         then [ Tagged.Reference, actor_fake_object_idx env {id with it = n} ]
         else []) @
        match  Array.fake_object_idx env n with
          | None -> []
          | Some code -> [ Tagged.Array, code ]
      )
  (* We only allow prims of certain shapes, as they occur in the prelude *)
  (* Binary prims *)
  |  CallE ({ it = AnnotE ({ it = PrimE p; _} as pe, _); _}, _, { it = TupE [e1;e2]; _}) ->
    begin
     compile_exp env e1 ^^
     compile_exp env e2 ^^
     match p with
      | "Array.init" -> Array.init env
      | "Array.tabulate" -> Array.tabulate env
      | _ -> todo "compile_exp" (Arrange.exp pe) (G.i_ Unreachable)
    end
  (* Unary prims *)
  | CallE ({ it = AnnotE ({ it = PrimE p; _} as pe, _); _}, _, e) ->
    begin
     compile_exp env e ^^
     match p with
      | "abs" -> Prim.prim_abs env
      | "printInt" -> Dfinity.prim_printInt env
      | "print" -> Dfinity.prim_print env
      | _ -> todo "compile_exp" (Arrange.exp pe) (G.i_ Unreachable)
    end
  | VarE var ->
     Var.get_val env var.it
  | AssignE (e1,e2) ->
     compile_lexp env e1 ^^
     compile_exp env e2 ^^
     store_ptr ^^
     compile_unit
  | LitE l_ref ->
     compile_lit env !l_ref
  | AssertE e1 ->
     compile_exp env e1 ^^
     BoxedInt.unbox env ^^
     G.if_ [I32Type] compile_unit (G.i (Unreachable @@ exp.at))
  | NotE e ->
     compile_exp env e ^^
     BoxedInt.unbox env ^^
     G.if_ [I32Type] (BoxedInt.lit_false env) (BoxedInt.lit_true env)
  | UnE (op, e1) ->
     compile_exp env e1 ^^
     compile_unop env op
  | BinE (e1, op, e2) ->
     compile_exp env e1 ^^
     compile_exp env e2 ^^
     compile_binop env op
  | RelE (e1, op, e2) ->
     compile_exp env e1 ^^
     compile_exp env e2 ^^
     compile_relop env op
  | OrE (e1, e2) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp env e2 in
     code1 ^^ BoxedInt.unbox env ^^
     G.if_ [I32Type] (BoxedInt.lit_true env) code2
  | AndE (e1, e2) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp env e2 in
     code1 ^^ BoxedInt.unbox env ^^
     G.if_ [I32Type] code2 (BoxedInt.lit_false env)
  | IfE (e1, e2, e3) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp env e2 in
     let code3 = compile_exp env e3 in
     code1 ^^ BoxedInt.unbox env ^^
     G.if_ [I32Type] code2 code3
  | IsE (e1, e2) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp env e2 in
     let (set_i, get_i) = new_local env "is_lhs" in
     let (set_j, get_j) = new_local env "is_rhs" in
     code1 ^^
     set_i ^^
     code2 ^^
     set_j ^^

     get_i ^^
     Tagged.branch env [I32Type]
      [ Tagged.Array,
        G.i_ Drop ^^ BoxedInt.lit_false env
      ; Tagged.Reference,
        (* TODO: Implement IsE for actor references? *)
        G.i_ Drop ^^ BoxedInt.lit_false env
      ; Tagged.Object,
        (* There are two cases: Either the class is a pointer to
           the object on the RHS, or it is -- mangled -- the
           function id stored therein *)
        Heap.load_field Object.class_position ^^
        (* Equal? *)
        get_j ^^
        G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
        G.if_ [I32Type]
          (BoxedInt.lit_true env)
          (* Static function id? *)
          ( get_i ^^
            Heap.load_field Object.class_position ^^
            get_j ^^
            Heap.load_field 0l ^^ (* get the function id *)
            compile_mul_const Heap.word_size ^^
            compile_add_const 1l ^^ 
            G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq))
          )
        ]
  | BlockE decs ->
     compile_decs env decs
  | DecE dec ->
     compile_decs env [dec]
  | LabelE (name, _ty, e) ->
      G.block_ [I32Type] (G.with_current_depth (fun depth ->
        let env1 = E.add_label env name depth in
        compile_exp env1 e 
      ))
  | BreakE (name, _ty) ->
      let d = E.get_label_depth env name in
      compile_unit ^^ G.branch_to_ d
  | LoopE (e, None) ->
     G.loop_ [] (
       let code = compile_exp env e in
       code ^^ G.i_ (Br (nr 0l))
     ) ^^
     G.i_ Unreachable
  | WhileE (e1, e2) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp env e2 in
     G.loop_ [] (
       code1 ^^ BoxedInt.unbox env ^^
       G.if_ [] (code2 ^^ G.i_ Drop ^^ G.i_ (Br (nr 1l))) G.nop
     ) ^^
     compile_unit
  | AnnotE (e, t) -> compile_exp env e
  | RetE e -> compile_exp env e ^^ G.i (Return @@ exp.at)
  | OptE e ->
     Opt.inject env (compile_exp env e)
  | TupE [] -> compile_unit
  | TupE es -> Array.lit env (List.map (compile_exp env) es)
  | ProjE (e1,n) ->
     compile_exp env e1 ^^ (* offset to tuple (an array) *)
     Array.load_n (Int32.of_int n)
  | ArrayE es -> Array.lit env (List.map (compile_exp env) es)
  | ObjE ({ it = Type.Object _ (*sharing*); _}, name, fs) -> (* TBR - really the same for local and shared? *)
     let fs' = List.map
      (fun (f : Syntax.exp_field) ->
        (f.it.id, f.it.priv, fun env -> compile_exp env f.it.exp)
      ) fs in
     Object.lit env (Some name) None fs'
  | ObjE ({ it = Type.Actor; _}, name, fs) ->
    let captured = Freevars.exp exp in
    let prelude_names = find_prelude_names env in
    if Freevars.S.is_empty (Freevars.S.diff captured prelude_names)
    then actor_lit env name fs
    else todo "non-closed actor" (Arrange.exp exp) G.i_ Unreachable
  | CallE (e1, _, e2) when isDirectCall env e1 <> None ->
     let fi = Lib.Option.value (isDirectCall env e1) in
     compile_exp env e2 ^^
     Closure.call_direct env fi exp.at
  | CallE (e1, _, e2) ->
     compile_exp env e1 ^^
     compile_exp env e2 ^^
     Closure.call_indirect env exp.at
  | SwitchE (e, cs) ->
    let code1 = compile_exp env e in
    let (set_i, get_i) = new_local env "switch_in" in
    let (set_j, get_j) = new_local env "switch_out" in

    let rec go env cs = match cs with
      | [] -> CanFail (fun k -> k)
      | (c::cs) ->
          let pat = c.it.pat in
          let e = c.it.exp in
          let (env1, alloc_code, code) = compile_pat env pat in
          CannotFail alloc_code ^^^
          orElse ( CannotFail get_i ^^^ code ^^^
                   CannotFail (compile_exp env1 e) ^^^ CannotFail set_j)
                 (go env cs)
          in
      let code2 = go env cs in
      code1 ^^ set_i ^^ orTrap code2 ^^ get_j
  | ForE (p, e1, e2) ->
     let code1 = compile_exp env e1 in
     let (env1, alloc_code, code2) = compile_mono_pat env p in
     let code3 = compile_exp env1 e2 in

     let (set_i, get_i) = new_local env "iter" in
     (* Store the iterator *)
     code1 ^^
     set_i ^^

     G.loop_ []
       ( get_i ^^
         Object.load_idx env1 "next" ^^
         compile_unit ^^
         Closure.call_indirect env1 Source.no_region ^^
         let (set_oi, get_oi) = new_local env "opt" in
         set_oi ^^

         (* Check for null *)
         get_oi ^^
         compile_null ^^
         G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
         G.if_ []
           G.nop
           ( alloc_code ^^ get_oi ^^ Opt.project ^^
             code2 ^^ code3 ^^ G.i_ Drop ^^ G.i_ (Br (nr 1l))
           )
     ) ^^
     compile_unit
  (* Async-wait lowering support features *)
  | DeclareE (name, _, e) ->
      let (env1, i) = E.add_local_with_offset env name.it 1l in
      Tagged.obj env Tagged.MutBox [ compile_unboxed_const 0l ] ^^
      G.i_ (SetLocal (nr i)) ^^
      compile_exp env1 e
  | DefineE (name, _, e) ->
      Var.get_payload_loc env name.it ^^
      compile_exp env e ^^
      store_ptr ^^
      compile_unit
  | NewObjE ({ it = Type.Object _ (*sharing*); _}, fs) -> (* TBR - really the same for local and shared? *)
     let fs' = List.map
      (fun ({ it = Name name; _}, id) -> (name, fun env ->
        Var.get_payload_loc env id.it ^^
        load_ptr
      )) fs in
     Object.lit_raw env fs'
  | _ -> todo "compile_exp" (Arrange.exp exp) (G.i_ Unreachable)


and isDirectCall env e = match e.it with
  | AnnotE (e, _) -> isDirectCall env e
  | VarE var ->
    begin match E.lookup_var env var.it with
    | Some (Deferred d) -> d.is_direct_call
    | _ -> None
    end
  | _ -> None


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


and compile_lit_pat env opo l = match opo, l with
  | None, NullLit ->
    compile_lit env l ^^
    G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq))
  | None, (NatLit _ | IntLit _ | BoolLit _) ->
    BoxedInt.unbox env ^^
    compile_lit env l ^^
    BoxedInt.unbox env ^^
    G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq))
  | Some uo, (NatLit _ | IntLit _) ->
    BoxedInt.unbox env ^^
    compile_lit env l ^^
    compile_unop env uo ^^
    BoxedInt.unbox env ^^
    G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq))
  | None, (TextLit t) ->
    Text.lit env t ^^
    Text.compare env
  | _ -> todo "compile_lit_pat" (Arrange.lit l) (G.i_ Unreachable)

and compile_pat env pat : E.t * G.t * patternCode = match pat.it with
  (* It returns:
     - the extended environment
     - the code to allocate memory
     - the code to do the pattern matching.
       This expects the  undestructed value is on top of the stack,
       consumes it, and fills the heap
       If the pattern does not match, it branches to the depth at fail_depth.
  *)
  | WildP -> (env, G.nop, CannotFail (G.i_ Drop))
  | AnnotP (p, _) -> compile_pat env p
  | OptP p ->
      let (env1, alloc_code1, code1) = compile_pat env p in
      let (set_i, get_i) = new_local env "opt_scrut" in
      let code = CanFail (fun fail_code ->
        set_i ^^
        get_i ^^
        compile_null ^^
        G.i_ (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ^^
        G.if_ [] fail_code
          ( get_i ^^
            Opt.project ^^
            with_fail fail_code code1
          )) in
      let env2 = env1 in
      (env2, alloc_code1, code)
  | LitP l ->
      let code = CanFail (fun fail_code ->
        compile_lit_pat env None !l ^^
        G.if_ [] G.nop fail_code)
      in (env, G.nop, code)
  | SignP (op, l) ->
      let code = CanFail (fun fail_code ->
        compile_lit_pat env (Some op) !l ^^
        G.if_ [] G.nop fail_code)
      in (env, G.nop, code)

  | VarP name ->
      let (env1,i) = Var.add_local env name.it; in
      let alloc_code =
        Tagged.obj env1 Tagged.MutBox [ compile_unboxed_const 0l ] ^^
        G.i_ (SetLocal (nr i)) in

      let code = CannotFail (
        set_tmp env ^^
        G.i_ (GetLocal (nr i)) ^^
        get_tmp env ^^
        Var.store
        ) in
      (env1, alloc_code, code)
  | TupP ps ->
      let (set_i, get_i) = new_local env "tup_scrut" in
      let rec go i ps env = match ps with
        | [] -> (env, G.nop, CannotFail G.nop)
        | (p::ps) ->
          let (env1, alloc_code1, code1) = compile_pat env p in
          let (env2, alloc_code2, code2) = go (i+1) ps env1 in
          ( env2,
            alloc_code1 ^^ alloc_code2,
            CannotFail (get_i ^^ Array.load_n (Int32.of_int i)) ^^^
            code1 ^^^
            code2) in
      let (env1, alloc_code, code) = go 0 ps env in
      (env1, alloc_code, CannotFail set_i ^^^ code)

  | AltP (p1, p2) ->
      let (env1, alloc_code1, code1) = compile_pat env p1 in
      let (env2, alloc_code2, code2) = compile_pat env1 p2 in

      let (set_i, get_i) = new_local env "alt_scrut" in
      let code =
        CannotFail set_i ^^^
        orElse (CannotFail get_i ^^^ code1)
               (CannotFail get_i ^^^ code2) in
      (env2, alloc_code1 ^^ alloc_code2,  code)

(* Used for mono patterns (let, function arguments) *)
and compile_mono_pat env pat =
  let (env1, alloc_code, code) = compile_pat env pat in
  let wrapped_code = set_tmp env ^^ orTrap (CannotFail (get_tmp env) ^^^ code) in
  (env1, alloc_code, wrapped_code)

and compile_dec last pre_env dec : E.t * G.t * (E.t -> G.t) = match dec.it with
  | TypD _ -> (pre_env, G.nop, fun _ -> G.nop)
  | ExpD e ->
    (pre_env, G.nop, fun env ->
      let code = compile_exp env e in
      let drop = if last then G.nop else G.i_ Drop in
      code ^^ drop
    )
  | LetD (p, e) ->
    let (pre_env1, alloc_code, code2) = compile_mono_pat pre_env p in
    ( pre_env1, alloc_code, fun env ->
      let code1 = compile_exp env e in
      let stack_fix = if last then dup env else G.nop in
      code1 ^^ stack_fix ^^ code2)
  | VarD (name, e) ->
      let (pre_env1, i) = E.add_local_with_offset pre_env name.it 1l in

      let alloc_code =
        Tagged.obj pre_env Tagged.MutBox [ compile_unboxed_const 0l ] ^^
        G.i_ (SetLocal (nr i)) in

      ( pre_env1, alloc_code, fun env ->
        let code1 = compile_exp env e in
        G.i_ (GetLocal (nr i)) ^^
        code1 ^^
        Var.store ^^
        if last then G.i_ (GetLocal (nr i)) ^^ Var.load else G.nop)

  | FuncD (_, name, _, p, _rt, e) ->
      (* Get captured variables *)
      let captured = Freevars.captured p e in
      let mk_pat env1 = compile_mono_pat env1 p in
      let mk_body env1 _ = compile_exp env1 e in
      Closure.dec pre_env last name captured mk_pat mk_body dec.at

  (* Classes are desguared to functions and objects. *)
  | ClassD (name, _, typ_params, s, p, efs) ->
      let captured = Freevars.captured_exp_fields p efs in
      let mk_pat env1 = compile_mono_pat env1 p in
      let mk_body env1 compile_fun_identifier =
        (* TODO: This treats actors like any old object *)
        let fs' = List.map (fun (f : Syntax.exp_field) ->
          (f.it.id, f.it.priv, fun env -> compile_exp env f.it.exp)
          ) efs in
        (* this is run within the function. The class id is the function
	identifier, as provided by Func.dec:
	For closures it is the pointer to the closure.
	For functions it is the function id (shifted to never class with pointers) *)
        Object.lit env1 None (Some compile_fun_identifier) fs' in
      Closure.dec pre_env last name captured mk_pat mk_body dec.at

and compile_decs env decs : G.t = snd (compile_decs_block env true decs)

and compile_decs_block env keep_last decs : (E.t * G.t) =
  let rec go pre_env decs = match decs with
    | []          -> (pre_env, G.nop, fun _ -> if keep_last then compile_unit else G.nop) (* empty declaration list? *)
    | [dec]       -> compile_dec keep_last pre_env dec
    | (dec::decs) ->
        let (pre_env1, alloc_code1, mk_code1) = compile_dec false pre_env dec    in
        let (pre_env2, alloc_code2, mk_code2) = go          pre_env1 decs in
        (pre_env2, alloc_code1 ^^ alloc_code2, fun env -> mk_code1 env ^^ mk_code2 env) in
  let (env1, alloc_code, mk_code) = go env decs in
  (env1, alloc_code ^^ mk_code env1)

and compile_prelude env =
  (* Allocate the primitive functions *)
  let (env1, code) = compile_decs_block env false (E.get_prelude env).it in
  (env1, code)

(* Is this a hack? When determining whether an actor is closed,
we should disregard the prelude, because every actor is compiled with the
prelude. So this function compiles the prelude, just to find out the bound names.
*)
and find_prelude_names env =
  (* Create a throw-away environment *)
  let env1 = E.mk_fun_env (E.mk_global (E.mode env) (E.get_prelude env) 0l) 0l in
  let (env2, _) = compile_prelude env1 in
  E.in_scope_set env2


and compile_start_func env (progs : Syntax.prog list) : E.func_with_names =
  Func.of_body env [] [] (fun env1 ->
    let rec go env = function
      | [] -> G.nop
      | (prog::progs) ->
          let (env1, code1) = compile_decs_block env false prog.it in
          let code2 = go env1 progs in
          code1 ^^ code2 in
    go env1 progs
    )

and compile_private_actor_field pre_env (f : Syntax.exp_field)  =
  let ptr = E.reserve_static_memory pre_env (Int32.mul 2l Heap.word_size) in
  let pre_env1 = E.add_local_static pre_env f.it.id.it (Int32.add Heap.word_size ptr) in
  ( pre_env1, fun env ->
    compile_unboxed_const ptr ^^
    Tagged.store Tagged.MutBox ^^

    compile_unboxed_const ptr ^^
    compile_exp env f.it.exp ^^
    Var.store
  )

and compile_public_actor_field pre_env (f : Syntax.exp_field) =
  let (name, _, pat, _rt, exp) =
    let rec find_func exp = match exp.it with
    | AnnotE (exp, _) -> find_func exp
    | DecE {it = FuncD (s, name, ty_args, pat, rt, exp); _ } -> (name, ty_args, pat, rt, exp)
    | _ -> raise (Invalid_argument "public actor field not a function")
    in find_func f.it.exp in

  (* Which name to use? f.it.id or name? Can they differ? *)
  (* crusso: use name for the name of the field, access by projection; id for the bound name. 
     They can differ after alpha-renaming of id due to CPS conversion, but are initially the same after the parsing
     I have not reviewed/fixed the code below.
  *)
  let (fi, fill) = E.reserve_fun pre_env in
  E.add_fun_name pre_env fi name.it;
  E.add_dfinity_type pre_env (fi, [CustomSections.ElemBuf]);
  E.add_export pre_env (nr {
    name = Dfinity.explode name.it;
    edesc = nr (FuncExport (nr fi))
  });
  let d = { allocate = Dfinity.static_self_message_pointer name; is_direct_call = None } in
  let pre_env1 = E.add_local_deferred pre_env name.it d in

  ( pre_env1, fun env ->
    let mk_pat inner_env = compile_mono_pat inner_env pat in
    let mk_body inner_env = compile_exp inner_env exp in
    let f = Message.compile env mk_pat mk_body f.at in
    fill f;
    G.nop
  )

and compile_actor_field pre_env (f : Syntax.exp_field) =
  if f.it.priv.it = Private
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



and actor_lit outer_env name fs =
  if E.mode outer_env <> DfinityMode then G.i_ Unreachable else

  let wasm =
    let env = E.mk_global (E.mode outer_env) (E.get_prelude outer_env) ClosureTable.table_end in

    if E.mode env = DfinityMode then Dfinity.system_imports env;

    Array.common_funcs env;
    if E.mode env = DfinityMode then Message.system_funs env;

    let start_fun = Func.of_body env [] [] (fun env3 ->
      (* Compile stuff here *)
      let (env4, prelude_code) = compile_prelude env3 in
      let (env5, init_code )  = compile_actor_fields env4 fs in
      prelude_code ^^ init_code) in
    let start_fi = E.add_fun env start_fun in
    E.add_fun_name env start_fi "start";

    OrthogonalPersistence.register env start_fi;

    let m = conclude_module env None in
    let (_map, wasm) = CustomModule.encode m in
    wasm in

  let code =
    Dfinity.compile_databuf_of_bytes outer_env wasm ^^

    (* Create actorref *)
    G.i_ (Call (nr (Dfinity.module_new_i outer_env))) ^^
    G.i_ (Call (nr (Dfinity.actor_new_i outer_env))) ^^
    ElemHeap.remember_reference outer_env in

  (* Wrap it in a tagged heap object *)
  Tagged.obj outer_env Tagged.Reference [ code ]

and actor_fake_object_idx env name =
    let (set_i, get_i) = new_local env "ref" in
    (* The wrapped actor table entry is on the stack *)
    Heap.load_field 1l ^^
    ElemHeap.recall_reference env ^^
    set_i ^^

    (* Export the methods and put it in a Reference object *)
    Tagged.obj env Tagged.Reference
      [ get_i ^^
        Dfinity.compile_databuf_of_bytes env (name.it) ^^
        G.i_ (Call (nr (Dfinity.actor_export_i env))) ^^
        ElemHeap.remember_reference env
      ]

and conclude_module env start_fi_o =

  Dfinity.default_exports env;
  GC.register env (E.get_end_of_static_memory env);

  let imports = E.get_imports env in
  let ni = List.length imports in
  let ni' = Int32.of_int ni in

  let funcs = E.get_funcs env in
  let nf = List.length funcs in
  let nf' = Wasm.I32.of_int_u nf in

  let table_sz = Int32.add nf' ni' in

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
      funcs = funcs;
      tables = [ nr { ttype = TableType ({min = table_sz; max = Some table_sz}, AnyFuncType) } ];
      elems = [ nr {
        index = nr 0l;
        offset = nr (G.to_instr_list (compile_unboxed_const ni'));
        init = List.mapi (fun i _ -> nr (Wasm.I32.of_int_u (ni + i))) funcs } ];
      start = start_fi_o;
      globals = globals;
      memories = [nr {mtype = MemoryType {min = 1024l; max = None}} ];
      imports;
      exports = E.get_exports env;
      data
    };
    types = E.get_dfinity_types env;
    persist =
           [ (OrthogonalPersistence.mem_global, CustomSections.DataBuf)
           ; (OrthogonalPersistence.elem_global, CustomSections.ElemBuf)
           ];
    function_names = E.get_func_names env;
    locals_names = E.get_func_local_names env;
  }

let compile mode (prelude : Syntax.prog) (progs : Syntax.prog list) : extended_module =
  let env = E.mk_global mode prelude ClosureTable.table_end in
  if E.mode env = DfinityMode then Dfinity.system_imports env;

  Array.common_funcs env;
  if E.mode env = DfinityMode then Message.system_funs env;

  let start_fun = compile_start_func env (prelude :: progs) in
  let start_fi = E.add_fun env start_fun in
  E.add_fun_name env start_fi "start";
  let start_fi_o =
    if E.mode env = DfinityMode
    then begin
      OrthogonalPersistence.register env start_fi;
      Dfinity.export_start_stub env;
      None
    end else Some (nr start_fi) in

  conclude_module env start_fi_o
