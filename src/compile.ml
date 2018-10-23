open Wasm.Ast
open Wasm.Types

open Source
open Syntax


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
  { allocate : 'env -> instr list
  ; is_direct_call : int32 option
    (* a little backdoor. could be expanded into a general 'call' field *)
  }

type 'env varloc =
  | Local of int32   (* A Wasm Local in the current function *)
  | Static of int32  (* A static memory location in the current module *)
  | Deferred of 'env deferred_loc

module E = struct

  (* Utilities, internal to E *)
  let reg (ref : 'a list ref) (x : 'a) : int32 =
      let i = Wasm.I32.of_int_u (List.length !ref) in
      ref := !ref @ [ x ];
      i

  let reg_promise (ref : 'a Lib.Promise.t list ref) (x : 'a) : int32 =
      let i = Wasm.I32.of_int_u (List.length !ref) in
      ref := !ref @ [ Lib.Promise.make_fulfilled x ];
      i

  let reserve_promise (ref : 'a Lib.Promise.t list ref) : (int32 * ('a -> unit)) =
      let p = Lib.Promise.make () in
      let i = Wasm.I32.of_int_u (List.length !ref) in
      ref := !ref @ [ p ];
      (i, Lib.Promise.fulfill p)

  (* The environment type *)
  module NameEnv = Env.Make(String)
  type t = {
    mode : mode;

    (* Imports defined *)
    imports : import list ref;
    (* Exports defined *)
    exports : export list ref;
    (* Function defined in this module *)
    funcs : func Lib.Promise.t list ref;
    (* Types registered in this module *)
    func_types : func_type Wasm.Source.phrase list ref;
    (* Number of parameters in the current function, to calculate indices of locals *)
    n_param : int32;
    (* Types of locals *)
    locals : value_type list ref;
    (* Current block nesting depth *)
    depth : int32;
    (* A mapping from jump label to their depth *)
    ld : int32 NameEnv.t;
    (* Mapping ActorScript variables to WebAssembly locals, globals or functions *)
    local_vars_env : t varloc NameEnv.t;
    (* Mapping primitives to WebAssembly locals *)
    primitives_env : int32 NameEnv.t;
    (* Field labels to index *)
    (* (This is for the prototypical simple tuple-implementations for objects *)
    field_env : int32 NameEnv.t ref;
    (* The prelude. We need to re-use this when compiling actors *)
    prelude : prog;
    (* Exports that need a custom type for the hypervisor *)
    dfinity_types : (int32 * int32) list ref;
    (* Where does static memory end and dynamic memory begin? *)
    end_of_static_memory : int32 ref;
    (* Static memory defined so far *)
    static_memory : (int32 * string) list ref;
  }

  let mode (e : t) = e.mode

  (* Common function types *)
  let start_fun_ty = nr (FuncType ([],[]))
  let start_fun_ty_i = 0l
  (* First argument is a pointer to the closure *)
  let unary_fun_ty = nr (FuncType ([I32Type; I32Type],[I32Type]))
  let unary_fun_ty_i = 1l
  (* Actor message type *)
  let actor_message_ty = nr (FuncType ([I32Type],[]))
  let actor_message_ty_i = 2l
  let nullary_fun_ty = nr (FuncType ([],[]))
  let nullary_fun_ty_i = 3l
  (* Type of the system API *)
  let test_print_fun_ty = nr (FuncType ([I32Type],[]))
  let test_print_fun_ty_i = 4l
  let test_show_i32fun_ty = nr (FuncType ([I32Type],[I32Type]))
  let test_show_i32fun_ty_i = 5l
  let data_externalize_fun_ty = nr (FuncType ([I32Type; I32Type],[I32Type]))
  let data_externalize_fun_ty_i = 6l
  let data_internalize_fun_ty = nr (FuncType ([I32Type; I32Type; I32Type; I32Type],[]))
  let data_internalize_fun_ty_i = 7l
  let data_length_ty = nr (FuncType ([I32Type],[I32Type]))
  let data_length_fun_ty_i = 8l
  let module_new_fun_ty = nr (FuncType ([I32Type],[I32Type]))
  let module_new_fun_ty_i = 9l
  let actor_new_fun_ty = nr (FuncType ([I32Type],[I32Type]))
  let actor_new_fun_ty_i = 10l
  let actor_self_fun_ty = nr (FuncType ([],[I32Type]))
  let actor_self_fun_ty_i = 11l
  let actor_export_fun_ty = nr (FuncType ([I32Type; I32Type],[I32Type]))
  let actor_export_fun_ty_i = 12l
  let func_internalize_fun_ty = nr (FuncType ([I32Type; I32Type],[]))
  let func_internalize_fun_ty_i = 13l
  let default_fun_tys = [
      start_fun_ty;
      unary_fun_ty;
      actor_message_ty;
      nullary_fun_ty;
      test_print_fun_ty;
      test_show_i32fun_ty;
      data_externalize_fun_ty;
      data_internalize_fun_ty;
      data_length_ty;
      module_new_fun_ty;
      actor_new_fun_ty;
      actor_self_fun_ty;
      actor_export_fun_ty;
      func_internalize_fun_ty;
      ]

  (* Indices of local variables *)
  let tmp_local env : var = nr (env.n_param) (* first local after the params *)
  let unary_closure_local env : var = nr 0l (* first param *)
  let unary_param_local env : var = nr 1l   (* second param *)
  let message_param_local env : var = nr 0l

  let init_field_env =
    NameEnv.add "get" 0l (
    NameEnv.add "set" 1l (
    NameEnv.add "len" 2l (
    NameEnv.add "keys" 3l (
    NameEnv.add "vals" 4l (
    NameEnv.empty
    )))))

  (* The initial global environment *)
  let mk_global mode prelude : t = {
    mode;
    imports = ref [];
    exports = ref [];
    funcs = ref [];
    func_types = ref default_fun_tys;
    dfinity_types = ref [];
    (* Actually unused outside mk_fun_env: *)
    locals = ref [];
    local_vars_env = NameEnv.empty;
    primitives_env = NameEnv.empty;
    n_param = 0l;
    depth = 0l;
    ld = NameEnv.empty;
    field_env = ref init_field_env;
    prelude;
    end_of_static_memory = ref 0l;
    static_memory = ref [];
  }

  (* Resetting the environment for a new function *)
  let mk_fun_env env n_param =
    (* We keep all local vars that are bound to known functions or globals *)
    let is_non_local = function
      | Local _ -> false
      | Static _ -> true
      | Deferred _ -> true
    in
    { env with
      locals = ref [I32Type]; (* the first tmp local *)
      n_param = n_param;
      local_vars_env = NameEnv.filter (fun _ -> is_non_local) env.local_vars_env;
      depth = 0l;
      ld = NameEnv.empty;
      }

  let lookup_var env var =
    match NameEnv.find_opt var env.local_vars_env with
      | Some l -> Some l
      | None   -> Printf.eprintf "Could not find %s\n" var; None

  let lookup_prim env var =
    match NameEnv.find_opt var env.primitives_env with
      | Some i -> Some i
      | None   -> Printf.eprintf "Could not find primitive %s\n" var; None

  let add_anon_local (env : t) ty =
      let i = reg env.locals ty in
      Wasm.I32.add env.n_param i

  let add_local (env : t) name =
      let i = add_anon_local env I32Type in
      ({ env with local_vars_env = NameEnv.add name (Local i) env.local_vars_env }, i)

  let add_local_static (env : t) name ptr =
      { env with local_vars_env = NameEnv.add name (Static ptr) env.local_vars_env }

  let add_local_deferred (env : t) name d =
      { env with local_vars_env = NameEnv.add name (Deferred d) env.local_vars_env }

  let get_locals (env : t) = !(env.locals)

  let in_scope_set (env : t) =
    let l = env.local_vars_env in
    NameEnv.fold (fun k _ -> Freevars.S.add k) l Freevars.S.empty

  let add_import (env : t) i =
    if !(env.funcs) = []
    then reg env.imports i
    else raise (Invalid_argument "add all imports before all functions!")

  let add_export (env : t) e = let _ = reg env.exports e in ()

  let add_dfinity_type (env : t) e = let _ = reg env.dfinity_types e in ()

  let add_fun (env : t) f =
    let i = reg_promise env.funcs f in
    let n = Wasm.I32.of_int_u (List.length !(env.imports)) in
    Int32.add i n

  let reserve_fun (env : t) =
    let (i, fill) = reserve_promise env.funcs in
    let n = Wasm.I32.of_int_u (List.length !(env.imports)) in
    (Int32.add i n, fill)

  let add_prim (env : t) name i =
    { env with primitives_env = NameEnv.add name i env.primitives_env }

  let get_imports (env : t) = !(env.imports)
  let get_exports (env : t) = !(env.exports)
  let get_dfinity_types (env : t) = !(env.dfinity_types)
  let get_funcs (env : t) = List.map Lib.Promise.value !(env.funcs)

  (* Currently unused, until we add functions to the table *)
  let _add_type (env : t) ty = reg env.func_types ty

  let get_types (env : t) = !(env.func_types)

  let current_depth (env : t) = env.depth

  let inc_depth (env : t) =
      let label_depths' = Wasm.I32.add env.depth 1l in
      {env with depth = label_depths'}

  (* This is a bit ugly, and maybe can be avoided if the last component returned
     by compile_pat takes its own env argument, like compile_dec *)
  let reset_depth (env1 : t) (env2 : t) =
      { env1 with depth = env2.depth }

  let depth_to (env) i = Wasm.I32.sub env.depth i

  let add_label (env : t) name =
      { env with ld = NameEnv.add name.it (env.depth) env.ld }

  let get_label_depth (env : t) name =
    match NameEnv.find_opt name.it env.ld with
      | Some i -> Wasm.I32.sub env.depth i
      | None   -> Printf.eprintf "Could not find %s\n" name.it; raise Not_found

  let field_to_index (env : t) name : int32 =
    let e = !(env.field_env) in
    match NameEnv.find_opt name.it e with
      | Some i -> i
      | None ->
        let i = Wasm.I32.of_int_u (NameEnv.cardinal e) in
        env.field_env := NameEnv.add name.it i e;
        i

  let get_prelude (env : t) = env.prelude

  let reserve_static_memory (env : t) : int32 =
    let ptr = !(env.end_of_static_memory) in
    env.end_of_static_memory := Int32.add ptr 4l;
    ptr

  let add_static_bytes (env : t) data : int32 =
    let ptr = !(env.end_of_static_memory) in
    env.end_of_static_memory := Int32.add ptr (Int32.of_int (String.length data));
    env.static_memory := !(env.static_memory) @ [ (ptr, data) ];
    ptr

  let get_end_of_static_memory env : int32 = !(env.end_of_static_memory)

  let get_static_memory env = !(env.static_memory)
end

(* General code generation functions:
   Rule of thumb: Here goes stuff that independent of the ActorScript AST.
*)

(* Function called compile_* return a list of instructions (and maybe other stuff) *)

let compile_true =    [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 1l))) ]
let compile_false =   [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 0l))) ]
let compile_zero =    [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 0l))) ]
let compile_unit =    [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 0l))) ]
let compile_const i = [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 i))) ]
(* This needs to be disjoint from all pointers *)
let compile_null =  [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 Int32.max_int))) ]


(* Stack utilities *)

let dup env : instr list = (* duplicate top element *)
  [ nr (TeeLocal (E.tmp_local env));
    nr (GetLocal (E.tmp_local env)) ]

let _swap env : instr list = (* swaps top elements *)
  let i = E.add_anon_local env I32Type in
  [ nr (SetLocal (E.tmp_local env));
    nr (SetLocal (nr i));
    nr (GetLocal (E.tmp_local env));
    nr (GetLocal (nr i))]

(* Heap and allocations *)


let load_ptr : instr list =
  [ nr (Load {ty = I32Type; align = 2; offset = 0l; sz = None}) ]

let store_ptr : instr list =
  [ nr (Store {ty = I32Type; align = 2; offset = 0l; sz = None}) ]

module Heap = struct

  (* General heap object functionalty (allocation, setting fields, reading fields) *)

  (* Until we have GC, we simply keep track of the end of the used heap
     in this global, and bump it if we allocate stuff.
     Memory addresses are 32 bit (I32Type).
     *)
  let word_size = 4l

  let heap_ptr : var = nr 1l

  let alloc_bytes (n : int32) : instr list =
    (* expect the size (in words), returns the pointer *)
    [ nr (GetGlobal heap_ptr);
      nr (GetGlobal heap_ptr) ] @
    compile_const n @
    [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
      nr (SetGlobal heap_ptr)]

  let alloc (n : int32) : instr list =
    alloc_bytes (Wasm.I32.mul word_size n)

  let load_field (i : int32) : instr list =
    [ nr (Load {ty = I32Type; align = 2; offset = Wasm.I32.mul word_size i; sz = None}) ]

  let store_field (i : int32) : instr list =
    [ nr (Store {ty = I32Type; align = 2; offset = Wasm.I32.mul word_size i; sz = None}) ]

end (* Heap *)

module Tuple = struct
  (* A tuple is a heap object with a statically known number of elements.
     This is also used as the primitive representation of objects etc. *)

  (* The argument is a list of functions that receive a function that
     puts the pointer to the array itself on to the stack, for recursive
     structures *)
  let lit_rec env element_instructions : instr list =
    let n = List.length element_instructions in

    let i = E.add_anon_local env I32Type in
    Heap.alloc (Wasm.I32.of_int_u n) @
    [ nr (SetLocal (nr i)) ] @

    let compile_self = [ nr (GetLocal (nr i)) ] in

    let init_elem idx instrs : Wasm.Ast.instr list =
      compile_self @
      instrs compile_self @
      Heap.store_field (Wasm.I32.of_int_u idx)
    in
    List.concat (List.mapi init_elem element_instructions) @

    compile_self

  let lit env eis = lit_rec env (List.map (fun x _ -> x) eis)

end (* Tuple *)

module Var = struct

  (* When accessing a variable that is a static function, then we need to create a
     heap-allocated thing on the fly. *)
  let static_fun_pointer fi env =
    Tuple.lit env [
      compile_const fi
    ]

  let get_val env var = match E.lookup_var env var with
    | Some (Local i)  -> [ nr (GetLocal (nr i)) ] @ load_ptr
    | Some (Static i) -> compile_const i @ load_ptr
    | Some (Deferred d) -> d.allocate env
    | None   -> [ nr Unreachable ]

  let get_loc env var = match E.lookup_var env var with
    | Some (Local i) -> [ nr (GetLocal (nr i)) ]
    | Some (Static i) -> compile_const i
    (* We have to do some boxing here *)
    | _ -> Tuple.lit env [ get_val env var ]

  let set_loc env var = match E.lookup_var env var with
    | Some (Local i) -> [ nr (SetLocal (nr i)) ]
    | Some (Static i) ->
      [ nr (SetLocal (E.tmp_local env)) ] @
      compile_const i @
      [ nr (GetLocal (E.tmp_local env)) ] @
      store_ptr
    | Some (Deferred _) -> raise (Invalid_argument "Cannot set heap location for a deferred thing")
    | None   -> [ nr Unreachable ]

end (* Var *)

module Opt = struct

let inject env e = Tuple.lit env [e]
let project = Heap.load_field 0l

end (* Opt *)

module Func = struct

  let load_the_closure = [ nr (GetLocal (nr 0l)) ]
  let load_closure i = load_the_closure @ Heap.load_field i
  let load_argument  = [ nr (GetLocal (nr 1l)) ]

  let static_function_id fi =
    (* should be different from any pointer *)
    Int32.add (Int32.mul fi Heap.word_size) 1l

  let unary_of_body env mk_body =
    (* Fresh set of locals *)
    (* Reserve two locals for closure and argument *)
    let env1 = E.mk_fun_env env 2l in
    let code = mk_body env1 in
    nr { ftype = nr E.unary_fun_ty_i;
         locals = E.get_locals env1;
         body = code
       }

  let nullary_of_body env mk_body =
    (* Fresh set of locals *)
    (* Reserve one local, no arguments *)
    let env1 = E.mk_fun_env env 0l in
    let code = mk_body env1 in
    nr { ftype = nr E.nullary_fun_ty_i;
         locals = E.get_locals env1;
         body = code
       }

  (* The argument on the stack *)
  let call_direct env fi at =
   (* Pop the argument *)
   let i = E.add_anon_local env I32Type in
   [ nr (SetLocal (nr i)) ] @

   (* First arg: The (unused) closure pointer *)
   compile_null @

   (* Second arg: The argument *)
   [ nr (GetLocal (nr i)) ] @

   (* All done: Call! *)
   [ Call (nr fi) @@ at ]

  (* Expect the function closure and the argument on the stack *)
  let call_indirect env at =
   (* Pop the argument *)
   let i = E.add_anon_local env I32Type in
   [ nr (SetLocal (nr i)) ] @

   (* Pop the closure pointer *)
   let fi = E.add_anon_local env I32Type in
   [ nr (SetLocal (nr fi)) ] @

   (* First arg: The closure pointer *)
   [ nr (GetLocal (nr fi)) ] @
   (* Second arg: The argument *)
   [ nr (GetLocal (nr i)) ] @
   (* And now get the table index *)
   [ nr (GetLocal (nr fi)) ] @
   Heap.load_field 0l @
   (* All done: Call! *)
   [ CallIndirect (nr E.unary_fun_ty_i) @@ at ]

   (* Create a WebAssembly func from a pattern (for the argument) and the body.
   Parameter `captured` should contain the, well, captured local variables that
   the function will find in the closure. *)

  let compile_func env captured mk_pat mk_body at : func =
    unary_of_body env (fun env1 ->
      (* Allocate locals for the captured variables *)
      let env2 = List.fold_left (fun e n -> fst (E.add_local e n)) env1 captured in
      (* Load the environment *)
      let load_capture i v =
          [ GetLocal (E.unary_closure_local env2) @@ at ] @
          Heap.load_field (Wasm.I32.of_int_u (1+i)) @
          Var.set_loc env2 v in
      let closure_code = List.concat (List.mapi load_capture captured) in
      (* Destruct the argument *)
      let (env3, alloc_args_code, destruct_args_code) = mk_pat env2  in

      (* Compile the body *)
      let body_code = mk_body env3 in

      closure_code @
      alloc_args_code @
      [ GetLocal (E.unary_param_local env3) @@ at ] @
      destruct_args_code @
      body_code)

  (* Compile a closed function declaration (has no free variables) *)
  let dec_closed pre_env last name mk_pat mk_body at =
      let (fi, fill) = E.reserve_fun pre_env in
      let d = { allocate = Var.static_fun_pointer fi; is_direct_call = Some fi } in
      let pre_env1 = E.add_local_deferred pre_env name.it d in
      ( pre_env1, [], fun env ->
        let mk_body' env = mk_body env (compile_const (static_function_id fi)) in
        let f = compile_func env [] mk_pat mk_body' at in
        fill f;
        if last then d.allocate env else [])

  (* Compile a closure declaration (has free variables) *)
  let dec_closure pre_env last name captured mk_pat mk_body at =
      let li = E.add_anon_local pre_env I32Type in
      let (pre_env1, vi) = E.add_local pre_env name.it in

      let alloc_code =
        (* Allocate a heap object for the function *)
        Heap.alloc (Wasm.I32.of_int_u (1 + List.length captured)) @
        [ SetLocal (li @@ at) @@ at ] @

        (* Allocate an extra indirection for the variable *)
        Tuple.lit pre_env1 [ [GetLocal (li @@ at) @@ at ] ] @
        [ SetLocal (vi @@ at) @@ at ]
      in

      ( pre_env1, alloc_code, fun env ->

	(* All functions are unary for now (arguments passed as heap-allocated tuples)
           with the closure itself passed as a first argument *)
        let mk_body' env = mk_body env load_the_closure in
        let f = compile_func env captured mk_pat mk_body' at in
        let fi = E.add_fun env f in

        (* Store the function number: *)
        [ GetLocal (li @@ at) @@ at;
          Wasm.Ast.Const (Wasm.Values.I32 fi @@ at) @@ at ] @ (* Store function number *)
        Heap.store_field 0l @
        (* Store all captured values *)
        let store_capture i v =
          [ GetLocal (li @@ at) @@ at ] @
          Var.get_loc env v @
          Heap.store_field (Wasm.I32.of_int_u (1+i)) in
        List.concat (List.mapi store_capture captured) @
        if last then [ GetLocal (li @@ at) @@ at ] else [])

  let dec pre_env last name captured mk_pat mk_body at =
    (* This could be smarter: It is ok to capture closed functions,
       but then we would have to move the call to compile_func in dec_closed
       above into the continuation. *)
    if captured = []
    then dec_closed pre_env last name mk_pat mk_body at
    else dec_closure pre_env last name captured mk_pat mk_body at

end (* Func *)

(* Primitive functions *)
module Prim = struct

  let abs_fun env = Func.unary_of_body env (fun env1 ->
      Func.load_argument @
      compile_zero @
      [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LtS));
        nr (If ([I32Type],
          compile_zero @
          Func.load_argument @
          [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ],
          Func.load_argument
        ))
      ])

  let register env n mk_fun =
    let fi = E.add_fun env (mk_fun env) in
    let env1 = E.add_prim env n fi in
    env1

  let rec declare env = function
      | [] -> env
      | (n,f) :: xs -> let env1 = register env n f in
                       let env2 = declare env1 xs
                       in env2

  let lit env p =
    begin match E.lookup_prim env p with
    (* This could be optimized if surrounded by a let or call *)
    | Some fi -> Var.static_fun_pointer fi env
    | None   -> [ nr Unreachable ]
    end

  let default_prims = [ "abs", abs_fun ]

end (* Prim *)

module Object = struct
  (* First word: Class pointer (0x1, an invalid pointer, when none) *)
  let header_size = 1l

  let class_position = 0l

  let default_header = [ compile_const 1l ]

  (* Takes the header into account *)
  let field_position env f =
     let fi = E.field_to_index env f in
     let i = Int32.add header_size fi in
     i

  let lit env this_name_opt class_option fs =
     (* Find largest index, to know the size of the heap representation *)
     let max a b = if Int32.compare a b >= 0 then a else b in
     let n = Int32.add header_size (
             Int32.add 1l (List.fold_left max 0l (List.map (fun (id, _) -> E.field_to_index env id) fs))) in

     (* Allocate memory *)
     let ri = E.add_anon_local env I32Type in
     Heap.alloc n @
     [ nr (SetLocal (nr ri)) ] @

     (* Bind the fields in the envrionment *)
     (* We could omit that if we extend E.local_vars_env to also have an offset,
        and just bind all of them to 'ri' *)
     let mk_field_ptr (env, code) (id, mk_is) =
       let (env', fi) = E.add_local env id.it in
       let offset = Wasm.I32.mul 4l (field_position env id) in
       let code' = [ nr (GetLocal (nr ri));
                     nr (Wasm.Ast.Const (nr (Wasm.Values.I32 offset)));
                     nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
                     nr (SetLocal (nr fi)); ] in
       (env', code @ code') in
     let (env1, field_code) = List.fold_left mk_field_ptr (env, []) fs in
     field_code @

     (* An extra indirection for the 'this' pointer, if present *)
     let (env2, this_code) = match this_name_opt with
      | Some name -> let (env2, ti) = E.add_local env1 name.it in
                     (env2, Tuple.lit env1 [ [nr (GetLocal (nr ri))] ] @
                            [ nr (SetLocal (nr ti)) ])
      | None -> (env1, []) in
     this_code @

     (* Write the class field *)
     [ nr (GetLocal (nr ri)) ] @
     (match class_option with
       | Some class_instrs -> class_instrs
       | None -> compile_const 1l ) @
     Heap.store_field class_position @

     (* Write all the fields *)
     let init_field (id, mk_is) : Wasm.Ast.instr list =
        let i = field_position env id in
        [ nr (GetLocal (nr ri)) ] @
	mk_is env2 @
        Heap.store_field i
     in
     List.concat (List.map init_field fs) @

     (* Return the pointer to the object *)
     [ nr (GetLocal (nr ri)) ]

  let idx env f =
     let i = field_position env f in
     [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Wasm.I32.mul 4l i)))) ] @
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ]

  let load_idx env f =
     let i = field_position env f in
     Heap.load_field i

end (* Object *)

module Text = struct
  let header_size = 1l

  let len_field = 0l

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
    let len = Int32.of_int (String.length s) in
    let len_bytes = bytes_of_int32 len in
    let data = len_bytes ^ s in
    let ptr = E.add_static_bytes env data in
    compile_const ptr

end (* String *)

module Array = struct
  let header_size = Int32.add Object.header_size 6l
  let element_size = 4l

  (* Indices of known global functions *)
  let fun_id env i =
    let ni = List.length (E.get_imports env) in
    let ni' = Int32.of_int ni in
    Int32.add i ni'

  let array_get_funid       env = fun_id env 0l
  let array_set_funid       env = fun_id env 1l
  let array_len_funid       env = fun_id env 2l
  let array_keys_funid      env = fun_id env 3l
  let array_keys_next_funid env = fun_id env 4l
  let array_vals_funid      env = fun_id env 5l
  let array_vals_next_funid env = fun_id env 6l

  let len_field = Int32.add Object.header_size 5l


  (* Expects on the stack the pointer to the array and the index
     of the element, and returns the point to the element. *)
  let idx =
    compile_const header_size @
    [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
    compile_const element_size @
    [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Mul)) ] @
    [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ]

  let common_funcs env =
    let get_array_object = Func.load_closure 1l in
    let get_single_arg =   Func.load_argument in
    let get_first_arg =    Func.load_argument @ Heap.load_field 0l in
    let get_second_arg =   Func.load_argument @ Heap.load_field 1l in

    let get_fun = Func.unary_of_body env (fun env1 ->
            get_array_object @
            get_single_arg @ (* the index *)
            idx @
            load_ptr
       ) in
    let set_fun = Func.unary_of_body env (fun env1 ->
            get_array_object @
            get_first_arg @ (* the index *)
            idx @
            get_second_arg @ (* the value *)
            store_ptr @
            compile_unit
       ) in
    let len_fun = Func.unary_of_body env (fun env1 ->
            get_array_object @
            Heap.load_field len_field
       ) in

    let mk_next_fun mk_code = Func.unary_of_body env (fun env1 ->
            let i = E.add_anon_local env1 I32Type in
            (* Get pointer to counter from closure *)
            Func.load_closure 1l @
            (* Read pointer *)
            load_ptr @
            [ nr (SetLocal (nr i)) ] @

            [ nr (GetLocal (nr i)) ] @
            (* Get pointer to array from closure *)
            Func.load_closure 2l @
            (* Get length *)
            Heap.load_field len_field @
            [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ] @
            [ nr (If ([I32Type],
              (* Then *)
              compile_null,
              (* Else *)
              (* Get point to counter from closure *)
              Func.load_closure 1l @
              (* Store increased counter *)
              [ nr (GetLocal (nr i)) ] @
              compile_const 1l @
              [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
              store_ptr @
              (* Return stuff *)
              Opt.inject (E.inc_depth env) (
                mk_code (Func.load_closure 2l) [ nr (GetLocal (nr i)) ]
              )
            )) ]
       ) in
    let mk_iterator next_funid = Func.unary_of_body env (fun env1 ->
            (* counter *)
            let i = E.add_anon_local env1 I32Type in
            Tuple.lit env1 [ compile_zero ] @
            [ nr (SetLocal (nr i)) ] @

            (* next function *)
            let ni = E.add_anon_local env1 I32Type in
            Tuple.lit env1 [
              compile_const next_funid;
              [ nr (GetLocal (nr i)) ];
              get_array_object ] @
            [ nr (SetLocal (nr ni)) ] @

            Object.lit env1 None None
              [ (nr_ "next", fun _ -> [ nr (GetLocal (nr ni)) ]) ]
       ) in


    let keys_next_fun = mk_next_fun (fun get_array get_i ->
              (* Return old value *)
              get_i
            ) in
    let keys_fun env = mk_iterator (array_keys_next_funid env) in

    let vals_next_fun = mk_next_fun (fun get_array get_i ->
              (* Lookup old value *)
              get_array @
              get_i @
              idx @
              load_ptr
            ) in
    let vals_fun env = mk_iterator (array_vals_next_funid env) in

    let i = E.add_fun env get_fun in
    assert (Int32.to_int i == Int32.to_int (array_get_funid env));

    let i = E.add_fun env set_fun in
    assert (Int32.to_int i == Int32.to_int (array_set_funid env));

    let i = E.add_fun env len_fun in
    assert (Int32.to_int i == Int32.to_int (array_len_funid env));

    let i = E.add_fun env (keys_fun env) in
    assert (Int32.to_int i == Int32.to_int (array_keys_funid env));

    let i = E.add_fun env keys_next_fun in
    assert (Int32.to_int i == Int32.to_int (array_keys_next_funid env));

    let i = E.add_fun env (vals_fun env) in
    assert (Int32.to_int i == Int32.to_int (array_vals_funid env));

    let i = E.add_fun env vals_next_fun in
    assert (Int32.to_int i == Int32.to_int (array_vals_next_funid env))

  (* Compile an array literal. *)
  let lit env element_instructions =
    Tuple.lit_rec env
     (List.map (fun i _ -> i) Object.default_header @
      [ (fun compile_self ->
        Tuple.lit env [ compile_const (array_get_funid env); compile_self])
      ; (fun compile_self ->
        Tuple.lit env [ compile_const (array_set_funid env); compile_self])
      ; (fun compile_self ->
        Tuple.lit env [ compile_const (array_len_funid env); compile_self])
      ; (fun compile_self ->
        Tuple.lit env [ compile_const (array_keys_funid env); compile_self])
      ; (fun compile_self ->
        Tuple.lit env [ compile_const (array_vals_funid env); compile_self])
      ; (fun compile_self ->
        compile_const (Wasm.I32.of_int_u (List.length element_instructions)))
      ] @ List.map (fun is _ -> is) element_instructions)

end (* Array *)

module Dfinity = struct

  (* We use the first table slot for calls to funcrefs *)
  (* This does not clash with slots for our functions as long as there
     is at least one imported function (which we do not add to the table) *)
  let tmp_table_slot = 0l

  (* function ids for imported stuff *)
  let test_print_i env = 0l
  let test_show_i32_i env = 1l
  let data_externalize_i env = 2l
  let data_internalize_i env = 3l
  let data_length_i env = 4l
  let module_new_i env = 5l
  let actor_new_i env = 6l
  let actor_self_i env = 7l
  let actor_export_i env = 8l
  let func_internalize_i env = 9l

  (* function ids for predefined functions (after array functions) *)
  let funcref_wrapper_i env = 17l
  let self_message_wrapper_i env = 18l

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
      idesc = nr (FuncImport (nr E.test_print_fun_ty_i))
    }) in
    assert (Int32.to_int i == Int32.to_int (test_print_i env));

    let i = E.add_import env (nr {
      module_name = explode "test";
      item_name = explode "show_i32";
      idesc = nr (FuncImport (nr E.test_show_i32fun_ty_i))
    }) in
    assert (Int32.to_int i == Int32.to_int (test_show_i32_i env));

    let i = E.add_import env (nr {
      module_name = explode "data";
      item_name = explode "externalize";
      idesc = nr (FuncImport (nr E.data_externalize_fun_ty_i))
    }) in
    assert (Int32.to_int i == Int32.to_int (data_externalize_i env));

    let i = E.add_import env (nr {
      module_name = explode "data";
      item_name = explode "internalize";
      idesc = nr (FuncImport (nr E.data_internalize_fun_ty_i))
    }) in
    assert (Int32.to_int i == Int32.to_int (data_internalize_i env));

    let i = E.add_import env (nr {
      module_name = explode "data";
      item_name = explode "length";
      idesc = nr (FuncImport (nr E.data_length_fun_ty_i))
    }) in
    assert (Int32.to_int i == Int32.to_int (data_length_i env));

    let i = E.add_import env (nr {
      module_name = explode "module";
      item_name = explode "new";
      idesc = nr (FuncImport (nr E.module_new_fun_ty_i))
    }) in
    assert (Int32.to_int i == Int32.to_int (module_new_i env));

    let i = E.add_import env (nr {
      module_name = explode "actor";
      item_name = explode "new";
      idesc = nr (FuncImport (nr E.actor_new_fun_ty_i))
    }) in
    assert (Int32.to_int i == Int32.to_int (actor_new_i env));

    let i = E.add_import env (nr {
      module_name = explode "actor";
      item_name = explode "self";
      idesc = nr (FuncImport (nr E.actor_self_fun_ty_i))
    }) in
    assert (Int32.to_int i == Int32.to_int (actor_self_i env));

    let i = E.add_import env (nr {
      module_name = explode "actor";
      item_name = explode "export";
      idesc = nr (FuncImport (nr E.actor_export_fun_ty_i))
    }) in
    assert (Int32.to_int i == Int32.to_int (actor_export_i env));

    let i = E.add_import env (nr {
      module_name = explode "func";
      item_name = explode "internalize";
      idesc = nr (FuncImport (nr E.func_internalize_fun_ty_i))
    }) in
    assert (Int32.to_int i == Int32.to_int (func_internalize_i env))

  let system_funs env =
    let f = Func.unary_of_body env (fun env1 ->
      compile_const tmp_table_slot @ (* slot number *)
      Func.load_closure 1l @ (* the funcref *)
      [ nr (Call (nr (func_internalize_i env))) ] @

      Func.load_argument @ (* Needs to be serialized somehow, can only pass i32 now *)
      compile_const tmp_table_slot @
      [ nr (CallIndirect (nr E.actor_message_ty_i)) ] @
      compile_unit
      ) in
    let fi = E.add_fun env f in
    assert (Int32.to_int fi == Int32.to_int (funcref_wrapper_i env));

    let f = Func.unary_of_body env (fun env1 ->
      compile_const tmp_table_slot @ (* slot number *)

      (* Create a funcref for the message *)
      [ nr (Call (nr (actor_self_i env))) ] @
      Func.load_closure 1l @ (* the databuf with the message name *)
      [ nr (Call (nr (actor_export_i env))) ] @

      (* Internalize *)
      [ nr (Call (nr (func_internalize_i env))) ] @

      Func.load_argument @ (* Needs to be serialized somehow, can only pass i32 now *)

      compile_const tmp_table_slot @
      [ nr (CallIndirect (nr E.actor_message_ty_i)) ] @
      compile_unit
      ) in
    let fi = E.add_fun env f in
    assert (Int32.to_int fi == Int32.to_int (self_message_wrapper_i env))

  let compile_databuf_of_bytes env (bytes : string) =
    Text.lit env bytes @

    let i = E.add_anon_local env I32Type in
    [ nr (SetLocal (nr i)) ] @

    (* Calculate the offset *)
    [ nr (GetLocal (nr i)) ] @
    compile_const (Int32.mul Heap.word_size Text.header_size) @
    [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
    (* Calculate the length *)
    [ nr (GetLocal (nr i)) ] @
    Heap.load_field (Text.len_field) @

    (* Externalize *)
    [ nr (Call (nr (data_externalize_i env))) ]

  (* For debugging *)
  let _compile_static_print env s =
      compile_databuf_of_bytes env s @
      [ nr (Call (nr (test_print_i env))) ]

  let static_self_message_pointer name env =
    Tuple.lit env [
      compile_const (self_message_wrapper_i env);
      compile_databuf_of_bytes env name.it
    ]

  let printInt_fun env = Func.unary_of_body env (fun env1 ->
      Func.load_argument @
      [ nr (Call (nr (test_show_i32_i env))) ] @
      [ nr (Call (nr (test_print_i env))) ] @
      compile_unit
      )

  let print_fun env = Func.unary_of_body env (fun env1 ->
      (* Calculate the offset *)
      Func.load_argument @
      compile_const (Int32.mul Heap.word_size Text.header_size) @
      [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
      (* Calculate the length *)
      Func.load_argument @
      Heap.load_field (Text.len_field) @
      (* Externalize *)
      [ nr (Call (nr (data_externalize_i env))) ] @
      (* Call print *)
      [ nr (Call (nr (test_print_i env))) ] @
      compile_unit
      )

  let prims : (string * (E.t -> func)) list =
    [ "printInt", printInt_fun;
      "print",    print_fun ]

  let stub_prims : (string * (E.t -> func)) list =
    [ "printInt", (fun env -> Func.unary_of_body env (fun _ -> [ nr Unreachable ]));
      "print",    (fun env -> Func.unary_of_body env (fun _ -> [ nr Unreachable ])) ]

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

  let export_start_fun env fi =
    E.add_export env (nr {
      name = explode "start";
      edesc = nr (FuncExport (nr fi))
    });

end (* Dfinity *)

module OrthogonalPersistence = struct
  (* This module implements the code that fakes orthogonal persistence *)

  let restore_mem_i env = 19l
  let save_mem_i env = 20l
  let mem_global = 0l

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
     * save_mem simply copies the whole memory (up to the end-of-memory pointer)
       to a new databuf and stores that in memstore.

    This does not persist references yet.
  *)

  let register pre_env =
    let (fi, fill_restore_mem) = E.reserve_fun pre_env in
    assert (Int32.to_int fi == Int32.to_int (restore_mem_i pre_env));

    let (fi, fill_save_mem) = E.reserve_fun pre_env in
    assert (Int32.to_int fi == Int32.to_int (save_mem_i pre_env));

    E.add_export pre_env (nr {
      name = Dfinity.explode "datastore";
      edesc = nr (GlobalExport (nr mem_global))
    });

    (fun env start_funid ->
      fill_restore_mem (Func.nullary_of_body env (fun env1 ->
         [ nr (GetGlobal (nr mem_global)) ] @
         [ nr (Call (nr (Dfinity.data_length_i env1))) ] @
         compile_const 0l @
         [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ] @
         [ nr (If ([],
           (* First run, call the start function *)
           [ nr (Call (nr start_funid)) ],

           (* Subsequent run *)
           (* Store length in global *)
           [ nr (GetGlobal (nr mem_global));
             nr (Call (nr (Dfinity.data_length_i env1)));
             nr (SetGlobal Heap.heap_ptr) ] @

           (* Load memory *)
           compile_zero @
           [ nr (GetGlobal Heap.heap_ptr) ] @
           [ nr (GetGlobal (nr mem_global)) ] @
           compile_zero @
           [ nr (Call (nr (Dfinity.data_internalize_i env1))) ]
         )) ]
      ));
      fill_save_mem (Func.nullary_of_body env (fun env1 ->
         compile_zero @
         [ nr (GetGlobal Heap.heap_ptr) ] @
         [ nr (Call (nr (Dfinity.data_externalize_i env))) ] @
         [ nr (SetGlobal (nr mem_global)) ]
      ))
    )


end (* OrthogonalPersistence *)

module Message = struct
  (* This module could be part of Func, if not for the reference to
     the module OrthogonalPersistence *)

  let message_of_body env mk_body =
    (* Fresh set of locals *)
    (* Reserve one local, only one argument *)
    let env1 = E.mk_fun_env env 1l in
    let code = mk_body env1 in
    nr { ftype = nr E.actor_message_ty_i;
         locals = E.get_locals env1;
         body = code
       }


  (* Message take no closure *)
  let compile env mk_pat mk_body at : func =
    message_of_body env (fun env1 ->
      (* Set up memory *)
      [ nr (Call (nr (OrthogonalPersistence.restore_mem_i env))) ] @

      (* Destruct the argument *)
      let (env2, alloc_args_code, destruct_args_code) = mk_pat env1  in

      (* Compile the body *)
      let body_code = mk_body env2 in

      alloc_args_code @
      [ GetLocal (E.message_param_local env2) @@ at ] @
      destruct_args_code @
      body_code @
      [ nr Drop ] @

      (* Save memory *)
      [ nr (Call (nr (OrthogonalPersistence.save_mem_i env))) ]
      )
end (* Message *)

(* The actual compiler code that looks at the AST *)

let compile_lit env lit = match lit with
  | BoolLit true ->  compile_true
  | BoolLit false -> compile_false
  (* This maps int to int32, instead of a proper arbitrary precision library *)
  | IntLit n      ->
    (try [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Big_int.int32_of_big_int n)))) ]
    with Failure _ -> Printf.eprintf "compile_lit: Overflow in literal %s\n" (Big_int.string_of_big_int n); [ nr Unreachable ])
  | NatLit n      ->
    (try [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Big_int.int32_of_big_int n)))) ]
    with Failure _ -> Printf.eprintf "compile_lit: Overflow in literal %s\n" (Big_int.string_of_big_int n); [ nr Unreachable ])
  | NullLit       -> compile_null
  | TextLit t     -> Text.lit env t
  | _ -> todo "compile_lit" (Arrange.lit lit) [ nr Unreachable ]

let compile_unop env op = match op with
  | NegOp ->
      [ nr (SetLocal (E.tmp_local env)) ] @
      compile_zero @
      [ nr (GetLocal (E.tmp_local env));
        nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ]
  | PosOp -> []
  | _ -> todo "compile_unop" (Arrange.unop op) [ nr Unreachable ]

let compile_binop op = match op with
  | AddOp -> [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ]
  | SubOp -> [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ]
  | MulOp -> [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Mul)) ]
  | _ -> todo "compile_binop" (Arrange.binop op) [ nr Unreachable ]

let compile_relop op = match op with
  | EqOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ]
  | GeOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.GeS)) ]
  | GtOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.GtS)) ]
  | LeOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LeS)) ]
  | LtOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LtS)) ]
  | _ -> todo "compile_relop" (Arrange.relop op) [ nr Unreachable ]


(* compile_lexp is used for expressions on the left of an
assignment operator, and calculates (puts on the stack) the
memory location of such a thing. *)
let rec compile_lexp (env : E.t) exp = match exp.it with
  | VarE var ->
     Var.get_loc env var.it
  | IdxE (e1,e2) ->
     compile_exp env e1 @ (* offset to array *)
     compile_exp env e2 @ (* idx *)
     Array.idx
  | DotE (e, f) ->
     compile_exp env e @
     Object.idx env f
  | _ -> todo "compile_lexp" (Arrange.exp exp) [ nr Unreachable ]

(* compile_exp returns an *value*.
Currently, number (I32Type) are just repesented as such, but other
types may be point (e.g. for function, array, tuple, object).

Local variables (which maybe mutable, or have delayed initialisation)
are also points, but points to such values, and need to be read first.  *)
and compile_exp (env : E.t) exp = match exp.it with
  (* We can reuse the code in compile_lexp here *)
  | IdxE _ | DotE _ ->
     compile_lexp env exp @
     load_ptr
  | VarE var ->
     Var.get_val env var.it
  | AssignE (e1,e2) ->
     compile_lexp env e1 @
     compile_exp env e2 @
     Heap.store_field 0l @
     compile_unit
  | LitE l_ref ->
     compile_lit env !l_ref
  | AssertE e1 ->
     compile_exp env e1 @
     [ If ([I32Type], compile_unit, [nr Unreachable]) @@ exp.at ]
  | NotE e ->
     compile_exp env e @
     [ nr (If ([I32Type], compile_false, compile_true)) ]
  | PrimE p -> Prim.lit env p
  | UnE (op, e1) ->
     compile_exp env e1 @
     compile_unop env op
  | BinE (e1, op, e2) ->
     compile_exp env e1 @
     compile_exp env e2 @
     compile_binop op
  | RelE (e1, op, e2) ->
     compile_exp env e1 @
     compile_exp env e2 @
     compile_relop op
  | OrE (e1, e2) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp (E.inc_depth env) e2 in
     code1 @ [ nr (If ([I32Type], compile_true, code2)) ]
  | AndE (e1, e2) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp (E.inc_depth env) e2 in
     code1 @ [ nr (If ([I32Type], code2, compile_false)) ]
  | IfE (e1, e2, e3) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp (E.inc_depth env) e2 in
     let code3 = compile_exp (E.inc_depth env) e3 in
     code1 @ [ If ([I32Type], code2, code3) @@ exp.at ]
  | IsE (e1, e2) ->
     (* There are two cases: Either the class is a pointer to
        the object on the RHS, or it is -- mangled -- the
        function id stored therein *)
     let code1 = compile_exp env e1 in
     let code2 = compile_exp env e2 in
     let i = E.add_anon_local env I32Type in
     let j = E.add_anon_local env I32Type in
     code1 @ Heap.load_field Object.class_position @
     [ nr (SetLocal (nr i)) ] @
     code2 @
     [ nr (SetLocal (nr j)) ] @
     (* Equal? *)
     [ nr (GetLocal (nr i)) ] @
     [ nr (GetLocal (nr j)) ] @
     [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ] @
     [ nr (If ([I32Type], compile_true,
       (* Static function id? *)
       [ nr (GetLocal (nr i)) ] @
       [ nr (GetLocal (nr j)) ] @
       Heap.load_field 0l @ (* get the function id *)
       compile_const Heap.word_size @ (* mangle *)
       [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Mul)) ] @
       compile_const 1l @ (* mangle *)
       [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
       [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ]
      )) ]
  | BlockE decs ->
     compile_decs env decs
  | DecE dec ->
     compile_decs env [dec]
  | LabelE (name, _ty, e) ->
      let env1 = E.add_label (E.inc_depth env) name in
      let code = compile_exp env1 e in
      [ nr (Block ([I32Type],code)) ]
  | BreakE (name, _ty) ->
      let i = E.get_label_depth env name in
      compile_unit @ [ nr (Br (nr i)) ]
  | LoopE (e, None) ->
     let code = compile_exp (E.inc_depth env) e in
     [ nr (Loop ([], code @ [ nr (Br (nr 0l)) ])) ] @
     [ nr Unreachable ]
  | WhileE (e1, e2) ->
     let code1 = compile_exp (E.inc_depth env) e1 in
     let code2 = compile_exp (E.inc_depth (E.inc_depth env)) e2 in
     [ nr (Loop ([], code1 @ [ nr (If ([], code2 @ [ nr Drop;  nr (Br (nr 1l)) ], [])) ])) ] @
     compile_unit
  | AnnotE (e, t) -> compile_exp env e
  | RetE e -> compile_exp env e @ [ Return @@ exp.at ]
  | OptE e ->
     Opt.inject env (compile_exp env e)
  | TupE [] -> compile_unit
  | TupE es -> Tuple.lit env (List.map (compile_exp env) es)
  | ArrayE es -> Array.lit env (List.map (compile_exp env) es)
  | ObjE ({ it = Type.Object; _}, name, fs) ->
     let fs' = List.map (fun (f : Syntax.exp_field) -> (f.it.id, fun env -> compile_exp env f.it.exp)) fs in
     Object.lit env (Some name) None fs'
  | ObjE ({ it = Type.Actor; _}, name, fs) ->
    let captured = Freevars.exp exp in
    let prelude_names = find_prelude_names env in
    if Freevars.S.is_empty (Freevars.S.diff captured prelude_names)
    then actor_lit env name fs
    else todo "non-closed actor" (Arrange.exp exp) [ nr Unreachable ]
  | CallE (e1, _, e2) when isDirectCall env e1 <> None ->
     let fi = Lib.Option.value (isDirectCall env e1) in
     compile_exp env e2 @
     Func.call_direct env fi exp.at
  | CallE (e1, _, e2) ->
     compile_exp env e1 @
     compile_exp env e2 @
     Func.call_indirect env exp.at
  | SwitchE (e, cs) ->
    let code1 = compile_exp env e in
    let i = E.add_anon_local env I32Type in

    let rec go env cs = match cs with
      | [] -> [ nr Unreachable ]
      | (c::cs) ->
          let pat = c.it.pat in
          let e = c.it.exp in
          let env1 = E.inc_depth env in
          let (env2, alloc_code, code) = compile_pat env1 (E.current_depth env1) pat in
          alloc_code @
          [ nr (Block ([I32Type],
              [ nr (GetLocal (nr i)) ] @
              code @
              compile_true
            ));
            nr (If ([I32Type],
              (* This is a bit of a hack: We increase the depth in compile_pat
                 for the Block. We ought to decrease it, and then increase it
                 again for the If.. but the result is the same *)
              compile_exp env2 e,
              go env1 cs));
          ] in
      let code2 = go env cs in
      code1 @ [ nr (SetLocal (nr i)) ] @ code2
  | ForE (p, e1, e2) ->
     let code1 = compile_exp env e1 in
     let (env1, alloc_code, code2) = compile_mono_pat (E.inc_depth env) p in
     let code3 = compile_exp env1 e2 in

     let i = E.add_anon_local env I32Type in
     (* Store the iterator *)
     code1 @
     [ nr (SetLocal (nr i)) ] @

     [ nr (Loop ([],
       [ nr (GetLocal (nr i)) ] @
       Object.load_idx env1 (nr__ "next") @
       compile_unit @
       Func.call_indirect env1 Source.no_region @
       let oi = E.add_anon_local env I32Type in
       [ nr (SetLocal (nr oi)) ] @

       (* Check for null *)
       [ nr (GetLocal (nr oi)) ] @
       compile_null @
       [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ] @
       [ nr (If ([],
          [],
          alloc_code @ [ nr (GetLocal (nr oi)) ] @ Opt.project @ code2 @ code3 @
          [ nr Drop ; nr (Br (nr 1l)) ]
       ))]
     ))] @
     compile_unit
  | _ -> todo "compile_exp" (Arrange.exp exp) [ nr Unreachable ]


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

and compile_lit_pat env fail_depth opo l = match opo, l with
  | None, (NatLit _ | IntLit _ | NullLit) ->
    compile_lit env l @
    [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ]
  | Some uo, (NatLit _ | IntLit _) ->
    compile_lit env l @
    compile_unop env uo @
    [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ]
  | _ -> todo "compile_lit_pat" (Arrange.lit l) [ nr Unreachable ]

and compile_pat env fail_depth pat : E.t * Wasm.Ast.instr list * Wasm.Ast.instr list = match pat.it with
  (* It returns:
     - the extended environment
     - the code to allocate memory
     - the code to do the pattern matching.
       This expects the  undestructed value is on top of the stack,
       consumes it, and fills the heap
       If the pattern does not match, it branches to the depth at fail_depth.
  *)
  | WildP -> (env, [], [ nr Drop ])
  | AnnotP (p, _) -> compile_pat env fail_depth p
  | OptP p ->
      let (env1, alloc_code1, code1) = compile_pat (E.inc_depth env) fail_depth p in
      let i = E.add_anon_local env I32Type in
      let code =
        [ nr (SetLocal (nr i)) ] @
        [ nr (GetLocal (nr i)) ] @
        compile_null @
        [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ] @
        [ nr (If ([],
                  compile_fail (E.inc_depth env) fail_depth,
                  [ nr (GetLocal (nr i)) ] @
                  Opt.project @
                  code1
          )) ] in
      let env2 = E.reset_depth env1 env in
      (env2, alloc_code1, code)
  | LitP l ->
      let code =
        compile_lit_pat env fail_depth None !l @
        [ nr (If ([],
                  [],
                  compile_fail (E.inc_depth env) fail_depth
          )) ]
      in (env, [], code)
  | SignP (op, l) ->
      let code =
        compile_lit_pat env fail_depth (Some op) !l @
        [ nr (If ([],
                  [],
                  compile_fail (E.inc_depth env) fail_depth
          )) ]
      in (env, [], code)

  | VarP name ->
      let (env1,i) = E.add_local env name.it; in
      let alloc_code = Heap.alloc 1l @ [ nr (SetLocal (nr i)) ] in
      let code =
        [ nr (SetLocal (E.tmp_local env));
          nr (GetLocal (nr i));
          nr (GetLocal (E.tmp_local env)) ] @
          store_ptr in
      (env1, alloc_code, code)
  | TupP ps ->
      let rec go i ps env = match ps with
        | [] -> (env, [], [ nr Drop ])
        | (p::ps) ->
          let (env1, alloc_code1, code1) = compile_pat env fail_depth p in
          let (env2, alloc_code2, code2) = go (i+1) ps env1 in
          ( env2,
            alloc_code1 @ alloc_code2,
            dup env @ Heap.load_field (Wasm.I32.of_int_u i) @ code1 @ code2) in
      go 0 ps env

  | AltP (p1, p2) ->
      let env1 = E.inc_depth env in
      let (env2, alloc_code1, code1) = compile_pat env1 (E.current_depth env1) p1 in
      let env2' = E.inc_depth env2 in
      let (env3, alloc_code2, code2) = compile_pat env2' (E.current_depth env2') p2 in
      let env4 = E.reset_depth env3 env in

      let i = E.add_anon_local env I32Type in
      let code =
        [ nr (SetLocal (nr i));
          nr (Block ([I32Type],
            [ nr (GetLocal (nr i)) ] @
            code1 @
            compile_true
          ));
          nr (If ([],
            [],
            [ nr (Block ([I32Type],
                [ nr (GetLocal (nr i)) ] @
                code2 @
                compile_true
              ));
              nr (If ([],
                [],
                compile_fail env3 fail_depth
              ))
            ]
          ));
       ] in
      (env4, alloc_code1 @ alloc_code2,  code)

and compile_fail env fail_depth =
  let t = E.depth_to env fail_depth in
  compile_false @ [ nr (Br (nr t)) ]

(* Used for mono patterns (let, function arguments) *)
and compile_mono_pat env pat =
  let (env1, alloc_code, code) = compile_pat (E.inc_depth env) (E.current_depth env) pat in
  let wrapped_code =
    [ nr (SetLocal (E.tmp_local env));
      nr (Block ([I32Type],
        [ nr (GetLocal (E.tmp_local env)) ] @
        code @
        compile_true
      ));
      nr (If ([],[], [ nr Unreachable ]))
    ] in
  (env1, alloc_code, wrapped_code)

and compile_dec last pre_env dec : E.t * Wasm.Ast.instr list * (E.t -> Wasm.Ast.instr list) = match dec.it with
  | TypD _ -> (pre_env, [], fun _ -> [])
  | ExpD e ->
    (pre_env, [], fun env ->
      let code = compile_exp env e in
      let drop = if last then [] else [nr Drop] in
      code @ drop
    )
  | LetD (p, e) ->
    let (pre_env1, alloc_code, code2) = compile_mono_pat pre_env p in
    ( pre_env1, alloc_code, fun env ->
      let code1 = compile_exp env e in
      let stack_fix = if last then dup env else [] in
      code1 @ stack_fix @ code2)
  | VarD (name, e) ->
      let (pre_env1, i) = E.add_local pre_env name.it in

      let alloc_code = Heap.alloc 1l @ [ nr (SetLocal (nr i)) ] in

      ( pre_env1, alloc_code, fun env ->
        let code1 = compile_exp env e in
        [ nr (GetLocal (nr i)) ] @
        code1 @
        store_ptr @
        if last then [ nr (GetLocal (nr i)) ] @ load_ptr else [])

  | FuncD (name, _, p, _rt, e) ->
      (* Get captured variables *)
      let captured = Freevars.captured p e in
      let mk_pat env1 = compile_mono_pat env1 p in
      let mk_body env1 _ = compile_exp env1 e in
      Func.dec pre_env last name captured mk_pat mk_body dec.at

  (* Classes are desguared to functions and objects. *)
  | ClassD (name, typ_params, s, p, efs) ->
      let captured = Freevars.captured_exp_fields p efs in
      let mk_pat env1 = compile_mono_pat env1 p in
      let mk_body env1 compile_fun_identifier =
        (* TODO: This treats actors like any old object *)
        let fs' = List.map (fun (f : Syntax.exp_field) -> (f.it.id, fun env -> compile_exp env f.it.exp)) efs in
        (* this is run within the function. The class id is the function
	identifier, as provided by Func.dec:
	For closures it is the pointer to the closure.
	For functions it is the function id (shifted to never class with pointers) *) 
        Object.lit env1 None (Some compile_fun_identifier) fs' in
      Func.dec pre_env last name captured mk_pat mk_body dec.at

and compile_decs env decs : Wasm.Ast.instr list = snd (compile_decs_block env true decs)

and compile_decs_block env keep_last decs : (E.t * Wasm.Ast.instr list) =
  let rec go pre_env decs = match decs with
    | []          -> (pre_env, [], fun _ -> if keep_last then compile_unit else []) (* empty declaration list? *)
    | [dec]       -> compile_dec keep_last pre_env dec
    | (dec::decs) ->
        let (pre_env1, alloc_code1, mk_code1) = compile_dec false pre_env dec    in
        let (pre_env2, alloc_code2, mk_code2) = go          pre_env1 decs in
        (pre_env2, alloc_code1 @ alloc_code2, fun env -> mk_code1 env @ mk_code2 env) in
  let (env1, alloc_code, mk_code) = go env decs in
  (env1, alloc_code @ mk_code env1)

and compile_prelude env =
  (* Allocate the primitive functions *)
  let env1 = Prim.declare env Prim.default_prims in
  let env2 = Prim.declare env1
    ( match E.mode env1 with
      | WasmMode ->    Dfinity.stub_prims
      | DfinityMode -> Dfinity.prims ) in
  let (env3, code) = compile_decs_block env2 false (E.get_prelude env).it in
  (env3, code)

(* Is this a hack? When determining whether an actor is closed,
we should disregard the prelude, because every actor is compiled with the
prelude. So this function compiles the prelude, just to find out the bound names.
*)
and find_prelude_names env =
  (* Create a throw-away environment *)
  let env1 = E.mk_fun_env (E.mk_global (E.mode env) (E.get_prelude env)) 0l in
  let (env2, _) = compile_prelude env1 in
  E.in_scope_set env2


and compile_start_func env (progs : Syntax.prog list) : func =
  (* Fresh set of locals *)
  let env1 = E.mk_fun_env env 0l in
  (* Allocate the primitive functions *)
  let env2 = Prim.declare env1 Prim.default_prims in
  let env3 = Prim.declare env2
    ( match E.mode env2 with
      | WasmMode ->    Dfinity.stub_prims
      | DfinityMode -> Dfinity.prims ) in

  let rec go env = function
    | []          -> (env, [])
    | (prog::progs) ->
        let (env1, code1) = compile_decs_block env false prog.it in
        let (env2, code2) = go env1 progs in
        (env2, code1 @ code2) in

  let (env4, code) = go env3 progs in

  nr { ftype = nr E.start_fun_ty_i;
       locals = E.get_locals env4;
       body = code
     }

and compile_private_actor_field pre_env (f : Syntax.exp_field)  =
  let ptr = E.reserve_static_memory pre_env in
  let pre_env1 = E.add_local_static pre_env f.it.id.it ptr in
  ( pre_env1, fun env ->
    compile_const ptr @
    compile_exp env f.it.exp @
    store_ptr
  )

and compile_public_actor_field pre_env (f : Syntax.exp_field) =
  let (name, _, pat, _rt, exp) =
    let rec find_func exp = match exp.it with
    | AnnotE (exp, _) -> find_func exp
    | DecE {it = FuncD (name, ty_args, pat, rt, exp); _ } -> (name, ty_args, pat, rt, exp)
    | _ -> raise (Invalid_argument "public actor field not a function")
    in find_func f.it.exp in

  (* Which name to use? f.it.id or name? Can they differ? *)
  let (fi, fill) = E.reserve_fun pre_env in
  E.add_dfinity_type pre_env (fi, 1l);
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
    []
  )

and compile_actor_field pre_env (f : Syntax.exp_field) =
  if f.it.priv.it = Private
  then compile_private_actor_field pre_env f
  else compile_public_actor_field pre_env f

and compile_actor_fields env fs =
  (* We need to tie the knot about the enrivonment *)
  let rec go env = function
    | []          -> (env, fun _ -> [])
    | (f::fs) ->
        let (env1, mk_code1) = compile_actor_field env f in
        let (env2, mk_code2) = go env1 fs in
        (env2, fun env -> mk_code1 env @ mk_code2 env) in
  let (env1, mk_code2) = go env fs in
  (env1, mk_code2 env1)

(* This function wraps an actor ref as an object, with all fields
   prepared to be callable as normal function.
   TODO: Store the actual actorref in a special field of the object
   representation, for further serialization.
   TODO: Needs more type information
 *)
and compile_actorref_wrapper env fields =
  (* The actor ref is on the stack *)
  let actorref_i = E.add_anon_local env I32Type in
  [ nr (SetLocal (nr actorref_i)) ] @

  let wrap_field name =
    (* Create a closure object that calls the funcref *)
    let code env =
      Tuple.lit env
        [ compile_const (Dfinity.funcref_wrapper_i env)
        ; [ nr (GetLocal (nr actorref_i)) ] @
          Dfinity.compile_databuf_of_bytes env (name.it) @
          [ nr (Call (nr (Dfinity.actor_export_i env))) ]
        ] in
    (name, code) in
  Object.lit env None None (List.map wrap_field fields)


and actor_lit outer_env name fs =
  let wasm =
    let env = E.mk_global (E.mode outer_env) (E.get_prelude outer_env) in

    if E.mode env = DfinityMode then Dfinity.system_imports env;
    Array.common_funcs env;
    if E.mode env = DfinityMode then Dfinity.system_funs env;

    let finish_op_register =
      if E.mode env = DfinityMode
      then OrthogonalPersistence.register env
      else fun _ _ -> () in

    let env1 = E.mk_fun_env env 0l in
    (* Compile stuff here *)
    let (env2, prelude_code) = compile_prelude env1 in
    let (env3, init_code )  = compile_actor_fields env2 fs in

    let start_fun = nr { ftype = nr E.start_fun_ty_i;
         locals = E.get_locals env3;
         body = prelude_code @ init_code
    } in
    let start_fi = E.add_fun env3 start_fun in

    finish_op_register env3 start_fi;

    let (m, custom_sections) = conclude_module env3 None in
    let (_map, wasm) = EncodeMap.encode m in
    wasm ^ custom_sections in

  Dfinity.compile_databuf_of_bytes outer_env wasm @

  (* Create actorref *)
  [ nr (Call (nr (Dfinity.module_new_i outer_env))) ] @
  [ nr (Call (nr (Dfinity.actor_new_i outer_env))) ] @

  (* Create an object around it *)
  compile_actorref_wrapper outer_env (List.map (fun (f : Syntax.exp_field) -> f.it.id) fs)

and conclude_module env start_fi_o =

  Dfinity.default_exports env;

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
        value = nr compile_zero
      };
      (* End-of-heap pointer *)
      nr { gtype = GlobalType (I32Type, Mutable);
        value = nr (compile_const (E.get_end_of_static_memory env))
      }
      ] in

  let data = List.map (fun (offset, init) -> nr {
    index = nr 0l;
    offset = nr (compile_const offset);
    init;
    }) (E.get_static_memory env) in

  let m = nr {
    types = E.get_types env;
    funcs = funcs;
    tables = [ nr { ttype = TableType ({min = table_sz; max = Some table_sz}, AnyFuncType) } ];
    elems = [ nr {
      index = nr 0l;
      offset = nr (compile_const ni');
      init = List.mapi (fun i _ -> nr (Wasm.I32.of_int_u (ni + i))) funcs } ];
    start = start_fi_o;
    globals = globals;
    memories = [nr {mtype = MemoryType {min = 1024l; max = None}} ];
    imports;
    exports = E.get_exports env;
    data
  } in

  (* Calculate the custom sections *)
  let dfinity_types = E.get_dfinity_types env in
  let custom_sections =
    CustomSections.encode ni' dfinity_types [ OrthogonalPersistence.mem_global ] in
  (m, custom_sections)


let compile mode (prelude : Syntax.prog) (progs : Syntax.prog list) : (module_ * string) =
  let env = E.mk_global mode prelude in

  if E.mode env = DfinityMode then Dfinity.system_imports env;
  Array.common_funcs env;
  if E.mode env = DfinityMode then Dfinity.system_funs env;

  let start_fun = compile_start_func env (prelude :: progs) in
  let start_fi = E.add_fun env start_fun in
  let start_fi_o =
    if E.mode env = DfinityMode
    then (Dfinity.export_start_fun env start_fi; None)
    else Some (nr start_fi) in

  conclude_module env start_fi_o
