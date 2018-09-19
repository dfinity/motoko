open Wasm.Ast
open Wasm.Types

open Source
open Syntax


(* Helper functions to produce annotated terms *)
let nr x = { Wasm.Source.it = x; Wasm.Source.at = Wasm.Source.no_region }
let nr_ x = { it = x; at = no_region; note = () }
let nr__ x = { it = x; at = no_region; note = {note_typ = Type.Any; note_eff = Type.Triv } }


let todo fn se x = Printf.eprintf "%s: %s" fn (Wasm.Sexpr.to_string 80 se); x


(* The compiler environment.

It is almost immutable..

The mutable parts (`ref`) are used to register things like locals and
functions.  This should be monotone in the sense that entries are only added,
and that the order should not matter in a significant way.

*)

module E = struct

  (* Utilities, internal to E *)
  let reg (ref : 'a list ref) (x : 'a) : int32 =
      let i = Wasm.I32.of_int_u (List.length !ref) in
      ref := !ref @ [ x ];
      i

  (* The environment type *)
  module NameEnv = Env.Make(String)
  type t = {
    dfinity_mode : bool;

    (* Imports defined *)
    imports : import list ref;
    (* Expors defined *)
    exports : export list ref;
    (* Function defined in this module *)
    funcs : func list ref;
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
    (* Mapping ActorScript varables to WebAssembly locals *)
    local_vars_env : int32 NameEnv.t;
    (* Mapping primitives to WebAssembly locals *)
    primitives_env : int32 NameEnv.t;
    (* Field labels to index *)
    (* (This is for the prototypical simple tuple-implementations for objects *)
    field_env : int32 NameEnv.t ref;
  }

  let dfinity_mode (e : t) = e.dfinity_mode

  (* Common function types *)
  let start_fun_ty = nr (FuncType ([],[]))
  let start_fun_ty_i = 0l
  (* First argument is a pointer to the closure *)
  let unary_fun_ty = nr (FuncType ([I32Type; I32Type],[I32Type]))
  let unary_fun_ty_i = 1l
  (* Type of the system API *)
  let test_print_fun_ty = nr (FuncType ([I32Type],[]))
  let test_print_fun_ty_i = 2l
  let test_show_i32fun_ty = nr (FuncType ([I32Type],[I32Type]))
  let test_show_i32fun_ty_i = 3l
  let data_externalize_fun_ty = nr (FuncType ([I32Type; I32Type],[I32Type]))
  let data_externalize_fun_ty_i = 4l
  let default_fun_tys = [
      start_fun_ty;
      unary_fun_ty;
      test_print_fun_ty;
      test_show_i32fun_ty;
      data_externalize_fun_ty;
      ]

  (* Indices of local variables *)
  let tmp_local env : var = nr (env.n_param) (* first local after the params *)
  let unary_closure_local env : var = nr 0l (* first param *)
  let unary_param_local env : var = nr 1l   (* second param *)


  let init_field_env =
    NameEnv.add "get" 0l (
    NameEnv.add "set" 1l (
    NameEnv.add "len" 2l (
    NameEnv.add "keys" 3l (
    NameEnv.add "vals" 4l (
    NameEnv.empty
    )))))

  (* The initial global environment *)
  let mk_global (dfinity_mode : bool) : t = {
    dfinity_mode = dfinity_mode;
    imports = ref [];
    exports = ref [];
    funcs = ref [];
    func_types = ref default_fun_tys;
    (* Actually unused outside mk_fun_env: *)
    locals = ref [];
    local_vars_env = NameEnv.empty;
    primitives_env = NameEnv.empty;
    n_param = 0l;
    depth = 0l;
    ld = NameEnv.empty;
    field_env = ref init_field_env;
  }

  (* Resetting the environment for a new function *)
  let mk_fun_env env n_param =
    { env with
      locals = ref [I32Type]; (* the first tmp local *)
      n_param = n_param;
      local_vars_env = NameEnv.empty;
      depth = 0l;
      ld = NameEnv.empty;
      }

  let lookup_var env var =
    match NameEnv.find_opt var env.local_vars_env with
      | Some i -> Some (nr i)
      | None   -> Printf.eprintf "Could not find %s\n" var; None

  let lookup_prim env var =
    match NameEnv.find_opt var env.primitives_env with
      | Some i -> Some (nr i)
      | None   -> Printf.eprintf "Could not find primitive %s\n" var; None

  let add_anon_local (env : t) ty =
      let i = reg env.locals ty in
      Wasm.I32.add env.n_param i

  let add_local (env : t) name =
      let i = add_anon_local env I32Type in
      ({ env with local_vars_env = NameEnv.add name i env.local_vars_env }, i)

  let get_locals (env : t) = !(env.locals)

  let add_import (env : t) i =
    if !(env.funcs) = []
    then reg env.imports i
    else raise (Invalid_argument "add all imports before all functions!")

  let add_export (env : t) e = let _ = reg env.exports e in ()

  let add_fun (env : t) f =
    let i = reg env.funcs f in
    let n = Wasm.I32.of_int_u (List.length !(env.imports)) in
    Int32.add i n


  let add_prim (env : t) name i =
    { env with primitives_env = NameEnv.add name i env.primitives_env }

  let get_imports (env : t) = !(env.imports)
  let get_exports (env : t) = !(env.exports)
  let get_funcs (env : t) = !(env.funcs)

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
(* A hack! This needs to be disjoint from all other values *)
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

  let globals = [ nr
      { gtype = GlobalType (I32Type, Mutable);
        value = nr compile_zero } ]

  let heap_ptr : var = nr 0l

  let alloc (n : int32) : instr list =
    (* expect the size (in words), returns the pointer *)
    [ nr (GetGlobal heap_ptr);
      nr (GetGlobal heap_ptr) ] @
    compile_const (Wasm.I32.mul word_size n) @
    [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
      nr (SetGlobal heap_ptr)]

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

  let get_loc env var = match E.lookup_var env var with
    | Some i -> [ nr (GetLocal i) ]
    | None   -> [ nr Unreachable ]

  let set_loc env var = match E.lookup_var env var with
    | Some i -> [ nr (SetLocal i) ]
    | None   -> [ nr Unreachable ]

end (* Var *)

module Func = struct

  let load_the_closure = [ nr (GetLocal (nr 0l)) ]
  let load_closure i = load_the_closure @ Heap.load_field i
  let load_argument  = [ nr (GetLocal (nr 1l)) ]

  let unary_of_body env mk_body =
    (* Fresh set of locals *)
    (* Reserve two locals for closure and argument *)
    let env1 = E.mk_fun_env env 2l in
    let code = mk_body env1 in
    nr { ftype = nr E.unary_fun_ty_i;
         locals = E.get_locals env1;
         body = code
       }

  (* Expect the function closure and the argument on the stack *)
  let call env =
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
   [ nr (CallIndirect (nr E.unary_fun_ty_i)) ]

   (* Create a WebAssembly func from a pattern (for the argument) and the body.
   Parameter `captured` should contain the, well, captured local variables that
   the function will find in the closure. *)

  let compile_func env captured mk_pat mk_body : func =
    unary_of_body env (fun env1 ->
      (* Allocate locals for the captured variables *)
      let env2 = List.fold_left (fun e n -> fst (E.add_local e n)) env1 captured in
      (* Load the environment *)
      let load_capture i v =
          [ nr (GetLocal (E.unary_closure_local env2)) ] @
          Heap.load_field (Wasm.I32.of_int_u (1+i)) @
          Var.set_loc env2 v in
      let closure_code = List.concat (List.mapi load_capture captured) in
      (* Destruct the argument *)
      let (env3, alloc_args_code, destruct_args_code) = mk_pat env2  in

      (* Compile the body *)
      let body_code = mk_body env3 in

      closure_code @
      alloc_args_code @
      [ nr (GetLocal (E.unary_param_local env3)) ] @
      destruct_args_code @
      body_code)

  (* Compile a function declaration *)
  let dec pre_env last name captured mk_pat mk_body =
      let li = E.add_anon_local pre_env I32Type in
      let (pre_env1, vi) = E.add_local pre_env name.it in

      let alloc_code =
        (* Allocate a heap object for the function *)
        Heap.alloc (Wasm.I32.of_int_u (1 + List.length captured)) @
        [ nr (SetLocal (nr li)) ] @

        (* Allocate an extra indirection for the variable *)
        Tuple.lit pre_env1 [ [nr (GetLocal (nr li))] ] @
        [ nr (SetLocal (nr vi)) ]
      in

      ( pre_env1, alloc_code, fun env ->

	(* All functions are unary for now (arguments passed as heap-allocated tuples)
           with the closure itself passed as a first argument *)
        let f = compile_func env captured mk_pat mk_body in
        let fi = E.add_fun env f in

        (* Store the function number: *)
        [ nr (GetLocal (nr li));
          nr (Wasm.Ast.Const (nr (Wasm.Values.I32 fi))) ] @ (* Store function number *)
        Heap.store_field 0l @
        (* Store all captured values *)
        let store_capture i v =
          [ nr (GetLocal (nr li)) ] @
          Var.get_loc env v @
          Heap.store_field (Wasm.I32.of_int_u (1+i)) in
        List.concat (List.mapi store_capture captured) @
        if last then [ nr (GetLocal (nr li)) ] else [])

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


  (* Until we have a notion of static function reference, we need to allocate
     a closure for the function *)
  let allocate env n mk_fun =
    let fi = E.add_fun env (mk_fun env) in
    let i = E.add_anon_local env I32Type in
    let code = Tuple.lit env [ compile_const fi ] @
               [ nr (SetLocal (nr i)) ] in
    let env1 = E.add_prim env n i in
    (env1, code)

  let rec declare env = function
      | [] -> (env,[])
      | (n,f) :: xs -> let (env1, code1) = allocate env n f in
                       let (env2, code2) = declare env1 xs
                       in (env2, code1 @ code2)

  let lit env p =
    begin match E.lookup_prim env p with
    | Some i -> [ nr (GetLocal i) ]
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

  let lit env no co fs =
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
     let (env2, this_code) = match no with
      | Some name -> let (env2, ti) = E.add_local env1 name.it in
                     (env2, Tuple.lit env1 [ [nr (GetLocal (nr ri))] ] @
                            [ nr (SetLocal (nr ti)) ])
      | None -> (env1, []) in
     this_code @

     (* Write the class field *)
     [ nr (GetLocal (nr ri)) ] @
     (match co with | Some class_instrs -> class_instrs
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

module Array = struct
  let header_size = Int32.add Object.header_size 6l
  let element_size = 4l

  (* Indices of known global functions *)
  let fun_id env i = if E.dfinity_mode env then Int32.add i 3l else i
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
              mk_code (Func.load_closure 2l) [ nr (GetLocal (nr i)) ]
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

  (* function ids for imported stuff *)
  let test_print_i env = 0l
  let test_show_i32_i env = 1l
  let data_externalize_i env = 2l

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
    assert (Int32.to_int i == Int32.to_int (data_externalize_i env))

  let log32_fun env = Func.unary_of_body env (fun env1 ->
      Func.load_argument @
      [ nr (Call (nr (test_show_i32_i env))) ] @
      [ nr (Call (nr (test_print_i env))) ] @
      compile_unit
      )

  let print_fun env = Func.unary_of_body env (fun env1 ->
      (* Calculate the offset *)
      Func.load_argument @
      [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Int32.mul Heap.word_size Array.header_size)))) ;
       nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
      (* Calculate the length *)
      Func.load_argument @
      Heap.load_field (Array.len_field) @
      (* Externalize *)
      [ nr (Call (nr (data_externalize_i env))) ] @
      (* Call print *)
      [ nr (Call (nr (test_print_i env))) ] @
      compile_unit
      )

  let prims = [ "log32", log32_fun;
                "print", print_fun ]

  let export_start_fun env i =
    E.add_export env (nr {
      name = explode "start";
      edesc = nr (FuncExport (nr i))
    });
    (* these export seems to be wanted by the hypervisor/v8 *)
    E.add_export env (nr {
      name = explode "mem";
      edesc = nr (MemoryExport (nr 0l))
    });
    E.add_export env (nr {
      name = explode "table";
      edesc = nr (TableExport (nr 0l))
    })

end (* Dfinity *)


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
  | TextLit t     -> Array.lit env (List.map compile_const (List.map Int32.of_int (Wasm.Utf8.decode t)))
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
  | VarE _ | IdxE _ | DotE _ ->
     compile_lexp env exp @
     load_ptr
  | AssignE (e1,e2) ->
     compile_lexp env e1 @
     compile_exp env e2 @
     Heap.store_field 0l @
     compile_unit
  | LitE l_ref ->
     compile_lit env !l_ref
  | AssertE e1 ->
     compile_exp env e1 @
     [ nr (If ([I32Type], compile_unit, [nr Unreachable])) ]
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
     code1 @ [ nr (If ([I32Type], code2, code3)) ]
  | IsE (e1, e2) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp env e2 in
     code1 @ Heap.load_field Object.class_position @
     code2 @
     [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ]
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
  | RetE e -> compile_exp env e @ [ nr Return ]
  | OptE e -> compile_exp env e (* Subtype! *)

  | TupE [] -> compile_unit
  | TupE es -> Tuple.lit env (List.map (compile_exp env) es)
  | ArrayE es -> Array.lit env (List.map (compile_exp env) es)
  | ObjE (_, name, fs) ->
     (* TODO: This treats actors like any old object *)
     let fs' = List.map (fun (f : Syntax.exp_field) -> (f.it.id, fun env -> compile_exp env f.it.exp)) fs in
     Object.lit env (Some name) None fs'
  | CallE (e1, _, e2) ->
     compile_exp env e1 @
     compile_exp env e2 @
     Func.call env
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
       Func.call env1 @
       let oi = E.add_anon_local env I32Type in
       [ nr (SetLocal (nr oi)) ] @

       (* Check for null *)
       [ nr (GetLocal (nr oi)) ] @
       compile_null @
       [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ] @
       [ nr (If ([],
          [],
          alloc_code @ [ nr (GetLocal (nr oi)) ] @ code2 @ code3 @
          [ nr Drop ; nr (Br (nr 1l)) ]
       ))]
     ))] @
     compile_unit

  | _ -> todo "compile_exp" (Arrange.exp exp) [ nr Unreachable ]


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
  | OptP p -> compile_pat env fail_depth p
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
      let mk_body env1 = compile_exp env1 e in
      Func.dec pre_env last name captured mk_pat mk_body

  (* Classes are desguared to functions and objects. *)
  | ClassD (name, typ_params, s, p, efs) ->
      let captured = Freevars.captured_exp_fields p efs in
      let mk_pat env1 = compile_mono_pat env1 p in
      let mk_body env1 =
        (* TODO: This treats actors like any old object *)
        let fs' = List.map (fun (f : Syntax.exp_field) -> (f.it.id, fun env -> compile_exp env f.it.exp)) efs in
        (* this is run within the function. The first argument is a pointer
           to the closure, which happens to be the class function. *)
        let mk_class = Func.load_the_closure in
        Object.lit env1 None (Some mk_class) fs' in
      Func.dec pre_env last name captured mk_pat mk_body

and compile_decs env decs : Wasm.Ast.instr list = snd (compile_decs_block env decs)

and compile_decs_block env decs : (E.t * Wasm.Ast.instr list) =
  let rec go pre_env decs = match decs with
    | []          -> (pre_env, [], fun _ -> compile_unit) (* empty declaration list? *)
    | [dec]       -> compile_dec true pre_env dec
    | (dec::decs) ->
        let (pre_env1, alloc_code1, mk_code1) = compile_dec false pre_env dec    in
        let (pre_env2, alloc_code2, mk_code2) = go          pre_env1 decs in
        (pre_env2, alloc_code1 @ alloc_code2, fun env -> mk_code1 env @ mk_code2 env) in
  let (env1, alloc_code, mk_code) = go env decs in
  (env1, alloc_code @ mk_code env1)


and compile_start_func env (progs : Syntax.prog list) : func =
  (* Fresh set of locals *)
  let env1 = E.mk_fun_env env 0l in
  (* Allocate the primitive functions *)
  let (env2, code1) = Prim.declare env1 Prim.default_prims in
  let (env3, code2) =
    if E.dfinity_mode env2
    then Prim.declare env2 Dfinity.prims
    else (env2, []) in

  let rec go env = function
    | []          -> (env, [])
    | (prog::progs) ->
        let (env1, code1) = compile_decs_block env prog.it in
        let (env2, code2) = go env1 progs in
        (env2, code1 @ [ nr Drop ] @ code2) in

  let (env3, code3) = go env3 progs in

  nr { ftype = nr E.start_fun_ty_i;
       locals = E.get_locals env3;
       body = code1 @ code2 @ code3
     }

let compile dfinity_mode (progs : Syntax.prog list) : module_ =
  let env = E.mk_global dfinity_mode in

  if E.dfinity_mode env then Dfinity.system_imports env;
  Array.common_funcs env;

  let start_fun = compile_start_func env progs in
  let i = E.add_fun env start_fun in
  if E.dfinity_mode env then Dfinity.export_start_fun env i;


  let funcs = E.get_funcs env in
  let nf = List.length funcs in
  let nf' = Wasm.I32.of_int_u nf in

  nr { empty_module with
    types = E.get_types env;
    funcs = funcs;
    tables = [ nr { ttype = TableType ({min = nf'; max = Some nf'}, AnyFuncType) } ];
    elems = [ nr {
      index = nr 0l;
      offset = nr compile_zero;
      init = List.mapi (fun i _ -> nr (Wasm.I32.of_int_u i)) funcs } ];
    start = Some (nr i);
    globals = Heap.globals;
    memories = [nr {mtype = MemoryType {min = 1024l; max = None}} ];
    imports = E.get_imports env;
    exports = E.get_exports env;
  }
