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
    (* Field labels to index *)
    (* (This is for the prototypical simple tuple-implementations for objects *)
    field_env : int32 NameEnv.t ref;
  }

  (* Common function types *)
  let start_fun_ty = nr (FuncType ([],[]))
  let start_fun_ty_i = 0l
  (* First argument is a pointer to the closure *)
  let unary_fun_ty = nr (FuncType ([I32Type; I32Type],[I32Type]))
  let unary_fun_ty_i = 1l
  let default_fun_tys = [start_fun_ty; unary_fun_ty]

  (* Indices of local variables *)
  let tmp_local env : var = nr (env.n_param) (* first local after the params *)
  let unary_closure_local env : var = nr 0l (* first param *)
  let unary_param_local env : var = nr 1l   (* second param *)

  (* Indices of known global functions *)
  let array_get_funid = 0l
  let array_set_funid = 1l
  let array_len_funid = 2l


  (* The initial global environment *)
  let mk_global (_ : unit) : t = {
    funcs = ref [];
    func_types = ref default_fun_tys;
    (* Actually unused outside mk_fun_env: *)
    locals = ref [];
    local_vars_env = NameEnv.empty;
    n_param = 0l;
    depth = 0l;
    ld = NameEnv.empty;
    field_env = ref NameEnv.empty;
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

  let add_anon_local (env : t) ty =
      let i = reg env.locals ty in
      Wasm.I32.add env.n_param i

  let add_local (env : t) name =
      let i = add_anon_local env I32Type in
      ({ env with local_vars_env = NameEnv.add name i env.local_vars_env }, i)

  let get_locals (env : t) = !(env.locals)

  let add_fun (env : t) f = reg env.funcs f

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

let swap env : instr list = (* swaps top elements *)
  let i = E.add_anon_local env I32Type in
  [ nr (SetLocal (E.tmp_local env));
    nr (SetLocal (nr i));
    nr (GetLocal (E.tmp_local env));
    nr (GetLocal (nr i))]

(* Heap and allocations *)

(* Until we have GC, we simply keep track of the end of the used heap
   in this global, and bump it if we allocate stuff.
   Memory addresses are 32 bit (I32Type).
   *)
let heap_ptr_global = nr
      { gtype = GlobalType (I32Type, Mutable);
        value = nr compile_zero }
let heap_ptr : var = nr 0l

let _alloc env : instr list = (* expect the size on the stack, returns the pointer *)
  [ nr (SetLocal (E.tmp_local env));
    nr (GetGlobal heap_ptr);
    nr (GetGlobal heap_ptr);
    nr (GetLocal (E.tmp_local env));
    nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
    nr (SetGlobal heap_ptr)]

let allocn (n : int32) : instr list =
  (* expect the size (in words), returns the pointer *)
  [ nr (GetGlobal heap_ptr);
    nr (GetGlobal heap_ptr);
    nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Wasm.I32.mul 4l n))));
    nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
    nr (SetGlobal heap_ptr)]

let load_field (i : int32) : instr list =
  [ nr (Load {ty = I32Type; align = 2; offset = Wasm.I32.mul 4l i; sz = None}) ]

let store_field (i : int32) : instr list =
  [ nr (Store {ty = I32Type; align = 2; offset = Wasm.I32.mul 4l i; sz = None}) ]

(* Compilation functions for the various syntactic categories *)

let compile_lit lit = match lit with
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
  | LtOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LtS)) ]
  | _ -> todo "compile_relop" (Arrange.relop op) [ nr Unreachable ]

let rec get_var_loc env var = match E.lookup_var env var with
  | Some i -> [ nr (GetLocal i) ]
  | None   -> [ nr Unreachable ]

and set_var_loc env var = match E.lookup_var env var with
  | Some i -> [ nr (SetLocal i) ]
  | None   -> [ nr Unreachable ]

(* compile_lexp is used for expressions on the left of an
assignment operator, and calculates (puts on the stack) the
memory location of such a thing. *)
and compile_lexp (env : E.t) exp = match exp.it with
  | VarE var ->
     get_var_loc env var.it
  | IdxE (e1,e2) ->
     compile_exp env e1 @ (* offset to array *)
     compile_exp env e2 @ (* idx *)
     [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 4l))) ] @
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
     [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 4l))) ] @
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Mul)) ] @
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ]
  | DotE (e, f) ->
     let i = E.field_to_index env f in
     compile_exp env e @
     [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Wasm.I32.mul 4l i)))) ] @
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ]
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
     load_field 0l
  | AssignE (e1,e2) ->
     compile_lexp env e1 @
     compile_exp env e2 @
     store_field 0l @
     compile_unit
  | LitE l_ref ->
     compile_lit !l_ref
  | AssertE e1 ->
     compile_exp env e1 @
     [ nr (If ([I32Type], compile_unit, [nr Unreachable])) ]
  | NotE e ->
     compile_exp env e @
     [ nr (If ([I32Type], compile_false, compile_true)) ]
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

  | TupE es ->
     (* Allocate memory, and put position on the stack (return value) *)
     allocn (Wasm.I32.of_int_u (List.length es)) @
     let init_elem i e : Wasm.Ast.instr list =
        dup env @ (* Duplicate position *)
	compile_exp env e @
        store_field (Wasm.I32.of_int_u i)
     in
     List.concat (List.mapi init_elem es)

  | ArrayE es ->
     (* Allocate memory, and put position on the stack (return value) *)
     let i = E.add_anon_local env I32Type in
     allocn (Wasm.I32.of_int_u (4 + List.length es)) @
     [ nr (SetLocal (nr i)) ] @

     (* Allocate closures for  get, set, len,
        and put them on the stack.
        Also put the length on the stack. *)
     allocn (Wasm.I32.of_int_u 2) @
     dup env @
     compile_const E.array_get_funid @
     store_field 0l @
     dup env @
     [ nr (GetLocal (nr i)) ] @
     store_field 1l @

     allocn (Wasm.I32.of_int_u 2) @
     dup env @
     compile_const E.array_set_funid @
     store_field 0l @
     dup env @
     [ nr (GetLocal (nr i)) ] @
     store_field 1l @

     allocn (Wasm.I32.of_int_u 2) @
     dup env @
     compile_const E.array_len_funid @
     store_field 0l @
     dup env @
     [ nr (GetLocal (nr i)) ] @
     store_field 1l @

     compile_const (Wasm.I32.of_int_u (List.length es)) @

     (* Write these four things to the array *)
     [ nr (GetLocal (nr i)) ] @ swap env @ store_field 3l @
     [ nr (GetLocal (nr i)) ] @ swap env @ store_field 2l @
     [ nr (GetLocal (nr i)) ] @ swap env @ store_field 1l @
     [ nr (GetLocal (nr i)) ] @ swap env @ store_field 0l @

     let init_elem idx e : Wasm.Ast.instr list =
        [ nr (GetLocal (nr i)) ] @
	compile_exp env e @
        store_field (Wasm.I32.of_int_u (4 + idx))
     in
     List.concat (List.mapi init_elem es) @
     [ nr (GetLocal (nr i)) ]

  | ObjE (_, name, fs) -> (* TODO: This treats actors like any old object *) 
     (* Resolve fields to index *)
     let fis = List.map (fun (f : exp_field) -> (E.field_to_index env (f.it.id), f.it.exp)) fs in

     (* Find largest index, to know the size of the heap representation *)
     let max a b = if Int32.compare a b >= 0 then a else b in
     let n = Int32.add 1l (List.fold_left max 0l (List.map (fun (f : exp_field) -> E.field_to_index env (f.it.id)) fs)) in

     (* Allocate memory *)
     let ri = E.add_anon_local env I32Type in
     allocn n @
     [ nr (SetLocal (nr ri)) ] @

     (* Bind the fields in the envrionment *)
     (* We could omit that if we extend E.local_vars_env to also have an offset,
        and just bind all of them to 'ri' *)
     let mk_field_ptr (env, code) (f : exp_field) =
       let (env', fi) = E.add_local env f.it.id.it in
       let offset = Wasm.I32.mul 4l (E.field_to_index env (f.it.id)) in
       let code' = [ nr (GetLocal (nr ri));
                     nr (Wasm.Ast.Const (nr (Wasm.Values.I32 offset)));
                     nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
                     nr (SetLocal (nr fi)); ] in
       (env', code @ code') in
     let (env1, field_code) = List.fold_left mk_field_ptr (env, []) fs in
     field_code @

     (* An extra indirection for the 'this' pointer *)
     let (env2, ti) = E.add_local env1 name.it in
     allocn 1l @
     [ nr (TeeLocal (nr ti)) ] @
     [ nr (GetLocal (nr ri)) ] @
     store_field 0l @

     (* Write all the fields *)
     let init_field (i, e) : Wasm.Ast.instr list =
        [ nr (GetLocal (nr ri)) ] @
	compile_exp env2 e @
        store_field i
     in
     List.concat (List.map init_field fis) @

     (* Return the pointer to the object *)
     [ nr (GetLocal (nr ri)) ]

  | CallE (e1, _, e2) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp env e2 in

     (* Get the closure pointer *)
     let i = E.add_anon_local env I32Type in
     code1 @
     [ nr (SetLocal (nr i)) ] @

     (* First arg: The closure pointer *)
     [ nr (GetLocal (nr i)) ] @
     (* Second arg: The argument *)
     code2 @
     (* And now get the table index *)
     [ nr (GetLocal (nr i)) ] @
     load_field 0l @
     (* All done: Call! *)
     [ nr (CallIndirect (nr E.unary_fun_ty_i)) ]

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
    compile_lit l @
    [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ]
  | Some uo, (NatLit _ | IntLit _) ->
    compile_lit l @
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
      let alloc_code = allocn 1l @ [ nr (SetLocal (nr i)) ] in
      let code =
        [ nr (SetLocal (E.tmp_local env));
          nr (GetLocal (nr i));
          nr (GetLocal (E.tmp_local env)) ] @
          store_field 0l in
      (env1, alloc_code, code)
  | TupP ps ->
      let rec go i ps env = match ps with
        | [] -> (env, [], [ nr Drop ])
        | (p::ps) ->
          let (env1, alloc_code1, code1) = compile_pat env fail_depth p in
          let (env2, alloc_code2, code2) = go (i+1) ps env1 in
          ( env2,
            alloc_code1 @ alloc_code2,
            dup env @ load_field (Wasm.I32.of_int_u i) @ code1 @ code2) in
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

      let alloc_code = allocn 1l @ [ nr (SetLocal (nr i)) ] in

      ( pre_env1, alloc_code, fun env ->
        let code1 = compile_exp env e in
        [ nr (GetLocal (nr i)) ] @
        code1 @
        store_field 0l @
        if last then [ nr (GetLocal (nr i)) ] @ load_field 0l else [])

  | FuncD (name, _, p, rt, e) ->
      let li = E.add_anon_local pre_env I32Type in
      let (pre_env1, vi) = E.add_local pre_env name.it in
      (* Get captured variables *)
      let captured = Freevars.captured p e in

      let alloc_code =
        (* Allocate a heap object for the function *)
        allocn (Wasm.I32.of_int_u (1 + List.length captured)) @
        [ nr (SetLocal (nr li)) ] @

        (* Allocate an extra indirection for the variable *)
        allocn 1l @
        [ nr (TeeLocal (nr vi)) ] @
        [ nr (GetLocal (nr li)) ] @
        store_field 0l
      in

      ( pre_env1, alloc_code, fun env ->

	(* All functions are unary for now (arguments passed as heap-allocated tuples)
           with the closure itself passed as a first argument *)
        let f = compile_func env captured p e in
        let fi = E.add_fun env f in

        (* Store the function number: *)
        [ nr (GetLocal (nr li));
          nr (Wasm.Ast.Const (nr (Wasm.Values.I32 fi))) ] @ (* Store function number *)
        store_field 0l @
        (* Store all captured values *)
        let store_capture i v =
          [ nr (GetLocal (nr li)) ] @
          get_var_loc env v @
          store_field (Wasm.I32.of_int_u (1+i)) in
        List.concat (List.mapi store_capture captured) @
        if last then [ nr (GetLocal (nr li)) ] else [])

  (* Classes are desguared to functions and objects. *)
  | ClassD (name, typ_params, s, p, efs) ->
    compile_dec last pre_env (nr_ (FuncD (name, typ_params, p, nr_ AnyT,
      nr__ (ObjE (s, nr_ "WHATTOPUTHERE", efs)))))

and compile_decs env decs : Wasm.Ast.instr list =
  let rec go pre_env decs = match decs with
    | []          -> (pre_env, [], fun _ -> compile_unit) (* empty declaration list? *)
    | [dec]       -> compile_dec true pre_env dec
    | (dec::decs) ->
        let (pre_env1, alloc_code1, mk_code1) = compile_dec false pre_env dec    in
        let (pre_env2, alloc_code2, mk_code2) = go          pre_env1 decs in
        (pre_env2, alloc_code1 @ alloc_code2, fun env -> mk_code1 env @ mk_code2 env) in
  let (env1, alloc_code, mk_code) = go env decs in
  alloc_code @ mk_code env1

(* Create a WebAssembly func from a pattern (for the argument) and the body.
Parameter `captured` should contain the, well, captured local variables that
the function will find in the closure. *)
and compile_func env captured p (e : exp) : func =
  (* Fresh set of locals *)
  let env1 = E.mk_fun_env env 2l in
  (* Allocate locals for the captured variables *)
  let env2 = List.fold_left (fun e n -> fst (E.add_local e n)) env1 captured in
  (* Load the environment *)
  let load_capture i v =
      [ nr (GetLocal (E.unary_closure_local env2)) ] @
      load_field (Wasm.I32.of_int_u (1+i)) @
      set_var_loc env2 v in
  let closure_code = List.concat (List.mapi load_capture captured) in
  (* Destruct the argument *)
  let (env3, alloc_args_code, destruct_args_code) = compile_mono_pat env2 p in
  (* Compile the body *)
  let body_code = compile_exp env3 e in
  nr { ftype = nr E.unary_fun_ty_i;
       locals = E.get_locals env3;
       body =
        closure_code @
        alloc_args_code @
        [ nr (GetLocal (E.unary_param_local env3)) ] @
        destruct_args_code @
        body_code
     }

and compile_start_func env ds : func =
  (* Fresh set of locals *)
  let env1 = E.mk_fun_env env 0l in
  let code = compile_decs env1 ds in
  nr { ftype = nr E.start_fun_ty_i;
       locals = E.get_locals env1;
       body = code @ [ nr Drop ]
     }

let add_array_funcs env =
  let header_size = 4l in
  let get_array_object = [ nr (GetLocal (nr 0l)) ] @ load_field 1l in
  let get_single_arg =   [ nr (GetLocal (nr 1l)) ] in
  let get_first_arg =    [ nr (GetLocal (nr 1l)) ] @ load_field 0l in
  let get_second_arg =   [ nr (GetLocal (nr 1l)) ] @ load_field 1l in
  let index_to_position =
        [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 header_size)));
          nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)); (* skip header *)
          nr (Wasm.Ast.Const (nr (Wasm.Values.I32 4l)));
          nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Mul));
          nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add))] in

  let get_fun = nr {
       ftype = nr E.unary_fun_ty_i;
       locals = [];
       body =
          get_array_object @
          get_single_arg @ (* the index *)
          index_to_position @
          load_field 0l
     } in
  let set_fun = nr {
       ftype = nr E.unary_fun_ty_i;
       locals = [];
       body =
          get_array_object @
          get_first_arg @ (* the index *)
          index_to_position @
          get_second_arg @ (* the value *)
          store_field 0l @
          compile_unit
     } in
  let len_fun = nr {
       ftype = nr E.unary_fun_ty_i;
       locals = [];
       body =
          get_array_object @
          load_field 3l
     } in
  let i = E.add_fun env get_fun in
  assert (Int32.to_int i == Int32.to_int E.array_get_funid);
  let _ = E.field_to_index env (nr_ "get") in

  let i = E.add_fun env set_fun in
  assert (Int32.to_int i == Int32.to_int E.array_set_funid);
  let _ = E.field_to_index env (nr_ "set") in

  let i = E.add_fun env len_fun in
  assert (Int32.to_int i == Int32.to_int E.array_len_funid);
  let _ = E.field_to_index env (nr_ "len") in

  ()



let compile (prog  : Syntax.prog) : unit =
  let m : module_ =
    let env = E.mk_global () in

    add_array_funcs env;

    let start_fun = compile_start_func env prog.it in
    let i = E.add_fun env start_fun in

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
      globals = [ heap_ptr_global ];
      memories = [nr {mtype = MemoryType {min = 1024l; max = None}} ];
    };
  in
  Wasm.Print.module_ stdout 100 m

