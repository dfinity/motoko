open Wasm.Ast
open Wasm.Types

open Source
open Syntax

let nr x = { Wasm.Source.it = x; Wasm.Source.at = Wasm.Source.no_region }
let nr_ x = { it = x; at = no_region; note = () }
let nr__ x = { it = x; at = no_region; note = {note_typ = Type.Any; note_eff = Type.Triv } }

let todo fn se x = Printf.eprintf "%s: %s" fn (Wasm.Sexpr.to_string 80 se); x


(* The compiler environment.

Almost immutable..
the mutable parts (`ref`) are used to register things like locals and functions.
This should be monotone in some sense: Stuff is only added, and it should not
have a semantics difference in which order they are added.

*)

module E = struct

  (* Utilities, internal to E *)
  let reg (ref : 'a list ref) (x : 'a) =
      let i = Wasm.I32.of_int_u (List.length !ref) in
      ref := !ref @ [ x ];
      i

  (* HACK, obviously *)
  let update_reg (ref : 'a list ref) i (y : 'a) =
    let rec update xs i = match xs, i with
              | ([], _)      -> []
              | (_ :: xs, 0) -> (y :: xs)
              | (x :: xs, i) -> x :: update xs (i - 1) in
    ref := update !ref (int_of_string (Wasm.I32.to_string_u i))

  (* The environment type *)
  module NameEnv = Env.Make(String)
  type t = {
    funcs : func list ref;
    func_types : func_type Wasm.Source.phrase list ref;
    n_param : int32; (* to calculate GetLocal indices *)
    locals : value_type list ref;
    (* Current depths *)
    depth : int32;
    (* Label to depth *)
    ld : int32 NameEnv.t;
    (* Local variables *)
    local_vars_env : int32 NameEnv.t;
    (* Functions (HACK: They are not really seperate,
       but for now we only support calls to known functions) *)
    funcs_env : int32 NameEnv.t;
    (* Field labels to index *)
    (* (This is for the prototypical simple tuple-implementations for objects *)
    field_env : int32 NameEnv.t ref;
  }

  (* The global environment *)
  let mk_global (_ : unit) : t = {
    funcs = ref [];
    func_types = ref [];
    funcs_env = NameEnv.empty;
    (* Actually unused outside mk_fun_env: *)
    locals = ref [];
    local_vars_env = NameEnv.empty;
    n_param = 0l;
    depth = 0l;
    ld = NameEnv.empty;
    field_env = ref NameEnv.empty;
  }

  (* A new function *)
  let mk_fun_env env params =
    { env with
      locals = ref [];
      n_param = Wasm.I32.of_int_u (List.length params);
      local_vars_env = List.fold_left (fun local_vars_env (x,y) -> NameEnv.add x y local_vars_env) NameEnv.empty
                 (List.mapi (fun i x -> (x.it, Wasm.I32.of_int_u i)) params);
      depth = 0l;
      ld = NameEnv.empty;
      }

  let lookup_var env var =
    match NameEnv.find_opt var.it env.local_vars_env with
      | Some i -> Some (nr i)
      | None   -> Printf.eprintf "Could not find %s\n" var.it; None

  let add_anon_local (env : t) ty = reg env.locals ty

  let add_local (env : t) name =
      let i = reg env.locals I32Type; in
      let i' = Wasm.I32.add env.n_param i in
      ({ env with local_vars_env = NameEnv.add name.it i' env.local_vars_env }, i')

  let get_locals (env : t) = !(env.locals)

  let lookup_fun env var =
    match NameEnv.find_opt var.it env.funcs_env with
      | Some i -> Some (nr i)
      | None   -> Printf.eprintf "Could not find %s\n" var.it; None

  let add_anon_fun (env : t) f = reg env.funcs f

  let add_fun (env : t) name f =
      let i = reg env.funcs f; in
      ({ env with funcs_env = NameEnv.add name i env.funcs_env }, i)

  let update_fun (env : t) i f = update_reg env.funcs i f

  let get_funcs (env : t) = !(env.funcs)

  let add_type (env : t) ty = reg env.func_types ty

  let get_types (env : t) = !(env.func_types)

  let inc_depth (env : t) =
      let label_depths' = Wasm.I32.add env.depth 1l in
      {env with depth = label_depths'}

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

let compile_true =  [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 1l))) ]
let compile_false = [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 0l))) ]
let compile_zero =  [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 0l))) ]
let compile_unit =  [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 0l))) ]

let heap_ptr_global = nr
      { gtype = GlobalType (I32Type, Mutable);
        value = nr compile_zero }

let heap_ptr : var = nr 0l

let tmp_local : var = nr 0l

let dup : instr list = (* duplicate top element *)
  [ nr (TeeLocal tmp_local);
    nr (GetLocal tmp_local) ]

let alloc : instr list = (* expect the size on the stack, returns the pointer *)
  [ nr (SetLocal tmp_local);
    nr (GetGlobal heap_ptr);
    nr (GetGlobal heap_ptr);
    nr (GetLocal tmp_local);
    nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
    nr (SetGlobal heap_ptr)]

let compile_lit lit = match lit with
  | BoolLit true ->  compile_true
  | BoolLit false -> compile_false
  (* This maps int to int32, instead of a proper arbitrary precision library *)
  | IntLit n      -> [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Z.to_int32 n)))) ]
  | NatLit n      -> [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Z.to_int32 n)))) ]
  | _ -> todo "compile_lit" (Arrange.lit lit) [ nr Unreachable ]

let compile_unop op = match op with
  | NegOp ->
      [ nr (SetLocal tmp_local) ] @
      compile_zero @
      [ nr (GetLocal tmp_local);
        nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ]
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

let rec compile_lexp (env : E.t) exp code = match exp.it with
  | VarE var -> (match E.lookup_var env var with
      | Some i -> code @ [ nr (SetLocal i) ]
      | None   -> [nr Unreachable])
  | IdxE (e1,e2) ->
     compile_exp env e1 @ (* offset to array *)
     compile_exp env e2 @ (* idx *)
     [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 4l))) ] @
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Mul)) ] @
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
     code @
     [ nr (Store {ty = I32Type; align = 0; offset = 0l; sz = None}) ]
  | _ -> todo "compile_lexp" (Arrange.exp exp) [ nr Unreachable ]

and compile_exp (env : E.t) exp = match exp.it with
  | VarE var -> (match E.lookup_var env var with
      | Some i -> [ nr (GetLocal i) ]
      | None   -> [ nr Unreachable ])
  | AssignE (e1,e2) ->
     let code1 = compile_exp env e2 in
     compile_lexp env e1 code1 @
     compile_unit
  | IdxE (e1,e2) ->
     compile_exp env e1 @ (* offset to array *)
     compile_exp env e2 @ (* idx *)
     [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 4l))) ] @
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Mul)) ] @
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
       nr (Load {ty = I32Type; align = 0; offset = 0l; sz = None})
     ]
  | LitE l_ref ->
     compile_lit !l_ref
  | AssertE e1 ->
     compile_exp env e1 @ [ nr (If ([I32Type], compile_unit, [nr Unreachable])) ]
  | NotE e ->
     compile_exp env e @
     [ nr (If ([I32Type], compile_false, compile_true)) ]
  | UnE (op, e1) ->
     compile_exp env e1 @
     compile_unop op
  | BinE (e1, op, e2) ->
     compile_exp env e1 @
     compile_exp env e2 @
     compile_binop op
  | RelE (e1, op, e2) ->
     compile_exp env e1 @
     compile_exp env e2 @
     compile_relop op
  | OrE (e1, e2) ->
     compile_exp env e1 @
     compile_exp env e2 @
     [ nr (Binary (Wasm.Values.I32 IntOp.Or)) ]
  | IfE (e1, e2, e3) ->
     let code1 = compile_exp env e1 in
     let code2 = compile_exp (E.inc_depth env) e2 in
     let code3 = compile_exp (E.inc_depth env) e3 in
     code1 @ [ nr (If ([I32Type], code2, code3)) ]
  | BlockE decs ->
     compile_decs env decs
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
  | TupE [] -> compile_unit
  | (ArrayE es | TupE es) ->
     (* Calculate size *)
     [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Wasm.I32.of_int_u (4 * List.length es))))) ] @
     (* Allocate memory, and put position on the stack (return value) *)
     alloc @
     let init_elem i e : Wasm.Ast.instr list =
        dup @ (* Duplicate position *)
	[ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Wasm.I32.of_int_u (4*i)))));
          nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
	compile_exp env e @
        [ nr (Store {ty = I32Type; align = 0; offset = 0l; sz = None}) ]
     in
     List.concat (List.mapi init_elem es)
  | DotE (e, f) ->
     let i = E.field_to_index env f in
     compile_exp env e @
     [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Int32.mul 4l i))));
       nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
       nr (Load {ty = I32Type; align = 0; offset = 0l; sz = None})
     ]
  | ObjE ({it = Type.Object;_}, _, fs) ->
     (* Resolve fields to index *)
     let fis = List.map (fun (f : exp_field) -> (E.field_to_index env (f.it.id), f.it.exp)) fs in
     (* Find largest index *)
     let max a b = if Int32.compare a b >= 0 then a else b in
     let n = Int32.add 1l (List.fold_left max 0l (List.map fst fis)) in
     (* Calculate size *)
     [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Int32.mul 4l n)))) ] @
     (* Allocate memory, and put position on the stack (return value) *)
     alloc @
     let init_field (i, e) : Wasm.Ast.instr list =
        dup @ (* Duplicate position *)
	[ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Int32.mul 4l i))));
          nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
	compile_exp env e @
        [ nr (Store {ty = I32Type; align = 0; offset = 0l; sz = None}) ]
     in
     List.concat (List.map init_field fis)
  | CallE ({it = VarE var; _}, [], e) ->
     let args = de_tupleE e in
     List.concat (List.map (compile_exp env) args)
     @ (match E.lookup_fun env var with
        | Some i -> [ nr (Call i) ]
        | None   -> [nr Unreachable])
  | _ -> todo "compile_exp" (Arrange.exp exp) [ nr Unreachable ]


and de_tupleP p = match p.it with
  | TupP ps -> ps
  | _       -> [p]

and de_tupleE e = match e.it with
  | TupE es -> es
  | _       -> [e]

and param_names ps =
  let rec param_name p = match p.it with
    | VarP i -> i
    | AnnotP (p, _) -> param_name p
    | _             -> nr_ "non-var-pattern" in
  List.map param_name ps


(*
The compilation of declarations (and patterns!) needs to handle mutual recursion.
This requires conceptually two steps:
 1. First we need to collect all names bound in a block,
    1a  find locations for then (which extends the environment)
        The environment is extended monotonously: The type-checker ensures that
        a Block does not bind the same name twice.
        We would not need to pass in the environment, just out ... but because
        it is bundled in the E.t type, threading it through is also easy.
    1b  we need to initialize them with a “uninitialized” flag.
        So one might want to pass out a list of instructions.
        But here we can be lazy, and rely on the fact that Wasm locals are
        initialized with zeros.
 2. Then we go through the declarations again and generate the actual code.

We could do this in two separate functions, but I chose to do it in one
 * it means all code related to one constructor is in one place and
 * when generating the actual code, we still “know” the memory location,
   and don’t have to look it up in the environment.
*)


and compile_pat env pat : E.t * Wasm.Ast.instr list  = match pat.it with
  (* So far, only irrefutable patterns *)
  (* The undestructed value is on top of the stack. *)
  (* The returned code consumes it, and fills all the variables. *)
  | WildP -> (env, [ nr Drop ])
  | VarP name ->
      let (env1,i) = E.add_local env name; in
      (env1, [ nr (SetLocal (nr i) ) ])
  | TupP ps ->
      let get_i i =
	[ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Wasm.I32.of_int_u (4*i)))));
          nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
          nr (Load {ty = I32Type; align = 0; offset = 0l; sz = None})] in
      let rec go i ps env = match ps with
        | [] -> (env, [ nr Drop ])
        | (p::ps) ->
          let (env1, code1) = compile_pat env p in
          let (env2, code2) = go (i+1) ps env1 in
          (env2, dup @ get_i i @ code1 @ code2) in
      go 0 ps env

  | _ -> todo "compile_pat" (Arrange.pat pat) (env, [ nr Drop ])

and compile_dec last pre_env dec : E.t * (E.t -> Wasm.Ast.instr list) = match dec.it with
  | TypD _ -> (pre_env, fun _ -> [])
  | ExpD e ->
    (pre_env, fun env ->
      let code = compile_exp env e in
      let drop = if last then [] else [nr Drop] in
      code @ drop
    )
  | LetD (p, e) ->
    let (pre_env1, code2) = compile_pat pre_env p in
    ( pre_env1, fun env ->
      let code1 = compile_exp env e in
      let stack_fix = if last then dup else [] in
      code1 @ stack_fix @ code2)
  | VarD (name, e) ->
      let (pre_env1, i) = E.add_local pre_env name in
      ( pre_env1, fun env ->
        let code1 = compile_exp env e in
        let cmd x = if last then TeeLocal x else SetLocal x in
        code1 @ [ nr (cmd (nr i) ) ])
  | FuncD ({it = name; _}, [], p, rt, e) ->
      let dummy_fun = nr {ftype = nr 0l; locals = []; body = []} in
      let (pre_env1, i) = E.add_fun pre_env name dummy_fun in
      ( pre_env1, fun env ->
        let ps = de_tupleP p in
        let rt' = match rt.it with | TupT [ty]-> [I32Type]
                                   | _        -> [I32Type] in (* TODO: Multi-value return *)
        let ty = nr (FuncType (List.map (fun _ -> I32Type) ps, rt')) in
        let params = param_names ps in
        let f = compile_func_body env params ty false e in
        E.update_fun env i f;
        let stack_fix = if last then compile_unit else [] in
        stack_fix)
  | _ -> todo "compile_dec" (Arrange.dec dec) (pre_env, fun _ -> [])

and compile_decs env decs : Wasm.Ast.instr list =
  let rec go pre_env decs = match decs with
    | []          -> (pre_env, fun _ -> compile_unit) (* empty declaration list? *)
    | [dec]       -> compile_dec true pre_env dec
    | (dec::decs) ->
        let (pre_env1, mk_code1) = compile_dec false pre_env dec    in
        let (pre_env2, mk_code2) = go          pre_env1 decs in
        (pre_env2, fun env -> mk_code1 env @ mk_code2 env) in
  let (env1, mk_code) = go env decs in
  mk_code env1

and compile_func_body env params func_type void (e : exp) : func =
  (* Fresh set of locals *)
  let env1 = E.mk_fun_env env params in
  (* Every function has a temporary local as local 0 *)
  let _ = E.add_anon_local env1 I32Type in
  let body = compile_exp env1 e in
  (* A slight hack until we track types more pervasively *)
  let drop = if void then [ nr Drop ] else [] in
  let ti = E.add_type env func_type in
  nr { ftype = nr ti;
       locals = E.get_locals env1;
       body = body @ drop
     }

let compile (prog  : Syntax.prog) : unit =
  let m : module_ =
    let start_fun_ty = nr (FuncType ([], [])) in
    let env = E.mk_global () in
    let start_fun = compile_func_body env [] start_fun_ty true (nr__ (BlockE prog.it)) in
    let i = E.add_anon_fun env start_fun in

    nr { empty_module with
      types = E.get_types env;
      funcs = E.get_funcs env;
      start = Some (nr i);
      globals = [ heap_ptr_global ];
      memories = [nr {mtype = MemoryType {min = 100l; max = None}} ];
    };
  in
  Wasm.Print.module_ stdout 100 m

