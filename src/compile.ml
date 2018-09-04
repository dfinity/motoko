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
    lve : int32 NameEnv.t;
    (* Functions (HACK: They are not really seperate,
       but for now we only support calls to known functions) *)
    fe : int32 NameEnv.t;
  }

  (* The global environment *)
  let mk_global (_ : unit) : t = {
    funcs = ref [];
    func_types = ref [];
    fe = NameEnv.empty;
    (* Actually unused outside mk_fun_env: *)
    locals = ref []; 
    lve = NameEnv.empty;
    n_param = 0l;
    depth = 0l;
    ld = NameEnv.empty;
  }

  (* A new function *)
  let mk_fun_env env params =
    { env with
      locals = ref [];
      n_param = Wasm.I32.of_int_u (List.length params);
      lve = List.fold_left (fun lve (x,y) -> NameEnv.add x y lve) NameEnv.empty
                 (List.mapi (fun i x -> (x.it, Wasm.I32.of_int_u i)) params);
      depth = 0l;
      ld = NameEnv.empty;
      }

  let lookup_var env var =
    match NameEnv.find_opt var.it env.lve with
      | Some i -> Some (nr i)
      | None   -> Printf.eprintf "Could not find %s\n" var.it; None

  let add_anon_local (env : t) ty = reg env.locals ty

  let add_local (env : t) name =
      let i = reg env.locals I32Type; in
      let i' = Wasm.I32.add env.n_param i in
      ({ env with lve = NameEnv.add name.it i' env.lve }, i')

  let get_locals (env : t) = !(env.locals)

  let lookup_fun env var =
    match NameEnv.find_opt var.it env.fe with
      | Some i -> Some (nr i)
      | None   -> Printf.eprintf "Could not find %s\n" var.it; None

  let add_anon_fun (env : t) f = reg env.funcs f

  let add_fun (env : t) name f =
      let i = reg env.funcs f; in
      ({ env with fe = NameEnv.add name i env.fe }, i)

  let update_fun (env : t) i f = update_reg env.funcs i f

  let get_funcs (env : t) = !(env.funcs)

  let add_type (env : t) ty = reg env.func_types ty

  let get_types (env : t) = !(env.func_types)

  let inc_depth (env : t) =
      let d' = Wasm.I32.add env.depth 1l in
      {env with depth = d'}

  let add_label (env : t) name =
      { env with ld = NameEnv.add name.it (env.depth) env.ld }

  let get_label_depth (env : t) name =
    match NameEnv.find_opt name.it env.ld with
      | Some i -> Wasm.I32.sub env.depth i
      | None   -> Printf.eprintf "Could not find %s\n" name.it; raise Not_found

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
  [ nr (GetGlobal heap_ptr);
    nr (SetLocal tmp_local);
    nr (GetGlobal heap_ptr);
    nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
    nr (SetGlobal heap_ptr);
    nr (GetLocal tmp_local)]

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
     fst (compile_decs env decs)
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
     (* Allocate position *)
     alloc @
     (* Put position on the stack, to be returned. *)
     let init_elem i e : Wasm.Ast.instr list =
        dup @ (* Dupliate position *)
	[ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Wasm.I32.of_int_u (4*i)))));
          nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
	compile_exp env e @
        [ nr (Store {ty = I32Type; align = 0; offset = 0l; sz = None}) ]
     in
     List.concat (List.mapi init_elem es)
  | CallE ({it = VarE var; _}, [], e) ->
     let args = de_tupleE e in
     List.concat (List.map (compile_exp env) args)
     @ (match E.lookup_fun env var with
        | Some i -> [ nr (Call i) ]
        | None   -> [nr Unreachable])
  | _ -> todo "compile_exp" (Arrange.exp exp) [ nr Unreachable ]

(* TODO: Mutual recursion! *)
and compile_decs env decs = match decs with
  | []          -> (compile_unit, env) (* empty declaration list? *)
  | [dec]       ->
      let (code1, env1) = compile_dec  true env dec    in
      (code1, env1)
  | (dec::decs) ->
      let (code1, env1) = compile_dec  false env dec    in
      let (code2, env2) = compile_decs env1 decs in
      (code1 @ code2, env2)

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

and compile_pat env pat = match pat.it with
  (* So far, only irrefutable patterns *)
  (* The undestructed value is on top of the stack. *)
  (* The returned code consumes it, and fills all the variables. *)
  | VarP name ->
      let (env1,i) = E.add_local env name; in
      ([ nr (SetLocal (nr i) ) ], env1)
  | TupP ps ->
      let get_i i =
	[ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Wasm.I32.of_int_u (4*i)))));
          nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
          nr (Load {ty = I32Type; align = 0; offset = 0l; sz = None})] in
      let rec go i ps env = match ps with
        | [] -> ([ nr Drop ], env)
        | (p::ps) ->
          let (code1, env1) = compile_pat env p in
          let (code2, env2) = go (i+1) ps env1 in
          (dup @ get_i i @ code1 @ code2, env2) in
      go 0 ps env

  | _ -> todo "compile_pat" (Arrange.pat pat) ([], env)

and compile_dec last env dec = match dec.it with
  | ExpD e ->
    let code = compile_exp env e in
    let drop = if last then [] else [nr Drop] in
    (code @ drop, env)
  | LetD (p, e) ->
      let code1 = compile_exp env e in
      let stack_fix = if last then dup else [] in
      let (code2,env1) = compile_pat env p in
      (code1 @ stack_fix @ code2, env1)
  | VarD (name, e) ->
      let code1 = compile_exp env e in
      let (env1, i) = E.add_local env name in
      let cmd x = if last then TeeLocal x else SetLocal x in
      (code1 @ [ nr (cmd (nr i) ) ], env1)
  | FuncD ({it = name; _}, [], p, rt, e) ->
      let ps = de_tupleP p in
      let rt' = match rt.it with | TupT [ty]-> [I32Type]
                                 | _        -> [I32Type] in (* TODO: Multi-value return *)
      let ty = nr (FuncType (List.map (fun _ -> I32Type) ps, rt')) in
      let params = param_names ps in
      let dummy_fun = nr {ftype = nr 0l; locals = []; body = []} in
      let (env1, i) = E.add_fun env name dummy_fun in
      let f = compile_func_body env1 params ty false e in
      E.update_fun env i f;
      let stack_fix = if last then compile_unit else [] in
      (stack_fix, env1)

  | _ -> todo "compile_dec" (Arrange.dec dec) ([], env)

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

