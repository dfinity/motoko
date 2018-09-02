open Wasm.Ast
open Wasm.Types

open Source
open Syntax

module LVE = Env.Make(String)

let nr x = { Wasm.Source.it = x; Wasm.Source.at = Wasm.Source.no_region }
let nr_ x = { it = x; at = no_region; note = () }
let nr__ x = { it = x; at = no_region; note = {note_typ = Type.Any; note_eff = Type.Triv } }

let todo fn se x = Printf.eprintf "%s: %s" fn (Wasm.Sexpr.to_string 80 se); x

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

let rec drop i (xs : 'a list) = match i, xs with
  | 0, xs      -> xs
  | _, []      -> []
  | i, (_::xs) -> drop (i-1) xs


let compile_lit lit = match lit with
  | BoolLit true ->  [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 1l))) ]
  | BoolLit false -> [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 1l))) ]
  (* This maps int to int32, instead of a proper arbitrary precision library *)
  | IntLit n      -> [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Z.to_int32 n)))) ]
  | NatLit n      -> [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Z.to_int32 n)))) ]
  | _ -> todo "compile_lit" (Arrange.lit lit) [ nr Unreachable ]

let compile_binop op = match op with
  | AddOp -> [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ]
  | SubOp -> [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ]
  | MulOp -> [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Mul)) ]
  | _ -> todo "compile_binop" (Arrange.binop op) [ nr Unreachable ]

let compile_relop op = match op with
  | EqOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ]
  | GeOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.GeU)) ]
  | GtOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.GtU)) ]
  | LtOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.LtU)) ]
  | _ -> todo "compile_relop" (Arrange.relop op) [ nr Unreachable ]

let rec compile_lexp funcs func_types locals lve exp code = match exp.it with
  | VarE var -> (match LVE.find_opt var.it lve with
      | Some i -> code @ [ nr (SetLocal (nr i)) ]
      | None   -> Printf.eprintf "Could not find %s\n" var.it; [nr Unreachable])
  | IdxE (e1,e2) ->
     compile_exp funcs func_types locals lve e1 @ (* offset to array *)
     compile_exp funcs func_types locals lve e2 @ (* idx *)
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ] @
     code @
     [ nr (Store {ty = I32Type; align = 0; offset = 0l; sz = None}) ]
  | _ -> todo "compile_lexp" (Arrange.exp exp) [ nr Unreachable ]

and compile_exp funcs func_types locals lve exp = match exp.it with
  | VarE var -> (match LVE.find_opt var.it lve with
      | Some i -> [ nr (GetLocal (nr i)) ]
      | None   -> Printf.eprintf "Could not find %s\n" var.it; [nr Unreachable])
  | AssignE (e1,e2) ->
     let code1 = compile_exp funcs func_types locals lve e2 in
     compile_lexp funcs func_types locals lve e1 code1
  | IdxE (e1,e2) ->
     compile_exp funcs func_types locals lve e1 @ (* offset to array *)
     compile_exp funcs func_types locals lve e2 @ (* idx *)
     [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add));
       nr (Load {ty = I32Type; align = 0; offset = 0l; sz = None})
     ]
  | LitE l_ref ->
     compile_lit !l_ref
  | AssertE e1 ->
     compile_exp funcs func_types locals lve e1 @ [ nr (If ([], [], [nr Unreachable])) ]
  | BinE (e1, op, e2) ->
     compile_exp funcs func_types locals lve e1 @
     compile_exp funcs func_types locals lve e2 @
     compile_binop op
  | RelE (e1, op, e2) ->
     compile_exp funcs func_types locals lve e1 @
     compile_exp funcs func_types locals lve e2 @
     compile_relop op
  | OrE (e1, e2) ->
     compile_exp funcs func_types locals lve e1 @
     compile_exp funcs func_types locals lve e2 @
     [ nr (Binary (Wasm.Values.I32 IntOp.Or)) ]
  | IfE (e1, e2, e3) ->
     let code1 = compile_exp funcs func_types locals lve e1 in
     let code2 = compile_exp funcs func_types locals lve e2 in
     let code3 = compile_exp funcs func_types locals lve e3 in
     code1 @ [ nr (If ([], code2, code3)) ]
  | BlockE decs ->
     fst (compile_decs funcs func_types locals lve decs)
  | LoopE (e, None) ->
     let code = compile_exp funcs func_types locals lve e in
     [ nr (Loop ([], code @ [ nr (Br (nr 0l)) ])) ]
  | WhileE (e1, e2) ->
     let code1 = compile_exp funcs func_types locals lve e1 in
     let code2 = compile_exp funcs func_types locals lve e2 in
     [ nr (Loop ([], code1 @ [ nr (If ([], code2 @ [ nr (Br (nr 0l)) ], [])) ])) ]
  | AnnotE (e, t) -> compile_exp funcs func_types locals lve e
  | RetE e -> compile_exp funcs func_types locals lve e @ [ nr Return ]
  | TupE [] -> [] (* Fishy *)
  | ArrayE es ->
     (* For now, only one array at position 0 *)
     let init_elem i e : Wasm.Ast.instr list =
	[ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Wasm.I32.of_int_u i)))) ] @
	compile_exp funcs func_types locals lve e @
        [ nr (Store {ty = I32Type; align = 0; offset = 0l; sz = None}) ]
     in
     List.concat (List.mapi init_elem es) @
     [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Wasm.I32.of_int_u 0)))) ]
  | CallE ({it = VarE var; _}, [], e) ->
     let args = de_tupleE e in
     List.concat (List.map (compile_exp funcs func_types locals lve) args)
     @ (match LVE.find_opt var.it lve with
        | Some i -> [ nr (Call (nr i)) ]
        | None   -> Printf.eprintf "Could not find %s\n" var.it; [nr Unreachable])
  | _ -> todo "compile_exp" (Arrange.exp exp) [ nr Unreachable ]

(* TODO: Mutual recursion! *)
and compile_decs funcs func_types locals lve decs = match decs with
  | []          -> ([], lve)
  | (dec::decs) ->
      let (code1, lve1) = compile_dec  funcs func_types locals lve dec    in
      let (code2, lve2) = compile_decs funcs func_types locals lve1 decs in
      (code1 @ code2, lve2)

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

and compile_dec funcs func_types locals lve dec = match dec.it with
  | ExpD e -> (compile_exp funcs func_types locals lve e, lve)
  | LetD ({it = VarP {it = name; _}; _}, e) ->
      let code1 = compile_exp funcs func_types locals lve e in
      let i = reg locals I32Type; in
      let lve1 = LVE.add name i lve in
      (code1 @ [ nr (SetLocal (nr i) ) ], lve1)
  | VarD ({it = name; _}, e) ->
      let code1 = compile_exp funcs func_types locals lve e in
      let i = reg locals I32Type; in
      let lve1 = LVE.add name i lve in
      (code1 @ [ nr (SetLocal (nr i) ) ], lve1)
  | FuncD ({it = name; _}, [], p, rt, e) ->
      let ps = de_tupleP p in
      let rt' = match rt.it with | TupT tys -> List.map (fun _ -> I32Type) tys
                                 | _        -> [I32Type] in
      let ty = nr (FuncType (List.map (fun _ -> I32Type) ps, rt')) in
      let params = param_names ps in
      let dummy_fun = nr {ftype = nr 0l; locals = []; body = []} in
      let i = reg funcs dummy_fun in
      let lve1 = LVE.add name i lve in
      let f = compile_func_body funcs func_types ty params lve1 e in
      update_reg funcs i f;
      (* TODO: currently, lve points to function ids for functions, and locals
         for others. This breaks once we no longer only call known non-closures. *)
      ([], lve1)

  | _ -> todo "compile_dec" (Arrange.dec dec) ([], lve)

and compile_func_body
      (funcs : func list ref) (func_types : type_ list ref)
      func_type (params : id list) lve
      (e : exp) :  func =
  (* HACK: We add the params to the locals (so that the index lines up)
     but we remove them before passing them on to wasm *)
  let locals = ref (List.map (fun _ -> I32Type) params) in
  (* We reuse the lve from outside. This is ok for now for functions, but
     for other variables we need to support closures. *)
  let lve1 = List.fold_left (fun lve (x,y) -> LVE.add x y lve) lve
                           (List.mapi (fun i x -> (x.it,Wasm.I32.of_int_u i)) params) in
  let body = compile_exp funcs func_types locals lve1 e in
  let ti = reg func_types func_type in
  nr { ftype = nr ti;
       locals = drop (List.length params) !locals;
       body = body
     }

let compile (prog  : Syntax.prog) : unit =
  let m : module_ =
    let funcs = ref [] in
    let func_types = ref [] in
    let start_fun_ty = nr (FuncType ([], [])) in
    let start_fun = compile_func_body funcs func_types start_fun_ty [] LVE.empty (nr__ (BlockE prog.it)) in
    let i = reg funcs start_fun in

    nr { empty_module with
      types = !func_types;
      funcs = !funcs;
      start = (Some (nr i));
      memories = [nr {mtype = MemoryType {min = 100l; max = None}} ];
    };
  in
  Wasm.Print.module_ stdout 100 m

