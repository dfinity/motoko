open Wasm.Ast
open Wasm.Types

open Source
open Syntax

module LVE = Env.Make(String)

let nr x = { Wasm.Source.it = x; Wasm.Source.at = Wasm.Source.no_region }

let todo fn se x = Printf.eprintf "%s: %s" fn (Wasm.Sexpr.to_string 80 se); x

let reg (ref : 'a list ref) (x : 'a) =
    let i = Wasm.I32.of_int_u (List.length !ref) in
    ref := !ref @ [ x ];
    i

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
  | _ -> todo "compile_binop" (Arrange.binop op) [ nr Unreachable ]

let compile_relop op = match op with
  | EqOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ]
  | _ -> todo "compile_relop" (Arrange.relop op) [ nr Unreachable ]

let rec compile_exp locals lve exp = match exp.it with
  | VarE var -> [ nr (GetLocal (nr (LVE.find var.it lve))) ]
  | LitE l_ref ->
     compile_lit !l_ref
  | AssertE e1 ->
     compile_exp locals lve e1 @ [ nr (If ([], [], [nr Unreachable])) ]
  | BinE (e1, op, e2) ->
     compile_exp locals lve e1 @
     compile_exp locals lve e2 @
     compile_binop op
  | RelE (e1, op, e2) ->
     compile_exp locals lve e1 @
     compile_exp locals lve e2 @
     compile_relop op
  | _ -> todo "compile_exp" (Arrange.exp exp) [ nr Unreachable ]

and compile_decs locals lve decs = match decs with
  | []          -> ([], lve)
  | (dec::decs) ->
      let (code1, lve1) = compile_dec locals lve dec    in
      let (code2, lve2) = compile_decs locals lve1 decs in
      (code1 @ code2, lve2)

and compile_dec locals lve dec = match dec.it with
  | ExpD e -> (compile_exp locals lve e, lve)
  | LetD ({it = VarP {it = name; _}; _}, e) ->
      let code1 = compile_exp locals lve e in
      let i = reg locals I32Type; in
      let lve1 = LVE.add name i lve in
      (code1 @ [ nr (SetLocal (nr i) ) ], lve1)
  | _ -> todo "compile_dec" (Arrange.dec dec) ([], lve)

let compile_func_body (funcs : func list ref) (func_types : type_ list ref) (decs : dec list) :  func =
  let locals = ref [] in
  let body = fst (compile_decs locals LVE.empty decs) in
  let ti = reg func_types (nr (FuncType ([], []))) in
  nr { ftype = nr ti;
       locals = !locals;
       body = body
     }


let compile (prog  : Syntax.prog) : unit =
  let m : module_ =
    let funcs = ref [] in
    let func_types = ref [] in
    let start_fun = compile_func_body funcs func_types prog.it in
    let i = Wasm.I32.of_int_u (List.length !funcs) in
    funcs := !funcs @ [ start_fun ];

    nr { empty_module with
      types = !func_types;
      funcs = !funcs;
      start = (Some (nr i));
    };
  in
  Wasm.Print.module_ stdout 100 m

