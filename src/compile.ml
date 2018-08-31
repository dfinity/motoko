open Wasm.Ast
open Wasm.Types

open Source
open Syntax


let nr x = { Wasm.Source.it = x; Wasm.Source.at = Wasm.Source.no_region }

let compile_lit lit = match lit with
  | BoolLit true ->  [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 1l))) ]
  | BoolLit false -> [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 1l))) ]
  (* This maps int to int32, instead of a proper arbitrary precision library *)
  | IntLit n      -> [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Z.to_int32 n)))) ]
  | NatLit n      -> [ nr (Wasm.Ast.Const (nr (Wasm.Values.I32 (Z.to_int32 n)))) ]
  | _ -> [ nr Unreachable ]

let compile_binop op = match op with
  | AddOp -> [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Add)) ]
  | SubOp -> [ nr (Binary (Wasm.Values.I32 Wasm.Ast.I32Op.Sub)) ]
  | _ -> [ nr Unreachable ]

let compile_relop op = match op with
  | EqOp -> [ nr (Compare (Wasm.Values.I32 Wasm.Ast.I32Op.Eq)) ]
  | _ -> [ nr Unreachable ]

let rec compile_exp exp = match exp.it with
  | LitE l_ref ->
     compile_lit !l_ref
  | AssertE e1 ->
     compile_exp e1 @ [ nr (If ([], [], [nr Unreachable])) ]
  | BinE (e1, op, e2) ->
     compile_exp e1 @
     compile_exp e2 @
     compile_binop op
  | RelE (e1, op, e2) ->
     compile_exp e1 @
     compile_exp e2 @
     compile_relop op
  | _ -> [ nr Unreachable ]

and compile_dec dec = match dec.it with
  | ExpD e -> compile_exp e
  | _      -> []


let compile (prog  : Syntax.prog) : unit =
  let m : module_ = nr { empty_module with
    types = [
      nr (FuncType ([], []))
    ];
    funcs = [
      nr { ftype = nr 0l;
           locals = [];
           body = List.concat (List.map compile_dec prog.it);
         }
    ];
    start = (Some (nr 0l));
  } in
  Wasm.Print.module_ stdout 100 m

