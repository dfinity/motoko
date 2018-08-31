open Wasm.Source
open Wasm.Ast
open Wasm.Types

let nr x = x @@ no_region

let compile prog =
  let m : module_ = nr { empty_module with
    types = [
      nr (FuncType ([], []))
    ];
    funcs = [
      nr { ftype = nr 0l;
           locals = [];
           body = [nr Unreachable];
         }
    ];
    start = (Some (nr 0l));
  } in
  Wasm.Print.module_ stdout 100 m

