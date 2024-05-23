(* exception for reporting unsupported Motoko syntax *)
exception Unsupported of Source.region * string

let unsupported at sexp =
  raise (Unsupported (at, (Wasm.Sexpr.to_string 80 sexp)))

let rec map_last ~f = function
  | [] -> []
  | [ x ] -> [ f x ]
  | x :: xs -> x :: map_last ~f xs
