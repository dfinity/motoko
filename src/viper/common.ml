(* exception for reporting unsupported Motoko syntax *)
exception Unsupported of Source.region * string

let unsupported at sexp =
  raise (Unsupported (at, (Wasm.Sexpr.to_string 80 sexp)))

let rec map_last ~f = function
  | [] -> []
  | [ x ] -> [ f x ]
  | x :: xs -> x :: map_last ~f xs

let tup_con_name n =
  Printf.sprintf "Tup$%d" n

let tup_prj_name n i =
  Printf.sprintf "tup$%d$%d" n i

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

module StrMap = Map.Make(String)

(* Requirements arising from the translated code. In trans.ml we collect those
   requirements, in prelude.ml we generate definitons to satisfy them. *)
type reqs =
  { tuple_arities : IntSet.t ref
  ; typed_fields : Syntax.typ StrMap.t ref
  }

let init_reqs () =
  { tuple_arities = ref IntSet.empty
  ; typed_fields = ref StrMap.empty
  }

