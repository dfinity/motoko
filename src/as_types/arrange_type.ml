open Type
open Wasm.Sexpr

let ($$) head inner = Node (head, inner)

let control c = match c with
  | Returns -> "Returns"
  | Promises -> "Promises"

let obj_sort s = match s with
  | Object -> Atom "Object"
  | Actor -> Atom "Actor"
  | Module -> Atom "Module"

let func_sort s = match s with
  | Local -> "Local"
  | Shared -> "Shared"

let prim p = match p with
  | Null -> Atom "Null"
  | Bool -> Atom "Bool"
  | Nat -> Atom "Nat"
  | Nat8 -> Atom "Nat8"
  | Nat16 -> Atom "Nat16"
  | Nat32 -> Atom "Nat32"
  | Nat64 -> Atom "Nat64"
  | Int -> Atom "Int"
  | Int8 -> Atom "Int8"
  | Int16 -> Atom "Int16"
  | Int32 -> Atom "Int32"
  | Int64 -> Atom "Int64"
  | Word8 -> Atom "Word8"
  | Word16 -> Atom "Word16"
  | Word32 -> Atom "Word32"
  | Word64 -> Atom "Word64"
  | Float -> Atom "Float"
  | Char -> Atom "Char"
  | Text -> Atom "Text"

let con c = Atom (Con.to_string c)

let rec typ (t:Type.typ) = match t with
  | Var (s, i)             -> "Var" $$ [Atom s; Atom (string_of_int i)]
  | Con (c, ts)            -> "Con" $$ (con c::List.map typ ts)
  | Prim p                 -> "Prim" $$ [prim p]
  | Obj (s, tfs)           -> "Obj" $$ [obj_sort s] @ List.map typ_field tfs
  | Array t                -> "Array" $$ [typ t]
  | Opt t                  -> "Opt" $$ [typ t]
  | Variant tfs            -> "Variant" $$ List.map typ_field tfs
  | Tup ts                 -> "Tup" $$ List.map typ ts
  | Func (s, c, tbs, at, rt) -> "Func" $$ [Atom (func_sort s); Atom (control c)] @ List.map typ_bind tbs @ [ "" $$ (List.map typ at); "" $$ (List.map typ rt)]
  | Async t               -> "Async" $$ [typ t]
  | Mut t                 -> "Mut" $$ [typ t]
  | Any                   -> Atom "Any"
  | Non                   -> Atom "Non"
  | Pre                   -> Atom "Pre"
  | Typ c                 -> "Typ" $$ [con c]

and typ_bind (tb : Type.bind) =
  tb.var $$ [typ tb.bound]

and typ_field (tf : Type.field) =
  tf.lab $$ [typ tf.typ]
