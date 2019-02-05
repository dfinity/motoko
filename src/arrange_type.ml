open Source
open Type
open Wasm.Sexpr

let ($$) head inner = Node (head, inner)

let id i = Atom i.it

let rec sharing sh = match sh with
  | Type.Local -> "Local"
  | Type.Sharable -> "Sharable"

and control c = match c with
  | Type.Returns -> "Returns"
  | Type.Promises -> "Promises"

and obj_sort s = match s with
  | Type.Object sh -> Atom ("Object " ^ sharing sh)
  | Type.Actor -> Atom "Actor"

and func_sort s = match s with
  | Type.Call sh -> Atom ("Call " ^ sharing sh)
  | Type.Construct -> Atom "Construct"

and prim p = match p with
  | Null -> Atom "Null"
  | Bool -> Atom "Bool"
  | Nat -> Atom "Nat"
  | Int -> Atom "Int"
  | Word8 -> Atom "Word8"
  | Word16 -> Atom "Word16"
  | Word32 -> Atom "Word32"
  | Word64 -> Atom "Word64"
  | Float -> Atom "Float"
  | Char -> Atom "Char"
  | Text -> Atom "Text"

and con c = Atom (Con.to_string c)

let rec typ (t:Type.typ) = match t with
  | Var (s, i)             -> "Var" $$ [Atom s; Atom (string_of_int i)]
  | Con (c,ts)             -> "Con" $$ (con c::List.map typ ts)
  | Prim p                 -> "Prim" $$ [prim p]
  | Obj (s, ts)            -> "Obj" $$ [obj_sort s] @ List.map typ_field ts
  | Array t                -> "Array" $$ [typ t]
  | Opt t                  -> "Opt" $$ [typ t]
  | Tup ts                 -> "Tup" $$ List.map typ ts
  | Func (s, c, tbs, at, rt) -> "Func" $$ [func_sort s; Atom (control c)] @ List.map typ_bind tbs @ [ "" $$ (List.map typ at); "" $$ (List.map typ rt)]
  | Async t               -> "Async" $$ [typ t]
  | Mut t                 -> "Mut" $$ [typ t]
  | Class                 -> Atom "Class"
  | Shared                -> Atom "Shared"
  | Any                   -> Atom "Any"
  | Non                   -> Atom "Non"
  | Pre                   -> Atom "Pre"

and typ_bind (tb : Type.bind) =
  tb.var $$ [typ tb.bound]

and typ_field (tf : Type.field) =
  tf.name $$ [typ tf.typ]

