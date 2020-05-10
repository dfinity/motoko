open ExpGraph
open Type

(*
This module exports a function

  typ_hash : typ -> string

with these properties
 - Its domain are closed types
 - It does not support abstract types (for now)
 - It does not support type parameters (for now)
 - Type.eq t1 t2 = (typ_hash t1 = typ_hash t2)
 - The output is readable, and still somewhat compact
*)

(* A hint about how to print the node *)
type arity = Nullary | Unary | Nary | Labeled of string list | TwoSeq of int

(*
Type construtors are (mostly) described by a single letter.
NB: This code better be prefix-free. Hence capital N, otherwise n8=u
is a bit ambigous
*)

let prim = function
  | Null -> "z"
  | Bool -> "b"
  | Nat -> "N"
  | Nat8 -> "n8"
  | Nat16 -> "n16"
  | Nat32 -> "n32"
  | Nat64 -> "n64"
  | Int -> "I"
  | Int8 -> "i8"
  | Int16 -> "i16"
  | Int32 -> "i32"
  | Int64 -> "i64"
  | Word8 -> "w8"
  | Word16 -> "w16"
  | Word32 -> "w32"
  | Word64 -> "w64"
  | Float -> "f"
  | Char -> "c"
  | Text -> "t"
  | Blob -> "b"
  | Error -> "e"
  | Principal -> "p"

let rec go = function
  | Prim p -> ((Nullary, prim p), [])
  | Any -> ((Nullary, "any"), [])
  | Non -> ((Nullary, "non"), [])
  | Opt t -> ((Unary, "?"), [t])
  | Tup [] -> ((Nullary, "u"), [])
  | Tup ts -> ((Nary, ""), ts)
  | Array (Mut t) -> ((Unary, "V"), [t])
  | Array t -> ((Unary, "v"), [t])

  | Obj (s, fs) ->
    ( ( Labeled (List.map (fun f -> f.lab ^ if is_mut f.typ then "!" else "") fs),
        (match s with Object -> "r" | Module -> "rm" | Memory -> "rs" | Actor -> "ra")
      )
    , List.map (fun f -> as_immut f.typ) fs
    )
  | Variant fs ->
    ( ( Labeled (List.map (fun f -> f.lab) fs),
        "v"
      )
    , List.map (fun f -> f.typ) fs
    )

  | Func (s, c, tbs, ts1, ts2) ->
    List.iter (fun bind -> assert (bind.sort = Scope)) tbs;
    ( ( TwoSeq (List.length ts1),
        "F" ^
        (match s with Local -> "" | Shared Query -> "q" | Shared Write -> "s") ^
        (match c with Returns -> "" | Promises -> "p" | Replies -> "r")
      )
    , ts1 @ ts2
    )
  | Async _ -> raise (Invalid_argument "typ_hash: Only supports serializable data")

  | Con _ as t -> go (normalize t)
  | Mut _ -> assert false
  | Pre -> assert false
  | Var _ -> assert false
  | Typ _ -> assert false

let paren xs = "(" ^ String.concat "" xs ^ ")"

let of_con (a, k) args =
  match a, args with
  | Nullary, [] -> k
  | Unary, [a] -> k ^ a
  | Nary, _ -> k ^ paren args
  | Labeled ls, _ -> k ^ "(" ^ String.concat "," (List.map2 (fun l a -> l ^ ":" ^ a) ls args) ^ ")"
  | TwoSeq n, _->
    let (a1, a2) = Lib.List.split_at n args in
    k ^ paren a1 ^ paren a2
  | _, _ -> assert false

let of_ref i x =
  Int.to_string i ^ "=" ^ x

let of_def i =
  "!" ^ Int.to_string i

let typ_hash : typ -> string =
  fun t -> t |> unfold go |> canonicalize |> fold of_con of_ref of_def


(* Some small unit tests *)

let test t expected =
  let actual = typ_hash t in
  if actual = expected then
    true
  else
    (Printf.printf "\nExpected:\n  %s\nbut got:\n  %s\n" expected actual; false)

let%test "monolist" =
  let con = Con.fresh "List" (Abs ([], Pre))  in
  let t = Con (con, []) in
  Con.unsafe_set_kind con (Def ([], Opt (Tup [nat; t])));
  test t "0=?(N!0)"

let%test "polylist" =
  let con = Con.fresh "List" (Abs ([], Pre))  in
  let bind = { var = "T"; sort = Type; bound = Any } in
  let v = Var ("T", 0) in
  Con.unsafe_set_kind con (Def ([bind], Opt (Tup [v; Con (con, [v])])));
  let t = Con (con, [nat]) in
  test t "0=?(N!0)"
