open Type
(* Collecting type constructors *)

type vertex = Type.con * int
type edge = vertex * int * vertex

module EdgeSet = Set.Make(struct
  type t = edge
  let compare = compare
end)

module VertexSet = Set.Make(struct
  type t = vertex
  let compare = compare
end)

let rec edges_typ c cs n exp non (es:EdgeSet.t) t : EdgeSet.t =
  match t with
  | Var (s, i) when i > n ->
    let ci = (c, i - n) in
    let es1 = VertexSet.fold (fun dj es -> EdgeSet.add (dj, 1, ci) es) exp es in
    let es2 = VertexSet.fold (fun dj es -> EdgeSet.add (dj, 0, ci) es) exp es1 in
    es2
  | Var (s, i) ->
    assert (i <= n);
    es
  | (Prim _ | Any | Non | Pre ) -> es
  | Con (d, ts) ->
    let rec go i ts es = match ts with
      | [] -> es
      | t1::ts1 ->
        let es1 = edges_typ c cs n
          (VertexSet.union exp non)
          (if List.mem d cs then
             VertexSet.singleton (d,i) (* is this the right index? *)
           else VertexSet.empty)
          es
          t1
        in
        go (i+1) ts1 es1
    in
    go 0 ts es
  | (Opt t | Mut t | Array t) ->
    edges_typ c cs n (VertexSet.union exp non) VertexSet.empty es t
  | Async (_t1, t2) ->
    (* TODO: consider t1 *)
    edges_typ c cs n (VertexSet.union exp non) VertexSet.empty es t2
  | Tup ts ->
    let exp1 = VertexSet.union exp non in
    let rec go ts es = match ts with
      | [] -> es
      | t1::ts1 ->
        let es1 = edges_typ c cs n
          exp1
          VertexSet.empty
          es
          t1
        in
        go ts1 es1
    in
    go ts es
  | Func (s, _c, tbs, ts1, ts2) ->
    let n1 = n + List.length tbs in
    let exp1 = VertexSet.union exp non in
    let es1 = List.fold_left
      (edges_bind c cs n1 exp1 VertexSet.empty) es tbs in
    let es2 = List.fold_left
      (edges_typ c cs n1 exp1 VertexSet.empty) es1 ts1
    in
    List.fold_left (edges_typ c cs n1 exp1 VertexSet.empty) es2 ts2
  | (Obj (_, fs) | Variant fs) ->
    let exp1 = VertexSet.union exp non in
    List.fold_left (edges_field c cs n exp1 VertexSet.empty) es fs
  | Typ c ->
    (* since constructors must be closed, no further edges possible *)
    es

and edges_bind c cs n exp non es tb =
  edges_typ c cs n exp non es tb.bound

and edges_field c cs n exp non es {lab; typ} =
  edges_typ c cs n exp non es typ

(*
let edges_kind c cs n exp non es k =
  match k with
  | Def (tbs, t)
  | Abs (tbs, t) ->
    let n1 = n + List.length tbs in
    let es1 = List.fold_left (edges_bind c cs n1 exp1 VertexSet.empty) es tbs
    in
    edges_typ c cs n exp non es1 t
*)
let edges_con cs es c : EdgeSet.t =
  match Con.kind c with
  | Def (tbs, t) ->
    edges_typ c cs (List.length tbs) VertexSet.empty VertexSet.empty es t
  | Abs (tbs, t) ->
    assert false

let edges cs =
  List.fold_left (edges_con cs) EdgeSet.empty cs

