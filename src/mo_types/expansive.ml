open Type

(* Collecting type constructors *)

let debug = 0

type vertex = Type.con * int
type edge = vertex * int * vertex

let compare_vertex (c,i) (d,j) =
  match Con.compare c d with
  | 0 -> compare i j
  | n -> n

let compare_edge (c1, w1, d1) (c2, w2, d2) =
  match compare_vertex c1 c2 with
  | 0 ->
    begin
      match compare w1 w2 with
      | 0 -> compare_vertex d1 d2
      | n -> n
    end
  | n -> n

module VertexSet = Set.Make(struct
  type t = vertex
  let compare = compare_vertex
end)

module EdgeSet = Set.Make(struct
   type t = edge
   let compare = compare_edge
end)

let string_of_vertex (c, i) = Printf.sprintf "(%s,%i)" (Con.name c) i

let string_of_vertices vs =
  Printf.sprintf "{ %s }" (String.concat "," (List.map string_of_vertex (VertexSet.elements vs)))

let string_of_edge (ci,w,dj) =
  Printf.sprintf ("%s -%i-> %s") (string_of_vertex ci) w (string_of_vertex dj)

let string_of_edges es =
  Printf.sprintf "{ %s }" (String.concat "," (List.map string_of_edge (EdgeSet.elements es)))

let rec edges_typ c cs i exp non (es:EdgeSet.t) t : EdgeSet.t =
  if debug > 1 then
    Printf.printf "{%s} {%s} %s\n"
    (string_of_vertices exp)
    (string_of_vertices non)
    (Type.string_of_typ t);
  match t with
  | Var (s, j) when j >= i  ->
    let ci = (c, j - i) in
    let es1 = VertexSet.fold (fun dj es -> EdgeSet.add (ci, 1, dj) es) exp es in
    let es2 = VertexSet.fold (fun dj es -> EdgeSet.add (ci, 0, dj) es) non es1 in
    es2
  | Var (s, j) ->
    assert (j < i);
    es
  | (Prim _ | Any | Non | Pre ) -> es
  | Con (d, ts) when ConSet.mem d cs ->
    let exp1 = VertexSet.union exp non in
    let _, es = List.fold_left
      (fun (k, es) t ->
        (k+1,
         edges_typ c cs i exp1 (VertexSet.singleton (d, k)) es t))
      (0, es)
      ts
    in
    es
  | Con (d, ts) ->
    let exp1 = VertexSet.union exp non in
    List.fold_left
      (edges_typ c cs i exp1 VertexSet.empty)
      es
      ts
  | (Opt t1 | Mut t1 | Array t1) ->
    edges_typ c cs i (VertexSet.union exp non) VertexSet.empty es t1
  | Async (_t1, t2) ->
    (* TODO: consider t1 *)
    edges_typ c cs i (VertexSet.union exp non) VertexSet.empty es t2
  | Tup ts ->
    let exp1 = VertexSet.union exp non in
    List.fold_left
      (edges_typ c cs i exp1 VertexSet.empty) es ts
  | Func (s, _c, tbs, ts1, ts2) ->
    let i1 = i + List.length tbs in
    let exp1 = VertexSet.union exp non in
    let es1 = List.fold_left
      (edges_bind c cs i1 exp1 VertexSet.empty) es tbs in
    let es2 = List.fold_left
      (edges_typ c cs i1 exp1 VertexSet.empty) es1 ts1
    in
    List.fold_left (edges_typ c cs i1 exp1 VertexSet.empty) es2 ts2
  | (Obj (_, fs) | Variant fs) ->
    let exp1 = VertexSet.union exp non in
    List.fold_left (edges_field c cs i exp1 VertexSet.empty) es fs
  | Typ c ->
    (* since constructors must be closed, no further edges possible *)
    es

and edges_bind c cs i exp non es tb =
  edges_typ c cs i exp non es tb.bound

and edges_field c cs i exp non es {lab; typ} =
  edges_typ c cs i exp non es typ

let edges_con cs c es : EdgeSet.t =
  match Con.kind c with
  | Def (tbs, t) ->
    (* TODO tbs *)
    edges_typ c cs 0 VertexSet.empty VertexSet.empty es t
  | Abs (tbs, t) ->
    assert false

let edges cs = ConSet.fold (edges_con cs) cs EdgeSet.empty

let vertices cs =
  ConSet.fold
    (fun c vs ->
      match Con.kind c with
      | Def (tbs, t) ->
        let ws = List.mapi (fun i _tb -> c,i) tbs in
        List.fold_left (fun vs v -> VertexSet.add v vs) vs ws
      | Abs (tbs, t) ->
        assert false) cs VertexSet.empty

module VertexMap = Map.Make(struct
  type t = vertex
  let compare = compare_vertex
end)

module EdgeMap = Map.Make(struct
  type t = vertex
  let compare = compare_vertex
end)


type state = {
    mutable index : int;
    mutable lowlink : int;
    mutable onstack : bool
  }

let sccs (vs, es) =
  let sccs = ref [] in
  let undefined = -1 in
  let index = ref 0 in
  let states = VertexSet.fold (fun v m ->
    VertexMap.add v {index = undefined; lowlink = 0; onstack = false} m)
    vs VertexMap.empty
  in
  let stack = ref [] in
  let successors = EdgeSet.fold (fun (ci, w, dj) m ->
     VertexMap.add ci (VertexSet.add dj (VertexMap.find ci m)) m)
     es
     (VertexSet.fold (fun v m -> VertexMap.add v VertexSet.empty m) vs VertexMap.empty)
  in
  let rec strongconnect v =
    let sv = VertexMap.find v states in
    sv.index <- !index;
    sv.lowlink <- !index;
    index := !index +1 ;
    stack := v::!stack;
    sv.onstack <- true;

    let ws = VertexMap.find v successors in
    ws |> VertexSet.iter (fun w ->
          let sw = VertexMap.find w states in
          if sw.index = undefined then begin
            strongconnect(w);
            sv.lowlink <- min sv.lowlink sw.lowlink;
            end
          else
            if sw.onstack then
              sv.lowlink <- min sv.lowlink sw.index;
            );
    if sv.lowlink = sv.index then
      let rec pop scc =
        let w = List.hd !stack in
        stack := List.tl !stack;
        let sw = VertexMap.find w states in
        sw.onstack <- false;
        let scc' = VertexSet.add w scc in
        if compare_vertex w v <> 0
        then pop scc'
        else sccs := scc' :: !sccs
      in
      pop VertexSet.empty
  in
  vs |> VertexSet.iter (fun v ->
    let sv = VertexMap.find v states in
    if sv.index = undefined then
      strongconnect v);
  !sccs

let string_of_sccs sccs =
  Printf.sprintf "{ %s }" (String.concat ","
   (List.map string_of_vertices sccs))


let is_expansive cs =
  let vs = vertices cs in
  if debug > 0 then Printf.printf "\nvertices %s" (string_of_vertices vs);
  let es = edges cs in
  if debug > 0 then Printf.printf "\nedges %s" (string_of_edges es);
  let vss = sccs (vs,es) in
  if debug > 0 then Printf.printf "\ncomponents %s" (string_of_sccs vss);
  let numbering = List.mapi (fun i vs -> vs,i) vss in
  let component = List.fold_left (fun m (vs,i) ->
    VertexSet.fold (fun v m -> VertexMap.add v i m) vs m)
    VertexMap.empty numbering
  in
  let is_expansive = EdgeSet.exists
    (fun (ci, w, dj) ->
      w > 0 &&
        VertexMap.find ci component = VertexMap.find dj component) es
  in
  if debug > 0 then Printf.printf "is_expansive: %b" is_expansive;
  if is_expansive then
    Some
      (Printf.sprintf
         "\n  vertices:\n    %s\n  edges:\n    %s\n  components:\n    %s"
         (string_of_vertices vs) (string_of_edges es) (string_of_sccs vss))
  else None

