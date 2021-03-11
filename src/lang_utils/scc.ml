(*
  A naive implementation of Tarjan's algorithm.

  Computes the strongly connected components of a directed graph, returning
  the list of components in topological order.

  Adapted from:
  https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
*)

module Make(Vertex : Set.OrderedType) :
  sig
    module VertexSet :
      Set.S with type elt = Vertex.t with type t = Set.Make(Vertex).t
    module Edge : Set.OrderedType with type t = Vertex.t * Vertex.t
    module EdgeSet : Set.S with type elt = Edge.t
    val scc : VertexSet.t -> EdgeSet.t -> VertexSet.t list
  end
 = struct

  module Edge = struct
    type t = Vertex.t * Vertex.t
    (* the lexicographic ordering on pairs *)
    let compare (v1, w1) (v2, w2) =
     match Vertex.compare v1 v2 with
     | 0 -> Vertex.compare w1 w2
     | o -> o
  end

  module VertexSet = Set.Make(Vertex)
  module EdgeSet = Set.Make(Edge)

  module VertexMap = Map.Make(Vertex)

  type state = {
    mutable index : int;
    mutable lowlink : int;
    mutable onstack : bool
  }

  let scc vs es =
  let sccs = ref [] in
  let undefined = -1 in
  let index = ref 0 in
  let states = VertexSet.fold (fun v m ->
    VertexMap.add v {index = undefined; lowlink = 0; onstack = false} m)
    vs VertexMap.empty
  in
  let stack = ref [] in
  let successors = EdgeSet.fold (fun (v, w) m ->
    VertexMap.add v (VertexSet.add w (VertexMap.find v m)) m)
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
          sv.lowlink <- min sv.lowlink sw.index);
    if sv.lowlink = sv.index then
      let rec pop scc =
        let w = List.hd !stack in
        stack := List.tl !stack;
        let sw = VertexMap.find w states in
        sw.onstack <- false;
        let scc' = VertexSet.add w scc in
        if Vertex.compare w v <> 0
        then pop scc'
        else sccs := scc' :: !sccs
      in
      pop VertexSet.empty
  in
  vs |> VertexSet.iter (fun v ->
    let sv = VertexMap.find v states in
    if sv.index = undefined then
      strongconnect v);
  List.rev !sccs

end
