(*
  Computes the strongly connected components of a directed graph, returning
  the list of components in topological order.
*)

module Make(Vertex : Set.OrderedType) :
  sig
    module VertexSet :
      Set.S with type elt = Vertex.t with type t = Set.Make(Vertex).t
    module Edge : Set.OrderedType with type t = Vertex.t * Vertex.t
    module EdgeSet : Set.S with type elt = Edge.t
    val scc : VertexSet.t -> EdgeSet.t -> VertexSet.t list
  end
