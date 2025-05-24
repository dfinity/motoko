module Srcs_tbl : Hashtbl.S with type key = Source.region
type srcs_tbl = Source.Region_set.t Srcs_tbl.t
type t = srcs_tbl

module Srcs_map : sig
   include module type of Source.Region_map with type key = Source.region

   val adjoin : Source.Region_set.t t -> Source.Region_set.t t -> Source.Region_set.t t
end
type srcs_map = Source.Region_set.t Srcs_map.t

val empty_srcs_tbl : unit -> t
val get_srcs : t -> Source.region -> Source.Region_set.t
val add_src : t -> Source.region -> unit
val of_immutable_map : srcs_map -> t
val of_mutable_tbl : t -> srcs_map
