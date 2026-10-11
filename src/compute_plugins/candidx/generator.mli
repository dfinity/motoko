(** Generate a direct encoder specialized to the original Motoko types. *)
module Make (B : Codegen.Plugin_codegen.Backend) : sig
  val compile : B.env -> Mo_types.Type.typ list -> Codegen.InstrList.t
end
