open As_frontend

module S : Set.S with type elt = String.t

val resolve : Syntax.prog -> string -> S.t Diag.result

