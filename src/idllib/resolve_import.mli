module Set : Set.S with type elt = String.t

val resolve : Syntax.prog -> string -> string list Diag.result                                   
