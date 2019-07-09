open As_def

module S : Set.S with type elt = String.t

type package_urls = (string * string) list

val resolve : package_urls -> Syntax.prog -> string -> S.t Diag.result

