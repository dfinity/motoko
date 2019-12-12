open Mo_def

module S : Set.S with type elt = String.t

type package_urls = (string * string) list

val collect_imports : Syntax.prog -> string list
val resolve : package_urls -> Syntax.prog -> string -> S.t Diag.result
val match_package_name : string -> (string * string) option
