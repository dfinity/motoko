open Mo_def

type filepath = string

module S : Set.S with type elt = Syntax.resolved_import
module RIM : Map.S with type key = Syntax.resolved_import

type actor_idl_path = string option
type package_urls = (string * string) list

type resolved_imports = Syntax.resolved_import Source.phrase list

val collect_imports : Syntax.prog -> string list
val resolve : actor_idl_path -> package_urls -> Syntax.prog -> filepath -> resolved_imports Diag.result
