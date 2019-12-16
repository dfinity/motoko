open Mo_def

type filepath = string

module S : Set.S with type elt = Syntax.resolved_import
module RIM : Map.S with type key = Syntax.resolved_import

type package_urls = (string * string) list

type parsed_import =
  | PackageImport of (string * string)
  | RelativeImport of string
  | ActorImport of string


type resolved_imports = Syntax.resolved_import Source.phrase list

val collect_imports : Syntax.prog -> string list
val resolve : package_urls -> Syntax.prog -> filepath -> resolved_imports Diag.result
val parse_import : string -> parsed_import
