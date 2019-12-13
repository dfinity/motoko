open Mo_def

module S : Set.S with type elt = Syntax.resolved_import

type package_urls = (string * string) list

type parsed_import =
  | PackageImport of (string * string)
  | RelativeImport of string
  | ActorImport of string

val collect_imports : Syntax.prog -> string list
val resolve : package_urls -> Syntax.prog -> string -> S.t Diag.result
val parse_import : string -> parsed_import
