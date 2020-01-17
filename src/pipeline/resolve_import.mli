open Mo_def
open Mo_config

type filepath = string

module S : Set.S with type elt = Syntax.resolved_import
module RIM : Map.S with type key = Syntax.resolved_import

type actor_idl_path = string option
type package_urls = string Flags.M.t
type actor_aliases = string Flags.M.t

type resolved_imports = Syntax.resolved_import Source.phrase list

val collect_imports : Syntax.prog -> string list

type flags = {
  package_urls : package_urls;
  actor_aliases : actor_aliases;
  actor_idl_path : actor_idl_path;
}

val resolve : flags -> Syntax.prog -> filepath -> resolved_imports Diag.result

(* Exported for tests *)
val append_extension : filepath -> filepath option
