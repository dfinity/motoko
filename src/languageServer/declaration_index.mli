open Source
open Mo_types

type t

val empty : string -> t

val string_of_index : t -> string

type value_decl = {
  name : string;
  typ : Type.typ;
  definition : region option;
  doc_comment : string option;
}

type type_decl = {
  name : string;
  typ : Type.con;
  definition : region option;
  doc_comment : string option;
}

type ide_decl = ValueDecl of value_decl | TypeDecl of type_decl

val string_of_ide_decl : ide_decl -> string

val name_of_ide_decl : ide_decl -> string

val shorten_import_path :
  Pipeline.ResolveImport.package_map
  (** The package map for searching package paths *) ->
  Pipeline.ResolveImport.aliases (** The aliases for looking up canister ids *) ->
  string (** The file that ends up containing the import *) ->
  string (** The path to be shortened *) ->
  string

val lookup_module : string -> string -> t -> (string * ide_decl list) option

val find_with_prefix :
  string (** The prefix *) ->
  string (** The file in which the results would be imported *) ->
  t (** The declaration index *) ->
  (string * ide_decl list) list

val make_index :
  string (** The project root *) ->
  Vfs.t ->
  string list (** The entry points *) ->
  t Diag.result
