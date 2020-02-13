open Source
open Mo_types

type t
val empty : unit -> t
val string_of_index : t -> string

type value_decl = {
    name : string;
    typ: Type.typ;
    definition: region option;
  }

type type_decl = {
    name : string;
    typ: Type.con;
    definition: region option;
  }

type ide_decl =
  | ValueDecl of value_decl
  | TypeDecl of type_decl

val string_of_ide_decl : ide_decl -> string
val name_of_ide_decl : ide_decl -> string

val shorten_import_path
    : Pipeline.ResolveImport.package_map
    (** The package map for searching package paths *)
    -> string
    (** The file that ends up containing the import *)
    -> string
    (** The path to be shortened *)
    -> string

val lookup_module : string -> t -> (string * ide_decl list) option

val make_index : (string -> string -> unit) -> Vfs.t -> string list -> t Diag.result
