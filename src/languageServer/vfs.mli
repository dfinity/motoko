module Lsp_t = Lsp.Lsp_t

type t
type uri = string

(* NOTE(Christoph): Track file version *)

val uri_from_file : string -> uri
val file_from_uri : uri -> string
val abs_file_from_uri : uri -> string
val parse_file : t -> Pipeline.no_region_parse_fn

(** Creates a new virtual file system *)
val empty : t

(** Reads a file from the OS's file system and adds it to the vfs *)
val open_file : Lsp_t.text_document_did_open_params -> t -> t

(** Reads a file from the vfs *)
val read_file : uri -> t -> string option

(** Updates the contents of a file in the vfs *)
val update_file : Lsp_t.text_document_did_change_params -> t -> t

(** Removes a file from the vfs *)
val close_file : Lsp_t.text_document_did_close_params -> t -> t

(** For tests *)
val apply_change :
  string list -> Lsp_t.text_document_content_change_event -> string list
