module Lsp_t = Lsp.Lsp_t

type t

type uri = string
(* TODO(Christoph): Track file version *)
type virtual_file = string

(** Creates a new virtual file system *)
val empty: t

(** Reads a file from the OS's file system and adds it to the vfs *)
val open_file: Lsp_t.text_document_did_open_params -> t -> t

(** Reads a file from the vfs *)
val read_file: uri -> t -> virtual_file option

(** Updates the contents of a file in the vfs *)
val update_file: Lsp_t.text_document_did_change_params -> t -> t

(** Removes a file from the vfs *)
val close_file: uri -> t -> t
