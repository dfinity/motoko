module Lsp_t = Lsp.Lsp_t

module type IO = sig
  val log_to_file : string -> string -> unit

  val send : string -> unit

  val read_message : unit -> string * Lsp_t.incoming_message option

  val show_message : Lsp.MessageType.t -> string -> unit

  val publish_diags : Lsp_t.document_uri -> Lsp_t.diagnostic list -> unit

  val clear_diagnostics : Lsp_t.document_uri -> unit
end

module MakeIO (_ : sig
  val debug_channel : out_channel option

  val in_channel : in_channel

  val out_channel : out_channel
end) : IO
