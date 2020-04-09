module Lsp_j = Lsp.Lsp_j
module Lsp_t = Lsp.Lsp_t

let jsonrpc_version : string = "2.0"

let notification :
    Lsp_t.notification_message_params -> Lsp_t.notification_message =
 fun params ->
  Lsp_t.
    {
      notification_message_jsonrpc = jsonrpc_version;
      notification_message_params = params;
    }

module MakeIO (Channels : sig
  val debug_channel : out_channel option

  val in_channel : in_channel

  val out_channel : out_channel
end) =
struct
  let log_to_file : string -> string -> unit =
   fun lbl txt ->
    Option.iter
      (fun oc ->
        Printf.fprintf oc "[%s] %s\n" lbl txt;
        flush oc)
      Channels.debug_channel

  let send : string -> unit =
   fun msg ->
    let length = String.length msg in
    Printf.fprintf Channels.out_channel "Content-Length: %d\r\n\r\n%s" length
      msg;
    flush Channels.out_channel

  let read_message : unit -> string * Lsp_t.incoming_message option =
   fun () ->
    let num =
      (* Every LSP message begins with a content length header *)
      input_line Channels.in_channel
      |> Lib.String.chop_prefix "Content-Length: "
      (* If this fails the protocol was broken and we abort. Should we recover from this? *)
      |> Option.get
      (* `input_line` does not consume the trailing \r, so we need to trim it off here *)
      |> String.trim
      |> int_of_string
    in
    (* The protocol terminates the content length header with two \r\n s. Our `input_line`
       call consumed the first, but we still need to eat the second one *)
    ignore (input_line Channels.in_channel);

    let buffer = Buffer.create num in
    Buffer.add_channel buffer Channels.in_channel num;
    let raw = String.trim (Buffer.contents buffer) in
    let msg =
      try Some (Lsp_j.incoming_message_of_string raw) with _ -> None
    in
    (raw, msg)

  let show_message : Lsp.MessageType.t -> string -> unit =
   fun typ msg ->
    let params =
      `WindowShowMessage
        Lsp_t.
          {
            window_show_message_params_type_ = typ;
            window_show_message_params_message = msg;
          }
    in
    let notification = notification params in
    send (Lsp_j.string_of_notification_message notification)

  let publish_diags : Lsp_t.document_uri -> Lsp_t.diagnostic list -> unit =
   fun uri diags ->
    let params =
      `PublishDiagnostics
        Lsp_t.
          {
            publish_diagnostics_params_uri = uri;
            publish_diagnostics_params_diagnostics = diags;
          }
    in
    let notification = notification params in
    send (Lsp_j.string_of_notification_message notification)

  let clear_diagnostics : Lsp_t.document_uri -> unit =
   fun uri -> publish_diags uri []
end

module type IO = sig
  val log_to_file : string -> string -> unit

  val send : string -> unit

  val read_message : unit -> string * Lsp_t.incoming_message option

  val show_message : Lsp.MessageType.t -> string -> unit

  val publish_diags : Lsp_t.document_uri -> Lsp_t.diagnostic list -> unit

  val clear_diagnostics : Lsp_t.document_uri -> unit
end
