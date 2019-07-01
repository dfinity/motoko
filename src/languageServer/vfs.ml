module Lsp_t = Lsp.Lsp_t

module VfsStore = Map.Make(String)

type virtual_file = string

type t = virtual_file VfsStore.t

type uri = string

let empty = VfsStore.empty

let file_uri_prefix = "file://" ^ Sys.getcwd () ^ "/"
let file_from_uri uri =
  Base.String.chop_prefix ~prefix:file_uri_prefix uri

let open_file did_open_params vfs =
  let text_document_item =
    did_open_params
      .Lsp_t
      .text_document_did_open_params_textDocument in
  let uri =
     text_document_item.Lsp_t.text_document_item_uri in
  let _ =
     text_document_item.Lsp_t.text_document_item_version in
  let text =
     text_document_item.Lsp_t.text_document_item_text in
  VfsStore.add uri text vfs

let read_file uri vfs =
  VfsStore.find_opt uri vfs

let update_file did_change_params =
  let change_event =
    did_change_params.Lsp_t.text_document_did_change_params_contentChanges
    |> (* TODO(Christoph): This needs to change once we allow
          incremental file updates*)
      Base.Fn.flip List.nth 0
  in
  let new_content = change_event.Lsp_t.text_document_content_change_event_text in
  let uri =
    did_change_params
      .Lsp_t.text_document_did_change_params_textDocument
      .Lsp_t.versioned_text_document_identifier_uri
  in
  VfsStore.add uri new_content

let close_file uri = VfsStore.remove uri
