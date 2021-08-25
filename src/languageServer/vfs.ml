module Lsp_t = Lsp.Lsp_t
module VfsStore = Map.Make (String)

(* TODO(Christoph): Terrible format, ideally we want some RRB or HAMT
   backed vector type with a zipper *)
type virtual_file = string list

type t = virtual_file VfsStore.t

type uri = string

let file_uri_prefix = "file://" ^ Sys.getcwd () ^ "/"

let uri_from_file path =
  "file://" ^ Lib.FilePath.make_absolute (Sys.getcwd ()) path

let file_from_uri uri =
  match Lib.String.chop_prefix file_uri_prefix uri with
  | Some file -> file
  | None ->
      let _ = Debug.log "error" ("Failed to strip filename from: " ^ uri) in
      uri

let abs_file_from_uri uri =
  Lib.FilePath.normalise
    (match Lib.String.chop_prefix "file://" uri with
    | Some file -> file
    | None ->
        let _ = Debug.log "error" ("Failed to strip filename from: " ^ uri) in
        uri)

let empty = VfsStore.empty

let open_file did_open_params vfs =
  let text_document_item =
    did_open_params.Lsp_t.text_document_did_open_params_textDocument
  in
  let uri = text_document_item.Lsp_t.text_document_item_uri in
  let _ = text_document_item.Lsp_t.text_document_item_version in
  let text = text_document_item.Lsp_t.text_document_item_text in
  let lines =
    (* TODO(Christoph): Windows compatibility issues *)
    Lib.String.split text '\n'
  in
  VfsStore.add uri lines vfs

let read_file uri vfs =
  VfsStore.find_opt uri vfs |> Option.map (String.concat "\n")

let close_file did_close_params =
  let uri =
    did_close_params.Lsp_t.text_document_did_close_params_textDocument
      .Lsp_t.text_document_identifier_uri
  in
  VfsStore.remove uri

(* This isn't recursive, but I prefer apply_change to be defined after
   update_file, to keep the public API of the module in one place *)
let rec update_file did_change_params (vfs : t) : t =
  let uri =
    did_change_params.Lsp_t.text_document_did_change_params_textDocument
      .Lsp_t.versioned_text_document_identifier_uri
  in
  let change_events =
    did_change_params.Lsp_t.text_document_did_change_params_contentChanges
  in
  let previous_content =
    (* TODO Let's not crash here *)
    VfsStore.find uri vfs
  in
  let new_content =
    List.fold_left apply_change previous_content change_events
  in
  VfsStore.add uri new_content vfs

and apply_change lines
    Lsp_t.
      {
        text_document_content_change_event_range = range;
        text_document_content_change_event_rangeLength = rangeLength;
        text_document_content_change_event_text = text;
      } =
  match range with
  | None ->
      (* If the range is None, the editor expects us to fully replace
           the file content *)
      (* TODO(Christoph): We should be normalising file endings here *)
      Lib.String.split text '\n'
  | Some
      Lsp_t.
        {
          range_start = { position_line = sl; position_character = sc };
          range_end_ = { position_line = el; position_character = ec };
        } ->
      let prev_lines, rest = Lib.List.split_at sl lines in
      let affected, past_lines = Lib.List.split_at (el - sl + 1) rest in
      (* let print_lines lbl ls =
       *   Printf.printf "%s: %s\n" lbl ("[" ^ join_with "; " ls ^ "]") in
       * let _ = print_lines "prev" prev_lines;
       *         print_lines "aff" affected;
       *         print_lines "past" past_lines in *)
      (* TODO(Christoph): All the String.sub calls following should
         operate on UTF16 code unit boundaries instead, as that's what
         LSP talks. *)
      let new_ =
        match affected with
        | [] -> text
        | [ aff ] ->
            String.sub aff 0 sc
            ^ text
            ^ String.sub aff ec (String.length aff - ec)
        | affected ->
            let init, last = Lib.List.split_last affected in
            String.sub (String.concat "\n" init) 0 sc
            ^ text
            ^ String.sub last ec (String.length last - ec)
      in
      prev_lines @ Lib.String.split new_ '\n' @ past_lines

let parse_file vfs path =
  match read_file (uri_from_file path) vfs with
  | None -> Pipeline.parse_file Source.no_region path
  | Some file -> Pipeline.parse_string path file
