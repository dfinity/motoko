open Mo_config
module Lsp_j = Lsp.Lsp_j
module Lsp_t = Lsp.Lsp_t

(* Ideally the following functions would be in the `Lsp` module, but doing so
   creates a circular dependency with the `Lsp_t` module generated by ATD.
 *)

let jsonrpc_version : string = "2.0"

let notification :
    Lsp_t.notification_message_params -> Lsp_t.notification_message =
 fun params ->
  Lsp_t.
    {
      notification_message_jsonrpc = jsonrpc_version;
      notification_message_params = params;
    }

let response_result_message :
    int -> Lsp_t.response_result -> Lsp_t.response_message =
 fun id result ->
  Lsp_t.
    {
      response_message_jsonrpc = jsonrpc_version;
      response_message_id = id;
      response_message_result = Some result;
      response_message_error = None;
    }

let response_error_message :
    int -> Lsp_t.response_error -> Lsp_t.response_message =
 fun id error ->
  Lsp_t.
    {
      response_message_jsonrpc = jsonrpc_version;
      response_message_id = id;
      response_message_result = None;
      response_message_error = Some error;
    }

module MakeIO (OC : sig
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
      OC.debug_channel

  let send : string -> unit =
   fun msg ->
    let length = String.length msg in
    Printf.fprintf OC.out_channel "Content-Length: %d\r\n\r\n%s" length msg;
    flush OC.out_channel

  let read_message : unit -> string * Lsp_t.incoming_message option =
   fun () ->
    let clength = input_line OC.in_channel in
    let cl = "Content-Length: " in
    let cll = String.length cl in
    let num =
      int_of_string String.(trim (sub clength cll (length clength - cll - 1)))
      + 2
    in
    let buffer = Buffer.create num in
    Buffer.add_channel buffer stdin num;
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

let position_of_pos : Source.pos -> Lsp_t.position =
 fun pos ->
  Lsp_t.
    {
      (* The LSP spec requires zero-based positions *)
      position_line = max 0 (pos.Source.line - 1);
      position_character = pos.Source.column;
    }

let range_of_region : Source.region -> Lsp_t.range =
 fun at ->
  Lsp_t.
    {
      range_start = position_of_pos at.Source.left;
      range_end_ = position_of_pos at.Source.right;
    }

let severity_of_sev : Diag.severity -> Lsp.DiagnosticSeverity.t = function
  | Diag.Error -> Lsp.DiagnosticSeverity.Error
  | Diag.Warning -> Lsp.DiagnosticSeverity.Warning
  | Diag.Info -> Lsp.DiagnosticSeverity.Information

let diagnostics_of_message : Diag.message -> Lsp_t.diagnostic * string =
 fun msg ->
  ( Lsp_t.
      {
        diagnostic_range = range_of_region msg.Diag.at;
        diagnostic_severity = Some (severity_of_sev msg.Diag.sev);
        diagnostic_code = None;
        diagnostic_source = Some "Motoko";
        diagnostic_message = msg.Diag.text;
        diagnostic_relatedInformation = None;
      },
    msg.Diag.at.Source.left.Source.file )

(** A record with all the mutable state and caches the Language Server needs *)
type ls_state = {
  decl_index : Declaration_index.t ref;  (** Our index of known definitions *)
  vfs : Vfs.t ref;
      (** The virtual file system. Our knowledge of opened files and their contents *)
  startup_diagnostics : Diag.messages ref;
      (** Diagnostics we encountered when building the index on startup. These need to
       ** be cached here because we can't report them until initialization is completed  *)
  files_with_diagnostics : Vfs.uri list ref;
      (** All files with known diagnostics. We need to store them so that we can clear
       ** their diagnostics once compilation succeeds *)
  shutdown : bool ref;
      (** Have we received the shutdown message?, A quirk of the LSP *)
}

let start : string -> bool -> 'a =
 fun entry_point debug ->
  let debug_channel : out_channel option =
    if debug then Some (open_out_gen [ Open_append; Open_creat ] 0o666 "ls.log")
    else None
  in
  let module IO = MakeIO (struct
    let debug_channel = debug_channel

    let in_channel = stdin

    let out_channel = stdout
  end) in
  let _ = IO.log_to_file "entry_point" entry_point in
  let _ =
    Flags.M.iter
      (fun k v -> IO.log_to_file "package" (Printf.sprintf "%s => %s" k v))
      !Flags.package_urls
  in
  let _ = Debug.logger := IO.log_to_file in
  let project_root = Sys.getcwd () in
  let ls_state =
    {
      vfs = ref Vfs.empty;
      decl_index = ref (Declaration_index.empty project_root);
      startup_diagnostics = ref [];
      files_with_diagnostics = ref [];
      shutdown = ref false;
    }
  in
  let client_capabilities = ref None in
  let _ = IO.log_to_file "project_root" project_root in
  let _ =
    ls_state.decl_index :=
      match
        Declaration_index.make_index IO.log_to_file project_root !(ls_state.vfs)
          [ entry_point ]
      with
      | Error errs ->
          List.iter
            (fun err -> IO.log_to_file "Error" (Diag.string_of_message err))
            errs;
          ls_state.startup_diagnostics := errs;
          Declaration_index.empty project_root
      | Ok (ix, _) -> ix
  in
  let sync_diagnostics msgs =
    let diags_by_file =
      msgs
      |> List.map diagnostics_of_message
      |> Lib.List.group (fun (_, f1) (_, f2) -> f1 = f2)
      |> List.map (fun diags ->
             let _, file = List.hd diags in
             (Vfs.uri_from_file file, List.map fst diags))
    in
    List.iter IO.clear_diagnostics !(ls_state.files_with_diagnostics);
    ls_state.files_with_diagnostics := List.map fst diags_by_file;
    List.iter (Lib.Fun.uncurry IO.publish_diags) diags_by_file
  in
  let handle_message raw = function
    | Some id, `Initialize params ->
        client_capabilities := Some params.Lsp_t.initialize_params_capabilities;
        let completion_options =
          Lsp_t.
            {
              completion_options_resolveProvider = Some false;
              completion_options_triggerCharacters = Some [ "." ];
            }
        in
        let text_document_sync_options =
          Lsp_t.
            {
              text_document_sync_options_openClose = Some true;
              (* Full *)
              (* text_document_sync_options_change = Some 1; *)
              (* Incremental *)
              text_document_sync_options_change = Some 2;
              text_document_sync_options_willSave = Some false;
              text_document_sync_options_willSaveWaitUntil = Some false;
              text_document_sync_options_save =
                Some { save_options_includeText = Some true };
            }
        in
        let result =
          `Initialize
            Lsp_t.
              {
                initialize_result_capabilities =
                  {
                    server_capabilities_textDocumentSync =
                      text_document_sync_options;
                    server_capabilities_hoverProvider = Some true;
                    server_capabilities_completionProvider =
                      Some completion_options;
                    server_capabilities_definitionProvider = Some true;
                  };
              }
        in
        let response = response_result_message id result in
        IO.send (Lsp_j.string_of_response_message response)
    | Some id, `TextDocumentHover params ->
        let uri =
          params.Lsp_t.text_document_position_params_textDocument
            .Lsp_t.text_document_identifier_uri
        in
        let position = params.Lsp_t.text_document_position_params_position in
        let response =
          match Vfs.read_file uri !(ls_state.vfs) with
          | None ->
              response_error_message id
                Lsp_t.
                  {
                    code = 1;
                    message =
                      "Tried to find hover a file that hadn't been opened yet";
                  }
          | Some file_content ->
              let result =
                Hover.hover_handler IO.log_to_file !(ls_state.decl_index)
                  position file_content project_root
                  (Vfs.abs_file_from_uri IO.log_to_file uri)
              in
              response_result_message id result
        in
        IO.send (Lsp_j.string_of_response_message response)
    | Some id, `TextDocumentDefinition params ->
        let uri =
          params.Lsp_t.text_document_position_params_textDocument
            .Lsp_t.text_document_identifier_uri
        in
        let position = params.Lsp_t.text_document_position_params_position in
        let response =
          match Vfs.read_file uri !(ls_state.vfs) with
          | None ->
              response_error_message id
                Lsp_t.
                  {
                    code = 1;
                    message =
                      "Tried to find a definition in a file that hadn't been \
                       opened yet";
                  }
          | Some file_content ->
              let result =
                Definition.definition_handler !(ls_state.decl_index) position
                  file_content project_root
                  (Vfs.abs_file_from_uri IO.log_to_file uri)
              in
              response_result_message id result
        in
        IO.send (Lsp_j.string_of_response_message response)
    | _, `TextDocumentDidOpen params ->
        ls_state.vfs := Vfs.open_file params !(ls_state.vfs)
    | _, `TextDocumentDidChange params ->
        ls_state.vfs := Vfs.update_file params !(ls_state.vfs)
    | _, `TextDocumentDidClose params ->
        ls_state.vfs := Vfs.close_file params !(ls_state.vfs)
    | _, `TextDocumentDidSave _ ->
        let msgs =
          match
            Declaration_index.make_index IO.log_to_file project_root
              !(ls_state.vfs) [ entry_point ]
          with
          | Error msgs' ->
              List.iter
                (fun msg ->
                  IO.log_to_file "rebuild_error" (Diag.string_of_message msg))
                msgs';
              msgs'
          | Ok (ix, msgs') ->
              ls_state.decl_index := ix;
              msgs'
        in
        sync_diagnostics msgs
    | None, `Initialized _ ->
        sync_diagnostics !(ls_state.startup_diagnostics);
        ls_state.startup_diagnostics := [];
        IO.show_message Lsp.MessageType.Info "Motoko LS initialized"
    | Some id, `Shutdown _ ->
        ls_state.shutdown := true;
        response_result_message id (`ShutdownResponse None)
        |> Lsp_j.string_of_response_message
        |> IO.send
    | _, `Exit _ -> if !(ls_state.shutdown) then exit 0 else exit 1
    | Some id, `CompletionRequest params ->
        let uri =
          params.Lsp_t.text_document_position_params_textDocument
            .Lsp_t.text_document_identifier_uri
        in
        let position = params.Lsp_t.text_document_position_params_position in
        let response =
          match Vfs.read_file uri !(ls_state.vfs) with
          | None ->
              response_error_message id
                Lsp_t.
                  {
                    code = 1;
                    message =
                      "Tried to find completions for a file that hadn't been \
                       opened yet";
                  }
          | Some file_content ->
              Completion.completion_handler !(ls_state.decl_index)
                IO.log_to_file project_root
                (Vfs.abs_file_from_uri IO.log_to_file uri)
                file_content position
              |> response_result_message id
        in
        IO.send (Lsp_j.string_of_response_message response)
    (* Unhandled messages *)
    | _ -> IO.log_to_file "unhandled message" raw
  in
  let rec loop () =
    let raw, message = IO.read_message () in
    ( match message with
    | None -> Debug.log "decoding error" raw
    | Some message ->
        let message_id = message.Lsp_t.incoming_message_id in
        handle_message raw (message_id, message.Lsp_t.incoming_message_params)
    );
    loop ()
  in
  loop ()
