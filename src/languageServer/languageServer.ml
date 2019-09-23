module Lsp_j = Lsp.Lsp_j
module Lsp_t = Lsp.Lsp_t

(* Ideally the following functions would be in the `Lsp` module, but doing so
   creates a circular dependency with the `Lsp_t` module generated by ATD.
 *)

let jsonrpc_version : string = "2.0"

let notification (params : Lsp_t.notification_message_params) : Lsp_t.notification_message = Lsp_t.
  { notification_message_jsonrpc = jsonrpc_version;
    notification_message_params = params;
  }

let response_result_message (id : int) (result : Lsp_t.response_result) : Lsp_t.response_message = Lsp_t.
  { response_message_jsonrpc = jsonrpc_version;
    response_message_id = id;
    response_message_result = Some result;
    response_message_error = None;
  }

let response_error_message (id : int) (error : Lsp_t.response_error) : Lsp_t.response_message = Lsp_t.
  { response_message_jsonrpc = jsonrpc_version;
    response_message_id = id;
    response_message_result = None;
    response_message_error = Some error;
  }

module Channel = struct
  let log_to_file (oc : out_channel) (lbl : string) (txt : string) : unit =
    Printf.fprintf oc "[%s] %s\n" lbl txt;
    flush oc

  let send (oc : out_channel) (label : string) (out : string) : unit =
    let cl = "Content-Length: " ^ string_of_int (String.length out) in
    print_string cl;
    print_string "\r\n\r\n";
    print_string out;
    flush stdout;
    log_to_file oc (label ^ "_length") cl;
    log_to_file oc label out

  let send_response (oc : out_channel) : string -> unit = send oc "response"
  let send_notification (oc : out_channel) : string -> unit = send oc "notification"

  let show_message (oc : out_channel) (typ : Lsp.MessageType.t) (msg: string): unit =
    let params =
      `WindowShowMessage
        (Lsp_t.
          { window_show_message_params_type_ = typ;
            window_show_message_params_message = msg;
          }) in
    let notification = notification params in
    send_notification oc (Lsp_j.string_of_notification_message notification)

  let publish_diagnostics (oc : out_channel) (uri : Lsp_t.document_uri) (diags : Lsp_t.diagnostic list): unit =
    let params = `PublishDiagnostics (Lsp_t.
      { publish_diagnostics_params_uri = uri;
        publish_diagnostics_params_diagnostics = diags;
      }) in
    let notification = notification params in
    send_notification oc (Lsp_j.string_of_notification_message notification)
end

let position_of_pos (pos : Source.pos) : Lsp_t.position = Lsp_t.
  (* The LSP spec requires zero-based positions *)
  { position_line = if pos.Source.line > 0 then pos.Source.line - 1 else 0;
    position_character = pos.Source.column;
  }

let range_of_region (at : Source.region) : Lsp_t.range = Lsp_t.
  { range_start = position_of_pos at.Source.left;
    range_end_ = position_of_pos at.Source.right;
  }

let severity_of_sev : Diag.severity -> Lsp.DiagnosticSeverity.t = function
  | Diag.Error -> Lsp.DiagnosticSeverity.Error
  | Diag.Warning -> Lsp.DiagnosticSeverity.Warning

let diagnostics_of_message (msg : Diag.message) : Lsp_t.diagnostic = Lsp_t.
  { diagnostic_range = range_of_region msg.Diag.at;
    diagnostic_severity = Some (severity_of_sev msg.Diag.sev);
    diagnostic_code = None;
    diagnostic_source = Some "ActorScript";
    diagnostic_message = msg.Diag.text;
    diagnostic_relatedInformation = None;
  }

let file_uri_prefix = "file://" ^ Sys.getcwd () ^ "/"
let file_from_uri logger uri =
  match Lib.String.chop_prefix file_uri_prefix uri with
   | Some file -> file
   | None ->
      let _ = logger "error" ("Failed to strip filename from: " ^ uri) in
      uri
let abs_file_from_uri logger uri =
  match Lib.String.chop_prefix "file://" uri with
   | Some file -> file
   | None ->
      let _ = logger "error" ("Failed to strip filename from: " ^ uri) in
      uri

let start () =
  let oc: out_channel = open_out_gen [Open_append; Open_creat] 0o666 "ls.log"; in

  let log_to_file = Channel.log_to_file oc in
  let publish_diagnostics = Channel.publish_diagnostics oc in
  let send_response = Channel.send_response oc in
  let show_message = Channel.show_message oc in
  let shutdown = ref false in
  let client_capabilities = ref None in
  let project_root = Sys.getcwd () in

  let vfs = ref Vfs.empty in
  let decl_index =
    let ix = match Declaration_index.make_index () with
      | Error(err) -> Declaration_index.Index.empty
      | Ok((ix, _)) -> ix in
    ref ix in
  let rec loop () =
    let clength = read_line () in
    log_to_file "content-length" clength;
    let cl = "Content-Length: " in
    let cll = String.length cl in
    let num =
      (int_of_string
        (String.trim
           (String.sub
              clength
              cll
              (String.length(clength) - cll - 1)))) + 2 in
    let buffer = Buffer.create num in
    Buffer.add_channel buffer stdin num;
    let raw = String.trim (Buffer.contents buffer) in
    log_to_file "raw" raw;

    let message = Lsp_j.incoming_message_of_string raw in
    let message_id = message.Lsp_t.incoming_message_id in

    (match (message_id, message.Lsp_t.incoming_message_params) with

    (* Request messages *)

    | (Some id, `Initialize params) ->
       client_capabilities := Some params.Lsp_t.initialize_params_capabilities;
       let completion_options =
         Lsp_t.{
             completion_options_resolveProvider = Some false;
             completion_options_triggerCharacters = Some ["."] } in
       let text_document_sync_options =
         Lsp_t.{
             text_document_sync_options_openClose = Some true;
             (* Full *)
             (* text_document_sync_options_change = Some 1; *)
             (* Incremental *)
             text_document_sync_options_change = Some 2;
             text_document_sync_options_willSave = Some false;
             text_document_sync_options_willSaveWaitUntil = Some false;
             text_document_sync_options_save = Some {
                 save_options_includeText = Some true
               }
         } in
        let result = `Initialize (Lsp_t.{
          initialize_result_capabilities = {
            server_capabilities_textDocumentSync = text_document_sync_options;
            server_capabilities_hoverProvider = Some true;
            server_capabilities_completionProvider = Some completion_options;
            server_capabilities_definitionProvider = Some true;
          }
        }) in
        let response = response_result_message id result in
        send_response (Lsp_j.string_of_response_message response);

    | (Some id, `TextDocumentHover params) ->
       let uri =
         params
           .Lsp_t.text_document_position_params_textDocument
           .Lsp_t.text_document_identifier_uri in
       let position =
         params.Lsp_t.text_document_position_params_position in
       let response = match Vfs.read_file uri !vfs with
         | None ->
            response_error_message
              id
              Lsp_t.{ code = 1
                    ; message = "Tried to find hover a file that hadn't been opened yet"}
         | Some file_content ->
            let result =
              Hover.hover_handler
                !decl_index
                position
                file_content
                project_root
                (abs_file_from_uri log_to_file uri) in
            response_result_message id result in
       send_response (Lsp_j.string_of_response_message response);
    | (Some id, `TextDocumentDefinition params) ->
       let uri =
         params
           .Lsp_t.text_document_position_params_textDocument
           .Lsp_t.text_document_identifier_uri in
       let position =
         params.Lsp_t.text_document_position_params_position in
       let response = match Vfs.read_file uri !vfs with
         | None ->
            response_error_message
              id
              Lsp_t.{ code = 1
                    ; message = "Tried to find a definition in a file that hadn't been opened yet"}
         | Some file_content ->
            let result =
              Definition.definition_handler
                !decl_index
                position
                file_content
                project_root
                (abs_file_from_uri log_to_file uri) in
            response_result_message id result in
       send_response (Lsp_j.string_of_response_message response);
    | (_, `TextDocumentDidOpen params) ->
       vfs := Vfs.open_file params !vfs
    | (_, `TextDocumentDidChange params) ->
       vfs := Vfs.update_file params !vfs
    | (_, `TextDocumentDidClose params) ->
       vfs := Vfs.close_file params !vfs
    | (_, `TextDocumentDidSave params) ->
       let textDocumentIdent = params.Lsp_t.text_document_did_save_params_textDocument in
       let uri = textDocumentIdent.Lsp_t.text_document_identifier_uri in
       let file_name = file_from_uri log_to_file uri in
       let result = Pipeline.check_files [file_name] in
       let msgs = match result with
         | Error msgs' -> msgs'
         | Ok (_, msgs') -> msgs' in
       (match Declaration_index.make_index () with
        | Error(err) -> ()
        | Ok((ix, _)) -> decl_index := ix);
       let diags = List.map diagnostics_of_message msgs in
       publish_diagnostics uri diags;

    (* Notification messages *)

    | (None, `Initialized _) ->
       show_message Lsp.MessageType.Info "Language server initialized"

    | (Some id, `Shutdown _) ->
       shutdown := true;
       response_result_message id (`ShutdownResponse None)
       |> Lsp_j.string_of_response_message
       |> send_response
    | (_, `Exit _) ->
       if !shutdown then exit 0 else exit 1
    | (Some id, `CompletionRequest params) ->
       let uri =
         params
           .Lsp_t.text_document_position_params_textDocument
           .Lsp_t.text_document_identifier_uri in
       let position =
         params.Lsp_t.text_document_position_params_position in
       let response = match Vfs.read_file uri !vfs with
         | None ->
            response_error_message
              id
              Lsp_t.{ code = 1
                    ; message = "Tried to find completions for a file that hadn't been opened yet"}
         | Some file_content ->
            Completion.completion_handler
              !decl_index
              log_to_file
              project_root
              (abs_file_from_uri log_to_file uri)
              file_content
              position
            |> response_result_message id in
       response
       |> Lsp_j.string_of_response_message
       |> send_response
    (* Unhandled messages *)
    | _ ->
      log_to_file "unhandled message" raw;
    );

    loop ()
  in loop ()
