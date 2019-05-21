let oc = open_out_gen [Open_append; Open_creat] 0o666 "ls.log"
let log_to_file lbl txt =
    Printf.fprintf oc "[%s] %s\n" lbl txt;
    flush oc

let send out =
  let cl = "Content-Length: " ^ string_of_int (String.length out) ^ "\r\n\r\n" in
  print_string cl;
  print_string out;
  flush stdout;
  log_to_file "response_length" cl;
  log_to_file "response" out

let start () =
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
        (match params.Lsp_t.trace with
        | Some trace -> (match trace with
                        | `Off -> log_to_file "trace" "Off"
                        | `Messages -> log_to_file "trace" "Messages"
                        | `Verbose -> log_to_file "trace" "Verbose"
                        );
        | None -> log_to_file "trace" "null"
        );
        let result = `Initialize (Lsp_t.
          { capabilities =
              { textDocumentSync = 1
              ; hoverProvider = Some true
              }
          }) in
        let response = Lsp.response_result_message id result in
        log_to_file "response" (Lsp_j.string_of_response_message response);
        send (Lsp_j.string_of_response_message response);

(*
TODO(Christoph): Can't figure out how this works yet
    | (Some id, `TextDocumentHover params) ->
       let result = `TextDocumentHoverResponse (Lsp_t. {
             contents = "Hovered over: "
           }) in
       let response = Lsp.response_result_message id result in
       send (Lsp_j.string_of_response_message response);
 *)
    (* Notification messages *)

    | (None, `Initialized _) ->
        let params = `WindowShowMessage (Lsp_t.
          { type_ = 3
          ; message = "Language server initialized"
          }) in
        let notification = Lsp.notification params in
        send (Lsp_j.string_of_notification_message notification);

    (* Unhandled messages *)

    | _ ->
      log_to_file "unhandled message" raw;
    );

    loop ()
  in loop ()
