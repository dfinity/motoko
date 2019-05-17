let oc = open_out_gen [Open_append; Open_creat] 0o666 "ls.log"
let log_to_file txt =
    Printf.fprintf oc "%s\n" txt;
    flush oc

let send out =
  let cl = "Content-Length: " ^ string_of_int (String.length out) ^ "\r\n\r\n" in
  print_string cl;
  print_string out;
  flush stdout;
  log_to_file "Response:";
  log_to_file cl;
  log_to_file out

let start () =
  let rec loop () =
    let clength = read_line () in
    log_to_file "Request:";
    log_to_file clength;
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
    let raw = Buffer.contents buffer in
    log_to_file raw;

    let message = Lsp_j.incoming_message_of_string raw in
    let message_id = message.Lsp_t.incoming_message_id in

    (match (message_id, message.Lsp_t.incoming_message_params) with

    (* Request messages *)

    | (Some id, `Initialize params) ->
        let result = `Initialize (Lsp_t.
          { capabilities =
              { hoverProvider = Some false
              }
          }) in
        let response = Lsp.response_result_message id result in
        send (Lsp_j.string_of_response_message response);


    (* Notification messages *)

    | (None, `Initialized _) ->
        let params = `ShowMessage (Lsp_t.
          { type_ = 3
          ; message = "Language server initialized"
          }) in
        let notification = Lsp.notification params in
        send (Lsp_j.string_of_notification_message notification);


    (* Unhandled messages *)
    | _ ->
      log_to_file "Unhandled message:";
      log_to_file raw;
    );

    loop ()
  in loop ()
