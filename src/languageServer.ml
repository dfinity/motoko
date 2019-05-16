(* https://microsoft.github.io/language-server-protocol/specification *)

let oc = open_out_gen [Open_append; Open_creat] 0o666 "ls.log"
let log_to_file txt =
    Printf.fprintf oc "%s\n" txt;
    flush oc

let respond out =
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

    let json = Yojson.Basic.from_string raw in
    let open Yojson.Basic.Util in
    let jsonrpc = json |> member "jsonrpc" |> to_string in
    let id = json |> member "id" |> to_int_option in
    let method_ = json |> member "method" |> to_string in
    (* let params = json |> member "params" in *)

    let string_of_int_option =
      function
      | None -> "None"
      | Some id -> string_of_int id in

    log_to_file (jsonrpc ^ ", " ^ string_of_int_option id ^ ", " ^ method_);

    if method_ = "initialize"
      then begin
        log_to_file "Handle initialize";
        let capabilities = `Assoc
          [ ("textDocumentSync", `Null)
          ] in
        let result = `Assoc
          [ ("capabilities", capabilities)
          ] in
        let response = `Assoc
          [ ("jsonrpc", `String "2.0")
          ; ("id", json |> member "id")
          ; ("result", result)
          ; ("error", `Null)
          ] in
        respond (Yojson.Basic.pretty_to_string response);
      end

    else if method_ = "initialized"
      then begin
        log_to_file "Handle initialized";
        let params = `Assoc
          [ ("type", `Int 3)
          ; ("message", `String "Language server initialized")
          ] in
        let notification = `Assoc
          [ ("jsonrpc", `String "2.0")
          ; ("method", `String "window/showMessage")
          ; ("params", params)
          ] in
        respond (Yojson.Basic.pretty_to_string notification);
      end

    else
      loop ();

    loop ()
  in loop ()
