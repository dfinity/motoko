(* https://microsoft.github.io/language-server-protocol/specification *)

open Rpc
open Idl

module ResponseError(*(D : RPC.t)*) = struct
  type t =
    { code: int
    ; message: string
    (* ; data: D option *) (* FIXME *)
    } [@@deriving rpcty]
end

module InitializeParams = struct
  type t =
    { processId: int (* FIXME *)
    } [@@deriving rpcty]
end

module ServerCapabilities = struct
  type t =
    { textDocumentSync: int option (* FIXME *)
    } [@@deriving rpcty]
end

module InitializeResult = struct
  type t =
    { capabilities: ServerCapabilities.t (* FIXME *)
    } [@@deriving rpcty]
end

type show_message_params =
    { type_: int [@key "type"] (* FIXME *)
    ; message: string
    } [@@deriving rpc]

module API(R : RPC) = struct
  open R

  let description = Interface.
    { name = "API"
    ; namespace = None
    ; description = [ "ActorScript LSP" ]
    ; version = (1, 0, 0)
    }

  let implementation = implement description

  (* FIXME: use ResponseError *)
  let error = Idl.DefaultError.err

  (* FIXME *)
  let initialize_params = Param.mk
    (* ~name:"initialize_params" *)
    InitializeParams.t

  (* FIXME *)
  let server_capabilities = Param.mk
    ~name:"server_capabilities"
    ServerCapabilities.t

  (* FIXME *)
  let initialize_result = Param.mk
    ~name:"initialize_result"
    InitializeResult.t

  (* let initialized_params = Param.mk *)

  (* let show_message_params = Param.mk
   *   ShowMessageParams.t *)

  let unit_p = Param.mk Types.unit

  let initialize = declare
    "initialize"
    [ "The initialize request is sent as the first request from the client to "
    ; "the server."
    ]
    (initialize_params @-> returning initialize_result error)

  (* let show_message = declare
   *   "window/showMessage"
   *   [ "The show message notification is sent from a "
   *   ; "server to a client to ask the client to display a particular message in the user interface."
   *   ]
   *   (show_message_params @-> returning unit_p error) *)
end

module M = Idl.IdM (* You can easily but ExnM here and the code would stay unchanged. *)
module MyIdl = Idl.Make(M)
module Server = API(MyIdl.GenServer ())
module Client = API(MyIdl.GenClient ())

let oc = open_out_gen [Open_append; Open_creat] 0o666 "ls.log"
let log_to_file txt =
    Printf.fprintf oc "%s\n" txt;
    flush oc

let respond out =
  let cl = "Content-Length: " ^ string_of_int (String.length out) ^ "\r\n\r\n" in
  print_string cl;
  print_string out;
  flush stdout

let my_start_server handler =
  let rec server_loop () =
    let clength = read_line () in
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
    (* Printf.fprintf oc "%s\n" (String.trim raw); *)
    let open M in
    let call = Jsonrpc.call_of_string raw in
    if call.name = "initialized"
    then
      let mes_res =
        (Jsonrpc.string_of_call ~version:V2
            (Rpc.call "window/showMessage"
              [ rpc_of_show_message_params { type_ = 2; message = "Handled command"}; ])); in
      log_to_file mes_res;
      respond mes_res;
      server_loop ()
    else
    handler call
      >>= fun response -> let res = Jsonrpc.string_of_response ~version:V2 response in
                          log_to_file res;
                          respond res;
    server_loop ()
  in server_loop ()

let initialize InitializeParams.{ processId } =
  log_to_file (string_of_int processId);
  MyIdl.ErrM.return (InitializeResult.{ capabilities = ServerCapabilities.{ textDocumentSync = None }})
  (* MyIdl.ErrM.return (ShowMessageParams.{ type_ = 2; message = "Hello from aslan"}) *)

let _ =
  Server.initialize initialize;
  let rpc_fn = MyIdl.server Server.implementation
  in my_start_server rpc_fn;
  (* my_start_server (); *)


(* let () =
 *   print_string "Hello, World!\n"; *)

  (* TODO: basic sanity check *)
  (* print_string (Rpc.string_of_call ...) *)
  (* print_string (Rpc.string_of_response ...) *)
