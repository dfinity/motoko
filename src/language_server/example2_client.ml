open Example2_idl

module M = Idl.IdM
module MyIdl = Idl.Make(M)
module Client = API(MyIdl.GenClient ())
module Cmds   = API(Cmdlinergen.Gen ())

(* Use a binary 16-byte length to frame RPC messages *)
let binary_rpc path (call: Rpc.call) : Rpc.response =
  let sockaddr = Unix.ADDR_UNIX path in
  let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect s sockaddr;
  let ic = Unix.in_channel_of_descr s in
  let oc = Unix.out_channel_of_descr s in
  let msg_buf = Jsonrpc.string_of_call call in
  let len = Printf.sprintf "%016d" (String.length msg_buf) in
  output_string oc len;
  output_string oc msg_buf;
  flush oc;
  let len_buf = Bytes.make 16 '\000' in
  really_input ic len_buf 0 16;
  let len = int_of_string (Bytes.unsafe_to_string len_buf) in
  let msg_buf = Bytes.make len '\000' in
  really_input ic msg_buf 0 len;
  let (response: Rpc.response) = Jsonrpc.response_of_string (Bytes.unsafe_to_string msg_buf) in
  response

let default_cmd =
  let doc = "a cli for an API" in
  Cmdliner.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Cmdliner.Term.info "cli" ~version:"1.6.1" ~doc

let server_cmd =
  let doc = "Start the server" in
  Cmdliner.Term.(const Example2_server.start_server $ const ()),
  Cmdliner.Term.info "server" ~doc

let cli () =
  let rpc = binary_rpc Example2_idl.sockpath in
  Cmdliner.Term.eval_choice default_cmd (
    server_cmd
    :: List.map 
      (fun t -> let (term, info) = t rpc in (Cmdliner.Term.(term $ const ()), info)) 
      (Cmds.implementation ())
    )
