(* https://github.com/mirage/ocaml-rpc/blob/4d542cb7b0c42a62196c6043ba556de54c1b7820/example/example2_server.ml *)
open Example2_idl

(* You can swap the rpc engine, by using a different monad here,
   note however that if you are using an asynchoronous one, like
   lwt or async, you should also use their specific IO functions
   including print functions. *)
module M = Idl.IdM (* You can easily but ExnM here and the code would stay unchanged. *)
module MyIdl = Idl.Make(M)
module Server = API(MyIdl.GenServer ())

(* Implementations of the methods *)
let query () =
  let open Datatypes.Query in
  Printf.printf "Received query API call\n%!";
  let result =
    { name = "Example2 server"
    ; vendor = "This is the example server showing how to use the ocaml-rpc IDL."
    ; version = "2.0.0"
    ; features =
        [ "defaults"
        ; "upgradability"
        ]
    ; instance_id = string_of_int (Random.int 1000)
    } in
    MyIdl.ErrM.return result

let diagnostics () =
  MyIdl.ErrM.return "This should be the diagnostics of the server."

let test i s1 s2 =
  Printf.printf "%Ld %s %s\n%!" i s1 s2;
  query ()

(* Utility and general non-specific server bits and bobs *)
let finally f g =
  try
    let result = f () in
      g ();
      result
  with e ->
    g ();
    raise e

let mkdir_rec dir perm =
  let rec p_mkdir dir =
    let p_name = Filename.dirname dir in
    if p_name <> "/" && p_name <> "."
    then p_mkdir p_name;
    (try Unix.mkdir dir perm with Unix.Unix_error(Unix.EEXIST, _, _) -> ()) in
  p_mkdir dir

let binary_handler process s =
  let ic = Unix.in_channel_of_descr s in
  let oc = Unix.out_channel_of_descr s in
  (* Read a 16 byte length encoded as a string *)
  let len_buf = Bytes.make 16 '\000' in
  really_input ic len_buf 0 (Bytes.length len_buf);
  let len = int_of_string (Bytes.unsafe_to_string len_buf) in
  let msg_buf = Bytes.make len '\000' in
  really_input ic msg_buf 0 (Bytes.length msg_buf);
  let (>>=) = M.bind in
  process msg_buf >>= fun result ->
  let len_buf = Printf.sprintf "%016d" (String.length result) in
  output_string oc len_buf;
  output_string oc result;
  flush oc;
  M.return ()

let serve_requests rpcfn path =
  (try Unix.unlink path with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
  mkdir_rec (Filename.dirname path) 0o0755;
  let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind sock (Unix.ADDR_UNIX path);
  Unix.listen sock 5;
  Printf.fprintf stdout "Listening on %s" path;
  while true do
    let this_connection, _ = Unix.accept sock in
    let (_: Thread.t) = Thread.create
      (fun () ->
        finally
          (* Here I am calling M.run to make sure that I am running the process,
            this is not much of a problem with IdM or ExnM, but in general you
            should ensure that the computation is started by a runner. *)
          (fun () -> binary_handler rpcfn this_connection |> M.run)
          (fun () -> Unix.close this_connection)
      ) () in
    ()
  done

let start_server () =
  Server.query query;
  Server.diagnostics diagnostics;
  Server.test test;

  let rpc_fn = MyIdl.server Server.implementation in

  let process x =
    let open M in
    rpc_fn (Jsonrpc.call_of_string (Bytes.unsafe_to_string x))
    >>= fun response -> Jsonrpc.string_of_response response |> return in

  serve_requests process sockpath
