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
    { processId: int option (* FIXME *)
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

module API(R : RPC) = struct
  open R

  (* FIXME: use ResponseError *)
  let error = Idl.DefaultError.err

  (* FIXME *)
  let initialize_params = Param.mk
    ~name:"initialize_params"
    InitializeParams.t

  (* FIXME *)
  let server_capabilities = Param.mk
    ~name:"server_capabilities"
    ServerCapabilities.t

  (* FIXME *)
  let initialize_result = Param.mk
    ~name:"initialize_result"
    InitializeResult.t

  let initialize = declare
    "initialize"
    [ "The initialize request is sent as the first request from the client to "
    ; "the server."
    ]
    (initialize_params @-> returning initialize_result error)
end

(* let _ = Example2_client.cli () *)

let () =
  print_string "Hello, World!\n";

  (* TODO: basic sanity check *)
  (* print_string (Rpc.string_of_call ...) *)
  (* print_string (Rpc.string_of_response ...) *)

