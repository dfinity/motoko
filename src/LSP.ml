open Yojson.Basic.Util

let jsonrpc_version = "2.0"
let jsonrpc_field = ("jsonrpc", `String jsonrpc_version)

let from_int_option =
  function
  | None -> `Null
  | Some i -> `Int i

type received_message =
  { id : int option
  ; method_ : string
  ; params : Yojson.Basic.t
  }

(* Parse and LSP request message or notification *)
let parse json =
  (* if json |> member "jsonrpc" |> to_string = jsonrpc_version
   * then Some *)
    { id = json |> member "id" |> to_int_option (* notifications omit id *)
    ; method_ = json |> member "method" |> to_string
    ; params = json |> member "params"
    }
  (* else None *)

(* Construct an LSP response message *)
let response id result error = `Assoc
  [ jsonrpc_field
  ; ("id", from_int_option id)
  ; ("result", result)
  ; ("error", error)
  ]

(* Construct an LSP notification message *)
let notification method_ params = `Assoc
  [ jsonrpc_field
  ; ("method", `String method_)
  ; ("params", `Assoc params)
  ]
