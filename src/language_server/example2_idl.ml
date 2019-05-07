(* https://github.com/mirage/ocaml-rpc/blob/4d542cb7b0c42a62196c6043ba556de54c1b7820/example/example2_idl.ml *)
(* Example2 *)

(* In this one we're going to use the PPX to generate the structure and
   variant values rather than doing it by hand as in example1. We'll also
   actually create a client and server process. *)

open Rpc
open Idl

let sockpath = "/tmp/rpcsock"

module Datatypes = struct
  module Query = struct
    type t =
      { name: string
      ; vendor: string
      ; version: string
      ; features: string list
      ; instance_id: string
      } [@@deriving rpcty] (* TODO: rpc? *)
  end

  type errty =
    | InternalError of string
    | FrobnicationFailed
    | OperationInProgress
  [@@deriving rpcty] (* TODO: rpc? *)

  exception DatatypeError of errty

  let err = Error.
    { def = errty
    ; raiser = (function | e -> raise (DatatypeError e))
    ; matcher = function | DatatypeError e -> Some e | _ -> None
    }
end

module API(R : RPC) = struct
  open R

  let description = Interface.
    { name = "API"
    ; namespace = None
    ; description = [ "This is another example of the ocaml-rpc IDL." ]
    ; version = (1, 0, 0)
    }

  let implementation = implement description

  let query_p = Param.mk
    ~name: "query"
    ~description: ["The result of the query operation"]
    Datatypes.Query.t

  let unit_p = Param.mk
    Types.unit

  let string_p = Param.mk
    Types.string

  let domid_p = Param.mk
    ~name: "domid"
    ~description: ["The domid on which to operate"]
    Types.int64

  let vm_p = Param.mk
    ~name: "vm"
    ~description: ["The uuid of the VM"]
    Types.string

  let vm_desc = Param.mk
    ~name: "description"
    ~description: ["The description of the VM"]
    Types.string

  let err = Datatypes.err

  let query = declare
    "query"
    ["Query the details of the server."]
    (unit_p @-> returning query_p err)

  let diagnostics = declare
    "get_diagnostics"
    ["Get diagnostics information from the server."]
    (unit_p @-> returning string_p err)

  let test = declare
    "test"
    ["A test of a bit more of the IDL stuff."]
    (domid_p @-> vm_p @-> vm_desc @-> returning query_p err)
end
