module Message_adapter = Atdgen_runtime.Json_adapter.Type_and_value_fields.Make(
  struct
    let type_field_name = "method"
    let value_field_name = "params"
    let known_tags = None
  end
)

module Response_message_adapter = Atdgen_runtime.Json_adapter.Type_and_value_fields.Make(
  struct
    let type_field_name = "response_for" (* Ideally we would omit this *)
    let value_field_name = "result"
    let known_tags = None
  end
)

let jsonrpc_version = "2.0"

let notification params = Lsp_t.
  { notification_message_jsonrpc = jsonrpc_version
  ; notification_message_params = params
  }

let response_result id result = Lsp_t.
  { response_message_jsonrpc = jsonrpc_version
  ; response_message_id = id
  ; response_message_result = Some result
  }

(* let response_error id error = *)
