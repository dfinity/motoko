module Message_adapter : Atdgen_runtime.Json_adapter.S
module Response_message_adapter : Atdgen_runtime.Json_adapter.S

val notification : Lsp_t.notification_message_params -> Lsp_t.notification_message

val response_result_message : int -> Lsp_t.response_result -> Lsp_t.response_message
(* val response_error_message : int -> Lsp_t.response_error -> Lsp_t.response_message *)
