module Message_adapter : Atdgen_runtime.Json_adapter.S
module Response_message_adapter : Atdgen_runtime.Json_adapter.S

val notification : Lsp2_t.notification_message_params -> Lsp2_t.notification_message

val response_result : int -> Lsp2_t.response_result -> Lsp2_t.response_message
(* val response_error : int -> Lsp2_t.response_error -> Lsp2_t.response_message *)
