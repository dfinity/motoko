module Message_adapter : Atdgen_runtime.Json_adapter.S

val response_result : int -> Lsp2_t.response_result -> Lsp2_t.response_message
(* val response_error : int -> Lsp2_t.response_error -> Lsp2_t.response_message *)
