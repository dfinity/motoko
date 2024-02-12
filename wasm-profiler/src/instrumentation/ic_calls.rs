use std::collections::HashMap;

use parity_wasm::elements::Module;

type FunctionId = u32;

// Source: https://github.com/dfinity/ic-wasm/blob/61692f44cf85b93d43311492283246bb443449d3/src/utils.rs#L26C1-L100C2
// With slight adjustments
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum InjectionKind {
    Static,
    Dynamic,
    Dynamic64,
}

pub(crate) struct FunctionCost(HashMap<FunctionId, (u64, InjectionKind)>);
impl FunctionCost {
    pub fn new(m: &Module) -> Self {
        let mut res = HashMap::new();
        for (function_index, import) in m.import_section().unwrap().entries().iter().enumerate() {
            if import.module() == "ic0" {
                // System API cost taken from https://github.com/dfinity/ic/blob/master/rs/embedders/src/wasmtime_embedder/system_api_complexity.rs
                let cost = ic_call_cost(import);
                res.insert(function_index as u32, cost);
            }
        }
        Self(res)
    }
    pub fn get_cost(&self, id: FunctionId) -> Option<(u64, InjectionKind)> {
        self.0.get(&id).copied()
    }
}

fn ic_call_cost(import: &parity_wasm::elements::ImportEntry) -> (u64, InjectionKind) {
    use InjectionKind::*;
    match import.field() {
        "accept_message" => (500, Static),
        "call_cycles_add" | "call_cycles_add128" => (500, Static),
        "call_data_append" => (500, Dynamic),
        "call_new" => (1500, Static),
        "call_on_cleanup" => (500, Static),
        "call_perform" => (5000, Static),
        "canister_cycle_balance" | "canister_cycle_balance128" => (500, Static),
        "canister_self_copy" => (500, Dynamic),
        "canister_self_size" => (500, Static),
        "canister_status" | "canister_version" => (500, Static),
        "certified_data_set" => (500, Dynamic),
        "data_certificate_copy" => (500, Dynamic),
        "data_certificate_present" | "data_certificate_size" => (500, Static),
        "debug_print" => (100, Dynamic),
        "global_timer_set" => (500, Static),
        "is_controller" => (1000, Dynamic),
        "msg_arg_data_copy" => (500, Dynamic),
        "msg_arg_data_size" => (500, Static),
        "msg_caller_copy" => (500, Dynamic),
        "msg_caller_size" => (500, Static),
        "msg_cycles_accept" | "msg_cycles_accept128" => (500, Static),
        "msg_cycles_available" | "msg_cycles_available128" => (500, Static),
        "msg_cycles_refunded" | "msg_cycles_refunded128" => (500, Static),
        "cycles_burn128" => (100, Static),
        "msg_method_name_copy" => (500, Dynamic),
        "msg_method_name_size" => (500, Static),
        "msg_reject_code" | "msg_reject_msg_size" => (500, Static),
        "msg_reject_msg_copy" => (500, Dynamic),
        "msg_reject" => (500, Dynamic),
        "msg_reply_data_append" => (500, Dynamic),
        "msg_reply" => (500, Static),
        "performance_counter" => (200, Static),
        "stable_grow" | "stable64_grow" => (100, Static),
        "stable_size" | "stable64_size" => (20, Static),
        "stable_read" => (20, Dynamic),
        "stable_write" => (20, Dynamic),
        "stable64_read" => (20, Dynamic64),
        "stable64_write" => (20, Dynamic64),
        "trap" => (500, Dynamic),
        "time" => (500, Static),
        _ => (20, Static),
    }
}
