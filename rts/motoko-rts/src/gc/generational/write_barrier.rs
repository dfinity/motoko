//! Write barrier, used for generational GC

use super::remembered_log::RememberedLog;
use crate::memory::Memory;
use crate::types::Value;
use motoko_rts_macros::ic_mem_fn;

pub static mut REMEMBERED_LOG: Option<RememberedLog> = None;
pub static mut HEAP_BASE: u32 = 0;
pub static mut LAST_HP: u32 = 0;

const LOG_LIMIT: usize = 8 * 1024 * 1024;

/// (Re-)initialize the write barrier for generational GC.
#[ic_mem_fn(ic_only)]
pub unsafe fn init_write_barrier<M: Memory>(mem: &mut M) {
    use crate::memory::ic;
    REMEMBERED_LOG = Some(RememberedLog::new(mem));
    HEAP_BASE = ic::get_aligned_heap_base();
    LAST_HP = ic::LAST_HP;
}

/// Write barrier to be called AFTER the pointer store, used for generational GC.
/// `location`: location of modified pointer (address of object field or array element).
///
/// As the barrier is called after the write, `*location` refers to the NEW value.
/// No effect is the write barrier is deactivated.
#[ic_mem_fn]
pub unsafe fn write_barrier<M: Memory>(mem: &mut M, location: u32) {
    debug_assert_eq!(location & 0b1, 0); // must be unskewed address
    match &mut REMEMBERED_LOG {
        None => return,
        Some(remembered_log) => {
            // Only record locations inside old generation, static roots are anyway marked by GC.
            if location >= HEAP_BASE && location < LAST_HP {
                let value = *(location as *mut Value);
                if value.is_ptr() && value.get_raw() >= LAST_HP {
                    // trap pointers that lead from old generation (or static roots) to young generation
                    remembered_log.insert(mem, Value::from_raw(location));
                    if remembered_log.size() > LOG_LIMIT {
                        REMEMBERED_LOG = None;
                    }
                }
            }
        }
    }
}
