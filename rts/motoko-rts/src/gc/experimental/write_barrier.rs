//! Write barrier, used for experimental GC

const MAX_UPDATES: usize = 1024;

static mut ACTIVATED: bool = false;

pub static mut UPDATED_FIELDS: [usize; MAX_UPDATES] = [0usize; MAX_UPDATES];

pub static mut N_UPDATED_FIELDS: usize = 0;

/// Activate the write barrier for experimental GC.
#[no_mangle]
pub unsafe extern "C" fn activate_write_barrier() {
    ACTIVATED = true;
}

/// Write barrier, used for experimental GC. Called before the actual write.
/// `loc`: updated location (object field, array element).
///
/// As the barrier is called before the write, `*loc` still refers to the old value.
/// No effect is the write barrier is deactivated.
#[no_mangle]
pub unsafe extern "C" fn write_barrier(loc: usize) {
    if !ACTIVATED {
        return;
    }
    //println!(100, "Write barrier {:#x}", loc);

    // Make sure we unskewed the object when calculating the field
    assert_eq!(loc & 0b1, 0);

    assert!(N_UPDATED_FIELDS < MAX_UPDATES);

    UPDATED_FIELDS[N_UPDATED_FIELDS] = loc;
    N_UPDATED_FIELDS += 1;
}
