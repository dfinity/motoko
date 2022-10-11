//! Write barrier, used for experimental GC

const MAX_UPDATES: usize = 1024;

pub static mut UPDATED_FIELDS: [usize; MAX_UPDATES] = [0usize; MAX_UPDATES];

pub static mut N_UPDATED_FIELDS: usize = 0;

// loc: updated location (object field)
//
// Called before writing the value, so `*loc` gives the old (current) value.
#[no_mangle]
pub unsafe extern "C" fn write_barrier(loc: usize) {
    //println!(100, "Write barrier {:#x}", loc);

    // Make sure we unskewed the object when calculating the field
    assert_eq!(loc & 0b1, 0);

    assert!(N_UPDATED_FIELDS < MAX_UPDATES);

    UPDATED_FIELDS[N_UPDATED_FIELDS] = loc;
    N_UPDATED_FIELDS += 1;
}
