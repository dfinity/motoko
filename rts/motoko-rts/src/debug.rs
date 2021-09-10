#![allow(dead_code)]

use crate::print::*;
use crate::types::*;

use core::fmt::Write;
use core::ptr::addr_of;

/// Print an object. The argument can be a skewed pointer to a boxed object, or a tagged scalar.
#[cfg(feature = "ic")]
#[no_mangle]
unsafe extern "C" fn print_value(value: Value) {
    let mut buf = [0u8; 1000];
    let mut write_buf = WriteBuf::new(&mut buf);

    match value.get() {
        PtrOrScalar::Scalar(scalar) => print_tagged_scalar(&mut write_buf, scalar),
        PtrOrScalar::Ptr(ptr) => print_boxed_object(&mut write_buf, ptr),
    }

    print(&write_buf);
}

pub unsafe fn dump_heap(
    heap_base: u32,
    hp: u32,
    static_roots: Value,
    continuation_table_loc: *mut Value,
) {
    print_continuation_table(continuation_table_loc);
    print_static_roots(static_roots);
    print_heap(heap_base, hp);
}

pub(crate) unsafe fn print_continuation_table(closure_tbl_loc: *mut Value) {
    if !crate::continuation_table::table_initialized() {
        println!(100, "Continuation table not initialized");
        return;
    }

    let arr = (*closure_tbl_loc).as_array() as *mut Array;
    let len = (*arr).len;

    if len == 0 {
        println!(50, "Closure table empty");
        return;
    }

    println!(50, "Closure table: {}", len);

    let mut buf = [0u8; 1000];
    let mut write_buf = WriteBuf::new(&mut buf);

    for i in 0..len {
        let elem = arr.get(i);
        if elem.is_ptr() {
            let _ = write!(&mut write_buf, "{}: ", i);
            print_boxed_object(&mut write_buf, elem.get_ptr());
            print(&write_buf);
            write_buf.reset();
        }
    }
    println!(50, "End of closure table");
}

pub(crate) unsafe fn print_static_roots(static_roots: Value) {
    let static_roots = static_roots.as_array();
    println!(100, "static roots at {:#x}", static_roots as usize);

    let len = (*static_roots).len;

    if len == 0 {
        println!(50, "Static roots empty");
        return;
    }

    println!(50, "Static roots: {}", len);

    let mut buf = [0u8; 1000];
    let mut write_buf = WriteBuf::new(&mut buf);

    let payload_addr = static_roots.payload_addr();
    for i in 0..len {
        let field_addr = payload_addr.add(i as usize);
        let _ = write!(&mut write_buf, "{}: {:#x} --> ", i, field_addr as usize);
        print_boxed_object(&mut write_buf, (*field_addr).get_ptr());
        print(&write_buf);
        write_buf.reset();
    }

    println!(50, "End of static roots");
}

unsafe fn print_heap(heap_start: u32, heap_end: u32) {
    println!(
        200,
        "Heap start={:#x}, heap end={:#x}, size={} bytes",
        heap_start,
        heap_end,
        heap_end - heap_start
    );

    let mut buf = [0u8; 1000];
    let mut write_buf = WriteBuf::new(&mut buf);

    let mut p = heap_start;
    let mut i: Words<u32> = Words(0);
    while p < heap_end {
        print_boxed_object(&mut write_buf, p as usize);
        print(&write_buf);
        write_buf.reset();

        let obj_size = object_size(p as usize);
        p += obj_size.to_bytes().0;
        i += obj_size;
    }
}

unsafe fn print_tagged_scalar(buf: &mut WriteBuf, p: u32) {
    let _ = write!(buf, "<Scalar {:#x}>", p);
}

/// Print boxed object at address `p`.
pub(crate) unsafe fn print_boxed_object(buf: &mut WriteBuf, p: usize) {
    let _ = write!(buf, "{:#x}: ", p);

    let obj = p as *mut Obj;
    let tag = obj.tag();

    if tag == 0 {
        return;
    }

    match tag {
        TAG_OBJECT => {
            let object = obj as *mut Object;
            let _ = write!(
                buf,
                "<Object size={:#x} hash_ptr={:#x} field=[",
                { (*object).size },
                { (*object).hash_ptr },
            );
            for i in 0..object.size() {
                let val = object.get(i);
                let _ = write!(buf, "{:#x}", val.get_raw());

                if let PtrOrScalar::Ptr(indirectee_ptr) = val.get() {
                    let _ = write!(buf, " (indirectee=");
                    print_boxed_object(buf, indirectee_ptr);
                    let _ = write!(buf, ")");
                }

                if i != (*object).size - 1 {
                    let _ = write!(buf, ",");
                }
            }
            let _ = write!(buf, "]>");
        }
        TAG_OBJ_IND => {
            let obj_ind = obj as *const ObjInd;
            let _ = write!(
                buf,
                "<ObjInd field={:#x}>",
                (&*addr_of!((*obj_ind).field)).get_raw()
            );
        }
        TAG_ARRAY => {
            let array = obj as *mut Array;
            let _ = write!(buf, "<Array len={:#x}", { (*array).len });

            for i in 0..::core::cmp::min(10, (*array).len) {
                let _ = write!(buf, " {:#x}", array.get(i).get_raw());
            }

            if (*array).len > 10 {
                let _ = write!(buf, " …>");
            } else {
                let _ = write!(buf, ">");
            }
        }
        TAG_BITS64 => {
            let bits64 = obj as *const Bits64;
            let _ = write!(buf, "<Bits64 {:#x}>", { (*bits64).bits() });
        }
        TAG_MUTBOX => {
            let mutbox = obj as *const MutBox;
            let _ = write!(
                buf,
                "<MutBox field={:#x}>",
                (&*addr_of!((*mutbox).field)).get_raw()
            );
        }
        TAG_CLOSURE => {
            let closure = obj as *const Closure;
            let _ = write!(buf, "<Closure size={:#x}>", { (*closure).size });
        }
        TAG_SOME => {
            let some = obj as *const Some;
            let _ = write!(
                buf,
                "<Some field={:#x}>",
                (&*addr_of!((*some).field)).get_raw()
            );
        }
        TAG_VARIANT => {
            let variant = obj as *const Variant;
            let _ = write!(
                buf,
                "<Variant tag={:#x} field={:#x}>",
                { (*variant).tag },
                (&*addr_of!((*variant).field)).get_raw()
            );
        }
        TAG_BLOB => {
            let blob = obj as *const Blob;
            let _ = write!(buf, "<Blob len={:#x}>", (*blob).len.0);
        }
        TAG_FWD_PTR => {
            let ind = obj as *const FwdPtr;
            let _ = write!(
                buf,
                "<Forwarding to {:#x}>",
                (&*addr_of!((*ind).fwd)).get_raw()
            );
        }
        TAG_BITS32 => {
            let bits32 = obj as *const Bits32;
            let _ = write!(buf, "<Bits32 {:#x}>", { (*bits32).bits });
        }
        TAG_BIGINT => {
            // Add more details here as needed
            let _ = write!(buf, "<BigInt>");
        }
        TAG_CONCAT => {
            let concat = obj as *const Concat;
            let _ = write!(
                buf,
                "<Concat n_bytes={:#x} obj1={:#x} obj2={:#x}>",
                (*concat).n_bytes.0,
                (&*addr_of!((*concat).text1)).get_raw(),
                (&*addr_of!((*concat).text2)).get_raw(),
            );
        }
        TAG_ONE_WORD_FILLER => {
            let _ = write!(buf, "<One word filler>",);
        }
        TAG_FREE_SPACE => {
            let free_space = obj as *const FreeSpace;
            let _ = write!(buf, "<Free space {} words>", (*free_space).words.0);
        }
        other => {
            let _ = write!(buf, "<??? {} ???>", other);
        }
    }
}
