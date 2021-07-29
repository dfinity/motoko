#![allow(dead_code)]

use crate::print::*;
use crate::types::*;

use core::fmt::Write;

/// Print an object. The argument can be a skewed pointer to a boxed object, or a tagged scalar.
#[cfg(feature = "ic")]
#[no_mangle]
unsafe extern "C" fn print_closure(p: usize) {
    let mut buf = [0u8; 1000];
    let mut write_buf = WriteBuf::new(&mut buf);

    if SkewedPtr(p).is_tagged_scalar() {
        print_tagged_scalar(&mut write_buf, p);
    } else {
        print_boxed_object(&mut write_buf, SkewedPtr(p).unskew());
    }

    print(&write_buf);
}

pub unsafe fn dump_heap(
    heap_base: u32,
    hp: u32,
    static_roots: SkewedPtr,
    continuation_table_loc: *mut SkewedPtr,
) {
    print_continuation_table(continuation_table_loc);
    print_static_roots(static_roots);
    print_heap(heap_base, hp);
}

pub(crate) unsafe fn print_continuation_table(closure_tbl_loc: *mut SkewedPtr) {
    if (*closure_tbl_loc).0 == 0 {
        println!(100, "Closure table not initialized");
        return;
    }

    let arr = (*closure_tbl_loc).unskew() as *mut Array;
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
        if !elem.is_tagged_scalar() {
            let _ = write!(&mut write_buf, "{}: ", i);
            print_boxed_object(&mut write_buf, elem.unskew());
            print(&write_buf);
            write_buf.reset();
        }
    }
    println!(50, "End of closure table");
}

pub(crate) unsafe fn print_static_roots(static_roots: SkewedPtr) {
    let static_roots = static_roots.unskew() as *mut Array;
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
        print_boxed_object(&mut write_buf, (*field_addr).unskew());
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

unsafe fn print_tagged_scalar(buf: &mut WriteBuf, p: usize) {
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
                (*object).size,
                (*object).hash_ptr
            );
            for i in 0..object.size() {
                let val = object.get(i).0;
                let _ = write!(buf, "{:#x}", val);

                if !SkewedPtr(val).is_tagged_scalar() {
                    let indirectee_ptr = SkewedPtr(val).unskew();
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
            let _ = write!(buf, "<ObjInd field={:#x}>", (*obj_ind).field.0);
        }
        TAG_ARRAY => {
            let array = obj as *mut Array;
            let _ = write!(buf, "<Array len={:#x}", (*array).len);

            for i in 0..::core::cmp::min(10, (*array).len) {
                let _ = write!(buf, " {:#x}", array.get(i).0);
            }

            if (*array).len > 10 {
                let _ = write!(buf, " …>");
            } else {
                let _ = write!(buf, ">");
            }
        }
        TAG_BITS64 => {
            let bits64 = obj as *const Bits64;
            let _ = write!(buf, "<Bits64 {:#x}>", (*bits64).bits);
        }
        TAG_MUTBOX => {
            let mutbox = obj as *const MutBox;
            let _ = write!(buf, "<MutBox field={:#x}>", (*mutbox).field.0);
        }
        TAG_CLOSURE => {
            let closure = obj as *const Closure;
            let _ = write!(buf, "<Closure size={:#x}>", (*closure).size);
        }
        TAG_SOME => {
            let some = obj as *const Some;
            let _ = write!(buf, "<Some field={:#x}>", (*some).field.0);
        }
        TAG_VARIANT => {
            let variant = obj as *const Variant;
            let _ = write!(
                buf,
                "<Variant tag={:#x} field={:#x}>",
                (*variant).tag,
                (*variant).field.0
            );
        }
        TAG_BLOB => {
            let blob = obj as *const Blob;
            let _ = write!(buf, "<Blob len={:#x}>", (*blob).len.0);
        }
        TAG_FWD_PTR => {
            let ind = obj as *const FwdPtr;
            let _ = write!(buf, "<Forwarding to {:#x}>", (*ind).fwd.0);
        }
        TAG_BITS32 => {
            let bits32 = obj as *const Bits32;
            let _ = write!(buf, "<Bits32 {:#x}>", (*bits32).bits);
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
                (*concat).text1.0,
                (*concat).text2.0
            );
        }
        other => {
            let _ = write!(buf, "<??? {} ???>", other);
        }
    }
}
