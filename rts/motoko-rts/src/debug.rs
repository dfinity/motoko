#![allow(dead_code)]

use crate::gc;
use crate::print::*;
use crate::types::*;

use core::fmt::Write;

/// Print an object. The argument can be a skewed pointer to a boxed object, or a tagged scalar.
#[no_mangle]
unsafe extern "C" fn print_closure(p: usize) {
    let mut buf = [0u8; 1000];
    let mut write_buf = WriteBuf::new(&mut buf);

    if gc::is_tagged_scalar(p) {
        print_tagged_scalar(&mut write_buf, p);
    } else {
        print_boxed_object(&mut write_buf, SkewedPtr(p).unskew());
    }

    print(&write_buf);
}

pub(crate) unsafe fn dump_heap() {
    print_closure_table();
    print_static_roots();
    print_heap();
}

pub(crate) unsafe fn print_closure_table() {
    let closure_tbl = gc::closure_table_loc().unskew() as *const SkewedPtr;

    if (*closure_tbl).0 == 0 {
        println!(100, "Closure table not initialized");
        return;
    }

    let arr = (*closure_tbl).unskew() as *const Array;
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
        if !gc::is_tagged_scalar(elem.0) {
            let _ = write!(&mut write_buf, "{}: {:#x} --> ", i, elem.unskew());
            print_boxed_object(&mut write_buf, elem.unskew());
            print(&write_buf);
            write_buf.reset();
        }
    }
    println!(50, "End of closure table");
}

pub(crate) unsafe fn print_static_roots() {
    let static_roots = gc::get_static_roots().unskew() as *const Array;
    let len = (*static_roots).len;

    if len == 0 {
        println!(50, "Static roots empty");
        return;
    }

    println!(50, "Static roots: {}", len);

    let mut buf = [0u8; 1000];
    let mut write_buf = WriteBuf::new(&mut buf);

    for i in 0..len {
        let obj = static_roots.get(i);
        let _ = write!(&mut write_buf, "{}: {:#x} --> ", i, obj.unskew());
        print_boxed_object(&mut write_buf, obj.unskew());
        print(&write_buf);
        write_buf.reset();
    }

    println!(50, "End of static roots");
}

unsafe fn print_heap() {
    let heap_begin = gc::get_heap_base();
    let heap_end = gc::HP;

    println!(
        200,
        "Heap begin={:#x}, heap end={:#x}, size={} bytes",
        heap_begin,
        heap_end,
        heap_end - heap_begin
    );

    let mut buf = [0u8; 1000];
    let mut write_buf = WriteBuf::new(&mut buf);

    let mut p = heap_begin;
    while p < heap_end {
        let _ = write!(&mut write_buf, "{:#x}: ", p);
        print_boxed_object(&mut write_buf, p as usize);
        print(&write_buf);
        write_buf.reset();

        p += gc::object_size(p as usize).to_bytes().0;
    }
}

unsafe fn print_tagged_scalar(buf: &mut WriteBuf, p: usize) {
    let _ = write!(buf, "<Scalar {:#x}>", p);
}

/// Print boxed object at address `p`.
unsafe fn print_boxed_object(buf: &mut WriteBuf, p: usize) {
    let _ = write!(buf, "{:#x}: ", p);

    let obj = p as *const Obj;
    let tag = (*obj).tag;

    match tag {
        TAG_OBJECT => {
            let object = obj as *const Object;
            let _ = write!(
                buf,
                "<Object size={:#x} hash_ptr={:#x} field=[",
                (*object).size,
                (*object).hash_ptr
            );
            let payload_addr = object.payload_addr();
            for i in 0..(*object).size {
                let _ = write!(buf, "{:#x}", (*payload_addr.offset(i as isize)).0);
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
            let array = obj as *const Array;
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
            let bigint = obj as *const BigInt;
            let _ = write!(
                buf,
                "<BigInt size={:#x} alloc={:#x} data_ptr={:#x}>",
                (*bigint).size,
                (*bigint).alloc,
                (*bigint).data_ptr
            );
        }
        TAG_CONCAT => {
            let concat = obj as *const Concat;
            let _ = write!(
                buf,
                "<Concat n_bytes={:#x} obj1={:#x} obj2{:#x}>",
                (*concat).n_bytes,
                (*concat).text1.0,
                (*concat).text2.0
            );
        }
        other => {
            let _ = write!(buf, "<??? {} ???>", other);
        }
    }
}
