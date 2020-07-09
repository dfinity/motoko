#![allow(dead_code)]

use crate::gc;
use crate::print::*;
use crate::types::*;

use core::fmt::Write;

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

    let len = (*((*closure_tbl).unskew() as *const Array)).len;

    if len == 0 {
        println!(50, "Closure table empty");
        return;
    }

    let arr = (*closure_tbl).unskew() as *const Array;

    println!(50, "Closure table: {}", len);

    let mut buf = [0u8; 1000];
    let mut write_buf = WriteBuf::new(&mut buf);

    for i in 0..len {
        let elem = array_get(arr, i);
        if !gc::is_tagged_scalar(SkewedPtr(elem as usize)) {
            let _ = write!(&mut write_buf, "{}: {:#x} --> ", i, elem.wrapping_add(1));
            print_closure(&mut write_buf, SkewedPtr(elem as usize).unskew());
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
        let obj = SkewedPtr(array_get(static_roots, i) as usize);
        let _ = write!(&mut write_buf, "{}: {:#x} --> ", i, obj.unskew());
        print_closure(&mut write_buf, obj.unskew());
        print(&write_buf);
        write_buf.reset();
    }

    println!(50, "End of static roots");
}

unsafe fn print_heap() {
    let heap_begin = gc::get_heap_base();
    let heap_end = gc::get_hp();

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
        print_closure(&mut write_buf, p);
        print(&write_buf);
        write_buf.reset();

        p += words_to_bytes(gc::object_size(p)).0 as usize;
    }
}

/// Print boxed object at address `p`.
unsafe fn print_closure(buf: &mut WriteBuf, p: usize) {
    let obj = p as *const Obj;
    let tag = (*obj).tag;

    match tag {
        TAG_OBJECT => {
            let object = obj as *const Object;
            let _ = write!(
                buf,
                "<Object size={:#x} hash_ptr={:#x}>",
                (*object).size,
                (*object).hash_ptr
            );
        }
        TAG_OBJ_IND => {
            let obj_ind = obj as *const ObjInd;
            let _ = write!(buf, "<ObjInd field={:#x}>", (*obj_ind).field.0);
        }
        TAG_ARRAY => {
            let array = obj as *const Array;
            let _ = write!(buf, "<Array len={:#x}>", (*array).len);
        }
        TAG_BITS64 => {
            let _ = write!(buf, "<Bits64>");
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
        TAG_INDIRECTION => {
            let ind = obj as *const Indirection;
            let _ = write!(buf, "<Indirection to {:#x}>", (*ind).fwd.0);
        }
        TAG_BITS32 => {
            let _ = write!(buf, "<Bits32>");
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
        TAG_STABLE_SEEN => {
            let _ = write!(buf, "<StableSeen>");
        }
        other => {
            let _ = write!(buf, "<??? {} ???>", other);
        }
    }
}
