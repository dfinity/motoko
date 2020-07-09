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

unsafe fn print_closure(buf: &mut WriteBuf, p: usize) {
    let obj = p as *const Obj;
    let tag = (*obj).tag;

    match tag {
        TAG_OBJECT => {
            let _ = write!(buf, "<Object>");
        }
        TAG_OBJ_IND => {
            let _ = write!(buf, "<ObjInd>");
        }
        TAG_ARRAY => {
            let _ = write!(buf, "<Array>");
        }
        TAG_BITS64 => {
            let _ = write!(buf, "<Bits64>");
        }
        TAG_MUTBOX => {
            let _ = write!(buf, "<MutBox>");
        }
        TAG_CLOSURE => {
            let _ = write!(buf, "<Closure>");
        }
        TAG_SOME => {
            let _ = write!(buf, "<Some>");
        }
        TAG_VARIANT => {
            let _ = write!(buf, "<Variant>");
        }
        TAG_BLOB => {
            let _ = write!(buf, "<Blob>");
        }
        TAG_INDIRECTION => {
            let _ = write!(buf, "<Indirection>");
        }
        TAG_BITS32 => {
            let _ = write!(buf, "<Bits32>");
        }
        TAG_BIGINT => {
            let _ = write!(buf, "<BigInt>");
        }
        TAG_CONCAT => {
            let _ = write!(buf, "<Concat>");
        }
        TAG_STABLE_SEEN => {
            let _ = write!(buf, "<StableSeen>");
        }
        other => {
            let _ = write!(buf, "<??? {} ???>", other);
        }
    }
}
