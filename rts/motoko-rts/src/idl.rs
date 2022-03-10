#![allow(non_upper_case_globals)]
use crate::bitrel::BitRel;
use crate::buf::{read_byte, read_word, skip_leb128, Buf};
use crate::idl_trap_with;
use crate::leb128::{leb128_decode, sleb128_decode};
use crate::memory::{alloc_blob, Memory};
use crate::types::Words;
use crate::utf8::utf8_validate;

use core::cmp::min;

use motoko_rts_macros::ic_mem_fn;

//
// IDL constants
//

const IDL_PRIM_null: i32 = -1;
const IDL_PRIM_bool: i32 = -2;
const IDL_PRIM_nat: i32 = -3;
const IDL_PRIM_int: i32 = -4;
const IDL_PRIM_nat8: i32 = -5;
const IDL_PRIM_nat16: i32 = -6;
const IDL_PRIM_nat32: i32 = -7;
const IDL_PRIM_nat64: i32 = -8;
const IDL_PRIM_int8: i32 = -9;
const IDL_PRIM_int16: i32 = -10;
const IDL_PRIM_int32: i32 = -11;
const IDL_PRIM_int64: i32 = -12;
const IDL_PRIM_float32: i32 = -13;
const IDL_PRIM_float64: i32 = -14;
const IDL_PRIM_text: i32 = -15;
const IDL_PRIM_reserved: i32 = -16;
const IDL_PRIM_empty: i32 = -17;

const IDL_CON_opt: i32 = -18;
const IDL_CON_vec: i32 = -19;
const IDL_CON_record: i32 = -20;
const IDL_CON_variant: i32 = -21;
const IDL_CON_func: i32 = -22;
const IDL_CON_service: i32 = -23;

const IDL_REF_principal: i32 = -24;

const IDL_CON_alias: i32 = 1;

const IDL_PRIM_lowest: i32 = -17;

unsafe fn is_primitive_type(ty: i32) -> bool {
    ty < 0 && (ty >= IDL_PRIM_lowest || ty == IDL_REF_principal)
}

// TBR; based on Text.text_compare
unsafe fn utf8_cmp(len1: u32, p1: *mut u8, len2: u32, p2: *mut u8) -> i32 {
    let len = min(len1, len2);
    let cmp = libc::memcmp(
        p1 as *mut libc::c_void,
        p2 as *mut libc::c_void,
        len as usize,
    );
    if cmp == -1 {
        return -1;
    } else if cmp == 1 {
        return 1;
    } else if len1 > len {
        return 1;
    } else if len2 > len {
        return -1;
    } else {
        return 0;
    }
}

unsafe fn check_typearg(ty: i32, n_types: u32) {
    // Arguments to type constructors can be primitive types or type indices
    if !(is_primitive_type(ty) || (ty >= 0 && (ty as u32) < n_types)) {
        idl_trap_with("invalid type argument");
    }
}

unsafe fn parse_fields(buf: *mut Buf, n_types: u32) {
    let mut next_valid = 0;
    for n in (1..=leb128_decode(buf)).rev() {
        let tag = leb128_decode(buf);
        if (tag < next_valid) || (tag == 0xFFFFFFFF && n > 1) {
            idl_trap_with("variant or record tag out of order");
        }
        next_valid = tag + 1;
        let t = sleb128_decode(buf);
        check_typearg(t, n_types);
    }
}

// NB. This function assumes the allocation does not need to survive GC
unsafe fn alloc<M: Memory>(mem: &mut M, size: Words<u32>) -> *mut u8 {
    alloc_blob(mem, size.to_bytes()).as_blob().payload_addr()
}

/// This function parses the IDL magic header and type description. It
///
/// * traps if the type description is not well-formed. In particular, it traps if any index into
///   the type description table is out of bounds, so that subsequent code can trust these values
///
/// * returns a pointer to the first byte after the IDL header
///
/// * allocates a type description table, and returns it
///   (via pointer argument, for lack of multi-value returns in C ABI)
///
/// * returns the size of that type description table
///   (again via pointer argument, for lack of multi-value returns in C ABI)
///
/// * returns a pointer to the beginning of the list of main types
///   (again via pointer argument, for lack of multi-value returns in C ABI)
#[ic_mem_fn]
unsafe fn parse_idl_header<M: Memory>(
    mem: &mut M,
    extended: bool,
    buf: *mut Buf,
    typtbl_out: *mut *mut *mut u8,
    typtbl_size_out: *mut u32,
    main_types_out: *mut *mut u8,
) {
    if (*buf).ptr == (*buf).end {
        idl_trap_with(
            "empty input. Expected Candid-encoded argument, but received a zero-length argument",
        );
    }

    // Magic bytes (DIDL)
    if read_word(buf) != 0x4C444944 {
        idl_trap_with("missing magic bytes");
    }

    // Create a table for the type description
    let n_types = leb128_decode(buf);

    // Early sanity check
    if (*buf).ptr.add(n_types as usize) >= (*buf).end {
        idl_trap_with("too many types");
    }

    // Let the caller know about the table size
    *typtbl_size_out = n_types;

    // Allocate the type table to be passed out
    let typtbl: *mut *mut u8 = alloc(mem, Words(n_types)) as *mut _;

    // Go through the table
    for i in 0..n_types {
        *typtbl.add(i as usize) = (*buf).ptr;

        let ty = sleb128_decode(buf);

        if extended && ty == IDL_CON_alias {
            // internal
            // See Note [mutable stable values] in codegen/compile.ml
            let t = sleb128_decode(buf);
            check_typearg(t, n_types);
        } else if ty >= 0 {
            idl_trap_with("illegal type table"); // illegal
        } else if is_primitive_type(ty) {
            // illegal
            idl_trap_with("primitive type in type table");
        } else if ty == IDL_CON_opt {
            let t = sleb128_decode(buf);
            check_typearg(t, n_types);
        } else if ty == IDL_CON_vec {
            let t = sleb128_decode(buf);
            check_typearg(t, n_types);
        } else if ty == IDL_CON_record {
            parse_fields(buf, n_types);
        } else if ty == IDL_CON_variant {
            parse_fields(buf, n_types);
        } else if ty == IDL_CON_func {
            // Arg types
            for _ in 0..leb128_decode(buf) {
                let t = sleb128_decode(buf);
                check_typearg(t, n_types);
            }
            // Ret types
            for _ in 0..leb128_decode(buf) {
                let t = sleb128_decode(buf);
                check_typearg(t, n_types);
            }
            // Annotations
            for _ in 0..leb128_decode(buf) {
                let a = read_byte(buf);
                if !(1 <= a && a <= 2) {
                    idl_trap_with("func annotation not within 1..2");
                }
                // TODO: shouldn't we also check
                // * 1 (query) or 2 (oneway), but not both
                // * 2 -> |Ret types| == 0
                // c.f. https://github.com/dfinity/candid/issues/318
                // NB: if this code changes, change sub type check below accordingly
            }
        } else if ty == IDL_CON_service {
            let mut last_len: u32 = 0 as u32;
            let mut last_p = core::ptr::null_mut();
            for _ in 0..leb128_decode(buf) {
                // Name
                let len = leb128_decode(buf);
                let p = (*buf).ptr;
                buf.advance(len);
                // Method names must be valid unicode
                utf8_validate(p as *const _, len);
                // Method names must be in order
                if last_p != core::ptr::null_mut() {
                    let cmp = libc::memcmp(
                        last_p as *mut libc::c_void,
                        p as *mut libc::c_void,
                        min(last_len, len) as usize,
                    );
                    if cmp > 0 || (cmp == 0 && last_len >= len) {
                        idl_trap_with("service method names out of order");
                    }
                }
                last_len = len;
                last_p = p;

                // Type
                let t = sleb128_decode(buf);
                check_typearg(t, n_types);
            }
        } else {
            // Future type
            let n = leb128_decode(buf);
            buf.advance(n);
        }
    }

    // Now that we have the indices, we can go through it again
    // and validate that all service method types are really function types
    // (We could not do that in the first run because of possible forward
    // references
    for i in 0..n_types {
        // do not modify the main buf
        let mut tmp_buf = Buf {
            end: (*buf).end,
            ptr: *typtbl.add(i as usize),
        };

        let ty = sleb128_decode(&mut tmp_buf);
        if ty == IDL_CON_service {
            for _ in 0..leb128_decode(&mut tmp_buf) {
                // Name
                let len = leb128_decode(&mut tmp_buf);
                Buf::advance(&mut tmp_buf, len);
                // Type
                let t = sleb128_decode(&mut tmp_buf);
                if !(t >= 0 && (t as u32) < n_types) {
                    idl_trap_with("service method arg not a constructor type");
                }
                let mut tmp_buf2 = Buf {
                    end: (*buf).end,
                    ptr: *typtbl.add(t as usize),
                };
                let mty = sleb128_decode(&mut tmp_buf2);
                if mty != IDL_CON_func {
                    idl_trap_with("service method arg not a function type");
                }
            }
        }
    }

    // Now read the main types
    *main_types_out = (*buf).ptr;
    for _ in 0..leb128_decode(buf) {
        let t = sleb128_decode(buf);
        check_typearg(t, n_types);
    }

    *typtbl_out = typtbl;
}

// used for opt, bool, references...
unsafe fn read_byte_tag(buf: *mut Buf) -> u8 {
    let b = read_byte(buf);
    if b > 1 {
        idl_trap_with("skip_any: byte tag not 0 or 1");
    }
    b
}

unsafe fn skip_blob(buf: *mut Buf) {
    let len = leb128_decode(buf);
    buf.advance(len);
}

unsafe fn skip_text(buf: *mut Buf) {
    let len = leb128_decode(buf);
    let p = (*buf).ptr;
    buf.advance(len); // advance first; does the bounds check
    utf8_validate(p as *const _, len);
}

unsafe fn skip_any_vec(buf: *mut Buf, typtbl: *mut *mut u8, t: i32, count: u32) {
    if count == 0 {
        return;
    }
    let ptr_before = (*buf).ptr;
    skip_any(buf, typtbl, t, 0);
    let ptr_after = (*buf).ptr;
    if ptr_after == ptr_before {
        // this looks like a vec null bomb, or equivalent, where skip_any
        // makes no progress. No point in calling it over and over again.
        // (This is easier to detect this way than by analyzing the type table,
        // where we’d have to chase single-field-records.)
        return;
    }
    for _ in 1..count {
        skip_any(buf, typtbl, t, 0);
    }
}

// Assumes buf is the encoding of type t, and fast-forwards past that
// Assumes all type references in the typtbl are already checked
//
// This is currently implemented recursively, but we could
// do this in a loop (by maintaining a stack of the t arguments)
#[no_mangle]
unsafe extern "C" fn skip_any(buf: *mut Buf, typtbl: *mut *mut u8, t: i32, depth: i32) {
    if depth > 100 {
        idl_trap_with("skip_any: too deeply nested record");
    }

    if t < 0 {
        // Primitive type
        match t {
            IDL_PRIM_null | IDL_PRIM_reserved => {}
            IDL_PRIM_bool => {
                read_byte_tag(buf);
            }
            IDL_PRIM_nat | IDL_PRIM_int => {
                skip_leb128(buf);
            }
            IDL_PRIM_nat8 | IDL_PRIM_int8 => {
                buf.advance(1);
            }
            IDL_PRIM_nat16 | IDL_PRIM_int16 => {
                buf.advance(2);
            }
            IDL_PRIM_nat32 | IDL_PRIM_int32 | IDL_PRIM_float32 => {
                buf.advance(4);
            }
            IDL_PRIM_nat64 | IDL_PRIM_int64 | IDL_PRIM_float64 => {
                buf.advance(8);
            }
            IDL_PRIM_text => skip_text(buf),
            IDL_PRIM_empty => {
                idl_trap_with("skip_any: encountered empty");
            }
            IDL_REF_principal => {
                if read_byte_tag(buf) != 0 {
                    skip_blob(buf);
                }
            }
            _ => {
                idl_trap_with("skip_any: unknown prim");
            }
        }
    } else {
        // t >= 0
        let mut tb = Buf {
            ptr: *typtbl.add(t as usize),
            end: (*buf).end,
        };
        let tc = sleb128_decode(&mut tb);
        match tc {
            IDL_CON_opt => {
                let it = sleb128_decode(&mut tb);
                if read_byte_tag(buf) != 0 {
                    skip_any(buf, typtbl, it, 0);
                }
            }
            IDL_CON_vec => {
                let it = sleb128_decode(&mut tb);
                let count = leb128_decode(buf);
                skip_any_vec(buf, typtbl, it, count);
            }
            IDL_CON_record => {
                for _ in 0..leb128_decode(&mut tb) {
                    skip_leb128(&mut tb);
                    let it = sleb128_decode(&mut tb);
                    // This is just a quick check; we should be keeping
                    // track of all enclosing records to detect larger loops
                    if it == t {
                        idl_trap_with("skip_any: recursive record");
                    }
                    skip_any(buf, typtbl, it, depth + 1);
                }
            }
            IDL_CON_variant => {
                let n = leb128_decode(&mut tb);
                let i = leb128_decode(buf);
                if i >= n {
                    idl_trap_with("skip_any: variant tag too large");
                }
                for _ in 0..i {
                    skip_leb128(&mut tb);
                    skip_leb128(&mut tb);
                }
                skip_leb128(&mut tb);
                let it = sleb128_decode(&mut tb);
                skip_any(buf, typtbl, it, 0);
            }
            IDL_CON_func => {
                if read_byte_tag(buf) == 0 {
                    idl_trap_with("skip_any: skipping references");
                } else {
                    if read_byte_tag(buf) == 0 {
                        idl_trap_with("skip_any: skipping references");
                    } else {
                        skip_blob(buf)
                    }
                    skip_text(buf)
                }
            }
            IDL_CON_service => {
                if read_byte_tag(buf) == 0 {
                    idl_trap_with("skip_any: skipping references");
                } else {
                    skip_blob(buf)
                }
            }
            IDL_CON_alias => {
                // See Note [mutable stable values] in codegen/compile.ml
                let it = sleb128_decode(&mut tb);
                let tag = read_byte_tag(buf);
                if tag == 0 {
                    buf.advance(8);
                    // this is the contents (not a reference)
                    skip_any(buf, typtbl, it, 0);
                } else {
                    buf.advance(4);
                }
            }
            _ => {
                // Future type
                let n_data = leb128_decode(buf);
                let n_ref = leb128_decode(buf);
                buf.advance(n_data);
                if n_ref > 0 {
                    idl_trap_with("skip_any: skipping references");
                }
            }
        }
    }
}

/*
This finds a field in a record.

Preconditions:
  tb:     points into the type table,
          into the sequence of tags/types that are the argument of IDL_CON_record,
          at the tag
  b:      points into the data buffer, at value corresponding to the field
          pointed to by tb
  typtbl: the type table
  tag:    the desired tag
  n:      the number of fields left in the data

If the tag exists:
  return value: 1
  tb:    points at the type corresponding to the found field
  b:     points at the value corresponding to the found field
  n:     the number of fields left after the found field

If the tag does not exist:
  return value: 0
  tb:    points at the tag of the first field with a higher tag
         or at the end of the buffer
  b:     points at the value corresponding to that field
         or at the value past the record
  n:     the number of fields left, including the field pointed to by tb
*/
#[no_mangle]
unsafe extern "C" fn find_field(
    tb: *mut Buf,
    buf: *mut Buf,
    typtbl: *mut *mut u8,
    tag: u32,
    n: *mut u8,
) -> u32 {
    while *n > 0 {
        let last_p = (*tb).ptr;
        let this_tag = leb128_decode(tb);
        if this_tag < tag {
            let it = sleb128_decode(tb);
            skip_any(buf, typtbl, it, 0);
            *n -= 1;
        } else if tag == this_tag {
            *n -= 1;
            return 1;
        } else {
            // Rewind reading tag
            (*tb).ptr = last_p;
            return 0;
        }
    }

    0
}

#[no_mangle]
unsafe extern "C" fn skip_fields(tb: *mut Buf, buf: *mut Buf, typtbl: *mut *mut u8, n: *mut u8) {
    while *n > 0 {
        skip_leb128(tb);
        let it = sleb128_decode(tb);
        skip_any(buf, typtbl, it, 0);
        *n -= 1;
    }
}

// TODO: delete me
unsafe fn unfold(buf: *mut Buf, typtbl: *mut *mut u8, t: i32) -> i32 {
    let mut tb = Buf {
        ptr: *typtbl.add(t as usize),
        end: (*buf).end,
    };
    return sleb128_decode(&mut tb);
}

unsafe fn opt_empty_sub(end: *mut u8, typtbl: *mut *mut u8, t: i32) -> bool {
    if is_primitive_type(t) {
        return t == IDL_PRIM_reserved;
    }

    // unfold t
    let mut t = t;

    let mut tb = Buf {
        ptr: *typtbl.add(t as usize),
        end: end,
    };

    t = sleb128_decode(&mut tb);

    return t == IDL_CON_opt;
}

// TODO: delete me
unsafe fn null_sub(buf: *mut Buf, typtbl: *mut *mut u8, t: i32) -> bool {
    if is_primitive_type(t) {
        return t == IDL_PRIM_empty || t == IDL_PRIM_null;
    }

    // unfold t
    let mut t = t;

    let mut tb = Buf {
        ptr: *typtbl.add(t as usize),
        end: (*buf).end,
    };

    t = sleb128_decode(&mut tb);

    return t == IDL_CON_opt;
}

// https://github.com/dfinity/candid/blob/master/rust/candid/src/types/subtype.rs#L10
// https://github.com/dfinity/candid/blob/20b84d1c1515e2c1db353ebe02b738486f835466/spec/Candid.md
unsafe fn sub(
    rel: &BitRel,
    p: bool,
    end1: *mut u8,
    end2: *mut u8,
    typtbl1: *mut *mut u8,
    typtbl2: *mut *mut u8,
    t1: i32,
    t2: i32,
    depth: i32,
) -> bool {
    if depth > 1023 {
        idl_trap_with("sub: subtyping too deep");
    }

    if t1 >= 0 && t2 >= 0 {
        let t1 = t1 as u32;
        let t2 = t2 as u32;
        if rel.get(p, t1, t2) {
            // cached: succeed!
            return true;
        };
        // cache and continue
        rel.set(p, t1, t2);
    };

    // re-declare as mut for any unfolding
    let mut t1 = t1;
    let mut t2 = t2;

    /* primitives reflexive */
    if is_primitive_type(t1) && is_primitive_type(t2) && t1 == t2 {
        return true;
    }

    // unfold t1, if necessary
    let mut tb1 = Buf {
        ptr: *typtbl1.add(if t1 < 0 { 0 } else { t1 as usize }), // better dummy?
        end: end1,
    };

    if t1 >= 0 {
        t1 = sleb128_decode(&mut tb1);
    };

    // unfold t2, if necessary
    let mut tb2 = Buf {
        ptr: *typtbl2.add(if t2 < 0 { 0 } else { t2 as usize }), // better dummy?
        end: end2,
    };

    if t2 >= 0 {
        t2 = sleb128_decode(&mut tb2);
    };

    debug_assert!(t1 < 0 && t2 < 0);

    match (t1, t2) {
        (_, IDL_PRIM_reserved) => true,
        (IDL_PRIM_empty, _) => true,
        /*
                (IDL_PRIM_null, IDL_CON_opt) => true,
                (IDL_CON_opt, IDL_CON_opt) => {
                    let t11 = sleb128_decode(&mut tb1);
                    let t21 = sleb128_decode(&mut tb2);
                    return sub(buf1, buf2, typtbl1, typtbl2, t11, t21, depth + 1);
                },
                (_, IDL_CON_opt) => {
                    let t21 = sleb128_decode(&mut tb2);
                    return
                        !null_sub(buf1, typetbl1, t1) &&
                        !sub(buf1, buf2, typtbl1, typtbl2, t11, t21, depth + 1);
                },
                (_, IDL_CON_opt) => {
                    let t21 = sleb128_decode(&mut tb2);
                    return
                        !null_sub(buf1, typetbl1, t1) &&
                        !sub(buf1, buf2, typtbl1, typtbl2, t11, t21, depth + 1);
                },
        */
        (_, IDL_CON_opt) => true, // apparently, this is admissable
        (IDL_CON_vec, IDL_CON_vec) => {
            let t11 = sleb128_decode(&mut tb1);
            let t21 = sleb128_decode(&mut tb2);
            return sub(rel, p, end1, end2, typtbl1, typtbl2, t11, t21, depth + 1);
        }
        (IDL_CON_func, IDL_CON_func) => {
            // contra in domain
            let in1 = leb128_decode(&mut tb1);
            let in2 = leb128_decode(&mut tb2);
            if in1 > in2 {
                return false;
            }
            for _ in 0..in1 {
                let t11 = sleb128_decode(&mut tb1);
                let t21 = sleb128_decode(&mut tb2);
                // NB: invert p and args!
                if !sub(rel, !p, end2, end1, typtbl2, typtbl1, t21, t11, depth + 1) {
                    return false;
                }
            }
            for _ in in1..in2 {
                let _ = sleb128_decode(&mut tb2);
            }
            // co in range
            let out1 = leb128_decode(&mut tb1);
            let out2 = leb128_decode(&mut tb2);
            if out2 > out1 {
                return false;
            }
            for _ in 0..out2 {
                let t11 = sleb128_decode(&mut tb1);
                let t21 = sleb128_decode(&mut tb2);
                if !sub(rel, p, end1, end2, typtbl1, typtbl2, t11, t21, depth + 1) {
                    return false;
                }
            }
            for _ in out2..out1 {
                let _ = sleb128_decode(&mut tb1);
            }
            // check annotations (that we care about)
            // TODO: more generally, we would check equality of 256-bit bit-vectors,
            // but validity ensures each entry is 1 or 2 (for now)
            // c.f. https://github.com/dfinity/candid/issues/318
            let mut a11 = false;
            let mut a12 = false;
            for _ in 0..leb128_decode(&mut tb1) {
                let a = read_byte(&mut tb1);
                if a == 1 {
                    a11 = true;
                };
                if a == 2 {
                    a12 = true;
                };
            }
            let mut a21 = false;
            let mut a22 = false;
            for _ in 0..leb128_decode(&mut tb2) {
                let a = read_byte(&mut tb2);
                if a == 1 {
                    a21 = true;
                };
                if a == 2 {
                    a22 = true;
                };
            }
            return (a11 == a21) && (a12 == a22);
        }
        (IDL_CON_record, IDL_CON_record) => {
            let mut n1 = leb128_decode(&mut tb1);
            let n2 = leb128_decode(&mut tb2);
            let mut tag1 = 0;
            let mut t11 = 0;
            let mut advance = true;
            for _ in 0..n2 {
                let tag2 = leb128_decode(&mut tb2);
                let t21 = sleb128_decode(&mut tb2);
                if n1 == 0 {
                    // check all remaining fields optional
                    if !opt_empty_sub(end2, typtbl2, t21) {
                        return false;
                    }
                    continue;
                };
                if advance {
                    loop {
                        tag1 = leb128_decode(&mut tb1);
                        t11 = sleb128_decode(&mut tb1);
                        n1 -= 1;
                        if !(tag1 < tag2 && n1 > 0) {
                            break;
                        }
                    }
                };
                if tag1 > tag2 {
                    if !opt_empty_sub(end2, typtbl2, t21) {
                        // missing, non_opt field
                        return false;
                    }
                    advance = false; // reconsider this field in next round
                    continue;
                };
                if !sub(rel, p, end1, end2, typtbl1, typtbl2, t11, t21, depth + 1) {
                    return false;
                }
                advance = true;
            }
            return true;
        }
        (IDL_CON_variant, IDL_CON_variant) => {
            let n1 = leb128_decode(&mut tb1);
            let mut n2 = leb128_decode(&mut tb2);
            for _ in 0..n1 {
                if n2 == 0 {
                    return false;
                };
                let tag1 = leb128_decode(&mut tb1);
                let t11 = sleb128_decode(&mut tb1);
                let mut tag2 = 0;
                let mut t21 = 0;
                loop {
                    tag2 = leb128_decode(&mut tb2);
                    t21 = sleb128_decode(&mut tb2);
                    n2 -= 1;
                    if !(tag2 < tag1 && n2 > 0) {
                        break;
                    }
                }
                if tag1 != tag2 {
                    return false;
                };
                if !sub(rel, p, end1, end2, typtbl1, typtbl2, t11, t21, depth + 1) {
                    return false;
                }
            }
            return true;
        }
        (IDL_CON_service, IDL_CON_service) => {
            let mut n1 = leb128_decode(&mut tb1);
            let n2 = leb128_decode(&mut tb2);
            for _ in 0..n2 {
                if n1 == 0 {
                    return false;
                };
                let len2 = leb128_decode(&mut tb2);
                let p2 = tb2.ptr;
                Buf::advance(&mut tb2, len2);
                let t21 = sleb128_decode(&mut tb2);
                let mut len1 = 0;
                let mut p1 = core::ptr::null_mut();
                let mut t11 = 0;
                let mut cmp: i32 = 0;
                loop {
                    len1 = leb128_decode(&mut tb1);
                    p1 = tb1.ptr;
                    Buf::advance(&mut tb1, len1);
                    t11 = sleb128_decode(&mut tb1);
                    n1 -= 1;
                    cmp = utf8_cmp(len1, p1, len2, p2);
                    if cmp < 0 && n1 > 0 {
                        continue;
                    };
                    break;
                }
                if !(cmp == 0) {
                    return false;
                };
                if !sub(rel, p, end1, end2, typtbl1, typtbl2, t11, t21, depth + 1) {
                    return false;
                }
            }
            return true;
        }
        // default
        (_, _) => false,
    }
}

// TODO: DELETE
#[no_mangle]
unsafe extern "C" fn table_size(buf: *mut Buf) -> u32 {
    let mut buf = Buf {
        ptr: (*buf).ptr,
        end: (*buf).end,
    };

    if buf.ptr == buf.end {
        idl_trap_with(
            "empty input. Expected Candid-encoded argument, but received a zero-length argument",
        );
    }

    // Magic bytes (DIDL)
    if read_word(&mut buf) != 0x4C444944 {
        idl_trap_with("missing magic bytes");
    }

    return leb128_decode(&mut buf);
}

#[no_mangle]
unsafe extern "C" fn idl_sub_buf_words(n_types1: u32, n_types2: u32) -> u32 {
    return ((2 * n_types1 * n_types2) + 31) / 32;
}

#[no_mangle]
unsafe extern "C" fn idl_sub(
    rel_buf: *mut u8, // a buffer with at least 2 * n * m bits
    end1: *mut u8,
    end2: *mut u8,
    n_types1: u32,
    n_types2: u32,
    typtbl1: *mut *mut u8, // of len n_types1
    typtbl2: *mut *mut u8, // of len n_types2
    t1: i32,
    t2: i32,
) -> bool {
    let rel = BitRel {
        ptr: rel_buf,
        end: rel_buf.add((idl_sub_buf_words(n_types1, n_types2) * 4) as usize),
        n: n_types1,
        m: n_types2,
    };

    debug_assert!(t1 < (n_types1 as i32) && t2 < (n_types2 as i32));

    rel.init();

    return sub(&rel, true, end1, end2, typtbl1, typtbl2, t1, t2, 0);
}
