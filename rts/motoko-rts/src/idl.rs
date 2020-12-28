#![allow(non_upper_case_globals)]

use crate::alloc::alloc_blob;
use crate::buf::{read_word, Buf};
use crate::leb128::{leb128_decode, sleb128_decode};
use crate::trap_with_prefix;
use crate::types::Words;

//
// IDL constants
//

// const IDL_PRIM_null: i32 = -1;
// const IDL_PRIM_bool: i32 = -2;
// const IDL_PRIM_nat: i32 = -3;
// const IDL_PRIM_int: i32 = -4;
// const IDL_PRIM_nat8: i32 = -5;
// const IDL_PRIM_nat16: i32 = -6;
// const IDL_PRIM_nat32: i32 = -7;
// const IDL_PRIM_nat64: i32 = -8;
// const IDL_PRIM_int8: i32 = -9;
// const IDL_PRIM_int16: i32 = -10;
// const IDL_PRIM_int32: i32 = -11;
// const IDL_PRIM_int64: i32 = -12;
// const IDL_PRIM_float32: i32 = -13;
// const IDL_PRIM_float64: i32 = -14;
// const IDL_PRIM_text: i32 = -15;
// const IDL_PRIM_reserved: i32 = -16;
// const IDL_PRIM_empty: i32 = -17;

const IDL_CON_opt: i32 = -18;
const IDL_CON_vec: i32 = -19;
const IDL_CON_record: i32 = -20;
const IDL_CON_variant: i32 = -21;
const IDL_CON_func: i32 = -22;
const IDL_CON_service: i32 = -23;

const IDL_REF_principal: i32 = -24;

const IDL_CON_alias: i32 = 1;

const IDL_PRIM_lowest: i32 = -17;

unsafe fn idl_trap_with(msg: &str) -> ! {
    trap_with_prefix("IDL error: ", msg);
}

#[no_mangle]
unsafe extern "C" fn is_primitive_type(ty: i32) -> bool {
    ty < 0 && (ty >= IDL_PRIM_lowest || ty == IDL_REF_principal)
}

#[no_mangle]
unsafe extern "C" fn check_typearg(ty: i32, n_types: u32) {
    // Arguments to type constructors can be primitive types or type indices
    if !(is_primitive_type(ty) || (ty >= 0 && (ty as u32) < n_types)) {
        idl_trap_with("invalid type argument");
    }
}

#[no_mangle]
unsafe extern "C" fn parse_fields(buf: *mut Buf, n_types: u32) {
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

// TODO (osa): This will cause problems at some point
#[no_mangle]
unsafe extern "C" fn alloc(size: Words<u32>) -> *mut u8 {
    alloc_blob(size.to_bytes()).as_blob().payload_addr()
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

// TODO (osa): typtbl_out handling here is very hacky, if we do GC before we're done with
// typtbl_out things will break

#[no_mangle]
unsafe extern "C" fn parse_idl_header(
    extended: bool,
    buf: *mut Buf,
    typtbl_out: *mut *mut *mut u8,
    typtbl_size_out: *mut u32,
    main_types_out: *mut *mut u8,
) {
    if (*buf).ptr == (*buf).end {
        idl_trap_with("empty input");
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

    // Go through the table
    let typtbl: *mut *mut u8 = alloc(Words(n_types)) as *mut _;

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
                buf.advance(1)
            }
        } else if ty == IDL_CON_service {
            for _ in 0..leb128_decode(buf) {
                // Name
                let size = leb128_decode(buf);
                buf.advance(size);
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

    // Now read the main types
    *main_types_out = (*buf).ptr;
    for _ in 0..leb128_decode(buf) {
        let t = sleb128_decode(buf);
        check_typearg(t, n_types);
    }

    *typtbl_out = typtbl;
}
