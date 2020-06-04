#include "rts.h"
#include "buf.h"

/* Code to read (S)LEB128 to ints (traps if does not fit in return type) */

export uint32_t read_u32_of_leb128(buf *buf) {
  uint32_t r = 0;
  unsigned int s = 0;
  uint8_t b;
  do {
    b = read_byte(buf);
    if (s > 0 && b == 0x00) {
        // The high 7 bits is all zeros, this is not a shortest encoding
        idl_trap_with("not shortest encoding");
    }
    if (s == 28 && !((b & (uint8_t)0xF0) == 0x00)) {
        // the 5th byte needs to be the last, and it must contribute at most 4 bits
        // else we have an int overflow
        idl_trap_with("int overflow");
    }
    r += (b & (uint8_t)0x7f) << s;
    s += 7;
  } while (b & (uint8_t)0x80);
  return r;
}

export int32_t read_i32_of_sleb128(buf *buf) {
  uint32_t r = 0;
  unsigned int s = 0;
  uint8_t b;
  bool last_sign_bit_set = 0;
  do {
    b = read_byte(buf);
    if (s == 28 && !((b & (uint8_t)0xF0) == 0x00 || (b & (uint8_t)0xF0) == 0x70)) {
        // the 5th byte needs to be the last, and it must contribute at most 4 bits
        // else we have an int overflow
        idl_trap_with("int overflow");
    }
    if (s > 0 && ((!last_sign_bit_set && b == 0x00) || (last_sign_bit_set && b == 0x7F))) {
        // The high 8 bits are all zeros or ones, so this is not a shortest encoding
        idl_trap_with("not shortest encoding");
    }
    last_sign_bit_set = (b & (uint8_t)0x40);
    r += (b & (uint8_t)0x7f) << s;
    s += 7;
  } while (b & (uint8_t)0x80);
  // sign extend
  if (s < 32 && last_sign_bit_set) {
    r |= ((~(uint32_t)0) << s);
  }
  return r;
}

/*
 * IDL constants
 */
#define IDL_PRIM_null     (-1)
#define IDL_PRIM_bool     (-2)
#define IDL_PRIM_nat      (-3)
#define IDL_PRIM_int      (-4)
#define IDL_PRIM_nat8     (-5)
#define IDL_PRIM_nat16    (-6)
#define IDL_PRIM_nat32    (-7)
#define IDL_PRIM_nat64    (-8)
#define IDL_PRIM_int8     (-9)
#define IDL_PRIM_int16    (-10)
#define IDL_PRIM_int32    (-11)
#define IDL_PRIM_int64    (-12)
#define IDL_PRIM_float32  (-13)
#define IDL_PRIM_float64  (-14)
#define IDL_PRIM_text     (-15)
#define IDL_PRIM_reserved (-16)
#define IDL_PRIM_empty    (-17)

#define IDL_CON_opt       (-18)
#define IDL_CON_vec       (-19)
#define IDL_CON_record    (-20)
#define IDL_CON_variant   (-21)
#define IDL_CON_func      (-22)
#define IDL_CON_service   (-23)

#define IDL_REF_principal (-24)

#define IDL_CON_alias     (1)

static bool is_primitive_type(int32_t ty) {
  static const int32_t IDL_PRIM_lowest = -17;
  return ty < 0 && (ty >= IDL_PRIM_lowest || ty == IDL_REF_principal);
}

static void check_typearg(int32_t ty, uint32_t n_types) {
  // arguments to type constructors can be:
  if (is_primitive_type(ty)) return;  // primitive types
  if (ty >=0 && ty < n_types) return; // type indices.
  idl_trap_with("invalid type argument");
}

/*
 * This function parses the IDL magic header and type description. It
 *  * traps if the type description is not well-formed. In particular, it traps if
 *    any index into the type description table is out of bounds, so that
 *    subsequent code can trust these values
 *  * returns a pointer to the first byte after the IDL header (via return)
 *  * allocates a type description table, and returns it
 *    (via pointer argument, for lack of multi-value returns in C)
 *  * returns a pointer to the beginning of the list of main types
 *    (again via pointer argument, for lack of multi-value returns in C)
 */
export void parse_idl_header(bool extended, buf *buf, uint8_t ***typtbl_out, uint8_t **main_types_out) {
  if (buf->p == buf->e) idl_trap_with("empty input");

  // Magic bytes (DIDL)
  if (read_word(buf) != 0x4C444944) idl_trap_with("missing magic bytes");

  // Create a table for the type description
  int32_t n_types = read_u32_of_leb128(buf);

  // read_u32_of_leb128 returns an uint32_t, we want an int32_t here so that the
  // comparisons below work, so let's make sure we did not wrap around in the
  // conversion.
  if (n_types < 0) { idl_trap_with("overflow in number of types"); }

  // Early sanity check
  if (&buf->p[n_types] >= buf->e) { idl_trap_with("too many types"); }

  // Go through the table
  uint8_t **typtbl = (uint8_t **)alloc(n_types * sizeof(uint8_t*));
  for (int i = 0; i < n_types; i++) {
    typtbl[i] = buf->p;
    int32_t ty = read_i32_of_sleb128(buf);
    if (extended && ty == IDL_CON_alias) { // internal
      // See Note [mutable stable values] in codegen/compile.ml
      int32_t t = read_i32_of_sleb128(buf);
      check_typearg(t, n_types);
    } else if (ty >= 0) {
      idl_trap_with("illegal type table"); // illegal
    } else if (is_primitive_type(ty)) {
      idl_trap_with("primitive type in type table"); // illegal
    } else if (ty == IDL_CON_opt) {
      int32_t t = read_i32_of_sleb128(buf);
      check_typearg(t, n_types);
    } else if (ty == IDL_CON_vec) {
      int32_t t = read_i32_of_sleb128(buf);
      check_typearg(t, n_types);
    } else if (ty == IDL_CON_record) {
      for (uint32_t n = read_u32_of_leb128(buf); n > 0; n--) {
        read_u32_of_leb128(buf);
        int32_t t = read_i32_of_sleb128(buf);
        check_typearg(t, n_types);
      }
    } else if (ty == IDL_CON_variant) {
      for (uint32_t n = read_u32_of_leb128(buf); n > 0; n--) {
        read_u32_of_leb128(buf);
        int32_t t = read_i32_of_sleb128(buf);
        check_typearg(t, n_types);
      }
    } else if (ty == IDL_CON_func) {
      // arg types
      for (uint32_t n = read_u32_of_leb128(buf); n > 0; n--) {
        int32_t t = read_i32_of_sleb128(buf);
        check_typearg(t, n_types);
      }
      // ret types
      for (uint32_t n = read_u32_of_leb128(buf); n > 0; n--) {
        int32_t t = read_i32_of_sleb128(buf);
        check_typearg(t, n_types);
      }
      // annotations
      for (uint32_t n = read_u32_of_leb128(buf); n > 0; n--) {
        (buf->p)++;
      }
    } else if (ty == IDL_CON_service) {
      for (uint32_t n = read_u32_of_leb128(buf); n > 0; n--) {
        // name
        uint32_t size = read_u32_of_leb128(buf);
        (buf->p) += size;
        // type
        int32_t t = read_i32_of_sleb128(buf);
        check_typearg(t, n_types);
      }
    } else { // future type
      uint32_t n = read_u32_of_leb128(buf);
      advance(buf, n);
    }
  }
  // Now read the main types
  *main_types_out = buf->p;
  for (uint32_t n = read_u32_of_leb128(buf); n > 0; n--) {
    int32_t t = read_i32_of_sleb128(buf);
    check_typearg(t, n_types);
  }

  *typtbl_out = typtbl;
}

// can also be used for sleb
export void skip_leb128(buf *buf) {
  uint8_t b;
  do {
    b = read_byte(buf);
  } while (b & (uint8_t)0x80);
}

// Assumes that buf is the encoding of type t, and fast-forwards past that
// Assumes that all type references in the typtbl are already checked
//
// This is currently implemented recursively, but we could
// do this in a loop (by maintaing a stack of the t arguments)
export void skip_any(buf *b, uint8_t **typtbl, int32_t t, int32_t depth) {
  if (depth > 100) {
        idl_trap_with("skip_any: too deeply nested record");
  }
  if (t < 0) { // primitive type
    switch(t) {
      case IDL_PRIM_null:
      case IDL_PRIM_reserved:
        return;
      case IDL_PRIM_bool:
      case IDL_PRIM_nat8:
      case IDL_PRIM_int8:
        advance(b, 1);
        return;
      case IDL_PRIM_nat:
      case IDL_PRIM_int:
        skip_leb128(b);
        return;
      case IDL_PRIM_nat16:
      case IDL_PRIM_int16:
        advance(b, 2);
        return;
      case IDL_PRIM_nat32:
      case IDL_PRIM_int32:
      case IDL_PRIM_float32:
        advance(b, 4);
        return;
      case IDL_PRIM_nat64:
      case IDL_PRIM_int64:
      case IDL_PRIM_float64:
        advance(b, 8);
        return;
      case IDL_PRIM_text:
        {
          uint32_t len = read_u32_of_leb128(b);
          advance(b, len);
        }
        return;
      case IDL_PRIM_empty:
        idl_trap_with("skip_any: encountered empty");
      case IDL_REF_principal:
        {
          if (read_byte(b)) {
            uint32_t len = read_u32_of_leb128(b);
            advance(b, len);
          }
          return;
        }
      default:
        idl_trap_with("skip_any: unknown prim");
    }
  } else {
    buf tb = { typtbl[t], b->e };
    int32_t tc = read_i32_of_sleb128(&tb);
    switch(tc) {
      case IDL_CON_opt: {
        int32_t it = read_i32_of_sleb128(&tb);
        if (read_byte(b)) {
          skip_any(b, typtbl, it, 0);
        }
        return;
      }
      case IDL_CON_vec: {
        int32_t it = read_i32_of_sleb128(&tb);
        for (uint32_t n = read_u32_of_leb128(b); n > 0; n--) {
          skip_any(b, typtbl, it, 0);
        }
        return;
      }
      case IDL_CON_record: {
        for (uint32_t n = read_u32_of_leb128(&tb); n > 0; n--) {
          skip_leb128(&tb);
          int32_t it = read_i32_of_sleb128(&tb);
          // This is just a quick check; we should be keeping
          // track of all enclosing records to detect larger loops
          if (it == t) idl_trap_with("skip_any: recursive record");
          skip_any(b, typtbl, it, depth + 1);
        }
        return;
      }
      case IDL_CON_variant: {
        uint32_t n = read_u32_of_leb128(&tb);
        uint32_t i = read_u32_of_leb128(b);
        if (i >= n) idl_trap_with("skip_any: variant tag too large");
        for (; i > 0; i--) {
          skip_leb128(&tb);
          skip_leb128(&tb);
        }
        skip_leb128(&tb);
        int32_t it = read_i32_of_sleb128(&tb);
        skip_any(b, typtbl, it, 0);
        return;
      }

      case IDL_CON_func:
        idl_trap_with("skip_any: func");

      case IDL_CON_service:
        idl_trap_with("skip_any: service");

      case IDL_CON_alias: {
        // See Note [mutable stable values] in codegen/compile.ml
        int32_t it = read_i32_of_sleb128(&tb);
        uint32_t tag = read_byte(b);
        if (tag == 0) {
          advance(b, 8);
          // this is the contents (not a reference)
          skip_any(b, typtbl, it, 0);
        } else {
          advance(b, 4);
        }
        return;
      }

      default: { // future type
        uint32_t n_data = read_u32_of_leb128(b);
        uint32_t n_ref = read_u32_of_leb128(b);
        advance(b, n_data);
        if (n_ref > 0) {
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
export uint32_t find_field(buf *tb, buf *b, uint8_t **typtbl, uint32_t tag, uint32_t *n) {
  while (*n > 0) {
    uint8_t *last_p = tb->p;
    uint32_t this_tag = read_u32_of_leb128(tb);
    if (this_tag < tag) {
      int32_t it = read_i32_of_sleb128(tb);
      skip_any(b, typtbl, it, 0);
      (*n)--;
    } else if (tag == this_tag) {
      (*n)--;
      return 1;
    } else {
      // rewind reading tag
      tb->p = last_p;
      return 0;
    }
  }
  return 0;
}

export void skip_fields(buf *tb, buf *b, uint8_t **typtbl, uint8_t *n) {
  while (*n > 0) {
    skip_leb128(tb);
    int32_t it = read_i32_of_sleb128(tb);
    skip_any(b, typtbl, it, 0);
    (*n)--;
  }
}
