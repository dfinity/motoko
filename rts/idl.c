#include "rts.h"
#include "buf.h"

uint32_t leb128_decode(buf *buf);
int32_t sleb128_decode(buf *buf);

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

bool is_primitive_type(int32_t ty);
void check_typearg(int32_t ty, uint32_t n_types);
void parse_fields(buf *buf, uint32_t n_types);

// can also be used for sleb
void skip_leb128(buf *buf);

// used for opt, bool, references...
static uint8_t read_byte_tag(buf *buf) {
  uint8_t b = read_byte(buf);
  if (b > 1) {
    idl_trap_with("skip_any: byte tag not 0 or 1");
  }
  return b;
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
        read_byte_tag(b);
        return;
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
          uint32_t len = leb128_decode(b);
	  const char *p = (const char *)b->p;
          advance(b, len); // advance first; does the bounds check
	  utf8_validate(p, len);
        }
        return;
      case IDL_PRIM_empty:
        idl_trap_with("skip_any: encountered empty");
      case IDL_REF_principal:
        {
          if (read_byte_tag(b)) {
            uint32_t len = leb128_decode(b);
            advance(b, len);
          }
          return;
        }
      default:
        idl_trap_with("skip_any: unknown prim");
    }
  } else {
    buf tb = { typtbl[t], b->e };
    int32_t tc = sleb128_decode(&tb);
    switch(tc) {
      case IDL_CON_opt: {
        int32_t it = sleb128_decode(&tb);
        if (read_byte_tag(b)) {
          skip_any(b, typtbl, it, 0);
        }
        return;
      }
      case IDL_CON_vec: {
        int32_t it = sleb128_decode(&tb);
        for (uint32_t n = leb128_decode(b); n > 0; n--) {
          skip_any(b, typtbl, it, 0);
        }
        return;
      }
      case IDL_CON_record: {
        for (uint32_t n = leb128_decode(&tb); n > 0; n--) {
          skip_leb128(&tb);
          int32_t it = sleb128_decode(&tb);
          // This is just a quick check; we should be keeping
          // track of all enclosing records to detect larger loops
          if (it == t) idl_trap_with("skip_any: recursive record");
          skip_any(b, typtbl, it, depth + 1);
        }
        return;
      }
      case IDL_CON_variant: {
        uint32_t n = leb128_decode(&tb);
        uint32_t i = leb128_decode(b);
        if (i >= n) idl_trap_with("skip_any: variant tag too large");
        for (; i > 0; i--) {
          skip_leb128(&tb);
          skip_leb128(&tb);
        }
        skip_leb128(&tb);
        int32_t it = sleb128_decode(&tb);
        skip_any(b, typtbl, it, 0);
        return;
      }

      case IDL_CON_func:
        idl_trap_with("skip_any: func");

      case IDL_CON_service:
        idl_trap_with("skip_any: service");

      case IDL_CON_alias: {
        // See Note [mutable stable values] in codegen/compile.ml
        int32_t it = sleb128_decode(&tb);
        uint32_t tag = read_byte_tag(b);
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
        uint32_t n_data = leb128_decode(b);
        uint32_t n_ref = leb128_decode(b);
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
    uint32_t this_tag = leb128_decode(tb);
    if (this_tag < tag) {
      int32_t it = sleb128_decode(tb);
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
    int32_t it = sleb128_decode(tb);
    skip_any(b, typtbl, it, 0);
    (*n)--;
  }
}
