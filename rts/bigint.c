#include "rts.h"
#include "buf.h"

/* Memory management for libtommath */

/*
A libtommath arbitrary precision integer is a struct (`mp_int`) that contains a
pointer to a data array.

 * The libtommath library never allocates the struct, so we are in full
   control. We can embed the struct simply in an Motoko heap object
   with a dedicated tag for it.

 * The data array is allocated with mp_calloc and mp_realloc. We provide these
   calls, allocate Motoko arrays (using the TAG_BLOB tag for byte arrays,
   not TAG_ARRAY for arrays of pointers) and store the pointer to the
   _payload_ in the `mp_digit* dp` field of the struct. This way, things look all nice
   and dandy from libtommath’s point of view.

   Our garbage collector has special knowledge about the dp field of the struct
   and understands that this pointer points inside the TAG_BLOB heap object. But
   we can still move them around in the GC without issues.

   The length of the byte array is always equal to the allocation asked for by
   libtommath (no shrinking via mp_realloc supported).
   This means we can assert that it matches the old_size passed to mp_realloc as
   an additional check.
   We can also support shrinking via mp_realloc, but then we have to drop that check.
*/

/* Wrapper functions for libtommath */

#include <tommath.h>
#define BIGINT_PAYLOAD(p) ((mp_int *)(&FIELD(p,1)))

/*
Note on libtommath error handling

Most libtommath operations return an int to signal error codes.
These are (see tommath.h):

   #define MP_OKAY       0   / * ok result * /
   #define MP_MEM        -2  / * out of mem * /
   #define MP_VAL        -3  / * invalid input * /
   #define MP_RANGE      MP_VAL
   #define MP_ITER       -4  / * Max. iterations reached * /

We will never hit MP_MEM, because our allocation functions trap if they cannot
allocate. But the others can happen (e.g. division by 0). In that case,
we call a trap function provided by the Wasm part of the runtime.
*/

#define CHECK(e) ((e == 0)?0:bigint_trap())

as_ptr bigint_alloc();

int bigint_count_bits(as_ptr a);

/* (S)LEB128 Encoding */
export int bigint_leb128_size(as_ptr n) {
  if (mp_iszero(BIGINT_PAYLOAD(n))) return 1;
  int x = bigint_count_bits(n);
  return ((x + 6) / 7); // divide by 7, round up
}

void bigint_leb128_encode_go(mp_int *tmp, unsigned char *buf, bool add_bit) {
  // now the number should be positive
  if (mp_isneg(tmp)) bigint_trap();
  while (true) {
    buf[0] = (unsigned char)(mp_get_u32(tmp)); // get low bits
    CHECK(mp_div_2d(tmp, 7, tmp, NULL));
    if (!mp_iszero(tmp) || (add_bit && (buf[0] & 1<<6))) {
      // more bytes to come, set high bit and continue
      buf[0] |= 1<<7;
      buf++;
    } else {
      // we are done. high bit should be cleared anyways
      return;
    }
  }
}

export void bigint_leb128_encode(as_ptr n, unsigned char *buf) {
  mp_int tmp;
  CHECK(mp_init_copy(&tmp, BIGINT_PAYLOAD(n)));
  bigint_leb128_encode_go(&tmp, buf, false);
}

export int bigint_2complement_bits(as_ptr n) {
  if (mp_isneg(BIGINT_PAYLOAD(n))) {
    mp_int tmp;
    CHECK(mp_init_copy(&tmp, BIGINT_PAYLOAD(n)));
    CHECK(mp_incr(&tmp));
    return 1 + mp_count_bits(&tmp);
  } else {
    return 1 + mp_count_bits(BIGINT_PAYLOAD(n));
  }
}

export int bigint_sleb128_size(as_ptr n) {
  int x = bigint_2complement_bits(n);
  return ((x + 6) / 7); // divide by 7, round up
}

export void bigint_sleb128_encode(as_ptr n, unsigned char *buf) {
  mp_int tmp;
  CHECK(mp_init_copy(&tmp, BIGINT_PAYLOAD(n)));

  if (mp_isneg(&tmp)) {
    // turn negative numbers into the two's complement of the right size
    int bytes = bigint_sleb128_size(n);
    mp_int big;
    CHECK(mp_init(&big));
    CHECK(mp_2expt(&big, 7*bytes));
    CHECK(mp_add(&tmp, &big, &tmp));
    bigint_leb128_encode_go(&tmp, buf, false);
  } else {
    bigint_leb128_encode_go(&tmp, buf, true);
  }
}

/* (S)LEB128 Decoding */

export as_ptr bigint_leb128_decode(buf *buf) {
  as_ptr r = bigint_alloc();
  mp_zero(BIGINT_PAYLOAD(r));
  mp_int tmp;
  CHECK(mp_init(&tmp));
  unsigned int s = 0;
  uint8_t b;
  do {
    b = read_byte(buf);
    if (s + 7 < s) {
        // shift overflow. number is absurdly large anyways
        idl_trap_with("absurdly large number");
    }
    mp_set_u32(&tmp, (b & (uint8_t)0x7f));
    CHECK(mp_mul_2d(&tmp, s, &tmp));
    CHECK(mp_add(BIGINT_PAYLOAD(r), &tmp, BIGINT_PAYLOAD(r)));
    s += 7;
  } while (b & (uint8_t)0x80);
  return r;
}

export as_ptr bigint_sleb128_decode(buf *buf) {
  as_ptr r = bigint_alloc();
  mp_zero(BIGINT_PAYLOAD(r));
  mp_int tmp;
  CHECK(mp_init(&tmp));
  unsigned int s = 0;
  uint8_t b;
  bool last_sign_bit_set = 0;
  do {
    b = read_byte(buf);
    if (s + 7 < s) {
        // shift overflow. number is absurdly large anyways
        idl_trap_with("absurdly large number");
    }
    mp_set_u32(&tmp, (b & (uint8_t)0x7f));
    CHECK(mp_mul_2d(&tmp, s, &tmp));
    CHECK(mp_add(BIGINT_PAYLOAD(r), &tmp, BIGINT_PAYLOAD(r)));
    last_sign_bit_set = (b & (uint8_t)0x40);
    s += 7;
  } while (b & (uint8_t)0x80);

  if (last_sign_bit_set) {
    // negative number, un-2-complement it
    mp_int big;
    CHECK(mp_init(&big));
    CHECK(mp_2expt(&big, s));
    CHECK(mp_sub(BIGINT_PAYLOAD(r), &big, BIGINT_PAYLOAD(r)));
  }

  return r;
}
