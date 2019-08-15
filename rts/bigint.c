#include "rts.h"
#include "buf.h"

/* Memory management for libtommath */

/*
A libtommath arbitrary precision integer is a struct (`mp_int`) that contains a
pointer to a data array.

 * The libtommath library never allocates the struct, so we are in full
   control. We can embed the struct simply in an ActorScript heap object
   with a dedicated tag for it.

 * The data array is allocated with mp_calloc and mp_realloc. We provide these
   calls, allocate ActorScript arrays (using the TAG_TEXT tag for byte arrays,
   not TAG_ARRAY for arrays of pointers) and store the pointer to the
   _payload_ in the `mp_digit* dp` field of the struct. This way, things look all nice
   and dandy from libtommath’s point of view.

   Our garbage collector has special knowledge about the dp field of the struct
   and understands that this pointer points inside the TAG_TEXT heap object. But
   we can still move them around in the GC without issues.

   The length of the byte array is always equal to the allocation asked for by
   libtommath (no shrinking via mp_realloc supported).
   This means we can assert that it matches the old_size passed to mp_realloc as
   an additional check.
   We can also support shrinking via mp_realloc, but then we have to drop that check.
*/

void* mp_alloc(size_t l) {
  as_ptr r = alloc_bytes (2*sizeof(void*) + l);
  FIELD(r, 0) = TAG_TEXT; // abusing text as byte array here
  FIELD(r, 1) = l;
  return &FIELD(r,2);
}

export void* mp_calloc(size_t n, size_t size) {
  size_t l = n * size; // check overflow?
  void *payload = mp_alloc(l);
  char *tmp = (char *)payload;
  for (size_t i = 0; i < l; i++) {
    *tmp++ = 0;
  }
  return payload;
}

export void* mp_realloc(void *ptr, size_t old_size, size_t new_size) {
  as_ptr r = (as_ptr)(((char *)ptr) - (2 * sizeof(void*) + 1));

  if (FIELD(r, 0) != TAG_TEXT) bigint_trap(); // assert block type

  if (new_size > FIELD(r, 1)) {
    void *newptr = mp_alloc(new_size);
    if (old_size != FIELD(r, 1)) bigint_trap();
    as_memcpy(newptr, ptr, old_size);
    return newptr;
  } else if (new_size == FIELD(r, 1)) {
    // No need to grow
    return ptr;
  } else {
    // libtommath only shrinks on explicit demand via mp_shrink
    // and we do not use that function
    // so this should not happen.
    bigint_trap();
  }
}

export void mp_free(void *ptr, size_t size) {
}

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

as_ptr bigint_alloc() {
  as_ptr r = alloc_bytes (1*sizeof(void*) + sizeof(mp_int));
  FIELD(r, 0) = TAG_BIGINT;
  CHECK(mp_init(BIGINT_PAYLOAD(r)));
  return r;
}

export as_ptr bigint_of_word32(uint32_t b) {
  as_ptr r = bigint_alloc();
  mp_set_u32(BIGINT_PAYLOAD(r), b);
  return r;
}

export as_ptr bigint_of_word32_signed(int32_t b) {
  as_ptr r = bigint_alloc();
  mp_int *n = BIGINT_PAYLOAD(r);
  mp_set_i32(n, b);
  return r;
}

export uint32_t bigint_to_word32_wrap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  return mp_get_u32(n);
}

export uint32_t bigint_to_word32_trap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  if (mp_isneg(n)) bigint_trap();
  if (mp_count_bits(n) > 32) bigint_trap();
  return mp_get_u32(n);
}

export int32_t bigint_to_word32_signed_trap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  if (mp_count_bits(n) > 32) bigint_trap();
  if (mp_isneg(n)) {
    int32_t x = - (int32_t)(mp_get_mag_u32(n));
    if (x >= 0) bigint_trap();
    return x;
  } else {
    int32_t x = (int32_t)(mp_get_mag_u32(n));
    if (x < 0) bigint_trap();
    return x;
  }
}

export uint64_t bigint_to_word64_wrap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  return mp_get_u64(n);
}

export uint64_t bigint_to_word64_trap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  if (mp_isneg(n)) bigint_trap();
  if (mp_count_bits(n) > 64) bigint_trap();
  return mp_get_u64(n);
}

export int64_t bigint_to_word64_signed_trap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  if (mp_count_bits(n) > 64) bigint_trap();
  if (mp_isneg(n)) {
    int64_t x = - (int64_t)(mp_get_mag_u64(n));
    if (x >= 0) bigint_trap();
    return x;
  } else {
    int64_t x = (int64_t)(mp_get_mag_u64(n));
    if (x < 0) bigint_trap();
    return x;
  }
}

export as_ptr bigint_of_word64(uint64_t b) {
  as_ptr r = bigint_alloc();
  mp_set_u64(BIGINT_PAYLOAD(r), b);
  return r;
}

export as_ptr bigint_of_word64_signed(int64_t b) {
  as_ptr r = bigint_alloc();
  mp_int *n = BIGINT_PAYLOAD(r);
  mp_set_i64(n, b);
  return r;
}

export bool bigint_eq(as_ptr a, as_ptr b) {
  return mp_cmp(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b)) == 0;
}
export bool bigint_lt(as_ptr a, as_ptr b) {
  return mp_cmp(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b)) < 0;
}
export bool bigint_gt(as_ptr a, as_ptr b) {
  return mp_cmp(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b)) > 0;
}
export bool bigint_le(as_ptr a, as_ptr b) {
  return mp_cmp(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b)) <= 0;
}
export bool bigint_ge(as_ptr a, as_ptr b) {
  return mp_cmp(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b)) >= 0;
}

export as_ptr bigint_add(as_ptr a, as_ptr b) {
  as_ptr r = bigint_alloc();
  CHECK(mp_add(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b), BIGINT_PAYLOAD(r)));
  return r;
}

export as_ptr bigint_sub(as_ptr a, as_ptr b) {
  as_ptr r = bigint_alloc();
  CHECK(mp_sub(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b), BIGINT_PAYLOAD(r)));
  return r;
}

export as_ptr bigint_mul(as_ptr a, as_ptr b) {
  as_ptr r = bigint_alloc();
  CHECK(mp_mul(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b), BIGINT_PAYLOAD(r)));
  return r;
}

export as_ptr bigint_pow(as_ptr a, as_ptr b) {
  uint32_t exp = bigint_to_word32_trap(b);
  as_ptr r = bigint_alloc();
  CHECK(mp_expt_u32(BIGINT_PAYLOAD(a), exp, BIGINT_PAYLOAD(r)));
  return r;
}

export as_ptr bigint_div(as_ptr a, as_ptr b) {
  as_ptr r = bigint_alloc();
  mp_int rem;
  CHECK(mp_init(&rem));
  CHECK(mp_div(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b), BIGINT_PAYLOAD(r), &rem));
  return r;
}

export as_ptr bigint_rem(as_ptr a, as_ptr b) {
  as_ptr r = bigint_alloc();
  mp_int quot;
  CHECK(mp_init(&quot));
  CHECK(mp_div(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b), &quot, BIGINT_PAYLOAD(r)));
  return r;
}

export as_ptr bigint_neg(as_ptr a) {
  as_ptr r = bigint_alloc();
  CHECK(mp_neg(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(r)));
  return r;
}

export as_ptr bigint_abs(as_ptr a) {
  as_ptr r = bigint_alloc();
  CHECK(mp_abs(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(r)));
  return r;
}

export bool bigint_isneg(as_ptr a) {
  return mp_isneg(BIGINT_PAYLOAD(a));
}

export as_ptr bigint_lsh(as_ptr a, int b) {
  as_ptr r = bigint_alloc();
  CHECK(mp_mul_2d(BIGINT_PAYLOAD(a), b, BIGINT_PAYLOAD(r)));
  return r;
}

export int bigint_count_bits(as_ptr a) {
  return mp_count_bits(BIGINT_PAYLOAD(a));
}

/* (S)LEB128 Encoding */
export int bigint_leb128_size(as_ptr n) {
  if (mp_iszero(BIGINT_PAYLOAD(n))) return 1;
  int x = bigint_count_bits(n);
  return ((x + 6) / 7); // divide by 7, round up
}

void bigint_leb128_encode_go(mp_int *tmp, unsigned char *buf) {
  // now the number should be positive
  if (mp_isneg(tmp)) bigint_trap();
  while (true) {
    buf[0] = (unsigned char)(mp_get_u32(tmp)); // get low bits
    CHECK(mp_div_2d(tmp, 7, tmp, NULL));
    if (mp_iszero(tmp)) {
      // we are done. high bit should be cleared anyways
      return;
    } else {
      // more bytes to come, set high bit and continue
      buf[0] |= 1<<7;
      buf++;
    }
  }
}

export void bigint_leb128_encode(as_ptr n, unsigned char *buf) {
  mp_int tmp;
  CHECK(mp_init_copy(&tmp, BIGINT_PAYLOAD(n)));
  bigint_leb128_encode_go(&tmp, buf);
}


int leb128_encoding_size(unsigned char *buf) {
  // zoom to the end
  int i = 0;
  while (buf[i] & (1<<7)) i++;
  return i+1;
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
  }

  bigint_leb128_encode_go(&tmp, buf);
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
    if (s > 0 && b == 0x00) {
        // The high byte is all zeroes, this is not a shortest encoding
        idl_trap();
    }
    if (s + 7 < s) {
        // shift overflow. number is absurdly large anyways
        idl_trap();
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
    if (s > 0 && (b == 0x00 || (last_sign_bit_set && b == 0x8F))) {
        // The high bits are all zeros or ones, so this is not a shortest encoding
        idl_trap();
    }
    if (s + 7 < s) {
        // shift overflow. number is absurdly large anyways
        idl_trap();
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

