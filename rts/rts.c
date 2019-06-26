#pragma GCC diagnostic ignored "-Wattributes"
#pragma GCC diagnostic ignored "-Wincompatible-library-redeclaration"
#define export __attribute__ ((visibility("default")))
#define from_rts __attribute__ ((import_module("env"))) extern

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

export void as_memcpy(char *str1, const char *str2, size_t n) {
  for (size_t i = 0; i < n; i++) {
    str1[i] = str2[i];
  }
}

/*
ActorScript pointers are offset by one. So let us represent
them as a typedef, and access the fields using the payload macro.
*/
typedef intptr_t as_ptr;
#define FIELD(p,n) (((size_t *)(p+1))[n])
#define TAG(p) FIELD(p,0)
#define TEXT_LEN(p) ((char *)(&FIELD(p,1)))
#define TEXT_PAYLOAD(p) ((char *)(&FIELD(p,2)))

/*

Ideally I’d do something like this

   __attribute__((global)) extern char* heap_ptr;
   export char* alloc_bytes(int n) {
     char *r = heap_ptr;
     heap_ptr += (n + 3) & ~0x03;
     return r;
   }

But seems we can’t import mutable globals like that,
see https://bugs.llvm.org/show_bug.cgi?id=41610

So in order to allocate on the ActorScript heap from C, we import
alloc_bytes from the Actorscript RTS:
*/
from_rts as_ptr alloc_bytes(size_t n);

/* Heap tags. Needs to stay in sync with compile.ml */
enum as_heap_tag {
  TAG_INVALID = 0,
  TAG_OBJECT = 1,
  TAG_OBJIND = 2,
  TAG_ARRAY = 3,
  TAG_REFERENCE = 4,
  TAG_INT = 5,
  TAG_MUTBOX = 6,
  TAG_CLOSURE = 7,
  TAG_SOME = 8,
  TAG_VARIANT = 9,
  TAG_TEXT = 10,
  TAG_INDIRECTION = 11,
  TAG_SMALLWORD = 12,
  TAG_BIGINT = 13,
  };

size_t as_strlen(const char* p) {
  size_t i = 0;
  while (p[i]) i++;
  return i;
}

as_ptr as_str_of_cstr(const char * const s) {
  size_t l = as_strlen(s);
  as_ptr r = alloc_bytes (2*sizeof(void*) + l);
  FIELD(r, 0) = TAG_TEXT;
  FIELD(r, 1) = l;
  as_memcpy((char *)(&FIELD(r,2)), s, l);
  return r;
}

// This is mostly to test static strings and access to the AS heap
const char* RTS_VERSION = "0.1";

// This is mostly to test function pointers
as_ptr get_version() { return as_str_of_cstr(RTS_VERSION); }
as_ptr (*version_getter)() = &get_version;

export as_ptr version() { return (*version_getter)(); }

/* IDL code */
from_rts __attribute__ ((noreturn)) void idl_trap();

// Initially, we just want to be able to zoom past the type description
// TODO: Defensive programming
// (not going past the size of the message, trapping if leb128 does not fit in int)

int read_leb128(char **ptr) {
  int r = 0;
  int s = 0;
  char b;
  do {
    b = *(*ptr)++;
    r += (b & (char)0x7f) << s;
    s += 7;
  } while (b & (char)0x80);
  return r;
}
int read_sleb128(char **ptr) {
  int r = 0;
  int s = 0;
  char b;
  do {
    b = *(*ptr)++;
    r += (b & (char)0x7f) << s;
    s += 7;
  } while (b & (char)0x80);
  // sign extend
  if (b & (char)0x40) {
    r |= (~0 << s);
  }
  return r;
}

export char *skip_idl_header(char *ptr) {
  // Magic bytes
  if (!(*ptr++ == 'D')) idl_trap();
  if (!(*ptr++ == 'I')) idl_trap();
  if (!(*ptr++ == 'D')) idl_trap();
  if (!(*ptr++ == 'L')) idl_trap();
  // Size of type list
  for (int count = read_leb128(&ptr); count > 0; count --) {
    int ty = read_sleb128(&ptr);
    if (ty >= -17) {
      idl_trap(); // illegal
    } else if (ty == -18) { // opt
      read_sleb128(&ptr);
    }
    else if (ty == -19) { // vec
      read_sleb128(&ptr);
    } else if (ty == -20) {  // record
      for (int n = read_leb128(&ptr); n > 0; n--) {
        read_leb128(&ptr);
        read_sleb128(&ptr);
      }
    } else if (ty == -21) {  // variant
      for (int n = read_leb128(&ptr); n > 0; n--) {
        read_leb128(&ptr);
        read_sleb128(&ptr);
      }
    } else if (ty == -22) {  // func
      // arg types
      for (int n = read_leb128(&ptr); n > 0; n--) {
        read_sleb128(&ptr);
      }
      // ret types
      for (int n = read_leb128(&ptr); n > 0; n--) {
        read_sleb128(&ptr);
      }
      // annotations
      for (int n = read_leb128(&ptr); n > 0; n--) {
        ptr++;
      }
    } else if (ty == -23) {  // func
      for (int n = read_leb128(&ptr); n > 0; n--) {
        // name
        ptr += read_leb128(&ptr);
        // type
        read_sleb128(&ptr);
      }
    } else {
      idl_trap();
    }
  }
  read_sleb128(&ptr); // index
  return ptr;
}

/* Memory management for libtommath */

/*
A libtommath arbitrary precision integer is a struct (`mp_int`) that contains a
pointer to a data array.

 * The libtommath library never allocates the struct, so we are in full
   control. We can embed the struct simply in an ActorScript heap object
   with a dedicated tag for it.

 * The data array is allocated with mp_calloc and mp_realloc. We provide these
   calls, allocate ActorScript arrays (using the TAG_TEXT tag for byte arrays,
   not TAG_ARRAY for arrays of pointers) and  store the pointer to the
   _payload_ in the `mp_digit* dp` field of the struct. This way, things look all nice
   and dandy from libtommath’s point of view.

   Our gargabe collector has special knowledge about the dp field of the struct
   and understands that this pointer points inside the TAG_TEXT heap object. But
   we can still move them around in the GC without issues.

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
  as_ptr r = (as_ptr)(((char *)ptr) - (2 * sizeof(void*) - 1));
  if (new_size > FIELD(r, 1)) {
    void *newptr = mp_alloc(new_size);
    as_memcpy(newptr, ptr, old_size);
    return newptr;
  } else {
    return ptr;
  }
}

export void mp_free(void *ptr, size_t size) {
}

/* Wrapper functions for libtommath */

#include <tommath.h>
#define BIGINT_PAYLOAD(p) ((mp_int *)(&FIELD(p,1)))

/*
Note on libtommmath error handling

Most libtommmath operations return an int to signal error codes.
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
from_rts __attribute__ ((noreturn)) void bigint_trap();

as_ptr bigint_alloc() {
  as_ptr r = alloc_bytes (1*sizeof(void*) + sizeof(mp_int));
  FIELD(r, 0) = TAG_BIGINT;
  CHECK(mp_init(BIGINT_PAYLOAD(r)));
  return r;
}

export as_ptr bigint_of_word32(unsigned long b) {
  as_ptr r = bigint_alloc();
  CHECK(mp_set_long(BIGINT_PAYLOAD(r), b));
  return r;
}

export as_ptr bigint_of_word32_signed(signed long b) {
  as_ptr r = bigint_alloc();
  mp_int *n = BIGINT_PAYLOAD(r);
  CHECK(mp_set_long(n, b));
  if (b < 0) {
    mp_int sub;
    CHECK(mp_init(&sub));
    CHECK(mp_2expt(&sub, 32));
    CHECK(mp_sub(n,&sub,n));
  }
  return r;
}

export unsigned long bigint_to_word32_wrap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  if (mp_isneg(n))
    return - mp_get_long(n);
  else
    return mp_get_long(n);
}

export unsigned long bigint_to_word32_trap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  if (mp_isneg(n)) bigint_trap();
  if (mp_count_bits(n) > 32) bigint_trap();
  return mp_get_long(n);
}

export signed long bigint_to_word32_signed_trap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  if (mp_count_bits(n) > 32) bigint_trap();
  if (mp_isneg(n)) {
    long x = - (signed long)(mp_get_long(n));
    if (x >= 0) bigint_trap();
    return x;
  } else {
    long x = (signed long)(mp_get_long(n));
    if (x < 0) bigint_trap();
    return x;
  }
}

export unsigned long long bigint_to_word64_wrap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  if (mp_isneg(n))
    return - mp_get_long_long(n);
  else
    return mp_get_long_long(n);
}

export unsigned long long bigint_to_word64_trap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  if (mp_isneg(n)) bigint_trap();
  if (mp_count_bits(n) > 64) bigint_trap();
  return mp_get_long_long(n);
}

export signed long long bigint_to_word64_signed_trap(as_ptr a) {
  mp_int *n = BIGINT_PAYLOAD(a);
  if (mp_count_bits(n) > 64) bigint_trap();
  if (mp_isneg(n)) {
    long long x = - (signed long long)(mp_get_long_long(n));
    if (x >= 0) bigint_trap();
    return x;
  } else {
    long long x = (signed long long)(mp_get_long_long(n));
    if (x < 0) bigint_trap();
    return x;
  }
}

export as_ptr bigint_of_word64(unsigned long long b) {
  as_ptr r = bigint_alloc();
  CHECK(mp_set_long_long(BIGINT_PAYLOAD(r), b));
  return r;
}

export as_ptr bigint_of_word64_signed(signed long long b) {
  as_ptr r = bigint_alloc();
  mp_int *n = BIGINT_PAYLOAD(r);
  CHECK(mp_set_long_long(n, b));
  if (b < 0) {
    mp_int sub;
    CHECK(mp_init(&sub));
    CHECK(mp_2expt(&sub, 64));
    CHECK(mp_sub(n,&sub,n));
  }
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
  unsigned long exp = bigint_to_word32_trap(b);
  as_ptr r = bigint_alloc();
  // Replace with mp_expt_long once available,
  // see https://github.com/libtom/libtommath/issues/243
  CHECK(mp_expt_d_ex(BIGINT_PAYLOAD(a), exp, BIGINT_PAYLOAD(r), 1));
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

/* LEB128 Encoding-decoding */
export int bigint_leb128_size(as_ptr n) {
  if (mp_iszero(BIGINT_PAYLOAD(n))) return 1;
  int x = bigint_count_bits(n);
  return ((x + 6) / 7); // divide by 7, round up
}

void leb128_encode_go(mp_int *tmp, unsigned char *buf) {
  // now the number should be positive
  if (mp_isneg(tmp)) bigint_trap();
  while (true) {
    buf[0] = (char)(mp_get_int(tmp)); // get low bits
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
  leb128_encode_go(&tmp, buf);
}


int leb128_encoding_size(unsigned char *buf) {
  // zoom to the end
  int i = 0;
  while (buf[i] & (1<<7)) i++;
  return i+1;
}

as_ptr leb128_decode_go(unsigned char *buf, int bytes) {
  // do not accept overlong encodings
  if (buf[bytes-1] == 1<<7) bigint_trap();

  as_ptr r = bigint_alloc();
  CHECK(mp_init(BIGINT_PAYLOAD(r)));
  int i = bytes-1;
  while (true) {
    CHECK(mp_add_d(BIGINT_PAYLOAD(r), buf[i] & ((1<<7)-1), BIGINT_PAYLOAD(r)));
    if (i > 0) {
      CHECK(mp_mul_2d(BIGINT_PAYLOAD(r), 7, BIGINT_PAYLOAD(r)));
      i--;
    } else {
      break;
    }
  }
  return r;
}

export as_ptr bigint_leb128_decode(unsigned char *buf) {
  int bytes = leb128_encoding_size(buf);
  return leb128_decode_go(buf, bytes);
}

/* SLEB128 Encoding-decoding */
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

  leb128_encode_go(&tmp, buf);
}

export as_ptr bigint_sleb128_decode(unsigned char *buf) {
  int bytes = leb128_encoding_size(buf);

  as_ptr r = leb128_decode_go(buf, bytes);

  // Now adjust sign if necessary
  if (buf[bytes-1] & (1<<6)) {
    // negative number
    mp_int big;
    CHECK(mp_init(&big));
    CHECK(mp_2expt(&big, 7*bytes));
    CHECK(mp_sub(BIGINT_PAYLOAD(r), &big, BIGINT_PAYLOAD(r)));
  }

  return r;
}
