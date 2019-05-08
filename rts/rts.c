#pragma GCC diagnostic ignored "-Wattributes"
#pragma GCC diagnostic ignored "-Wincompatible-library-redeclaration"
#define export __attribute__ ((visibility("default")))
#define from_rts __attribute__ ((import_module("env"))) extern

export void as_memcpy(char *str1, const char *str2, int n) {
  for (int i = 0; i < n; i++) {
    str1[i] = str2[i];
  }
}

/*
ActorScript pointers are offset by one. So let us represent
them as a typedef, and access the fields using the payload macro.
*/
typedef long as_ptr;
#define FIELD(p,n) (((int *)(p+1))[n])
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
from_rts as_ptr alloc_bytes(int n);

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

int as_strlen(const char* p) {
  int i = 0;
  while (p[i]) i++;
  return i;
}

as_ptr as_str_of_cstr(const char * const s) {
  int l = as_strlen(s);
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

void* mp_alloc(int l) {
  as_ptr r = alloc_bytes (2*sizeof(void*) + l);
  FIELD(r, 0) = TAG_TEXT; // abusing text as byte array here
  FIELD(r, 1) = l;
  return &FIELD(r,2);
}

export void* mp_calloc(int n, int size) {
  int l = n * size; // check overflow?
  void *payload = mp_alloc(l);
  char *tmp = (char *)payload;
  for (int i = 0; i < l; i++) {
    *tmp++ = 0;
  }
  return payload;
}

export void* mp_realloc(void *ptr, int old_size, int new_size) {
  as_ptr r = (as_ptr)(((char *)ptr) - (2 * sizeof(void*) - 1));
  if (new_size > FIELD(r, 1)) {
    void *newptr = mp_alloc(new_size);
    as_memcpy(newptr, ptr, old_size);
    return newptr;
  } else {
    return ptr;
  }
}

export void mp_free(void *ptr, int size) {
}

/* Wrapper functions for libtommath */

#include <tommath.h>
#define BIGINT_PAYLOAD(p) ((mp_int *)(&FIELD(p,1)))

as_ptr bigint_alloc() {
  as_ptr r = alloc_bytes (1*sizeof(void*) + sizeof(mp_int));
  FIELD(r, 0) = TAG_BIGINT;
  mp_init(BIGINT_PAYLOAD(r));
  return r;
}

export as_ptr bigint_of_word32(unsigned long b) {
  as_ptr r = bigint_alloc();
  mp_set_int(BIGINT_PAYLOAD(r), b);
  return r;
}

export unsigned long bigint_to_word32(as_ptr a) {
  return mp_get_int(BIGINT_PAYLOAD(a));
}

export as_ptr bigint_of_word64(unsigned long long b) {
  as_ptr r = bigint_alloc();
  mp_set_long_long(BIGINT_PAYLOAD(r), b);
  return r;
}

export unsigned long long bigint_to_word64(as_ptr a) {
  return mp_get_long_long(BIGINT_PAYLOAD(a));
}

export int bigint_eq(as_ptr a, as_ptr b) {
  return mp_cmp(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b)) == 0;
}
export int bigint_lt(as_ptr a, as_ptr b) {
  return mp_cmp(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b)) < 0;
}
export int bigint_gt(as_ptr a, as_ptr b) {
  return mp_cmp(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b)) > 0;
}
export int bigint_le(as_ptr a, as_ptr b) {
  return mp_cmp(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b)) <= 0;
}
export int bigint_ge(as_ptr a, as_ptr b) {
  return mp_cmp(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b)) >= 0;
}

export as_ptr bigint_add(as_ptr a, as_ptr b) {
  as_ptr r = bigint_alloc();
  mp_add(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b), BIGINT_PAYLOAD(r));
  return r;
}

export as_ptr bigint_sub(as_ptr a, as_ptr b) {
  as_ptr r = bigint_alloc();
  mp_sub(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b), BIGINT_PAYLOAD(r));
  return r;
}

export as_ptr bigint_mul(as_ptr a, as_ptr b) {
  as_ptr r = bigint_alloc();
  mp_mul(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b), BIGINT_PAYLOAD(r));
  return r;
}

export as_ptr bigint_mod(as_ptr a, as_ptr b) {
  as_ptr r = bigint_alloc();
  mp_mod(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b), BIGINT_PAYLOAD(r));
  return r;
}

export as_ptr bigint_neg(as_ptr a) {
  as_ptr r = bigint_alloc();
  mp_neg(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(r));
  return r;
}

export as_ptr bigint_abs(as_ptr a) {
  as_ptr r = bigint_alloc();
  mp_abs(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(r));
  return r;
}

export as_ptr bigint_lshd(as_ptr a, int b) {
  as_ptr r = bigint_alloc();
  mp_copy(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(r));
  mp_lshd(BIGINT_PAYLOAD(a), b);
  return r;
}

export as_ptr bigint_div(as_ptr a, as_ptr b) {
  as_ptr r = bigint_alloc();
  mp_int rem;
  mp_init(&rem);
  mp_div(BIGINT_PAYLOAD(a), BIGINT_PAYLOAD(b), BIGINT_PAYLOAD(r), &rem);
  return r;
}

