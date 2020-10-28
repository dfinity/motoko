#ifndef RTS_H
#define RTS_H

#pragma GCC diagnostic ignored "-Wattributes"
#pragma GCC diagnostic ignored "-Wincompatible-library-redeclaration"
#define export __attribute__ ((visibility("default")))
#define from_rts __attribute__ ((import_module("env"))) extern

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/*
Motoko pointers are offset by one. So let us represent
them as a typedef, and access the fields using the payload macro.

Note that for the purpose of ./test_rts, this needs to be 64 bit safe. So do
_not_ encode that a word is 4 bytes!
*/
typedef intptr_t as_ptr;
#define SKEW(p) ((as_ptr)p-1)
#define UNSKEW(p) ((size_t *)((as_ptr)p+1))
#define IS_SKEWED(p) ((p & 0x02)==0x02)

#define FIELD(p,n) (UNSKEW(p)[n])
#define TAG(p) FIELD(p,0)

typedef as_ptr blob_t;
#define BLOB_HEADER_SIZE 2
#define BLOB_LEN(p) (FIELD(p,1))
#define BLOB_PAYLOAD(p) ((char *)(&FIELD(p,2)))

#define ARRAY_HEADER_SIZE 2
#define ARRAY_LEN(p) (FIELD(p,1))
#define ARRAY_FIELD(p,n) (FIELD(p,ARRAY_HEADER_SIZE+n))

#define TUPLE_HEADER_SIZE 2
#define TUPLE_LEN(p) (FIELD(p,1))
#define TUPLE_FIELD(p,n,t) (*(t *)(&FIELD(p,TUPLE_HEADER_SIZE+n)))

/* Heap tags. Needs to stay in sync with compile.ml */
enum as_heap_tag {
  TAG_INVALID = 0,
  TAG_OBJECT = 1,
  TAG_OBJIND = 2,
  TAG_ARRAY = 3,
  TAG_INT = 5,
  TAG_MUTBOX = 6,
  TAG_CLOSURE = 7,
  TAG_SOME = 8,
  TAG_VARIANT = 9,
  TAG_BLOB = 10,
  TAG_INDIRECTION = 11,
  TAG_SMALLWORD = 12,
  TAG_BIGINT = 13,
  TAG_CONCAT = 14,
  };

/** Functions imported from the Motoko RTS */

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

So in order to allocate on the Motoko heap from C, we import
alloc_bytes from the Motoko RTS:
*/
from_rts as_ptr alloc_bytes(size_t n);
from_rts as_ptr alloc_words(size_t n);


/* IDL code */
/*@ assigns \nothing; ensures \false ; */
from_rts __attribute__ ((noreturn)) void rts_trap(const char* str, size_t n);
from_rts __attribute__ ((noreturn)) void bigint_trap();

typedef as_ptr text_t; // a skewed pointer to a Blob or Concat heap object
char *alloc(size_t n);
as_ptr alloc_blob(size_t n);
as_ptr alloc_array(uint32_t n_elems);
as_ptr text_of_ptr_size(const char *buf, size_t n);
as_ptr text_of_cstr(const char *buf);
int text_compare(as_ptr s1, as_ptr s2);

export __attribute__ ((noreturn)) void idl_trap_with(const char *str1);
export __attribute__ ((noreturn)) void rts_trap_with(const char *str1);

export as_ptr blob_of_text(as_ptr);
export uint32_t compute_crc32(blob_t);
export blob_t base32_of_checksummed_blob(blob_t);
export blob_t base32_to_blob(blob_t);
export int blob_compare(blob_t s1, blob_t s2);

export blob_t blob_of_principal(text_t);
export blob_t base32_to_principal(blob_t);

export void utf8_validate(const char *src, size_t len);

#endif /* RTS_H */
