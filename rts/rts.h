#pragma GCC diagnostic ignored "-Wattributes"
#pragma GCC diagnostic ignored "-Wincompatible-library-redeclaration"
#define export __attribute__ ((visibility("default")))
#define from_rts __attribute__ ((import_module("env"))) extern

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/*
ActorScript pointers are offset by one. So let us represent
them as a typedef, and access the fields using the payload macro.
*/
typedef intptr_t as_ptr;
#define FIELD(p,n) (((size_t *)(p+1))[n])
#define TAG(p) FIELD(p,0)
#define TEXT_LEN(p) ((char *)(&FIELD(p,1)))
#define TEXT_PAYLOAD(p) ((char *)(&FIELD(p,2)))

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

/** Functions imported from the ActorScript RTS */

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
char *alloc(size_t n);


/* IDL code */
/*@ assigns \nothing; ensures \false ; */
from_rts __attribute__ ((noreturn)) void idl_trap();
from_rts __attribute__ ((noreturn)) void bigint_trap();

/** Functions used in multiple modules of the RTS */
export void as_memcpy(char *str1, const char *str2, size_t n);

