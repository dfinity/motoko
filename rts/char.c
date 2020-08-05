#include "rts.h"

#ifdef __wasm__

export int32_t char_to_upper(int32_t c) {
  extern unsigned towupper(unsigned);
  return (int32_t)towupper((unsigned)c);
}

export int32_t char_to_lower(int32_t c) {
  extern unsigned towlower(unsigned);
  return (int32_t)towlower((unsigned)c);
}

int printf(const char *__restrict, ...);

// Boolean values in Motoko are 0 for False and 1 for True, so we make sure we
// return those here don't do any marshalling in generated code.
#define CHAR_PRED(rts_fn, musl_fn) \
  export int32_t rts_fn(int32_t c) { \
    extern int musl_fn(unsigned); \
    printf("just testing\n"); \
    return musl_fn((unsigned)c) != 0; \
  }

CHAR_PRED(char_is_whitespace, iswspace)
CHAR_PRED(char_is_uppercase, iswupper)
CHAR_PRED(char_is_lowercase, iswlower)
CHAR_PRED(char_is_alphabetic, iswalpha)

#endif
