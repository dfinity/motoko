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

export int32_t char_is_whitespace(int32_t c) {
  extern int iswspace(unsigned);
  // Boolean values in Motoko are 0 for False and 1 for True, so we make sure we
  // return those here don't do any marshalling in generated code.
  return iswspace((unsigned)c) != 0;
}

export int32_t char_is_uppercase(int32_t c) {
  extern int iswupper(unsigned);
  return iswupper((unsigned)c) != 0;
}

export int32_t char_is_lowercase(int32_t c) {
  extern int iswlower(unsigned);
  return iswlower((unsigned)c) != 0;
}

#endif
