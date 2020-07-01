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
  return (int32_t)iswspace((unsigned)c);
}

#endif
