#include "rts.h"

// this is not intended for native compilation/testing (yet)
#ifdef __wasm__

export as_ptr float_fmt(double a) {
  extern int snprintf(char *__restrict, size_t, const char *__restrict, ...);
  char buf[50]; // corresponds to 150 bits of pure floatness, room for 64 bits needed
  const int chars = snprintf(buf, sizeof buf, "%f", a);
  return text_of_ptr_size(buf, chars);
}

// re-export transcendental and trigonometric functions under a modified naming scheme
// e.g. pow(a, b) ==> float_pow

#define EXPORT_UNARY(IMP, A1, EXP) \
  export double float_ ## EXP(double A1) { \
    extern double IMP(double); \
    return IMP(A1); \
  }

#define EXPORT_BINARY(IMP, A1, A2, EXP) \
  export double float_ ## EXP(double A1, double A2) { \
    extern double IMP(double, double); \
    return IMP(A1, A2); \
  }

EXPORT_BINARY(pow, a, b, pow)
EXPORT_UNARY(sin, a, sin)
EXPORT_UNARY(cos, a, cos)
EXPORT_UNARY(tan, a, tan)
EXPORT_UNARY(asin, a, arcsin)
EXPORT_UNARY(acos, a, arccos)
EXPORT_UNARY(atan, a, arctan)
EXPORT_BINARY(atan2, y, x, arctan2)
EXPORT_UNARY(exp, a, exp)
EXPORT_UNARY(log, a, log)

#endif
