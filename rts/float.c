#include "rts.h"

// this is not intended for native compilation/testing (yet)
#ifdef __wasm__

export as_ptr float_fmt(double a) {
  extern int snprintf(char *__restrict, size_t, const char *__restrict, ...);
  char buf[50]; // corresponds to 150 bits of pure floatness, room for 64 bits needed
  const int chars = snprintf(buf, sizeof buf, "%f", a);
  return text_of_ptr_size(buf, chars);
}

export double float_pow(double a, double b) {
  extern double pow(double, double);
  return pow(a, b);
}

export double float_sin(double a) {
  extern double sin(double);
  return sin(a);
}

export double float_cos(double a) {
  extern double cos(double);
  return cos(a);
}

export double float_tan(double a) {
  extern double tan(double);
  return tan(a);
}

export double float_arcsin(double a) {
  extern double asin(double);
  return asin(a);
}

export double float_arccos(double a) {
  extern double acos(double);
  return acos(a);
}

export double float_arctan(double a) {
  extern double atan(double);
  return atan(a);
}

#endif
