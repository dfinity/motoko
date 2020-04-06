#include "rts.h"

export as_ptr float_fmt(double a) {
  extern int snprintf(char *__restrict, size_t, const char *__restrict, ...);
  char buf[50];
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
