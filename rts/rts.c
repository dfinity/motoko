#include "rts.h"

char *alloc(size_t n) {
  as_ptr r = alloc_bytes (2*sizeof(void*) + n);
  FIELD(r, 0) = TAG_TEXT;
  FIELD(r, 1) = n;
  return (char *)&FIELD(r,2);
}


export void as_memcpy(char *str1, const char *str2, size_t n) {
  for (size_t i = 0; i < n; i++) {
    str1[i] = str2[i];
  }
}

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
