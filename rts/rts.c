#include "rts.h"

char *alloc(size_t n) {
  as_ptr r = alloc_blob(n);
  return BLOB_PAYLOAD(r);
}

void __attribute__ ((noreturn)) trap_with_prefix(const char* prefix, const char *str) {
  int len1 = strlen(prefix);
  int len2 = strlen(str);
  char msg[len1 + len2];
  memcpy(msg, prefix, len1);
  memcpy(msg + len1, str, len2);
  rts_trap(msg, len1 + len2);
}

void __attribute__ ((noreturn)) idl_trap_with(const char *str) {
  trap_with_prefix("IDL error: ", str);
}

void __attribute__ ((noreturn)) rts_trap_with(const char *str) {
  trap_with_prefix("RTS error: ", str);
}

// This is mostly to test static strings and access to the AS heap
const char* RTS_VERSION = "0.1";

// This is mostly to test function pointers
as_ptr get_version() { return text_of_cstr(RTS_VERSION); }
as_ptr (*version_getter)() = &get_version;

export as_ptr version() { return (*version_getter)(); }
