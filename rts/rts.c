#include "rts.h"

// Copied these from musl source, for some reason it's not easily possible to include string.h
void *memcpy (void *__restrict, const void *__restrict, size_t);
size_t strlen (const char *);

char *alloc(size_t n);

export void __attribute__ ((noreturn)) trap_with_prefix(const char* prefix, const char *str) {
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
