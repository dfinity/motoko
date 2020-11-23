#include "rts.h"

/* Stubbery for musl */

int wctomb(char *s, wchar_t wc) { *s = '\0'; }
int fputs(const char *s, void *f) { return 0; }
void abort(void) __attribute__((__noreturn__)) { rts_trap("abort", 5); }
char *strerror(int e) { return ""; }
