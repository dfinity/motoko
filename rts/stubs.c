#include <stddef.h>
#include <stdint.h>

__attribute__ ((noreturn)) void rts_trap(char *msg, uint32_t len);

/* Stubbery for musl */

// NOTE (osa): This function previously did not return even though it should
// (return type declared as `int`). Returning `0` here for now but we should
// probably make sure is never used (maybe trap).
int wctomb(char *s, wchar_t wc) { *s = '\0'; return 0; }

int fputs(const char *s, void *f) { return 0; }

void abort(void) { rts_trap("abort", 5); }

char *strerror(int e) { return ""; }
