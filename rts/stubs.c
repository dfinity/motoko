#include <stddef.h>
#include <stdint.h>

__attribute__ ((noreturn)) void rts_trap(char *msg, uint32_t len);

// Stubbery for musl. Functions below are used by some of the musl functions we
// compile. musl implementations use system calls. We don't want them to be
// used and we can't provide sensible implementations for them, so we provide
// stubs below that just trap when called.

int wctomb(char *s, wchar_t wc) { rts_trap("wctomb", 6); }

int fputs(const char *s, void *f) { rts_trap("fputs", 5); }

void abort(void) { rts_trap("abort", 5); }

char *strerror(int e) { rts_trap("strerror", 8); }
