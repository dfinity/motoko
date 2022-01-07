#include <stddef.h>
#include <stdint.h>

__attribute__ ((noreturn)) void rts_trap(char *msg, uint32_t len);

// Stubbery for musl. Functions below are used by some of the musl functions we
// compile.

// musl implementation uses system calls so we provide our own implementation
void abort(void) { rts_trap("abort", 5); }

// Functions below should not be called so we trap when they're called.

int wctomb(char *s, wchar_t wc) { __builtin_trap(); }

int fputs(const char *s, void *f) { __builtin_trap(); }

char *strerror(int e) { __builtin_trap(); }
