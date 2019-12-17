#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "rts.h"

typedef intptr_t as_ptr;


as_ptr alloc_bytes(size_t n) {
    void *ptr = malloc(n);
    if (ptr == NULL) { printf("OOM\n"); exit(1); };
    return ((as_ptr)ptr) - 1;
};
as_ptr alloc_words(size_t n) {
    return alloc_bytes(sizeof(size_t) * n);
};

void rts_trap() {
  printf("RTS trap\n");
  abort();
}
void bigint_trap() {
  printf("Bigint trap\n");
  exit(1);
}

int ret = EXIT_SUCCESS;

void assert(bool check, const char *fmt, ...) {
  if (!check) {
    ret = EXIT_FAILURE;
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
  }
}

int main () {
  printf("Motoko RTS test suite\n");


  /*
   * Testing BigInt
   */
  printf("Testing BigInt...\n");

  extern as_ptr bigint_of_word32(uint32_t b);
  extern as_ptr bigint_mul(as_ptr a, as_ptr b);
  extern as_ptr bigint_pow(as_ptr a, as_ptr b);
  extern bool bigint_eq(as_ptr a, as_ptr b);

  printf("70**32 = 70**31 * 70: %s\n",
   bigint_eq(
    bigint_pow(bigint_of_word32(70), bigint_of_word32(32)),
    bigint_mul(
      bigint_pow(bigint_of_word32(70), bigint_of_word32(31)),
      bigint_of_word32(70)
	       )) ? "ok" : (ret = EXIT_FAILURE, "not ok"));

  /*
   * Testing UTF8
   */
  printf("Testing UTF8...\n");

  extern bool utf8_valid(const char*, size_t);
  const int cases = 33;
  const char* utf8_inputs[cases] = {
    "abcd",

    // from https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
    //
    // 3.5  Impossible bytes
    "\xfe",
    "\xff",

    // 4.1  Examples of an overlong ASCII character
    "\xc0\xaf",
    "\xe0\x80\xaf",
    "\xf0\x80\x80\xaf",
    "\xf8\x80\x80\x80\xaf",
    "\xfc\x80\x80\x80\x80\xaf",

    // 4.2  Maximum overlong sequences
    "\xc1\xbf",
    "\xe0\x9f\xbf",
    "\xf0\x8f\xbf\xbf",
    "\xf8\x87\xbf\xbf\xbf",
    "\xfc\x83\xbf\xbf\xbf\xbf",

    // 4.3  Overlong representation of the NUL character
    "\xc0\x80",
    "\xe0\x80\x80",
    "\xf0\x80\x80\x80",
    "\xf8\x80\x80\x80\x80",
    "\xfc\x80\x80\x80\x80\x80",

    // 5.1 Single UTF-16 surrogates
    "\xed\xa0\x80",
    "\xed\xad\xbf",
    "\xed\xae\x80",
    "\xed\xaf\xbf",
    "\xed\xb0\x80",
    "\xed\xbe\x80",
    "\xed\xbf\xbf",

    // 5.2 Paired UTF-16 surrogates
    "\xed\xa0\x80\xed\xb0\x80",
    "\xed\xa0\x80\xed\xbf\xbf",
    "\xed\xad\xbf\xed\xb0\x80",
    "\xed\xad\xbf\xed\xbf\xbf",
    "\xed\xae\x80\xed\xb0\x80",
    "\xed\xae\x80\xed\xbf\xbf",
    "\xed\xaf\xbf\xed\xb0\x80",
    "\xed\xaf\xbf\xed\xbf\xbf"
  };
  for (int i = 0; i < cases; ++i)
  {
    bool invalid = i > 0;
    assert( invalid != utf8_valid(utf8_inputs[i], strlen(utf8_inputs[i])),
            "%svalid UTF-8 test #%d failed\n", invalid ? "in" : "", i + 1);
  }

  /*
   * Testing the closure table
   */
  printf("Testing Closuretable ...\n");

  extern uint32_t remember_closure(as_ptr cls);
  extern uint32_t recall_closure(as_ptr cls);
  extern uint32_t closure_count();

  static int N = 2000; // >256, to exercise double_closure_table()
  // We remember and recall a bunch of closures,
  // and compare against a reference array.
  uint32_t reference[N];
  if (closure_count() != 0) {
    printf("Initial count wrong\n");
    ret = EXIT_FAILURE;
  }
  for (int i = 0; i<N; i++) {
     reference[i] = remember_closure((i<<2)-1);
     assert(closure_count() == i+1, "Closure count wrong\n");
  }
  for (int i = 0; i<N/2; i++) {
     assert((i<<2)-1 == recall_closure(reference[i]), "Recall went wrong\n");
     assert(closure_count() == N-i-1, "Closure count wrong\n");
  }
  for (int i = 0; i<N/2; i++) {
     reference[i] = remember_closure((i<<2)-1);
     assert(closure_count() == N/2 + i+1, "Closure count wrong\n");
  }
  for (int i = N-1; i>=0; i--) {
     assert((i<<2)-1 == recall_closure(reference[i]), "Recall went wrong\n");
     assert(closure_count() == i, "Closure count wrong\n");
  }

  /*
   * Testing 'IC:' scheme URL decoding
   */
  printf("Testing IC: URL...\n");

  extern as_ptr crc8_decode(as_ptr);
  as_ptr blob0 = alloc_blob(7);
  char* blob0p = (char*)BLOB_PAYLOAD(blob0);
  blob0p[0] = 'I';
  blob0p[1] = 'c';
  blob0p[2] = ':';
  blob0p[3] = blob0p[4] = blob0p[5] = blob0p[6] = '0';
  (void)crc8_decode(blob0);

  as_ptr blob1 = alloc_blob(7);
  char* blob1p = (char*)BLOB_PAYLOAD(blob1);
  memcpy(blob1p, "ic:C0FEFED00D41", 15);
  (void)crc8_decode(blob1);

  return ret;
}
