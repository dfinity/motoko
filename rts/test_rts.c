#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

typedef intptr_t as_ptr;

as_ptr alloc_bytes(size_t n) {
    void *ptr = malloc(n);
    if (ptr == NULL) { printf("OOM\n"); exit(1); };
    return ((as_ptr)ptr) - 1;
};

void idl_trap() {
  printf("IDL trap\n");
  exit(1);
}
void bigint_trap() {
  printf("Bigint trap\n");
  exit(1);
}

extern as_ptr bigint_of_word32(uint32_t b);
extern as_ptr bigint_mul(as_ptr a, as_ptr b);
extern as_ptr bigint_pow(as_ptr a, as_ptr b);
extern bool bigint_eq(as_ptr a, as_ptr b);


int main () {
  printf("ASC RTS test suite\n");

  printf("70**32 = 70**31 * 70: %s\n",
   bigint_eq(
    bigint_pow(bigint_of_word32(70), bigint_of_word32(32)),
    bigint_mul(
      bigint_pow(bigint_of_word32(70), bigint_of_word32(31)),
      bigint_of_word32(70)
    )) ? "ok" : "not ok");


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
    printf("%svalid UTF-8 test #%d: %s\n",
	   invalid ? "in" : "",
	   i + 1,
	   invalid != utf8_valid(utf8_inputs[i], strlen(utf8_inputs[i])) ? "ok" : "not ok");
  }
}
