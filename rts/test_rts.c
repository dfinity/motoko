#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

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
}
