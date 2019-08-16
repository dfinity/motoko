#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "buf.h"

typedef intptr_t as_ptr;

as_ptr alloc_bytes(size_t n) {
    void *ptr = malloc(n);
    if (ptr == NULL) { printf("OOM\n"); exit(1); };
    return ((as_ptr)ptr) - 1;
};

void idl_trap() {
  printf("IDL trap\n");
  abort();
}
void bigint_trap() {
  printf("Bigint trap\n");
  abort();
}

extern void leb128_encode(uint32_t n, unsigned char *buf);
extern uint32_t read_u32_of_leb128(buf *buf);
extern void sleb128_encode(int32_t n, unsigned char *buf);
extern int32_t read_i32_of_sleb128(buf *buf);


int main(int argc, char** argv) {
  printf("ASC LEB128 round-trip tests\n");

  if (argc < 2) {
    printf("Usage: %s inputfile\n", argv[0]);
    return 1;
  }

  // read file
  FILE *f = fopen(argv[1], "rb");
  if (f == NULL) {
      fprintf(stderr, "Error opening file %s: %s\n", argv[1], strerror( errno ));
      exit(1);
  }
  fseek(f, 0, SEEK_END);
  long input_size = ftell(f);
  fseek(f, 0, SEEK_SET);  /* same as rewind(f); */
  char *input = malloc(input_size);
  fread(input, 1, input_size, f);
  fclose(f);

  // read word
  if (input_size != 1+sizeof(uint32_t)) {
    printf("Input file should have %zu bytes", 1+sizeof(uint32_t));
    free(input);
    return 1;
  }

  uint8_t sign = *input;

  uint8_t b[100];
  buf buf = { b, b + 100 };

  if (sign) {
    int32_t i = *((int32_t *)(input+1));
    sleb128_encode(i, b);
    int32_t o = read_i32_of_sleb128(&buf);
    printf("Sign: %d Input: %d Output: %d\n", sign, i, o);
    if (i != o) { abort(); }
  } else {
    uint32_t i = *((uint32_t *)(input+1));
    leb128_encode(i, b);
    uint32_t o = read_u32_of_leb128(&buf);
    printf("Sign: %d Input: %u Output: %u\n", sign, i, o);
    if (i != o) { abort(); }
  }
  printf("Encoding: ");
  for (uint8_t *p = b; p < buf.p; p++) printf("%02X", *p);
  printf("\n");

  free(input);
  return 0;
}


