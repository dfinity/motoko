#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "rts.h"
#include "buf.h"

typedef intptr_t as_ptr;


as_ptr alloc_bytes(size_t n) {
    void *ptr = malloc(n);
    if (ptr == NULL) { printf("OOM\n"); exit(1); };
    return ((as_ptr)ptr) - 1;
};

as_ptr alloc_words(size_t n) {
    return alloc_bytes(sizeof(size_t) * n);
};

export as_ptr alloc_array(uint32_t len) {
  // Array payload should not be larger than half of the memory.
  if (len > 1 << (32 - 2 - 1)) { // 2 for word size, 1 to divide by two
    rts_trap_with("Array allocation too large");
  }

  as_ptr a = alloc_words(ARRAY_HEADER_SIZE + len);
  TAG(a) = TAG_ARRAY;
  ARRAY_LEN(a) = len;
  return a;
}

void rts_trap(const char* str, size_t n) {
  printf("RTS trap: %.*s\n", (int)n, str);
  abort();
}
void bigint_trap(const char* str, size_t n) {
  printf("Bigint trap: %.*s\n", (int)n, str);
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

extern as_ptr bigint_of_word32(uint32_t b);
extern as_ptr bigint_sub(as_ptr a, as_ptr b);
extern as_ptr bigint_add(as_ptr a, as_ptr b);
extern as_ptr bigint_mul(as_ptr a, as_ptr b);
extern as_ptr bigint_pow(as_ptr a, as_ptr b);
extern as_ptr bigint_neg(as_ptr a);
extern bool bigint_eq(as_ptr a, as_ptr b);
extern int bigint_leb128_size(as_ptr n);
extern void bigint_leb128_encode(as_ptr n, unsigned char *buf);
extern as_ptr bigint_leb128_decode(buf *buf);
extern int bigint_sleb128_size(as_ptr n);
extern void bigint_sleb128_encode(as_ptr n, unsigned char *buf);
extern as_ptr bigint_sleb128_decode(buf *buf);


void test_bigint_leb128(as_ptr n) {
  unsigned char b[100];
  int s = bigint_leb128_size(n);
  bigint_leb128_encode(n, b);
  buf buf = { b, b + 100 };
  as_ptr n2 = bigint_leb128_decode(&buf);

  printf("roundtrip: %s ", bigint_eq(n, n2) ? "ok" : (ret = EXIT_FAILURE, "not ok"));
  printf("size: %s\n", (buf.p - b) == s ? "ok" : (ret = EXIT_FAILURE, "not ok"));
}

void test_bigint_sleb128(as_ptr n) {
  unsigned char b[100];
  int s = bigint_sleb128_size(n);
  bigint_sleb128_encode(n, b);
  buf buf = { b, b + 100 };
  as_ptr n2 = bigint_sleb128_decode(&buf);

  printf("roundtrip: %s ", bigint_eq(n, n2) ? "ok" : (ret = EXIT_FAILURE, "not ok"));
  printf("size: %s\n", (buf.p - b) == s ? "ok" : (ret = EXIT_FAILURE, "not ok"));
}

int main () {
  printf("Motoko RTS test suite\n");

  /*
   * Testing crc32
   */
  printf("Testing crc32...\n");

  extern uint32_t compute_crc32(blob_t);
  assert(
    compute_crc32(text_of_ptr_size("123456789", 9)) == 0xCBF43926,
    "crc32 of 123456789 mismatch\n");

  assert(
    compute_crc32(text_of_ptr_size("abcdefghijklmnop", 16)) == 0x943AC093,
    "crc32 of abcdefghijklmnop mismatch\n");

  /*
   * Testing base32
   */
  printf("Testing base32 encoding...\n");

  assert(
    text_compare(
     base32_of_checksummed_blob(text_of_ptr_size("123456789", 9)),
     text_of_ptr_size("ZP2DSJRRGIZTINJWG44DS", 21)
    ) == 0,
    "checksummed base32 of 123456789 mismatch\n");
  assert(
    text_compare(
     base32_of_checksummed_blob(text_of_ptr_size("abcdefghijklmnop", 16)),
     text_of_ptr_size("SQ5MBE3BMJRWIZLGM5UGS2TLNRWW433Q", 32)
    ) == 0,
    "checksummed base32 of abcdefghijklmnop mismatch\n");

  printf("Testing base32 decoding...\n");

  assert(
    text_compare(
     base32_to_blob(text_of_ptr_size("", 0)),
     text_of_ptr_size("", 0)
    ) == 0,
    "base32 to empty mismatch\n");

  assert(
    text_compare(
     base32_to_blob(text_of_ptr_size("GEZDGNBVGY3TQOI", 15)),
     text_of_ptr_size("123456789", 9)
    ) == 0,
    "base32 to 123456789 mismatch\n");

  assert(
    text_compare(
     base32_to_blob(text_of_ptr_size("MFRGGZDFMZTWQ2LKNNWG23TPOA", 26)),
     text_of_ptr_size("abcdefghijklmnop", 16)
    ) == 0,
    "base32 to abcdefghijklmnop mismatch\n");

  static char hex[7] = { 0x23, 0x3F, 0xF2, 0x06, 0xAB, 0xCD, 0x01 };
  assert(
    text_compare(
     base32_to_blob(text_of_ptr_size("em77e-bvlzu-aq", 14)),
     text_of_ptr_size(hex, sizeof hex)
    ) == 0,
    "checksummed base32 to em77e-bvlzu-aq mismatch\n");

  /*
   * Testing principal encoding
   */
  printf("Testing principal encoding...\n");

  extern blob_t principal_of_blob(blob_t);
  assert(
    text_compare(
     principal_of_blob(text_of_ptr_size("", 0)),
     text_of_ptr_size("aaaaa-aa", 8)
    ) == 0,
    "principal name to aaaaa-aa conversion mismatch\n");

  extern blob_t principal_of_blob(blob_t);
  assert(
    text_compare(
     principal_of_blob(text_of_ptr_size("\xC0\xFE\xFE\xD0\x0D", 5)),
     text_of_ptr_size("bfozs-kwa73-7nadi", 17)
    ) == 0,
    "principal name to bfozs-kwa73-7nadi conversion mismatch\n");

  /*
   * Testing princpal decoding
   */
  printf("Testing principal decoding...\n");

  assert(
    text_compare(
     blob_of_principal(text_of_cstr("aaaaa-aa")),
     text_of_ptr_size("",0)
    ) == 0,
    "aaaaa-aa not decoded correctly\n");

  assert(
    text_compare(
     blob_of_principal(text_of_cstr("bfozs-kwa73-7nadi")),
     text_of_ptr_size("\xC0\xFE\xFE\xD0\x0D",5)
    ) == 0,
    "bfozs-kwa73-7nadi not decoded correctly\n");

  return ret;
}
