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
   * Testing BigInt
   */
  printf("Testing BigInt...\n");

  printf("70**32 = 70**31 * 70: %s\n",
   bigint_eq(
    bigint_pow(bigint_of_word32(70), bigint_of_word32(32)),
    bigint_mul(
      bigint_pow(bigint_of_word32(70), bigint_of_word32(31)),
      bigint_of_word32(70)
	       )) ? "ok" : (ret = EXIT_FAILURE, "not ok"));

  /*
   * Testing BigInt (s)leb128 encoding
   */
  as_ptr one = bigint_of_word32(1);
  as_ptr two = bigint_of_word32(2);
  for (uint32_t i = 0; i < 100; i++){
    as_ptr two_pow_i = bigint_pow(two, bigint_of_word32(i));
    printf("leb128 2^%u-1: ", i);
    test_bigint_leb128(bigint_sub(two_pow_i, one));
    printf("leb128 2^%u:   ", i);
    test_bigint_leb128(two_pow_i);
    printf("leb128 2^%u+1: ", i);
    test_bigint_leb128(bigint_add(two_pow_i, one));
  }

  for (uint32_t i = 0; i < 100; i++){
    as_ptr two_pow_i = bigint_pow(two, bigint_of_word32(i));
    printf("sleb128  2^%u-1: ", i);
    test_bigint_sleb128(bigint_sub(two_pow_i, one));
    printf("sleb128  2^%u:   ", i);
    test_bigint_sleb128(two_pow_i);
    printf("sleb128  2^%u+1: ", i);
    test_bigint_sleb128(bigint_add(two_pow_i, one));
    printf("sleb128 -2^%u-1: ", i);
    test_bigint_sleb128(bigint_neg(bigint_sub(two_pow_i, one)));
    printf("sleb128 -2^%u:   ", i);
    test_bigint_sleb128(bigint_neg(two_pow_i));
    printf("sleb128 -2^%u+1: ", i);
    test_bigint_sleb128(bigint_neg(bigint_add(two_pow_i, one)));
  }

  /*
   * Testing UTF8
   */
  printf("Testing UTF8...\n");

  extern bool utf8_valid(const char*, size_t);
  const char* utf8_inputs[] = {
    "abcd",

    // issue 1208
    " \xe2\x96\x88 ",

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
  const int cases = sizeof utf8_inputs / sizeof *utf8_inputs;
  for (int i = 0; i < cases; ++i)
  {
    bool invalid = i >= 2; // the first two tests should pass
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
