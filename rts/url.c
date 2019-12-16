/*
The URLs with conversions and integrity checking.
*/

#include "rts.h"

typedef as_ptr blob_t; // a skewed pointer to a Blob heap object
typedef as_ptr text_t; // a skewed pointer to a Blob (or, later, Concat) heap object
typedef unsigned char uint8_t;

static void check_all_uppercase_hex(const char* s, const char* const e) {
  if (s == e) return;
  const char c = *s;
  if ((c >= '0' && c <= '9')
   || (c >= 'A' && c <= 'F')) check_all_uppercase_hex(s + 1, e);
  else rts_trap_with("ic_url_decode: Not uppercase hex digit");
}

static void check_ci_schema(const char* s) {
  const char c0 = *s++;
  const char c1 = *s++;
  const char c2 = *s;
  if ((c0 == 'i' || c0 == 'I')
   && (c1 == 'c' || c1 == 'C')
   && c2 == ':') return;
  else rts_trap_with("ic_url_decode: Wrong URL schema (not 'ic:')");
}

static uint8_t compute_crc8(const uint8_t data[], uint32_t len) {
   uint8_t crc = 0;
   for (uint32_t i = 0; i < len; ++i) {
     crc ^= data[i];
     for (uint32_t j = 0; j < 8; ++j) {
       if (crc & 0x80)
         crc = (uint8_t)((crc << 1) ^ 0x7);
       else
         crc <<= 1;
     }
   }
   return crc;
 }

// assumption: uppercase hex
static uint8_t hex_digit(uint8_t c) {
  if (c >= '0' && c <= '9') return c - '0';
  else return c - 'A';
}

// CRC-8 from IC-URL
export blob_t crc8_decode(text_t s0) {
  uint32_t n = BLOB_LEN(s0);
  if (n < 3) rts_trap_with("ic_url_decode: Not an URL");
  const char* const s = BLOB_PAYLOAD(s0);
  const char* const e = s + n;
  check_ci_schema(s);
  const char* const hex = s + 3;
  uint32_t const hex_len = n - 5;
  check_all_uppercase_hex(hex, e);
  if (hex_len & 1) rts_trap_with("ic_url_decode: Not even number of hex digits");
  uint8_t crc = compute_crc8((const uint8_t*)hex, hex_len);
  uint8_t exp = hex_digit(*(e - 2)) << 4 | hex_digit(*(e - 1));
  if (crc != exp) {
    rts_trap_with("ic_url_decode: CRC-8 mismatch");
  }
  as_ptr r = alloc_blob(hex_len);
  as_memcpy(BLOB_PAYLOAD(r), hex, hex_len);
  return r;
}
