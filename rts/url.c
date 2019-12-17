/*
The URLs with conversions and integrity checking.
*/

#include "rts.h"

typedef as_ptr blob_t; // a skewed pointer to a Blob heap object
typedef as_ptr text_t; // a skewed pointer to a Blob (or, later, Concat) heap object
typedef unsigned char uint8_t;

static void check_all_uppercase_hex(const char* s, const char* const e) {
  while (s != e) {
    const char c = *s++;
    if ((c >= '0' && c <= '9')
	|| (c >= 'A' && c <= 'F')) continue;
    else rts_trap_with("ic_url_decode: Not all uppercase hex digit");
  }
}

static void check_ci_scheme(const char* s) {
  const char c0 = *s++;
  const char c1 = *s++;
  const char c2 = *s;
  if ((c0 == 'i' || c0 == 'I')
   && (c1 == 'c' || c1 == 'C')
   && c2 == ':') return;
  else rts_trap_with("ic_url_decode: Wrong URL scheme (not 'ic:')");
}

// assumption: uppercase hex
static uint8_t hex_digit(uint8_t c) {
  if (c >= '0' && c <= '9') return c - '0';
  else return c - 'A' + 0xA;
}

// assumption: uppercase hex
static uint8_t hex_byte(const char* h) {
  return hex_digit(h[0]) << 4 | hex_digit(h[1]);
}

// assumption: uppercase hex, len is even
static uint8_t compute_crc8(const char data[], size_t len) {
  uint8_t crc = 0;
  for (size_t i = 0; i < len; i += 2) {
    crc ^= hex_byte(data + i);
    for (size_t j = 0; j < 8; ++j) {
      if (crc & 0x80)
	crc = (uint8_t)((crc << 1) ^ 0x7);
      else
	crc <<= 1;
    }
  }
  return crc;
}

// CRC-8 from IC-URL
export blob_t crc8_decode(text_t t) {
  blob_t b0 = blob_of_text(t);
  size_t n = BLOB_LEN(b0);
  if (n < 3) rts_trap_with("ic_url_decode: Not an URL");
  const char* const s = BLOB_PAYLOAD(b0);
  const char* const e = s + n;
  check_ci_scheme(s);
  const char* hex = s + 3; // skip over "ic:"
  size_t hex_len = n - 5; // strip "ic:" and 2 last digits
  check_all_uppercase_hex(hex, e);
  if (hex_len & 1) rts_trap_with("ic_url_decode: Not an even number of hex digits");
  uint8_t crc = compute_crc8(hex, hex_len);
  uint8_t exp = hex_byte(e - 2);
  if (crc != exp) {
    rts_trap_with("ic_url_decode: CRC-8 mismatch");
  }
  as_ptr r = alloc_blob(hex_len / 2);
  for (char *bytes = BLOB_PAYLOAD(r); hex_len; hex += 2, hex_len -= 2) {
    *bytes++ = (char)hex_byte(hex);
  }
  return r;
}
