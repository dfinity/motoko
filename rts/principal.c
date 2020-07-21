/*
Prinicpal encoding and decoding with integrity checking.
*/

#include "rts.h"

typedef as_ptr blob_t; // a skewed pointer to a Blob heap object
typedef as_ptr text_t; // a skewed pointer to a text heap object
typedef unsigned char uint8_t;

static void check_all_uppercase_hex(const char* s, const char* const e) {
  while (s != e) {
    const char c = *s++;
    if ((c >= '0' && c <= '9')
	|| (c >= 'A' && c <= 'F')) continue;
    else rts_trap_with("blob_of_principal: Not all uppercase hex digit");
  }
}

static void check_ci_scheme(const char* s) {
  const char c0 = *s++;
  const char c1 = *s++;
  const char c2 = *s;
  if ((c0 == 'i' || c0 == 'I')
   && (c1 == 'c' || c1 == 'C')
   && c2 == ':') return;
  else rts_trap_with("blob_of_principal: Wrong URL scheme (not 'ic:')");
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

static uint8_t compute_crc8(const char data[], size_t len) {
  uint8_t crc = 0;
  for (size_t i = 0; i < len; i++) {
    crc ^= (uint8_t)(data[i]);
    for (size_t j = 0; j < 8; ++j) {
      if (crc & 0x80)
	crc = (uint8_t)((crc << 1) ^ 0x7);
      else
	crc <<= 1;
    }
  }
  return crc;
}

static uint32_t compute_crc32(const char data[], size_t len) {
  extern uint32_t crc_32(const unsigned char*, size_t);
  return crc_32((const unsigned char*)data, len);
}

// Decode an textual principal representation into a blob
export blob_t blob_of_principal(text_t t) {
  blob_t b0 = blob_of_text(t);
  size_t n = BLOB_LEN(b0);
  if (n < 5) rts_trap_with("ic_blob_of_principal: too short for an ic: URL");
  const char* const s = BLOB_PAYLOAD(b0);
  const char* const e = s + n;
  check_ci_scheme(s);
  const char* hex = s + 3; // skip over "ic:"
  size_t hex_len = n - 5; // strip "ic:" and 2 last digits
  check_all_uppercase_hex(hex, e);
  if (hex_len & 1) rts_trap_with("ic_blob_of_principal: Not an even number of hex digits");
  as_ptr r = alloc_blob(hex_len / 2);
  for (char *bytes = BLOB_PAYLOAD(r); hex_len; hex += 2, hex_len -= 2) {
    *bytes++ = (char)hex_byte(hex);
  }
  uint8_t crc = compute_crc8(BLOB_PAYLOAD(r), (n-5)/2);
  uint8_t exp = hex_byte(e - 2);
  if (crc != exp) {
    rts_trap_with("ic_blob_of_principal: CRC-8 mismatch");
  }
  return r;
}

static char to_hex_digit(uint8_t n) {
  if (n < 10) return '0' + n;
  if (n < 16) return 'A' + (n - 10);
  rts_trap_with("to_hex_digit: out of range");
}

// Encode a blob into an textual representation
export text_t principal_of_blob(blob_t b) {
  size_t n = BLOB_LEN(b);
  as_ptr r = alloc_blob(3 + 2*n + 2);
  uint8_t *p = (uint8_t *)BLOB_PAYLOAD(r);
  uint8_t *q = (uint8_t *)BLOB_PAYLOAD(b);
  as_memcpy((char *)p, "ic:", 3);
  p += 3;
  for (;n>0; n--) {
    *p++ = to_hex_digit(*q >> 4);
    *p++ = to_hex_digit(*q % 16);
    q++;
  }
  uint8_t checksum = compute_crc8(BLOB_PAYLOAD(b), BLOB_LEN(b));
  *p++ = to_hex_digit(checksum >> 4);
  *p++ = to_hex_digit(checksum % 16);
  return r;
}

static void stash_base32_group(uint64_t g, int chars, uint8_t* dest) {
  for (int c = chars; c >= 0; --c) {
    dest[c] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"[g & 0x1F];
    g >>= 5;
  }
}

static uint64_t pickup(int bytes, uint8_t* data) {
  uint64_t g = 0;
  for (int i = 0; i < bytes; ++i)
    g = g << 8 | *data++;
  return g;
}

// prepends (big-endian) crc32 checksum of b to b and returns
// it base32 encoded without padding
export blob_t base32_of_checksummed_blob(blob_t b) {
  size_t n = BLOB_LEN(b);
  uint8_t* data = (uint8_t *)BLOB_PAYLOAD(b);
  uint32_t checksum = compute_crc32(BLOB_PAYLOAD(b), n);
  blob_t r = alloc_blob((n + sizeof checksum + 4) / 5 * 8); // TODO: minus padding
  
  uint8_t* dest = (uint8_t *)BLOB_PAYLOAD(r);
  if (!n) {
    stash_base32_group((uint64_t)checksum << 3, 7, dest);
    return r;
  }
  stash_base32_group((uint64_t)checksum << 8 | *data++, 8, dest);
  --n;
  dest += 8;
  for (size_t i = 0; i < n; i += 5, data += 5, dest += 8) {
    switch (n - i) {
    case 1: stash_base32_group(*data, 2, dest); return r;
    case 2: stash_base32_group(pickup(2, data) << 4, 4, dest); return r;
    case 3: stash_base32_group(pickup(3, data) << 1, 5, dest); return r;
    case 4: stash_base32_group(pickup(4, data) << 3, 7, dest); return r;
    default: stash_base32_group(pickup(5, data), 8, dest);
    }
  }
  return r;
}
