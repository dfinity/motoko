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

static inline uint32_t compute_crc32(const unsigned char data[], size_t len) {
  extern uint32_t crc_32(const unsigned char*, size_t);
  return crc_32(data, len);
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

struct Pump {
  const int inp_gran, out_gran;
  uint8_t *dest;
  uint32_t pending_data, pending_bits;
};

static void stash_enc_base32(uint8_t d, uint8_t* dest) {
  *dest = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"[d & 0x1F];
}

static inline void enc_stash(struct Pump* pump, uint8_t data) {
  pump->pending_data <<= pump->inp_gran;
  pump->pending_data |= data;
  pump->pending_bits += pump->inp_gran;

  while (pump->pending_bits >= pump->out_gran) {
    pump->pending_bits -= pump->out_gran;
    stash_enc_base32(pump->pending_data >> pump->pending_bits, pump->dest++);
    pump->pending_data &= (1 << pump->pending_bits) - 1;
  }
}

// Encode a blob into an checksum-prepended base32 representation
export blob_t base32_of_checksummed_blob(blob_t b) {
  size_t n = BLOB_LEN(b);
  uint8_t* data = (uint8_t *)BLOB_PAYLOAD(b);
  uint32_t checksum = compute_crc32(data, n);
  blob_t r = alloc_blob((n + sizeof checksum + 4) / 5 * 8); // contains padding
  uint8_t* dest = (uint8_t *)BLOB_PAYLOAD(r);

  struct Pump pump = { .inp_gran = 8, .out_gran = 5, .dest = dest };
  enc_stash(&pump, checksum >> 24); // checksum is serialised as big-endian
  enc_stash(&pump, checksum >> 16);
  enc_stash(&pump, checksum >> 8);
  enc_stash(&pump, checksum);
  for (size_t i = 0; i < n; ++i)
    enc_stash(&pump, *data++);
  if (pump.pending_bits) {
    // flush odd bits
    pump.pending_data <<= pump.out_gran - pump.pending_bits;
    stash_enc_base32(pump.pending_data, pump.dest++);
    // discount padding
    BLOB_LEN(r) = pump.dest - dest;
  }
  return r;
}

static void accum_base32(struct Pump* pump, uint8_t d) {
  // tolerant conversion array
  // accepts lower case and fillers/padding '-', '='
  // values are one more than base32 value
  // 0 elements signify invalid alphabet letter
  // fillers/padding have upper bit set
  const uint8_t conv[0x80] = {
    ['-'] = 0xF0, ['='] = 0xF1,
    ['A'] = 1, ['B'] = 2, ['C'] = 3, ['D'] = 4, ['E'] = 5, ['F'] = 6, ['G'] = 7, ['H'] = 8, ['I'] = 9, ['J'] = 10, ['K'] = 11,
    ['L'] = 12, ['M'] = 13, ['N'] = 14, ['O'] = 15, ['P'] = 16, ['Q'] = 17, ['R'] = 18, ['S'] = 19, ['T'] = 20, ['U'] = 21, ['V'] = 22,
    ['W'] = 23, ['X'] = 24, ['Y'] = 25, ['Z'] = 26,
    ['a'] = 1, ['b'] = 2, ['c'] = 3, ['d'] = 4, ['e'] = 5, ['f'] = 6, ['g'] = 7, ['h'] = 8, ['i'] = 9, ['j'] = 10, ['k'] = 11,
    ['l'] = 12, ['m'] = 13, ['n'] = 14, ['o'] = 15, ['p'] = 16, ['q'] = 17, ['r'] = 18, ['s'] = 19, ['t'] = 20, ['u'] = 21, ['v'] = 22,
    ['w'] = 23, ['x'] = 24, ['y'] = 25, ['z'] = 26,
    ['2'] = 27, ['3'] = 28, ['4'] = 29, ['5'] = 30, ['6'] = 31, ['7'] = 32
  };
  uint8_t v = conv[d & 0x7F] - 1;
  if (v < 0x20) {
    pump->pending_bits += pump->inp_gran;
    pump->pending_data <<= pump->inp_gran;
    pump->pending_data |= v;
  }
}
#include <stdio.h>
static inline void dec_stash(struct Pump* pump, uint8_t data) {
  accum_base32(pump, data);

  while (pump->pending_bits >= pump->out_gran) {
    pump->pending_bits -= pump->out_gran;
    *pump->dest++ = pump->pending_data >> pump->pending_bits;
    printf("stashed %c...\n", pump->dest[-1]);

    pump->pending_data &= (1 << pump->pending_bits) - 1;
  }
}

// Decode a checksum-prepended base32 blob into text representation
export blob_t base32_to_checksummed_blob(blob_t b, uint32_t* checksum) {
  size_t n = BLOB_LEN(b);
  uint8_t* data = (uint8_t *)BLOB_PAYLOAD(b);
  blob_t r = alloc_blob((n /*+ sizeof *checksum*/ + 7) / 8 * 5); // padding we deal with later
  uint8_t* dest = (uint8_t *)BLOB_PAYLOAD(r);

  struct Pump pump = { .inp_gran = 5, .out_gran = 8, .dest = dest };
  for (size_t i = 0; i < n; ++i)
    dec_stash(&pump, *data++);
  BLOB_LEN(r) = pump.dest - dest;
  return r;
}
