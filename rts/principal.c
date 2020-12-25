/*
Principal encoding and decoding with integrity checking.
*/
#include "rts.h"

typedef unsigned char uint8_t;

// Base32 encoding/decoding of blobs
//
// These routines assume contiguous memory layout.
// Primarily intended for encoding principals.

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
  uint32_t checksum = compute_crc32(b);
  size_t n = BLOB_LEN(b);
  uint8_t* data = (uint8_t *)BLOB_PAYLOAD(b);
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

static void accum_base32(struct Pump* pump, uint8_t c) {
  // tolerant conversion array
  // accepts lower case and fillers/padding '-', '='
  // values are one more than base32 value
  // 0 elements signify invalid alphabet letter
  // fillers/padding have upper bit set
  static const uint8_t conv[0x80] = {
    ['-'] = 0xF0, ['='] = 0xF1,
    ['A'] = 1, ['B'] = 2, ['C'] = 3, ['D'] = 4, ['E'] = 5, ['F'] = 6, ['G'] = 7, ['H'] = 8, ['I'] = 9, ['J'] = 10, ['K'] = 11,
    ['L'] = 12, ['M'] = 13, ['N'] = 14, ['O'] = 15, ['P'] = 16, ['Q'] = 17, ['R'] = 18, ['S'] = 19, ['T'] = 20, ['U'] = 21, ['V'] = 22,
    ['W'] = 23, ['X'] = 24, ['Y'] = 25, ['Z'] = 26,
    ['a'] = 1, ['b'] = 2, ['c'] = 3, ['d'] = 4, ['e'] = 5, ['f'] = 6, ['g'] = 7, ['h'] = 8, ['i'] = 9, ['j'] = 10, ['k'] = 11,
    ['l'] = 12, ['m'] = 13, ['n'] = 14, ['o'] = 15, ['p'] = 16, ['q'] = 17, ['r'] = 18, ['s'] = 19, ['t'] = 20, ['u'] = 21, ['v'] = 22,
    ['w'] = 23, ['x'] = 24, ['y'] = 25, ['z'] = 26,
    ['2'] = 27, ['3'] = 28, ['4'] = 29, ['5'] = 30, ['6'] = 31, ['7'] = 32
  };
  if (c > 'z') rts_trap_with("accum_base32: Base32 symbol out of range");
  uint8_t v = conv[c & 0x7F] - 1;
  if (v > 0xF1) rts_trap_with("accum_base32: Illegal base32 symbol");
  if (v < 0x20) {
    pump->pending_bits += pump->inp_gran;
    pump->pending_data <<= pump->inp_gran;
    pump->pending_data |= v;
  }
}

static inline void dec_stash(struct Pump* pump, uint8_t data) {
  accum_base32(pump, data);

  while (pump->pending_bits >= pump->out_gran) {
    pump->pending_bits -= pump->out_gran;
    *pump->dest++ = pump->pending_data >> pump->pending_bits;
    pump->pending_data &= (1 << pump->pending_bits) - 1;
  }
}

// Decode a base32 encoded blob into raw bytes
export blob_t base32_to_blob(blob_t b) {
  size_t n = BLOB_LEN(b);
  uint8_t* data = (uint8_t *)BLOB_PAYLOAD(b);
  blob_t r = alloc_blob((n + 7) / 8 * 5); // padding we deal with later
  uint8_t* dest = (uint8_t *)BLOB_PAYLOAD(r);

  struct Pump pump = { .inp_gran = 5, .out_gran = 8, .dest = dest };
  for (size_t i = 0; i < n; ++i) {
    dec_stash(&pump, *data++);
  }
  // adjust resulting blob length
  BLOB_LEN(r) = pump.dest - dest;
  return r;
}


// Convert a checksum-prepended base32 representation blob into the public
// principal name format by hyphenating and lowercasing
export blob_t base32_to_principal(blob_t b) {
  size_t n = BLOB_LEN(b);
  uint8_t* data = (uint8_t *)BLOB_PAYLOAD(b);
  blob_t r = alloc_blob((n + 4) / 5 * 6); // trailing hyphen we deal with later
  uint8_t* dest = (uint8_t *)BLOB_PAYLOAD(r);
  for (size_t i = 0; i < n; ++i) {
    *dest++ = *data++;
    // if uppercase, covert to lowercase
    if (dest[-1] >= 'A' && dest[-1] <= 'Z')
      dest[-1] += 'a' - 'A';
    // if quintet done, add hyphen
    if ((data - (uint8_t *)BLOB_PAYLOAD(b)) % 5 == 0 && i + 1 < n)
      *dest++ = '-';
  }
  // adjust result length
  BLOB_LEN(r) = dest - (uint8_t *)BLOB_PAYLOAD(r);
  return r;
}

// Encode a blob into its textual representation
export text_t principal_of_blob(blob_t b) {
  return base32_to_principal(base32_of_checksummed_blob(b));
}

// Decode an textual principal representation into a blob
export blob_t blob_of_principal(text_t t) {
  blob_t b0 = blob_of_text(t);
  blob_t bytes = base32_to_blob(b0);
  // Strip first four bytes
  if (BLOB_LEN(bytes) < 4) {
    rts_trap_with("blob_of_principal: principal too short");
  }
  blob_t stripped = alloc_blob(BLOB_LEN(bytes) - 4);
  memcpy(BLOB_PAYLOAD(stripped), BLOB_PAYLOAD(bytes) + 4, BLOB_LEN(bytes) - 4);
  // check encoding
  blob_t expected = principal_of_blob(stripped);
  if (blob_compare(b0, expected) != 0) {
    // rts_trap(BLOB_PAYLOAD(expected), BLOB_LEN(expected));
    rts_trap_with("blob_of_principal: invalid principal");
  }
  return stripped;
}
