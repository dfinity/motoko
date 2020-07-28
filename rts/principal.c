/*
Principal encoding and decoding with integrity checking.
*/
#include "rts.h"

typedef unsigned char uint8_t;

// CRC32 for blobs

// loosely based on https://rosettacode.org/wiki/CRC-32#Implementation_2
//
export uint32_t compute_crc32(blob_t b)
{
  if (TAG(b) != TAG_BLOB) rts_trap_with("compute_crc32: Blob expected");

  uint32_t crc = 0;
  const char *buf = BLOB_PAYLOAD(b);
  size_t len = BLOB_LEN(b);

  static uint32_t const table[256] = {
    0x0, 0x77073096, 0xee0e612c, 0x990951ba, 0x76dc419, 0x706af48f, 0xe963a535, 0x9e6495a3, 0xedb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988, 0x9b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91, 
    0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de, 0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7, 0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9, 0xfa0f3d63, 0x8d080df5, 
    0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172, 0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b, 0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59, 
    0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423, 0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924, 0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d, 
    0x76dc4190, 0x1db7106, 0x98d220bc, 0xefd5102a, 0x71b18589, 0x6b6b51f, 0x9fbfe4a5, 0xe8b8d433, 0x7807c9a2, 0xf00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x86d3d2d, 0x91646c97, 0xe6635c01, 
    0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950, 0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65, 
    0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9, 
    0x5005713c, 0x270241aa, 0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f, 0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad, 
    0xedb88320, 0x9abfb3b6, 0x3b6e20c, 0x74b1d29a, 0xead54739, 0x9dd277af, 0x4db2615, 0x73dc1683, 0xe3630b12, 0x94643b84, 0xd6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0xa00ae27, 0x7d079eb1, 
    0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb, 0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc, 0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5, 
    0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b, 0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55, 0x316e8eef, 0x4669be79, 
    0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236, 0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe, 0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d, 
    0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x26d930a, 0x9c0906a9, 0xeb0e363f, 0x72076785, 0x5005713, 0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0xcb61b38, 0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0xbdbdf21, 
    0x86d3d2d4, 0xf1d4e242, 0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777, 0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45, 
    0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc, 0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9, 
    0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693, 0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94, 0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
  };

  crc = ~crc;
  const char *q = buf + len;
  for (const char *p = buf; p < q; p++) {
    uint8_t octet = *p;  /* Cast to unsigned octet. */
    crc = (crc >> 8) ^ table[(crc & 0xff) ^ octet];
  }

  return ~crc;
}

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

// Encode a blob into an textual representation
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
  as_memcpy(BLOB_PAYLOAD(stripped), BLOB_PAYLOAD(bytes) + 4, BLOB_LEN(bytes) - 4);
  // check encoding
  blob_t expected = principal_of_blob(stripped);
  if (blob_compare(b0, expected) != 0) {
    // rts_trap(BLOB_PAYLOAD(expected), BLOB_LEN(expected));
    rts_trap_with("blob_of_principal: invalid principal");
  }
  return stripped;
}
