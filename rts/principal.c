/*
Principal encoding and decoding with integrity checking.
*/
#include "rts.h"

typedef unsigned char uint8_t;

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
