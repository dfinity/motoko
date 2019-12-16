/*
The URLs with conversions and integrity checking.
*/

#include "rts.h"

typedef as_ptr blob_t; // a skewed pointer to a Blob heap object
typedef as_ptr text_t; // a skewed pointer to a Blob (or, later, Concat) heap object

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

// CRC-8 from IC-URL
export blob_t crc8_decode(text_t s0) {
  uint32_t n = BLOB_LEN(s0);
  if (n < 3) rts_trap_with("ic_url_decode: Not an URL");
  const char* const s = BLOB_PAYLOAD(s0);
  const char* const e = s + n;
  check_ci_schema(s);
  const char* const h = s + 3;
  check_all_uppercase_hex(h, e);
  if (n & 1) rts_trap_with("ic_url_decode: Not even number of hex digits");
  // TODO: check CRC
  as_ptr r = alloc_blob(n - 5);
  as_memcpy(BLOB_PAYLOAD(r), h, n - 5);
  return r;
}
