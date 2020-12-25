/*
The implementation of the Text type in Motoko.

One main goal of this datastructure (inspired by ropes and similar) is to
support constant time concatenation, by having a dedicated heap object for
the concatenation of two strings.

The first goal was to wire up
this C code with the RTS that encapsulates the internals of strings.

This encapsulation is not complete (and likely never will)
 * the compiler needs to emit static text literals,
 * the garbage collector needs to know some of the internals.

In a subsequent step, the actual concatenation node has been introduced.

From here on, there are stretch goals like:
 - restructure recursive code to not use unbounded C stack
 - maybe rebalancing
*/

#include "rts.h"

/*
Layout of a concat node:

     ┌─────┬─────────┬───────┬───────┐
     │ tag │ n_bytes │ text1 │ text2 │
     └─────┴─────────┴───────┴───────┘

Note that CONCAT_LEN and BLOB_LEN are identical, so no need to check the
tag to know the size of the text.
*/

#define CONCAT_WORDS 4
#define CONCAT_LEN(p) (FIELD(p,1))
#define CONCAT_ARG1(p) (FIELD(p,2))
#define CONCAT_ARG2(p) (FIELD(p,3))


#define MAX_STR_SIZE ((1<<30)-1)
// strings smaller than this _must_ be blobs
// You can set this to MAX_STR_SIZE to disable the use of ropes completely,
// e.g. for debugging
#define MIN_CONCAT_SIZE (9)

text_t text_of_cstr(const char * const s) {
  size_t l = strlen(s);
  return text_of_ptr_size(s, l);
}

extern blob_t alloc_text_blob(size_t n);

// Stuff that deals with characters

// decodes the character at pointer
// returns the character, the size via the out parameter
export uint32_t decode_code_point(char *s, size_t *n) {
  *n = 0;
  int k = s[*n] ? __builtin_clz(~(s[*n] << 24)) : 0; // Count # of leading 1 bits.
  int mask = (1 << (8 - k)) - 1;                     // All 1's with k leading 0's.
  uint32_t value = s[*n] & mask;
  for (++*n, --k; k > 0; --k, ++*n) {                // Note that k = 0 or #total bytes
    value <<= 6;
    value += (s[*n] & 0x3F);
  }
  return value;
}

// Length in characters
export uint32_t text_len(text_t s) {
  if (TAG(s) == TAG_BLOB) {
    char *p = BLOB_PAYLOAD(s);
    size_t n = 0;
    uint32_t c = 0;
    while (n < BLOB_LEN(s)) {
      int k = p[n] ? __builtin_clz(~(p[n] << 24)) : 0;     // Count # of leading 1 bits.
      n += k ? k : 1;
      c += 1;
    }
    return c;
  } else {
    return text_len(CONCAT_ARG1(s)) + text_len(CONCAT_ARG2(s));
  }
}

// Text from Char
export text_t text_singleton(uint32_t code) {
  // adapted from https://gist.github.com/tylerneylon/9773800
  char val[4];
  int lead_byte_max = 0x7F;
  int val_index = 0;
  while (code > lead_byte_max) {
    val[val_index++] = (code & 0x3F) | 0x80;
    code >>= 6;
    lead_byte_max >>= (val_index == 1 ? 2 : 1);
  }
  val[val_index++] = (code & lead_byte_max) | (~lead_byte_max << 1);

  as_ptr r = alloc_text_blob(val_index);
  char *p = BLOB_PAYLOAD(r);
  while (val_index--) {
    *p = val[val_index];
    p++;
  }
  return r;
}
