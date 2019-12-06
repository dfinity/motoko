/*
The implementation of the Text type in Motoko.

One main goal of this datastructure (inspired by ropes and similar) is to
support linear time concatenation, but having a dedicated heap object for
the concatentation of two strings.

The current implementation does not do any this; the first goal is to wire up
this C code with the RTS that encapsulates the internals of strings.

This encapsulation will never be complete because
 * the compiler needs to emit static text literals,
 * the garbage collector needs to know some of the internals.

In a subsequent step, the actual concatentation node will be introduced.

From then on, there are stretch goals like:
 - when conctatenating short (<= 8 bytes maybe) strings, just copy them
 - restructure recursive code to not use unbounded C stack
 - maybe rebalancing
*/

#include "rts.h"

typedef as_ptr blob_t; // a skewed pointer to a Blob heap object
typedef as_ptr text_t; // a skewed pointer to a Blob (or, later, Concat) heap object

blob_t alloc_blob(size_t n) {
  as_ptr r = alloc_bytes (2*sizeof(void*) + n);
  TAG(r) = TAG_BLOB;
  BLOB_LEN(r) = n;
  return r;
}

// Create
export text_t text_of_ptr_size(const char *buf, size_t n) {
  as_ptr r = alloc_blob(n);
  as_memcpy(BLOB_PAYLOAD(r), buf, n);
  return r;
}

// Concat
export text_t text_concat(text_t s1, text_t s2) {
  uint32_t n1 = BLOB_LEN(s1);
  uint32_t n2 = BLOB_LEN(s2);
  as_ptr r = alloc_blob(n1 + n2);
  as_memcpy(BLOB_PAYLOAD(r), BLOB_PAYLOAD(s1), n1);
  as_memcpy(BLOB_PAYLOAD(r) + n1, BLOB_PAYLOAD(s2), n2);
  return r;
}

// put into contiguous memory, if needed (e.g. for system calls)
export blob_t blob_of_text(text_t s) {
  return s;
}

// Compare
export int text_compare(text_t s1, text_t s2) {
  uint32_t n1 = BLOB_LEN(s1);
  uint32_t n2 = BLOB_LEN(s2);
  uint32_t n = n1 < n2 ? n1 : n2;
  uint32_t r = as_memcmp(BLOB_PAYLOAD(s1), BLOB_PAYLOAD(s2), n);
  if (r == 0) {
    if (n1 < n2) { return -1; }
    else if (n1 > n2) { return 1; }
    else return 0;
  } else {
    return r;
  }
}

// Stuff that deals with characters

// decodes the characater at position in in the array
// returns the character, and updates n
// based on https://gist.github.com/tylerneylon/9773800
uint32_t decode_code_point(char *s, size_t *n) {
  if (s[*n] == 0) {
    rts_trap_with("decode_code_point: null byte encountered");
  }
  int k = __builtin_clz(~(s[*n] << 24));     // Count # of leading 1 bits.
  int mask = (1 << (8 - k)) - 1;             // All 1's with k leading 0's.
  uint32_t value = s[*n] & mask;
  for (++n, --k; k > 0; --k, ++n) {          // Note that k = #total bytes, or 0.
    value <<= 6;
    value += (s[*n] & 0x3F);
  }
  return value;
}

// Length in characters
export uint32_t text_len(text_t s) {
  char *p = BLOB_PAYLOAD(s);
  size_t n = 0;
  uint32_t c = 0;
  while (n < BLOB_LEN(s)) {
    int k = __builtin_clz(~(p[n] << 24));     // Count # of leading 1 bits.
    n += k + 1;
    c++;
  }
  return c;
}



// Iterators

// Currently a vanilla tuple:
// First component the array to the text
// Second the index into the array (shifted by two for GC's sake)
//
// TODO: do we have to worry about texts longer than 2^30 bytes
// (and thus shifting is // bad)
//
// Eventually, this will be a pointer into a tree or something.

typedef as_ptr text_iter_t; // the data structure used to iterate a text value
#define TEXT_ITER_TEXT(p) (ARRAY_FIELD(p,0))
#define TEXT_ITER_POS(p) (ARRAY_FIELD(p,1))


export text_iter_t text_iter(text_t s) {
  // Maybe use a dedicated heap type instead of a vanilla tuple?
  as_ptr i = alloc_words(ARRAY_HEADER_SIZE + 2);
  TAG(i) = TAG_ARRAY;
  TEXT_ITER_TEXT(i) = s;
  TEXT_ITER_POS(i) = 0;
  return i;
}

export uint32_t text_iter_done(text_iter_t i) {
  return TEXT_ITER_POS(i) >= BLOB_LEN(TEXT_ITER_TEXT(i));
}

export uint32_t text_iter_next(text_iter_t i) {
  if (text_iter_done(i)) {
    rts_trap_with("text_iter_next: Iter already done");
  }
  size_t n = TEXT_ITER_POS(i) >> 2;
  uint32_t c = decode_code_point(BLOB_PAYLOAD(TEXT_ITER_TEXT(i)), &n);
  TEXT_ITER_POS(i) = n << 2;
  return c;
}
