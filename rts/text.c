/*
The implementation of the Text type in Motoko.

One main goal of this datastructure (inspired by ropes and similar) is to
support constant time concatenation, by having a dedicated heap object for
the concatentation of two strings.

The current implementation does not do any of this; the first goal is to wire up
this C code with the RTS that encapsulates the internals of strings.

This encapsulation is not complete (and likely never will)
 * the compiler needs to emit static text literals,
 * the garbage collector needs to know some of the internals.

In a subsequent step, the actual concatentation node will be introduced.

From then on, there are stretch goals like:
 - when concatenating short (<= 8 bytes maybe) strings, just copy them
 - restructure recursive code to not use unbounded C stack
 - maybe rebalancing
*/

#include "rts.h"

typedef as_ptr blob_t; // a skewed pointer to a Blob heap object
typedef as_ptr text_t; // a skewed pointer to a Blob or Concat heap object

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

blob_t alloc_text_blob(size_t n) {
  if (n > MAX_STR_SIZE) {
    rts_trap_with("alloc_blob: Text too large");
  }
  return alloc_blob(n);
}

// Create
export text_t text_of_ptr_size(const char *buf, size_t n) {
  as_ptr r = alloc_text_blob(n);
  as_memcpy(BLOB_PAYLOAD(r), buf, n);
  return r;
}

// Concat
export text_t text_concat(text_t s1, text_t s2) {
  // empty strings are ignored
  if (BLOB_LEN(s1) == 0) return s2;
  if (BLOB_LEN(s2) == 0) return s1;
  uint32_t n1 = BLOB_LEN(s1);
  uint32_t n2 = BLOB_LEN(s2);
  uint32_t n = n1 + n2;
  // short text are copied into a single blob
  if (n < MIN_CONCAT_SIZE) {
    as_ptr r = alloc_text_blob(n1 + n2);
    as_memcpy(BLOB_PAYLOAD(r), BLOB_PAYLOAD(s1), n1);
    as_memcpy(BLOB_PAYLOAD(r) + n1, BLOB_PAYLOAD(s2), n2);
    return r;
  }
  // Check max size
  if (n > MAX_STR_SIZE) {
    rts_trap_with("text_concat: Text too large");
  }
  // Create concat node
  as_ptr r = alloc_words(CONCAT_WORDS);
  TAG(r) = TAG_CONCAT;
  CONCAT_LEN(r) = n;
  CONCAT_ARG1(r) = s1;
  CONCAT_ARG2(r) = s2;
  return r;
}

// write all data into a buffer (must have the right size)
export void text_to_buf(text_t s, char *buf) {
  if (TAG(s) == TAG_BLOB) {
    as_memcpy(buf, BLOB_PAYLOAD(s), BLOB_LEN(s));
  } else {
    text_to_buf(CONCAT_ARG1(s), buf);
    text_to_buf(CONCAT_ARG2(s), buf + BLOB_LEN(CONCAT_ARG1(s)));
  }
}

// straighten into contiguous memory, if needed (e.g. for system calls)
export blob_t blob_of_text(text_t s) {
  if (TAG(s) == TAG_BLOB) {
    return s;
  } else {
    as_ptr r = alloc_text_blob(CONCAT_LEN(s));
    text_to_buf(s, BLOB_PAYLOAD(r));
    return r;
  }
}

export uint32_t text_size(text_t s) {
  return BLOB_LEN(s);
}

// Compare
export int blob_compare(text_t s1, text_t s2) {
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

// compares the texts from the given offset on for the given number of bytes
// all assumed to be in range
int text_compare_range(text_t s1, size_t offset1, text_t s2, size_t offset2, size_t n) {
  // strip off left legs if range is in the right leg
  if (TAG(s1) == TAG_CONCAT && BLOB_LEN(CONCAT_ARG1(s1)) <= offset1) {
    return text_compare_range(CONCAT_ARG2(s1), offset1 - BLOB_LEN(CONCAT_ARG1(s1)), s2, offset2, n);
  }
  if (TAG(s2) == TAG_CONCAT && BLOB_LEN(CONCAT_ARG1(s2)) <= offset2) {
    return text_compare_range(s1, offset1, CONCAT_ARG2(s2), offset2 - BLOB_LEN(CONCAT_ARG1(s2)), n);
  }
  // strip off rights legs if range is in the left leg
  if (TAG(s1) == TAG_CONCAT && BLOB_LEN(CONCAT_ARG1(s1)) >= offset1 + n ) {
    return text_compare_range(CONCAT_ARG1(s1), offset1, s2, offset2, n);
  }
  if (TAG(s2) == TAG_CONCAT && BLOB_LEN(CONCAT_ARG1(s2)) >= offset2 + n) {
    return text_compare_range(s1, offset1, CONCAT_ARG1(s2), offset2, n);
  }
  // Decompose concats
  if (TAG(s1) == TAG_CONCAT) {
    uint32_t n1 = BLOB_LEN(CONCAT_ARG1(s1)) - offset1;
    int r1 = text_compare_range(CONCAT_ARG1(s1), offset1, s2, offset2, n1);
    if (r1 != 0) return r1;
    else return text_compare_range(CONCAT_ARG2(s1), 0, s2, offset2 + n1, n - n1);
  }
  if (TAG(s2) == TAG_CONCAT) {
    uint32_t n1 = BLOB_LEN(CONCAT_ARG1(s2)) - offset2;
    int r1 = text_compare_range(s1, offset1, CONCAT_ARG1(s2), offset2, n1);
    if (r1 != 0) return r1;
    else return text_compare_range(s1, offset1 + n1, CONCAT_ARG2(s2), 0, n - n1);
  }
  // now both are blobs
  return as_memcmp(BLOB_PAYLOAD(s1) + offset1, BLOB_PAYLOAD(s2) + offset2, n);
}

export int text_compare(text_t s1, text_t s2) {
  uint32_t n1 = BLOB_LEN(s1);
  uint32_t n2 = BLOB_LEN(s2);
  uint32_t n = n1 < n2 ? n1 : n2;
  int r = text_compare_range(s1, 0, s2, 0, n);
  if (r != 0) return r;
  if (n1 > n) return 1;
  if (n2 > n) return -1;
  return 0;
}

// Stuff that deals with characters

// decodes the character at pointer
// returns the character, the size via the out parameter
uint32_t decode_code_point(char *s, size_t *n) {
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



// Iterators

// The iterator needs to point to a specific position in the tree
//
// This is currently a simple triple:
// 1. a pointer to a current leave (must be a BLOB)
// 2. index into that blob (shifted by two for GC's sake)
// 3. 0, or a pointer to a linked list of non-empty text values to do next
//
// The linked list (text_cont_t) is a tuple with
// 1. a pointer to the text_t
// 2. 0, or a pointer to the next list entry
//

typedef as_ptr text_iter_cont_t;
#define TEXT_CONT_TEXT(p) (TUPLE_FIELD(p,0,text_t))
#define TEXT_CONT_NEXT(p) (TUPLE_FIELD(p,1,text_iter_cont_t))

typedef as_ptr text_iter_t; // the data structure used to iterate a text value
#define TEXT_ITER_BLOB(p) (TUPLE_FIELD(p,0,blob_t))
#define TEXT_ITER_POS(p) (TUPLE_FIELD(p,1,uint32_t))
#define TEXT_ITER_TODO(p) (TUPLE_FIELD(p,2,text_iter_cont_t))


// A “smart constructor” that ensures finds the left-most leaf,
// putting all others onto the list, used to enforce the invarinat
// about TEXT_ITER_BLOB to be a blob.
as_ptr find_leaf(text_t s, text_iter_cont_t *todo) {
  while (TAG(s) == TAG_CONCAT) {
    as_ptr c = alloc_words(ARRAY_HEADER_SIZE + 2);
    TAG(c) = TAG_ARRAY;
    TEXT_CONT_TEXT(c) = CONCAT_ARG2(s);
    TEXT_CONT_NEXT(c) = *todo;
    *todo = c;
    s = CONCAT_ARG1(s);
  }
  return s;
}

export text_iter_t text_iter(text_t s) {
  as_ptr i = alloc_words(ARRAY_HEADER_SIZE + 3);
  TAG(i) = TAG_ARRAY;
  TEXT_ITER_POS(i) = 0;
  TEXT_ITER_TODO(i) = 0;
  TEXT_ITER_BLOB(i) = find_leaf(s, &(TEXT_ITER_TODO(i)));
  return i;
}

export uint32_t text_iter_done(text_iter_t i) {
  return (TEXT_ITER_POS(i) >> 2) >= BLOB_LEN(TEXT_ITER_BLOB(i))
      && TEXT_ITER_TODO(i) != 0;
}

// pointer into the leaf at the given byte position
char *text_pos(text_t s, size_t offset) {
  if (TAG(s) == TAG_BLOB) return (BLOB_PAYLOAD(s) + offset);
  uint32_t n1 = BLOB_LEN(CONCAT_ARG1(s));
  if (offset < n1) return text_pos(CONCAT_ARG1(s), offset);
  else             return text_pos(CONCAT_ARG2(s), offset - n1);
}

export uint32_t text_iter_next(text_iter_t i) {
  size_t n = TEXT_ITER_POS(i) >> 2;
  text_t s = TEXT_ITER_BLOB(i);

  // If we are at the end, find the next iterator to use
  if (n >= BLOB_LEN(s)) {
    text_iter_cont_t c = TEXT_ITER_TODO(i);
    // this one is done, try next
    // are we done?
    if (c == 0) rts_trap_with("text_iter_next: Iter already done");
    // if next one is a concat node, re-use both text iterator structures
    // (avoids an allocation)
    if (TAG(s) == TAG_CONCAT) {
      TEXT_ITER_POS(i) = 0;
      TEXT_CONT_TEXT(i) = CONCAT_ARG2(s);
      TEXT_ITER_BLOB(i) = find_leaf(CONCAT_ARG1(s), &(TEXT_ITER_TODO(i)));
      return text_iter_next(i);
    // else remove that entry from the chain
    } else {
      TEXT_ITER_BLOB(i) = s;
      TEXT_ITER_POS(i) = 0;
      TEXT_ITER_TODO(i) = TEXT_CONT_NEXT(c);
      return text_iter_next(i);
    }
  }

  size_t step = 0;
  uint32_t c = decode_code_point(BLOB_PAYLOAD(s) + n, &step);
  TEXT_ITER_POS(i) = (n+step) << 2;
  return c;
}
