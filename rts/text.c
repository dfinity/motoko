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

static blob_t alloc_text_blob(size_t n) {
  if (n > MAX_STR_SIZE) {
    rts_trap_with("alloc_blob: Text too large");
  }
  return alloc_blob(n);
}

// Create
export text_t text_of_ptr_size(const char *buf, size_t n) {
  as_ptr r = alloc_text_blob(n);
  memcpy(BLOB_PAYLOAD(r), buf, n);
  return r;
}

text_t text_of_cstr(const char * const s) {
  size_t l = strlen(s);
  return text_of_ptr_size(s, l);
}


// Concat
export text_t text_concat(text_t s1, text_t s2) {
  // empty strings are ignored
  if (BLOB_LEN(s1) == 0) return s2;
  if (BLOB_LEN(s2) == 0) return s1;
  uint32_t n1 = BLOB_LEN(s1);
  uint32_t n2 = BLOB_LEN(s2);
  uint32_t n = n1 + n2;
  // short texts are copied into a single blob
  if (n < MIN_CONCAT_SIZE) {
    as_ptr r = alloc_text_blob(n1 + n2);
    memcpy(BLOB_PAYLOAD(r), BLOB_PAYLOAD(s1), n1);
    memcpy(BLOB_PAYLOAD(r) + n1, BLOB_PAYLOAD(s2), n2);
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

// Leaving breadcrumps in the destination buffer about where to continue
// the destination is implicitly the location of the crumb struct
typedef struct crumb {
  text_t t;
  struct crumb *next;
} crumb;



// write all data into a buffer (must have the right size)
export void text_to_buf(text_t s, char *buf) {
  crumb *next_crumb = NULL; // what do do after we are done with s
  while (true) {
    if (TAG(s) == TAG_BLOB) {
      memcpy(buf, BLOB_PAYLOAD(s), BLOB_LEN(s));

      // return if we are done
      if (next_crumb == NULL) return;

      // else load text from crumb
      s = next_crumb->t;
      buf = (char *)next_crumb;
      next_crumb = next_crumb->next;
    } else {
      as_ptr s1 = CONCAT_ARG1(s);
      as_ptr s2 = CONCAT_ARG2(s);
      if (CONCAT_LEN(s2) < sizeof(crumb)) {
        // if the second leg is too small to leave a crumb, just do it directly
        text_to_buf(s2, buf + BLOB_LEN(s1));
        s = s1;
      } else {
        // else we use the space where the second leg will be written to
        // to remember a text to write there
        crumb *new_crumb = (crumb *)(buf + BLOB_LEN(s1));
        new_crumb->t = s2;
        new_crumb->next = next_crumb;
        next_crumb = new_crumb;
        s = s1;
      }
    }
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
  uint32_t r = memcmp(BLOB_PAYLOAD(s1), BLOB_PAYLOAD(s2), n);
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
static int text_compare_range(text_t s1, size_t offset1, text_t s2, size_t offset2, size_t n) {
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
  return memcmp(BLOB_PAYLOAD(s1) + offset1, BLOB_PAYLOAD(s2) + offset2, n);
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
static uint32_t decode_code_point(char *s, size_t *n) {
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
// 1. a pointer to a current leaf (must be a BLOB)
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


// Find the leftmost leaf of a text, putting all the others onto a list,
// used to enforce the invariant about TEXT_ITER_BLOB to be a blob.
static blob_t find_leaf(text_t s, text_iter_cont_t *todo) {
  while (TAG(s) == TAG_CONCAT) {
    as_ptr c = alloc_words(TUPLE_HEADER_SIZE + 2);
    TAG(c) = TAG_ARRAY;
    TUPLE_LEN(c) = 2;
    TEXT_CONT_TEXT(c) = CONCAT_ARG2(s);
    TEXT_CONT_NEXT(c) = *todo;
    *todo = c;
    s = CONCAT_ARG1(s);
  }
  return s;
}

export text_iter_t text_iter(text_t s) {
  as_ptr i = alloc_words(TUPLE_HEADER_SIZE + 3);
  TAG(i) = TAG_ARRAY;
  TUPLE_LEN(i) = 3;
  TEXT_ITER_POS(i) = 0;
  TEXT_ITER_TODO(i) = 0;
  TEXT_ITER_BLOB(i) = find_leaf(s, &TEXT_ITER_TODO(i));
  return i;
}

export uint32_t text_iter_done(text_iter_t i) {
  size_t n = TEXT_ITER_POS(i) >> 2;
  text_t s = TEXT_ITER_BLOB(i);
  return n >= BLOB_LEN(s) && TEXT_ITER_TODO(i) == 0;
}

export uint32_t text_iter_next(text_iter_t i) {
  size_t n = TEXT_ITER_POS(i) >> 2;
  text_t s = TEXT_ITER_BLOB(i);

  // If we are at the end, find the next iterator to use
  if (n >= BLOB_LEN(s)) {
    // this one is done, try next
    text_iter_cont_t c = TEXT_ITER_TODO(i);
    // are we done?
    if (c == 0) rts_trap_with("text_iter_next: Iter already done");
    text_t s2 = TEXT_CONT_TEXT(c);
    // if next one is a concat node, re-use both text iterator structures
    // (avoids an allocation)
    if (TAG(s2) == TAG_CONCAT) {
      TEXT_CONT_TEXT(c) = CONCAT_ARG2(s2);
      TEXT_ITER_POS(i) = 0;
      TEXT_ITER_BLOB(i) = find_leaf(CONCAT_ARG1(s2), &TEXT_ITER_TODO(i));
      return text_iter_next(i);
    // else remove that entry from the chain
    } else {
      TEXT_ITER_BLOB(i) = s2;
      TEXT_ITER_POS(i) = 0;
      TEXT_ITER_TODO(i) = TEXT_CONT_NEXT(c);
      return text_iter_next(i);
    }
  } else {
  // We are not at the end, so read the next character
    size_t step = 0;
    uint32_t c = decode_code_point(BLOB_PAYLOAD(s) + n, &step);
    TEXT_ITER_POS(i) = (n+step) << 2;
    return c;
  }
}
