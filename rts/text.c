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

static blob_t alloc_text_blob(size_t n) {
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

text_t text_of_cstr(const char * const s) {
  size_t l = as_strlen(s);
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
