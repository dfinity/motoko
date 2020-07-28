#include "rts.h"

// Iterators

// The iterator needs to point to a specific position into the blob
//
// This is currently a simple pair:
// 1. a pointer to the blob
// 2. index into that blob (shifted by two for GC's sake)
//

typedef as_ptr blob_iter_t; // the data structure used to iterate a text value
#define BLOB_ITER_BLOB(p) (TUPLE_FIELD(p,0,blob_t))
#define BLOB_ITER_POS(p) (TUPLE_FIELD(p,1,uint32_t))


export blob_iter_t blob_iter(blob_t s) {
  as_ptr i = alloc_words(TUPLE_HEADER_SIZE + 2);
  TAG(i) = TAG_ARRAY;
  BLOB_ITER_BLOB(i) = s;
  BLOB_ITER_POS(i) = 0;
  return i;
}

export uint32_t blob_iter_done(blob_iter_t i) {
  size_t n = BLOB_ITER_POS(i) >> 2;
  blob_t s = BLOB_ITER_BLOB(i);
  return n >= BLOB_LEN(s);
}

export uint32_t blob_iter_next(blob_iter_t i) {
  size_t n = BLOB_ITER_POS(i) >> 2;
  blob_t s = BLOB_ITER_BLOB(i);
  BLOB_ITER_POS(i) = (n + 1) << 2;
  return *(uint8_t*)(BLOB_PAYLOAD(s) + n);
}
