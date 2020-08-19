#include "rts.h"

/*
This file implements the data structure the Motoko runtime uses to keep track of outstanding closures. It needs to support the following operations

 1. Adding a closure (any heap pointer) and getting an index (i32)
 2. Looking up a closure by index, which also frees it
 3. GC must traverse all closures, and possibly move the table
 4. Obtain number of closures registered
 5. Obtain size of table

This stores the closures in a normal, heap-allocated, Motoko array. This means
3. is simple: just traverse this array in GC as normal.

To efficiently look up the next free things, we use an implicit free list:
`free_slot` is the index (shifted 2 bit to the left) into the array payload of
the next free item. Each free item contains the index of another free item.
The last free slot is marked as (-1 << 2).

We shift these 2 bit to the left so that GC treats them as scalars, not as
pointers. It also means they can be used as a byte offset, but that is more
cute than actually important.

When the table is full, we double the size, copy the existing table, and add the
second half to the free list. Since all indices are relative to the payload
begin, they stay valid. We never shrink the table.
*/

/*
#define FULL ((uint32_t)(-1) << 2)
#define INITIAL_SIZE 256

// Skewed pointer to the Array object
static as_ptr table = 0;
// counter of allocated things
static uint32_t n_closures = 0;
// next free
static uint32_t free_slot = FULL;

static as_ptr alloc_array(uint32_t len) {
  as_ptr a = alloc_words(ARRAY_HEADER_SIZE + len);
  TAG(a) = TAG_ARRAY;
  ARRAY_LEN(a) = len;
  return a;
}

static void create_closure_table() {
  table = alloc_array(INITIAL_SIZE);
  free_slot = 0;
  for (uint32_t i = 0; i+1 < INITIAL_SIZE; i++) {
    ARRAY_FIELD(table, i) = (i+1) << 2;
  }
  ARRAY_FIELD(table, INITIAL_SIZE - 1) = FULL;
}

static void double_closure_table() {
  uint32_t old_size = ARRAY_LEN(table);
  as_ptr old = table;
  as_ptr new = alloc_array(2*old_size);
  for (uint32_t i = 0; i < old_size; i++) {
    ARRAY_FIELD(new, i) = ARRAY_FIELD(old, i);
  }
  for (uint32_t i = old_size; i+1 < 2*old_size; i++) {
    ARRAY_FIELD(new, i) = (i+1)<<2;
  }
  ARRAY_FIELD(new, 2*old_size - 1) = free_slot;
  free_slot = old_size << 2;
  table = new;
}

export uint32_t remember_closure(as_ptr cls) {
  if (table == 0)
    create_closure_table();
  else if (free_slot == FULL)
    double_closure_table();

  if (!IS_SKEWED(cls))
    // we could support this, but then we couldn't detect a double recall_closure
    rts_trap_with("remember_closure: Storing unboxed literals not supports");

  uint32_t idx = free_slot >> 2;
  free_slot = ARRAY_FIELD(table, idx);
  ARRAY_FIELD(table, idx) = cls;
  n_closures++;
  return idx;
}

export as_ptr recall_closure(uint32_t idx) {
  if (table == 0)
    rts_trap_with("recall_closure: No closure table allocated");
  if (idx >= ARRAY_LEN(table))
    rts_trap_with("recall_closure: Closure index out of range");

  as_ptr cls = ARRAY_FIELD(table, idx);
  ARRAY_FIELD(table, idx) = free_slot;
  free_slot = idx << 2;
  n_closures--;

  if (!IS_SKEWED(cls))
    rts_trap_with("recall_closure: Closure index not in table");

  return cls;
}

export as_ptr closure_count() {
  return n_closures;
}

export as_ptr closure_table_loc() {
  return SKEW(&table);
}

export as_ptr closure_table_size() {
  if (table == 0)
    return 0;
  else
    return ARRAY_LEN(table);
}
*/
