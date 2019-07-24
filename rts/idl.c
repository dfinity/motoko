#include "rts.h"

// Initially, we just want to be able to zoom past the type description
// TODO: Defensive programming
// (not going past the size of the message, trapping if leb128 does not fit in int)

uint32_t read_leb128(char **ptr, char *end) {
  uint32_t r = 0;
  unsigned int s = 0;
  uint8_t b;
  do {
    if (*ptr >= end) (idl_trap());
    b = *((*ptr)++);
    if (s == 28 && !((b & (uint8_t)0xF0) == 0x00)) {
        // the 5th byte needs to be the last, and it must contribute at most 4 bits
        // else we have an int overflow
        idl_trap();
    }
    r += (b & (uint8_t)0x7f) << s;
    s += 7;
  } while (b & (uint8_t)0x80);
  return r;
}

int32_t read_sleb128(char **ptr, char *end) {
  uint32_t r = 0;
  unsigned int s = 0;
  uint8_t b;
  do {
    if (*ptr >= end) (idl_trap());
    b = *((*ptr)++);
    if (s == 28 && !((b & (uint8_t)0xF0) == 0x00 || (b & (uint8_t)0xF0) == 0x70)) {
        // the 5th byte needs to be the last, and it must contribute at most 4 bits
        // else we have an int overflow
        idl_trap();
    }
    r += (b & (uint8_t)0x7f) << s;
    s += 7;
  } while (b & (uint8_t)0x80);
  // sign extend
  if (s < 32 && (b & (uint8_t)0x40)) {
    r |= (~(uint32_t)0 << s);
  }
  return r;
}

export char *skip_idl_header(char *ptr, char *end) {
  if (ptr + 3 >= end) (idl_trap());
  // Magic bytes
  if (*ptr++ != 'D') idl_trap();
  if (*ptr++ != 'I') idl_trap();
  if (*ptr++ != 'D') idl_trap();
  if (*ptr++ != 'L') idl_trap();
  // Size of type list
  for (int count = read_leb128(&ptr,end); count > 0; count --) {
    int ty = read_sleb128(&ptr,end);
    if (ty >= -17) {
      idl_trap(); // illegal
    } else if (ty == -18) { // opt
      read_sleb128(&ptr,end);
    }
    else if (ty == -19) { // vec
      read_sleb128(&ptr,end);
    } else if (ty == -20) {  // record
      for (int n = read_leb128(&ptr,end); n > 0; n--) {
        read_leb128(&ptr,end);
        read_sleb128(&ptr,end);
      }
    } else if (ty == -21) {  // variant
      for (int n = read_leb128(&ptr,end); n > 0; n--) {
        read_leb128(&ptr,end);
        read_sleb128(&ptr,end);
      }
    } else if (ty == -22) {  // func
      // arg types
      for (int n = read_leb128(&ptr,end); n > 0; n--) {
        read_sleb128(&ptr,end);
      }
      // ret types
      for (int n = read_leb128(&ptr,end); n > 0; n--) {
        read_sleb128(&ptr,end);
      }
      // annotations
      for (int n = read_leb128(&ptr,end); n > 0; n--) {
        ptr++;
      }
    } else  if (ty == -23) {  // service
      for (int n = read_leb128(&ptr,end); n > 0; n--) {
        // name
        unsigned int size = read_leb128(&ptr,end);
        ptr += size;
        // type
        read_sleb128(&ptr,end);
      }
    } else {
      // no support for future types yet
      idl_trap();
    }
  }
  read_sleb128(&ptr,end); // index
  return ptr;
}
