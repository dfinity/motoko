#include "rts.h"

/*
An abstraction for a buffer with and end-pointer.

This mirrors module Serialization.Buf in `compile.ml`
*/

typedef struct {
  uint8_t *p;
  uint8_t *e;
} buf;

uint8_t read_byte(buf *buf) {
  if (buf->p >= buf->e) (idl_trap());
  return ((buf->p)++)[0];
}

// Initially, we just want to be able to zoom past the type description
// TODO: Defensive programming
// (not going past the size of the message, trapping if leb128 does not fit in int)

uint32_t read_leb128(buf *buf) {
  uint32_t r = 0;
  unsigned int s = 0;
  uint8_t b;
  do {
    b = read_byte(buf);
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

int32_t read_sleb128(buf *buf) {
  uint32_t r = 0;
  unsigned int s = 0;
  uint8_t b;
  do {
    b = read_byte(buf);
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

export void skip_idl_header(buf *buf) {
  // Magic bytes
  if (read_byte(buf) != 'D') idl_trap();
  if (read_byte(buf) != 'I') idl_trap();
  if (read_byte(buf) != 'D') idl_trap();
  if (read_byte(buf) != 'L') idl_trap();
  // Size of type list
  for (int count = read_leb128(buf); count > 0; count --) {
    int ty = read_sleb128(buf);
    if (ty >= -17) {
      idl_trap(); // illegal
    } else if (ty == -18) { // opt
      read_sleb128(buf);
    }
    else if (ty == -19) { // vec
      read_sleb128(buf);
    } else if (ty == -20) {  // record
      for (int n = read_leb128(buf); n > 0; n--) {
        read_leb128(buf);
        read_sleb128(buf);
      }
    } else if (ty == -21) {  // variant
      for (int n = read_leb128(buf); n > 0; n--) {
        read_leb128(buf);
        read_sleb128(buf);
      }
    } else if (ty == -22) {  // func
      // arg types
      for (int n = read_leb128(buf); n > 0; n--) {
        read_sleb128(buf);
      }
      // ret types
      for (int n = read_leb128(buf); n > 0; n--) {
        read_sleb128(buf);
      }
      // annotations
      for (int n = read_leb128(buf); n > 0; n--) {
        (buf->p)++;
      }
    } else  if (ty == -23) {  // service
      for (int n = read_leb128(buf); n > 0; n--) {
        // name
        unsigned int size = read_leb128(buf);
        (buf->p) += size;
        // type
        read_sleb128(buf);
      }
    } else {
      // no support for future types yet
      idl_trap();
    }
  }
  read_sleb128(buf); // index
}
