#include "rts.h"
#include "buf.h"

// See comments in buf.h

uint8_t read_byte(buf *buf) {
  if (buf->p >= buf->e) (idl_trap_with("byte read out of buffer"));
  return ((buf->p)++)[0];
}

uint32_t read_word(buf *buf) {
  if (buf->p + sizeof(uint8_t) > buf->e) (idl_trap_with("word read out of buffer"));
  uint32_t r = ((uint32_t*)(buf->p))[0];
  buf->p += sizeof(uint32_t);
  return r;
}

