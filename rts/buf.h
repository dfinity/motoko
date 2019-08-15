#ifndef BUF_H
#define BUF_H

#include "rts.h"

/*
An abstraction for a buffer with and end-pointer.

This mirrors module ReadBuf in `compile.ml`
*/

typedef struct {
  uint8_t *p;
  uint8_t *e;
} buf;

extern uint8_t read_byte(buf *buf);
extern uint32_t read_word(buf *buf);

#endif /* BUF_H */
