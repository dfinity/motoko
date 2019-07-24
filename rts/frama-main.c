#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include "__fc_builtin.h"

// See README.md, Static analysis with Frama-C

extern char *skip_idl_header(char *, char *);

int main() {
  int n = Frama_C_interval(0, 11);
  if (n > 10) (n = 100);
  //@ split n;
  uint8_t *test = malloc(n);
  if (test != NULL) {
    //@ loop unroll n;
    for (int i=0; i<n; i++) test[i]=Frama_C_interval(0, 255);

    char* beg, *end;
    beg = &test[0];
    end = &test[0] + n;
    skip_idl_header(beg, end);
  }
}

