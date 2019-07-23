#include <stddef.h>

// See README.md, Static analysis with Frama-C

// see https://stackoverflow.com/a/57116260/946226
#define N 100
char test[N];

extern char *skip_idl_header(char *, char *);

int main() {
  char* beg, *end;
  beg = &test[0];
  end = &test[0] + N;
  skip_idl_header(beg, end);
}

