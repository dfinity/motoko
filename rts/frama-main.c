#include <stddef.h>

#define N 100

// see https://stackoverflow.com/a/57116260/946226

char test[N];

extern char *skip_idl_header(char *, char *);

int main() {
  char* beg, *end;
  beg = &test[0];
  end = &test[0] + N;
  skip_idl_header(beg, end);
}

