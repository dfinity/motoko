#pragma GCC diagnostic ignored "-Wattributes"
#define export __attribute__ ((visibility("default")))
#define from_rts __attribute__ ((import_module("as_rts"))) extern

export void as_memcpy(char *str1, const char *str2, int n) {
  for (int i = 0; i < n; i++) {
    str1[i] = str2[i];
  }
}


/*
ActorScript pointers are offset by one. So lets represent
them as a typedef, and access the fields using the payload macro.
*/
typedef long as_ptr;
#define FIELD(p,n) (((int *)(p+1))[n])

/*
It seems we can’t get our hand on the heap bump pointer here.
See https://bugs.llvm.org/show_bug.cgi?id=41610
So if we want to allocate on the AS heap from C, we need to import
alloc_bytes from the Actorscript RTS

extern char *heap_ptr;
export char* alloc_bytes(int n) {
  char *r = heap_ptr;
  heap_ptr += (n + 3) & ~0x03;
  return r;
}
*/
from_rts as_ptr alloc_bytes(int n);

// This is mostly to test static strings and access to the AS heap
const char* RTS_VERSION = "0.1";

int as_strlen(const char* p) {
  int i = 0;
  while (p[i]) i++;
  return i;
}

as_ptr as_str_of_cstr(const char * const s) {
  int l = as_strlen(s);
  as_ptr r = alloc_bytes (2*sizeof(void*) + l);
  FIELD(r, 0) = 10;
  FIELD(r, 1) = l;
  as_memcpy((char *)(&FIELD(r,2)), RTS_VERSION, l);
  return r;
}

export as_ptr version() {
  return as_str_of_cstr(RTS_VERSION);
}
