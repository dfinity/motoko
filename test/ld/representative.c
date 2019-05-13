#define export __attribute__ ((visibility("default")))
#define from_rts __attribute__ ((import_module("env"))) extern

from_rts void resolved_import();
from_rts void unresolved_import();

export int square(int i) {
  resolved_import();
  unresolved_import();
  return i * i;
}

