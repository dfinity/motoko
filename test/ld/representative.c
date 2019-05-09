#define export __attribute__ ((visibility("default")))
#define from_rts __attribute__ ((import_module("env"))) extern

export int square(int i) { return i * i; }
