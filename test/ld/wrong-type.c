#define export __attribute__ ((visibility("default")))
export int exported() { return 42; }

