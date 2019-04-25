#define export __attribute__ ((visibility("default")))

export void as_memcpy(char *str1, const char *str2, int n) {
  for (int i = 0; i < n; i++) {
    str1[i] = str2[i];
  }
}
