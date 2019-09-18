#include "utf8_valid.h"

export void utf8_validate(const char *src, size_t len) {
  if (utf8_valid(src, len))
    return;
  idl_trap_with("UTF-8 validation failure");
}
