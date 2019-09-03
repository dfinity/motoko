#include "dfinity/api.h"
#include "../../../../sdk/tests/include/utils.h"

WASM_EXPORT
void get_data(dfn_funcref log) {
  char buf[128];
  _strcpy(buf, "DATA1\nDATA2");
  CALL_FUNC(log, externalize_string(buf));
  _strcpy(buf, "DATA3");
  CALL_FUNC(log, externalize_string(buf));
}

WASM_EXPORT
void get_elem(dfn_actorref console, dfn_elembuf elem) {
  CALL_ACTOR(console, "logAny", elem);
}
