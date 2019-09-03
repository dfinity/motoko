#include "dfinity/api.h"
#include <string.h>

DECLARE_GLOBAL(dfn_actorref, actor)

WASM_EXPORT
void new(dfn_databuf mod) {
  SET_GLOBAL(actor, dfn_actor_new(dfn_module_new(mod)));
}

WASM_EXPORT
void call(dfn_databuf func, dfn_elembuf args) {
  dfn_funcref f = dfn_actor_export(GET_GLOBAL(actor), func);
  int len = dfn_elem_length(args);
  dfn_anyref xs[len];
  dfn_elem_internalize(&xs, len, args, 0);
  switch (len) {
    case 0: CALL_FUNC(f); break;
    case 1: CALL_FUNC(f, xs[0]); break;
    case 2: CALL_FUNC(f, xs[0], xs[1]); break;
    case 3: CALL_FUNC(f, xs[0], xs[1], xs[2]); break;
    case 4: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3]); break;
    case 5: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4]); break;
    case 6: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5]); break;
    case 7: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6]); break;
    case 8: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7]); break;
    case 9: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7], xs[8]); break;
    case 10: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7], xs[8], xs[9]); break;
    case 11: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7], xs[8], xs[9], xs[10]); break;
    case 12: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7], xs[8], xs[9], xs[10], xs[11]); break;
    case 13: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7], xs[8], xs[9], xs[10], xs[11], xs[12]); break;
    case 14: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7], xs[8], xs[9], xs[10], xs[11], xs[12], xs[13]); break;
    case 15: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7], xs[8], xs[9], xs[10], xs[11], xs[12], xs[13], xs[14]); break;
    case 16: CALL_FUNC(f, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7], xs[8], xs[9], xs[10], xs[11], xs[12], xs[13], xs[14], xs[15]); break;
  }
}

WASM_EXPORT
void newAndCall(dfn_databuf mod, dfn_databuf func, dfn_elembuf args) {
  new(mod);
  call(func, args);
}
