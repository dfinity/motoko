let prelude = [%blob "prelude/prelude.mo"]
let internals = [%blob "prelude/internals.mo"]
let timers_api = [%blob "prelude/timers-api.mo"]
let prim_module' = [%blob "prelude/prim.mo"]
let prim_module ~timers:required =
  if required
  then prim_module' ^ timers_api
  else prim_module'
