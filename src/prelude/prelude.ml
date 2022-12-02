let prelude = [%blob "prelude.mo"]
let internals = [%blob "internals.mo"]
let timers_api = [%blob "timers-api.mo"]
let prim_module' = [%blob "prim.mo"]
let prim_module want_timers =
  if want_timers
  then prim_module' ^ timers_api
  else prim_module'
