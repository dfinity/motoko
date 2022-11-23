let prelude = [%blob "prelude.mo"]
let internals' = [%blob "internals.mo"]
let timers = [%blob "timers.mo"]
let internals want_timers =
  if want_timers
  then internals' ^ timers
  else internals'
let timers_api = [%blob "timers-api.mo"]
let prim_module' = [%blob "prim.mo"]
let prim_module want_timers =
  if want_timers
  then prim_module' ^ timers_api
  else prim_module'
