let prelude = [%blob "prelude.mo"]
let internals' = [%blob "internals.mo"]
let timers = [%blob "timers.mo"]
let internals want_timers =
  if want_timers
  then internals' ^ timers
  else internals'
let prim_module = [%blob "prim.mo"]
