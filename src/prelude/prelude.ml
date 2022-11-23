let prelude = [%blob "prelude.mo"]
let internals' = [%blob "internals.mo"]
let timers = [%blob "timers.mo"]
let internals =
  if !Mo_config.Flags.global_timer
  then internals' ^ timers
  else internals'
let prim_module = [%blob "prim.mo"]
