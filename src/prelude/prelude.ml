let prelude = [%blob "prelude/prelude.mo"]
let internals = [%blob "prelude/internals.mo"]
let timers_api = [%blob "prelude/timers-api.mo"]
let stable_memory_api = [%blob "prelude/stable-memory-api.mo"]
let prim_module' = [%blob "prelude/prim.mo"]
let prim_module ~timers:required ~experimental_stable_memory:level=
  prim_module' ^
  (if level < 1 then "\n/* no stable memory */\n" (* no prims *)
   else if level = 1
   then stable_memory_api (* deprecated prims *)
   else String.map (function '@' -> ' ' | c -> c) stable_memory_api) ^ (* undeprecated prims *)
  (if required
   then timers_api
   else "")
