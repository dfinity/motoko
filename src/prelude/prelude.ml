let prelude = [%blob "prelude/prelude.mo"]
let internals = [%blob "prelude/internals.mo"]
let timers_api = [%blob "prelude/timers-api.mo"]
let stable_memory_api = [%blob "prelude/stable-memory-api.mo"]
let prim_module' = [%blob "prelude/prim.mo"]
let prim_module ~timers:required ~legacy_experimental_stable_memory:legacy=
  prim_module' ^
  (if legacy
   then String.map (function '@' -> ' ' | c -> c) stable_memory_api (* disable deprecation *)
   else stable_memory_api (* deprecate use of Prims *)) ^
  (if required
   then timers_api
   else "")
