(* Async capabilities *)
module T = Type

type async_cap =
  | QueryCap of T.con  (* can (query) async (i.e. in a shared query func) *)
  | ErrorCap           (* can try, catch  (i.e. in the async body of shared query func) *)
  | AsyncCap of T.con  (* can async, send (i.e. in a func of async type or shared func) *)
  | AwaitCap of T.con  (* can async, send, try, catch, await (i.e. in an async expression *)
  | SystemCap of T.con   (* can call protected system functions (e,g, addCycles<system>(...), setTimer<system>(...) *)
  | NullCap            (* none of the above *)
  | CompositeCap of T.con (* can (query) async (i.e. in a shared composite query func) *)
  | CompositeAwaitCap of T.con (* can send a (composite or vanilla) query, try, catch, await (i.e. in a composite query func) *)

let top_cap = Cons.fresh "$top-level" (T.Def([],T.Any))

let bogus_cap = Cons.fresh "$bogus" (T.Def([],T.Non))

let initial_cap () = AwaitCap top_cap
