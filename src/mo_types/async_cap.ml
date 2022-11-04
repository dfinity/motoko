(* Async capabilities *)
module T = Type

type async_cap =
  | QueryCap of T.con  (* can (query) async (i.e. in a shared query func) *)
  | ErrorCap           (* can try, catch  (i.e. in the async body of shared query func) *)
  | AsyncCap of T.con  (* can async, send (i.e. in a func of async type or shared func) *)
  | AwaitCap of T.con  (* can async, send, try, catch, await (i.e. in an async expression *)
  | NullCap            (* none of the above *)

let top_cap = Cons.fresh "$top-level" (T.Def([],T.Any))

let initial_cap () = AwaitCap top_cap
