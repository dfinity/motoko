(* Async capabilities *)
open Mo_config

module T = Type

type async_cap =
  | NullCap
  | AsyncCap of T.con
  | AwaitCap of T.con

let top_cap = Con.fresh "@" (T.Abs([],T.Scope))

let initial_cap () = if !Flags.compiled then NullCap else AsyncCap top_cap
