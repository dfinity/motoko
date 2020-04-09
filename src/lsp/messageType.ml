type t
  = Error
  | Warning
  | Info
  | Log
  | Unknown of int

let wrap : int -> t = function
  | 1 -> Error
  | 2 -> Warning
  | 3 -> Info
  | 4 -> Log
  | x -> Unknown x

let unwrap : t -> int = function
  | Error -> 1
  | Warning -> 2
  | Info -> 3
  | Log -> 4
  | Unknown x -> x
