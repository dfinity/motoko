type t
  = Error
  | Warning
  | Info
  | Log
  | Unknown

let wrap : int -> t =
function
| 1 -> Error
| 2 -> Warning
| 3 -> Info
| 4 -> Log
| _ -> Unknown

let unwrap : t -> int =
function
| Unknown -> 0
| Error -> 1
| Warning -> 2
| Info -> 3
| Log -> 4
