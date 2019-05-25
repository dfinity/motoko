type t
  = Error
  | Warning
  | Information
  | Hint
  | Unknown of int

let wrap : int -> t =
function
| 1 -> Error
| 2 -> Warning
| 3 -> Information
| 4 -> Hint
| x -> Unknown x

let unwrap : t -> int =
function
| Error -> 1
| Warning -> 2
| Information -> 3
| Hint -> 4
| Unknown x -> x
