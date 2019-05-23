type t
  = Error
  | Warning
  | Information
  | Hint
  | Unknown

let wrap : int -> t =
function
| 1 -> Error
| 2 -> Warning
| 3 -> Information
| 4 -> Hint
| _ -> Unknown

let unwrap : t -> int =
function
| Unknown -> 0
| Error -> 1
| Warning -> 2
| Information -> 3
| Hint -> 4
