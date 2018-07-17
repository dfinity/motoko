type pos = {file : string; line : int; column : int}
type region = {left : pos; right : pos}
type ('a,'i) annotated_phrase = {at : region; it : 'a; mutable note: 'i}
type 'a phrase = ('a,unit) annotated_phrase

let (@@) x region = {it = x; at = region; note = ()}

(* Positions and regions *)

let no_pos = {file = ""; line = 0; column = 0}
let no_region = {left = no_pos; right = no_pos}

let span r1 r2 = {left = r1.left; right = r2.right}

let string_of_pos pos =
  if pos.line = -1 then
    Printf.sprintf "0x%x" pos.column
  else
    string_of_int pos.line ^ "." ^ string_of_int (pos.column + 1)

let string_of_region r =
  r.left.file ^ ":" ^ string_of_pos r.left ^
  (if r.right = r.left then "" else "-" ^ string_of_pos r.right)
