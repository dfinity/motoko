type pos = {file : string; line : int; column : int}
type region = {left : pos; right : pos}
type ('a, 'b) annotated_phrase = {at : region; it : 'a; mutable note: 'b}
type 'a phrase = ('a, unit) annotated_phrase

val no_pos : pos
val no_region : region

val string_of_pos : pos -> string
val string_of_region : region -> string

val span : region -> region -> region
val between : region -> region -> region

val annotate : 'b -> 'a -> region -> ('a, 'b) annotated_phrase
val (@@) : 'a -> region -> 'a phrase

val read_region_with_markers : region -> string option

exception ParseError of region * string
