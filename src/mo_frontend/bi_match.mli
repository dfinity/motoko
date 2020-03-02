open Mo_types
open Type

val bi_match_typ :
  scope option -> bind list -> typ -> typ ->
  typ list (* raises Failure *)
