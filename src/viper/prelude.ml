let prelude : string = {prelude|/* BEGIN PRELUDE */
domain Array {
  function $loc(a: Array, i: Int): Ref
  function $size(a: Array): Int
  function $first(r: Ref): Array
  function $second(r: Ref): Int
  axiom $all_diff { forall a: Array, i: Int :: {$loc(a, i)} $first($loc(a, i)) == a && $second($loc(a, i)) == i }
  axiom $size_nonneg { forall a: Array :: $size(a) >= 0 }
}
define $array_acc(a, t, p) forall j: Int :: 0 <= j && j < $size(a) ==> acc($loc(a, j).t, p)
define $array_untouched(a, t) forall j: Int :: 0 <= j && j < $size(a) ==> $loc(a, j).t == old($loc(a, j).t)
field $int: Int
field $bool: Bool
field $ref: Ref
field $array: Array
/* END PRELUDE */|prelude}