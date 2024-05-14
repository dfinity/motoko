let prelude : string = {prelude|/* BEGIN PRELUDE */
/* Array encoding */
domain Array {
  function $loc(a: Array, i: Int): Ref
  function $size(a: Array): Int
  function $loc_inv1(r: Ref): Array
  function $loc_inv2(r: Ref): Int
  axiom $all_diff_array { forall a: Array, i: Int :: {$loc(a, i)} $loc_inv1($loc(a, i)) == a && $loc_inv2($loc(a, i)) == i }
  axiom $size_nonneg { forall a: Array :: $size(a) >= 0 }
}
define $array_acc(a, t, p) forall j: Int :: 0 <= j && j < $size(a) ==> acc($loc(a, j).t, p)
define $array_untouched(a, t) forall j: Int :: 0 <= j && j < $size(a) ==> $loc(a, j).t == old($loc(a, j).t)
/* Tuple encoding */
domain Tuple {
  function $prj(a: Tuple, i: Int): Ref
  function $prj_inv1(r: Ref): Tuple
  function $prj_inv2(r: Ref): Int
  axiom $all_diff_tuple { forall a: Tuple, i: Int :: {$prj(a, i)} $prj_inv1($prj(a, i)) == a && $prj_inv2($prj(a, i)) == i }
}
/* Option encoding */
adt Option[T] {
  None()
  Some(some$0: T)
}
/* Typed references */
field $int: Int
field $bool: Bool
field $ref: Ref
field $array: Array
field $tuple: Tuple
field $option_int: Option[Int]
field $option_bool: Option[Bool]
field $option_array: Option[Array]
field $option_tuple: Option[Tuple]
/* END PRELUDE */|prelude}
