/* check cyclic bounds are rejected, would loop sans check */

func f<A <: B, B <: C, C <: A>(a : A, b : B, c : C) {
  let _ : () = a;
  let _ : () = b;
  let _ : () = c;
};
