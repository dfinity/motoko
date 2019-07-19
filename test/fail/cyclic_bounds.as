/* check cyclic bounds are rejected, would loop sans check */
func w1<A <: A>(a : A)
{
  let _ : () = a;
};

func w2<A <: B, B <: A>(a : A, b : B) {
  let _ : () = a; let _ : () = b;
};

func w3<A <: B, B <: C, C <: A>(a : A, b : B, c : C) {
  let _ : () = a;
  let _ : () = b;
  let _ : () = c;
};
