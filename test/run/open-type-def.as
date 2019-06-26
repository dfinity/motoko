// a tricky test of open/close on open type defs
// T<B> is an open definition since it mentions an outer parameter A.
// T<-> is applied at different binding depths, requiring care in
// open and close to only share definitions at the same binding depth.

class D<A>(){
  type T<B> = (A,B);
  func f<B>(tb:T<B>) :
           <C>(tc:T<C>)->T<B>
       { let _ : (A,B) = tb;
         func g<C>(tc:T<C>) : T<B> {
	   let _ : (A,B) = tb;
	   let _ : (A,C) = tc;
	    (g<C>(tc)):(A,B)
	  };
        };
  let g = f: <B>((A,B)) -> <C>((A,C)) -> ((A,B));
};

