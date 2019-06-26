// a tricky test of open/close on open type defs
// t<B> is an open definition since it mentions an outer parameter A.
// t is applied at different binding depths, requiring care in open and close to only
// share definitions at the same binding depth.

class D<A>(){
  type t<B> = (A,B);
  func f<B>(x:t<B>) :
           <C>(x:t<C>)->t<B>
       { let y : (A,B) = x;
         func g<C>(z:t<C>) : t<B> {
	   let u : (A,B) = y;
	   let w : (A,C) = z;
	    (g<C>(z)):(A,B)
	  };
        };
  let g = f: <B>((A,B)) -> <C>((A,C)) -> ((A,B));
};

