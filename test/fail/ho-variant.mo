
type Val = { #int : Int;
             #fun : Val -> Val; // no parens required
	     #wrong
	   };