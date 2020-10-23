import P "mo:prim";

let t = "BlobText";
let c = t # "2" # t;
let d = c # "4" # c;

func dup(t : Text) {
  let it = t.chars();
  label out loop {
    switch (it.next()) {
      case (?ch) {
	P.debugPrint (debug_show ch);
	if (ch == 'T')
	  for (cpy in P.iter_shallow_copy it)
	    P.debugPrint (debug_show cpy)
      };
      case _ break out
    }
  }
};

//dup t;

dup c;

//dup d;
