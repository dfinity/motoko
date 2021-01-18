import P "mo:prim";

let t = "BlobText";
let c = t # "2" # t;
let d = c # "4" # c;

func dup(t : Text) {
  var want_t = true;
  let it = t.chars();
  label out loop {
    switch (it.next()) {
      case (?ch) {
	P.debugPrint (debug_show ch);
	if (want_t and ch == 'T') {
	  want_t := false;
	  for (cpy in P.iter_shallow_copy it)
	    P.debugPrint (debug_show cpy)
	}
      };
      case _ break out
    }
  }
};

P.debugPrint("--------t-------- = " # t);
dup t;

P.debugPrint("--------c-------- = " # c);
dup c;

P.debugPrint("--------d-------- = " # d);
dup d;
