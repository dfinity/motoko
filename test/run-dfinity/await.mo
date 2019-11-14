var cnt : Nat = 0;

func f(i:Nat) : async Nat {
    Debug.print "cnt: ";
    Debug.printNat cnt;
    Debug.print " i: ";
    Debug.printNat i;
    Debug.print "\n";
    cnt += 1;
    cnt;
};

Debug.print "a";

let a = async await f(0);

Debug.print "b";

let b = async { await f(1); };

Debug.print "c";

let c = async {
    let _ = await f(2);
    await f(3);
};

Debug.print "d";

let d  = (async { return await f(4); }) : async Int; 

Debug.print "e";

let e = async {
    var i = 5;
    Debug.print "e-while";
    while (i < 8) {
	let _ = await f(i);
	i += 1;
    };
    Debug.print "e-exit";
};

Debug.print "g";

let g = async {
    var i = 10;
    Debug.print "g-label";
    label lp
    while (true) {
	if (i < 13) {
    	    Debug.print ".";
     	    let _ = await f(i);
     	    i += 1;
	    continue lp; 
	} else {};
	break lp;
    };
    Debug.print "g-exit";
};

Debug.print "holy";

func p():async (Text,Text) { ("fst","snd"); };
let h = async {
   let (a,b) = ("a","b"); /* await p(a,b);*/
   Debug.print a;
   Debug.print b;
};

