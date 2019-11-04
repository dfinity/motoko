var cnt : Nat = 0;

func f(i:Nat) : async Nat {
    debug_print "cnt: ";
    debug_print_Nat cnt;
    debug_print " i: ";
    debug_print_Nat i;
    debug_print "\n";
    cnt += 1;
    cnt;
};

debug_print "a";

let a = async await f(0);

debug_print "b";

let b = async { await f(1); };

debug_print "c";

let c = async {
    let _ = await f(2);
    await f(3);
};

debug_print "d";

let d  = (async { return await f(4); }) : async Int; 

debug_print "e";

let e = async {
    var i = 5;
    debug_print "e-while";
    while (i < 8) {
	let _ = await f(i);
	i += 1;
    };
    debug_print "e-exit";
};

debug_print "g";

let g = async {
    var i = 10;
    debug_print "g-label";
    label lp
    while (true) {
	if (i < 13) {
    	    debug_print ".";
     	    let _ = await f(i);
     	    i += 1;
	    continue lp; 
	} else {};
	break lp;
    };
    debug_print "g-exit";
};

debug_print "holy";

func p():async (Text,Text) { ("fst","snd"); };
let h = async {
   let (a,b) = ("a","b"); /* await p(a,b);*/
   debug_print a;
   debug_print b;
};

