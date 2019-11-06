var cnt : Nat = 0;

func f(i:Nat) : future Nat {
    print "cnt: ";
    printNat cnt;
    print " i: ";
    printNat i;
    print "\n";
    cnt += 1;
    cnt;
};

print "a";

let a = future await f(0);

print "b";

let b = future { await f(1); };

print "c";

let c = future {
    let _ = await f(2);
    await f(3);
};

print "d";

let d  = (future { return await f(4); }) : future Int; 

print "e";

let e = future {
    var i = 5;
    print "e-while";
    while (i < 10) {
	let _ = await f(i);
	i += 1;
    };
    print "e-exit";
};

print "g";

let g = future {
    var i = 10;
    print "g-label";
    label lp
    while (true) {
	if (i < 15) {
    	    print ".";
     	    let _ = await f(i);
     	    i += 1;
	    continue lp; 
	} else {};
	break lp;
    };
    print "g-exit";
};

print "holy";

func p():future (Text,Text) { ("fst","snd"); };
let h = future {
   let (a,b) = ("a","b"); /* await p(a,b);*/
   print a;
   print b;
};

