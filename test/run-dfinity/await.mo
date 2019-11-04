var cnt : Nat = 0;

func f(i:Nat) : async Nat {
    debugPrint "cnt: ";
    debugPrintNat cnt;
    debugPrint " i: ";
    debugPrintNat i;
    debugPrint "\n";
    cnt += 1;
    cnt;
};

debugPrint "a";

let a = async await f(0);

debugPrint "b";

let b = async { await f(1); };

debugPrint "c";

let c = async {
    let _ = await f(2);
    await f(3);
};

debugPrint "d";

let d  = (async { return await f(4); }) : async Int; 

debugPrint "e";

let e = async {
    var i = 5;
    debugPrint "e-while";
    while (i < 8) {
	let _ = await f(i);
	i += 1;
    };
    debugPrint "e-exit";
};

debugPrint "g";

let g = async {
    var i = 10;
    debugPrint "g-label";
    label lp
    while (true) {
	if (i < 13) {
    	    debugPrint ".";
     	    let _ = await f(i);
     	    i += 1;
	    continue lp; 
	} else {};
	break lp;
    };
    debugPrint "g-exit";
};

debugPrint "holy";

func p():async (Text,Text) { ("fst","snd"); };
let h = async {
   let (a,b) = ("a","b"); /* await p(a,b);*/
   debugPrint a;
   debugPrint b;
};

