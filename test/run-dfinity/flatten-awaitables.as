// test flattening of awaitable, shared function arguments

let a = actor {
    m0():async() {};
    m1(x:Int):async Int {return x;};
    m2(x:Int,y:Bool):async(Int,Bool) {return (x,y);};
    m3(x:Int,y:Bool,z:Text):async(Int,Bool,Text) {return (x,y,z);};

    n0(u:()):async() {return u};
    n1(x:Int):async Int {return x;};
    n2(xy:(Int,Bool)):async(Int,Bool) {return xy;};
    n3(xyz:(Int,Bool,Text)):async(Int,Bool,Text) {return xyz;};

    // higher-order cases
    h0 (f0:shared () -> async (),u:()) : async ()
       { await f0 u;};
    h1 (f1:shared (Int) -> async Int,x:Int)  : async Int
       { await f1 x;};
    h2 (f2:shared (Int,Bool) -> async (Int,Bool), xy:(Int,Bool)) : async (Int,Bool)
       { await f2 xy; };
    h3 (f3:shared (Int,Bool,Text) -> async (Int,Bool,Text), xyz:(Int,Bool,Text)) : async (Int,Bool,Text)
       { await f3 xyz; };

    g0 (f0:shared (()) -> async (),u:()) : async ()
       { await f0 u;};
    g1 (f1:shared (Int) -> async Int,x:Int)  : async Int
       { await f1 x;};
    g2 (f2:shared ((Int,Bool)) -> async (Int,Bool), xy:(Int,Bool)) : async (Int,Bool)
       { await f2 xy; };
    g3 (f3:shared ((Int,Bool,Text)) -> async (Int,Bool,Text), xyz:(Int,Bool,Text)) : async (Int,Bool,Text)
       { await f3 xyz; };

};

func println(s:Text) {print s;print ",";};

let _ = async {

    println "\nfirst-order\n";

    let () = await a.m0();
    println "0";
    let 1 = await a.m1(1);
    println "1";
    let (2,true) = await a.m2(2,true);
    println "2";
    let (3,false,"text") = await a.m3(3,false,"text");
    println "3";

    let () = await a.n0();
    println "4";
    let 1 = await a.n1(1);
    println "5";
    let (2,true) = await a.n2(2,true);
    println "6";
    let (3,false,"text") = await a.n3(3,false,"text");
    println "7";

    let u = ();
    let x:Int = 1;
    let xy:(Int,Bool) = (2,true);
    let xyz:(Int,Bool,Text) = (3,false,"text");

    let () = await a.m0 u;
    println "8";
    let 1 = await a.m1 x;
    println "9";
    let (2,true) = await a.m2 xy;
    println "10";
    let (3,false,"text") = await a.m3 xyz;
    println "11";

    let () = await a.n0 u;
    println "12";
    let 1 = await a.n1 x;
    println "13";
    let (2,true) = await a.n2 xy;
    println "14";
    let (3,false,"text") = await a.n3 xyz;
    println "15";
   
    println "\nhigher-order\n";
    let () = await a.h0(a.m0,());
    println "0";
    let 1 = await a.h1(a.m1,1);
    println "1";
    let (2,true) = await a.h2(a.m2,(2,true));
    println "2";
    let (3,false,"text") = await a.h3(a.m3,(3,false,"text"));
    println "3";

    let () = await a.g0(a.n0,());
    println "4";
    let 1 = await a.g1(a.n1,1);
    println "5";
    let (2,true) = await a.g2(a.n2,(2,true));
    println "6";
    let (3,false,"text") = await a.g3(a.n3,(3,false,"text"));
    println "7";

    let () = await a.h0(a.m0,u);
    println "8";
    let 1 = await a.h1(a.m1,x);
    println "9";
    let (2,true) = await a.h2(a.m2,xy);
    println "10";
    let (3,false,"text") = await a.h3(a.m3,xyz);
    println "11";

    let () = await a.g0(a.n0,u);
    println "12";
    let 1 = await a.g1(a.n1,x);
    println "13";
    let (2,true) = await a.g2(a.n2,xy);
    println "14";
    let (3,false,"text") = await a.g3(a.n3,xyz);
    println "15";

    print "\n";
};

