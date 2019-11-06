actor counter = {
  var c = 1;
  public func inc() {
    c += 1;
    printNat c; print "\n";
  };
  public func printCounter () {
    printNat c; print "\n";
  };
  public func get() : future Nat {
    return c
  };
  public query func read() : future Nat {
    let tmp = c;
    c += 1;
    printNat c; print "(read)\n";
    return tmp;
  };

};


{ // fully explicit syntax

let _ : actor { read : shared query () -> future Nat } = counter;

shared query func f () : future Nat { 666; };

type Query = shared query () -> future Nat;

let _ : Query = counter.read;

};

{

// sugar, surpressing shared

let _ : actor { read: query () -> future Nat } = counter;

query func f () : future Nat { 666; };

type Query = query () -> future Nat;

let _ : Query = counter.read;

};

future {
 counter.inc();
 counter.inc();
 counter.inc();
 counter.printCounter();
 let c1 = await counter.get();
 assert c1 == 4;
 let c2 = await counter.read();
 counter.printCounter();
 assert c2 == 4;
 let c3 = await counter.read();
 counter.printCounter();
 assert c3 == 4;
};
