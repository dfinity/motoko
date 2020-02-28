
type Instance<A <: actor{},O> = (A,()->O);
type Canister<I,A <: actor{},O> = I->Instance<A,O>;

func up<A <: actor{},O <: I1, I1,A1 <: A,O1>(
   c: Instance<A,O>,
   d: Canister<I1, A1, O1>) : Instance<A1,O1> {
      let (a,p) = c;
      d(p ()); 
   };

type A1 = actor {
};

type A2 = actor {
   x : () -> async Int;
   set_x : Int -> ();
};

type A3 = actor {
   x : () -> async Int;
   set_x : Int -> ();
   y : () -> async Int;
   set_y : Int -> ();
};

// test compatibilty
// NB: relation compatible is the inverse of subtyping
// we want to update an existing implementation with a new one whose interface is compatible,
// i.e.  of a subtype.
// That means existing clients can't break and new clients receive more functionality.

type Compatible<A<:actor{}, A1 <: A> = A1->A;
func check<A>(prf:A){};
// sanity
check<Compatible<A1,A2>>(func b = b);
check<Compatible<A1,A3>>(func b = b);

// upgradebility is an indexed relation between canisters
// Provided c's output state subtypes the c1's input state and 
// and c's interface is compatible with c1's interface, then 
// c0 is upgradeable to c1 if every instance of c is convertible to an instance of c1.

type Upgrade<I,A <: actor {},O<:I1,I1,A1 <: A,O1> = 
    (Canister<I,A,O>,Canister<I1,A1,O1>) -> Instance<A,O> -> Instance<A1,O1>;
   

type Unit = ();
let unit = ();

let c1 : Canister<Unit,A1,Unit> = 
  func(_:Unit):Instance<A1,Unit> = {
   var state = unit;
    (actor {
     },
     func ():Unit = state)
  };

let c2 : Canister<(),A2,Int> = 
  func(()):Instance<A2,Int> = {
   var state : Int = 0;
    (actor {
      public func x() :async Int { state };
      public func set_x(x:Int) { state := x};
     },
     (func ():Int = state));
  };

let c3 : Canister<Int,A3,(Int,Int)> =
   func(i):Instance<A3,(Int,Int)> = {
    var state : (Int,Int) = (0,0);
    (actor {
      public func x() :async Int { state.0 };
      public func set_x(x:Int) { state := (x,state.1)};
      public func y() :async Int { state.1 };
      public func set_y(y:Int) { state := (state.0,y)};
     },
     (func (): ((Int,Int)) = state));
  };

// check specific upgradeability
check<Upgrade<Unit,A1,Unit,Unit,A2,Int>>(func (c1,c2) = 
  func i = up<A1,Unit,Unit,A2,Int>(c1(i.1()),c2));

check<Upgrade<Int,A2,Int,Int,A3,(Int,Int)>>(func (c2,c3) = 
  func i2 = up<A2,Int,Int,A3,(Int,Int)>(c2(i2.1()),c3));

// check generic upgradeability
check< <I,A,A1,O <: I1,I1,A1 <: A,O1>(Canister<I,A,O>,Canister<I1,A1,O1>) -> Upgrade<I,A0,O,I1,A1,O1>(
   (func <I,A,A1, O <:I1 ,I1,A1 <: A,O1>(c:Canister<I,A,O>,Canister<I1,A1,O1> : Upgrade<I,A0,O,I1,A1,O1> 
   = func i2 = up<A,O,I,A1,O1>(c2(i2.1()),c3));

// examples

let i1 = c1 unit;
var a1 : A1 = i1.0;
let i2 = up<A1,Unit,Unit,A2,Int>(i1,c2);
var a2 : A2 = i2.0; // new clients see a2 : A2
a1 := a2; // old clients see a2 : A1
let i3 = up<A2,Int,Int,A3,(Int,Int)>(i2,c3);
var a3 : A3 = i3.0; // new clients see a3 : A3
a1 := a3; // old clients of a1 see a3 : A1
a2 := a3; // old clients of a2 see a3 : A2

// ad infinitum

