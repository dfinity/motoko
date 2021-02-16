func bot() : None { bot() };

// Any
let _ : [() -> Any] = do { let x =  // use block to force inference mode
  [ func() : Int { bot() },
    func() : Any { bot() },
  ]; x
};
let _ : [Nat -> ()] = do { let x =
  [ func(_ : Any) {},
    func(_ : Nat) {},
  ]; x
};

// None
let _ : [() -> Int] = do { let x =
  [ func() : Int { bot() },
    func() : None { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : None) {},
    func(_ : Nat) {},
  ]; x
};

// Primitives
let _ : [() -> Int] = do { let x =
  [ func() : Int { bot() },
    func() : Nat { bot() },
  ]; x
};
let _ : [Nat -> ()] = do { let x =
  [ func(_ : Int) {},
    func(_ : Nat) {},
  ]; x
};
let _ : [() -> Any] = do { let x =
  [ func() : Int { bot() },
    func() : Bool { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : Text) {},
    func(_ : Nat) {},
  ]; x
};

// Options
let _ : [() -> ?Int] = do { let x =
  [ func() : ?Int { bot() },
    func() : ?Nat { bot() },
  ]; x
};
let _ : [?Nat -> ()] = do { let x =
  [ func(_ : ?Int) {},
    func(_ : ?Nat) {},
  ]; x
};
let _ : [() -> ?Nat] = do { let x =
  [ func() : Null { bot() },
    func() : ?Nat { bot() },
  ]; x
};
let _ : [Null -> ()] = do { let x =
  [ func(_ : ?Nat) {},
    func(_ : Null) {},
  ]; x
};
let _ : [() -> Any] = do { let x =
  [ func() : Nat { bot() },
    func() : ?Nat { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : ?Nat) {},
    func(_ : Nat) {},
  ]; x
};

// Arrays
let _ : [() -> [Int]] = do { let x =
  [ func() : [Int] { bot() },
    func() : [Nat] { bot() },
  ]; x
};
let _ : [[Nat] -> ()] = do { let x =
  [ func(_ : [Int]) {},
    func(_ : [Nat]) {},
  ]; x
};
let _ : [() -> [var Nat]] = do { let x =
  [ func() : [var Nat] { bot() },
    func() : [var Nat] { bot() },
  ]; x
};
let _ : [[var Nat] -> ()] = do { let x =
  [ func(_ : [var Nat]) {},
    func(_ : [var Nat]) {},
  ]; x
};
let _ : [() -> Any] = do { let x =
  [ func() : [var Nat] { bot() },
    func() : [var Int] { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : [var Nat]) {},
    func(_ : [var Int]) {},
  ]; x
};
let _ : [() -> Any] = do { let x =
  [ func() : Nat { bot() },
    func() : [Nat] { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : Nat) {},
    func(_ : [Nat]) {},
  ]; x
};
let _ : [() -> Any] = do { let x =
  [ func() : Nat { bot() },
    func() : [var Nat] { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : Nat) {},
    func(_ : [var Nat]) {},
  ]; x
};

// Tuples
let _ : [() -> (Int, Int)] = do { let x =
  [ func() : (Nat, Int) { bot() },
    func() : (Int, Nat) { bot() },
  ]; x
};
let _ : [((Nat, Nat)) -> ()] = do { let x =
  [ func(_ : (Nat, Int)) {},
    func(_ : (Int, Nat)) {},
  ]; x
};
let _ : [() -> Any] = do { let x =
  [ func() : Nat { bot() },
    func() : ((Nat, Nat)) { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : Nat) {},
    func(_ : (Nat, Nat)) {},
  ]; x
};
let _ : [() -> Any] = do { let x =
  [ func() : ((Nat, Nat)) { bot() },
    func() : ((Nat, Nat, Nat)) { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : (Nat, Nat)) {},
    func(_ : (Nat, Nat, Nat)) {},
  ]; x
};

// Objects
let _ : [() -> {a : Int}] = do { let x =
  [ func() : {a : Nat; b : Nat} { bot() },
    func() : {a : Int; var c : Nat} { bot() },
  ]; x
};
/* TODO(crusso): IR subtype violation
let _ : [{a : Nat; b : Nat; var c : Nat} -> ()] = do { let x =
  [ func(_ : {a : Nat; b : Nat}) {},
    func(_ : {a : Int; var c : Nat}) {},
  ]; x
};
*/
let _ : [() -> Any] = do { let x =
  [ func() : {a : Nat; b : Nat} { bot() },
    func() : {a : Nat; var b : Nat} { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : {a : Nat; b : Nat}) {},
    func(_ : {a : Nat; var b : Nat}) {},
  ]; x
};

// Variants
let _ : [() -> {#a : Int; #b : Nat; #c : Nat}] = do { let x =
  [ func() : {#a : Nat; #b : Nat} { bot() },
    func() : {#a : Int; #c : Nat} { bot() },
  ]; x
};
let _ : [{#a : Nat} -> ()] = do { let x =
  [ func(_ : {#a : Nat; #b : Nat}) {},
    func(_ : {#a : Int; #c : Nat}) {},
  ]; x
};
let _ : [() -> Any] = do { let x =
  [ func() : {a : Nat} { bot() },
    func() : {#a : Nat} { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : {a : Nat}) {},
    func(_ : {#a : Nat}) {},
  ]; x
};

// Functions
let _ : [() -> (<A, B <: A>(Nat, B) -> (Int, A))] = do { let x =
  [ func() : <A, B <: A>(Nat, A) -> (Int, A) { bot() },
    func() : <A, B <: A>(Int, B) -> (Nat, B) { bot() },
  ]; x
};
let _ : [(<A, B <: A>(Int, A) -> (Nat, B)) -> ()] = do { let x =
  [ func(_ : <A, B <: A>(Nat, A) -> (Int, A)) {},
    func(_ : <A, B <: A>(Int, B) -> (Nat, B)) {},
  ]; x
};
let _ : [() -> Any] = do { let x =
  [ func() : <A <: Nat>() -> () { bot() },
    func() : <A <: Int>() -> () { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : <A <: Int>() -> ()) {},
    func(_ : <A <: Nat>() -> ()) {},
  ]; x
};
let _ : [() -> Any] = do { let x =
  [ func() : <A>() -> () { bot() },
    func() : <A, B>() -> () { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : <A>() -> ()) {},
    func(_ : <A, B>() -> ()) {},
  ]; x
};

// Abstract types
func f<A <: {a : Nat}, B <: {a : Int; b : Nat}, C <: A>() {
  let _ : [() -> {a : Int}] = do { let x =
    [ func() : A { bot() },
      func() : B { bot() },
    ]; x
  };
  let _ : [None -> ()] = do { let x =
    [ func(_ : A) {},
      func(_ : B) {},
    ]; x
  };

  let _ : [() -> A] = do { let x =
    [ func() : A { bot() },
      func() : C { bot() },
    ]; x
  };
  let _ : [C -> ()] = do { let x =
    [ func(_ : A) {},
      func(_ : C) {},
    ]; x
  };

  let _ : [() -> {a : Int}] = do { let x =
    [ func() : B { bot() },
      func() : C { bot() },
    ]; x
  };
  let _ : [None -> ()] = do { let x =
    [ func(_ : B) {},
      func(_ : C) {},
    ]; x
  };

  let _ : [() -> {}] = do { let x =
    [ func() : A { bot() },
      func() : {b : Nat} { bot() },
    ]; x
  };
  let _ : [None -> ()] = do { let x =
    [ func(_ : A) {},
      func(_ : {b : Nat}) {},
    ]; x
  };
  let _ : [() -> Any] = do { let x =
    [ func() : A { bot() },
      func() : Nat { bot() },
    ]; x
  };
  let _ : [None -> ()] = do { let x =
    [ func(_ : A) {},
      func(_ : Nat) {},
    ]; x
  };
};

// Recursive types
type A = {b : B};
type B = {a : A};
type C = {b : {a : C; c : C}};

let _ : [() -> A] = do { let x =
  [ func() : A { bot() },
    func() : C { bot() },
  ]; x
};
let _ : [C -> ()] = do { let x =
  [ func(_ : A) {},
    func(_ : C) {},
  ]; x
};

type D = {x : {x : D; y : Int}};
type E = {x : {x : {x : E}; y : Nat}; z : Int};
type lubDE =
  { x :
    { x :
      { x :
        { x :
          { x :
            { x : lubDE;
            };
          };
        };
      };
      y : Int;
    };
  };
type glbDE =
  { x :
    { x :
      { x :
        { x :
          { x :
            { x : glbDE;
              y : Int;
            };
            y : Nat;
          };
          y : Int;
          z : Int;
        };
      };
      y : Nat;
    };
    z : Int;
  };

let _ : [() -> lubDE] = do { let x =
  [ func() : D { bot() },
    func() : E { bot() },
  ]; x
};
/* TODO(crusso): IR subtype violation
let _ : [glbDE -> ()] = do { let x =
  [ func(_ : D) {},
    func(_ : E) {},
  ]; x
};
*/

// Type members
class T() {
  public type T = Nat;
};
class U() {
  public type T = Int;
};

let _ : [() -> T] = do { let x =
  [ func() : T { bot() },
    func() : T { bot() },
  ]; x
};
let _ : [U -> ()] = do { let x =
  [ func(_ : U) {},
    func(_ : U) {},
  ]; x
};
let _ : [() -> {}] = do { let x =
  [ func() : T { bot() },
    func() : U { bot() },
  ]; x
};
let _ : [None -> ()] = do { let x =
  [ func(_ : T) {},
    func(_ : U) {},
  ]; x
};
let _ : [() -> {}] = do { let x =
  [ func() : T { bot() },
    func() : {T : Nat} { bot() },
  ]; x
};
/* TODO(crusso): IR subtype violation
let _ : [None -> ()] = do { let x =
  [ func(_ : T) {},
    func(_ : {T : Nat}) {},
  ]; x
};
*/
