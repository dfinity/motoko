func bot() : None { bot() };

// Any
let _ : [() -> Any] =
  [ func() : Int { bot() },
    func() : Any { bot() },
  ];
let _ : [Nat -> ()] =
  [ func(_ : Any) {},
    func(_ : Nat) {},
  ];

// None
let _ : [() -> Int] =
  [ func() : Int { bot() },
    func() : None { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : None) {},
    func(_ : Nat) {},
  ];

// Primitives
let _ : [() -> Int] =
  [ func() : Int { bot() },
    func() : Nat { bot() },
  ];
let _ : [Nat -> ()] =
  [ func(_ : Int) {},
    func(_ : Nat) {},
  ];
let _ : [() -> Any] =
  [ func() : Int { bot() },
    func() : Bool { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : Text) {},
    func(_ : Nat) {},
  ];

// Options
let _ : [() -> ?Int] =
  [ func() : ?Int { bot() },
    func() : ?Nat { bot() },
  ];
let _ : [?Nat -> ()] =
  [ func(_ : ?Int) {},
    func(_ : ?Nat) {},
  ];
let _ : [() -> ?Nat] =
  [ func() : Null { bot() },
    func() : ?Nat { bot() },
  ];
let _ : [Null -> ()] =
  [ func(_ : ?Nat) {},
    func(_ : Null) {},
  ];
let _ : [() -> Any] =
  [ func() : Nat { bot() },
    func() : ?Nat { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : ?Nat) {},
    func(_ : Nat) {},
  ];

// Arrays
let _ : [() -> [Int]] =
  [ func() : [Int] { bot() },
    func() : [Nat] { bot() },
  ];
let _ : [[Nat] -> ()] =
  [ func(_ : [Int]) {},
    func(_ : [Nat]) {},
  ];
let _ : [() -> [var Nat]] =
  [ func() : [var Nat] { bot() },
    func() : [var Nat] { bot() },
  ];
let _ : [[var Nat] -> ()] =
  [ func(_ : [var Nat]) {},
    func(_ : [var Nat]) {},
  ];
let _ : [() -> Any] =
  [ func() : [var Nat] { bot() },
    func() : [var Int] { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : [var Nat]) {},
    func(_ : [var Int]) {},
  ];
let _ : [() -> Any] =
  [ func() : Nat { bot() },
    func() : [Nat] { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : Nat) {},
    func(_ : [Nat]) {},
  ];
let _ : [() -> Any] =
  [ func() : Nat { bot() },
    func() : [var Nat] { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : Nat) {},
    func(_ : [var Nat]) {},
  ];

// Tuples
let _ : [() -> (Int, Int)] =
  [ func() : (Nat, Int) { bot() },
    func() : (Int, Nat) { bot() },
  ];
let _ : [(Nat, Nat) -> ()] =
  [ func(_ : (Nat, Int)) {},
    func(_ : (Int, Nat)) {},
  ];
let _ : [() -> Any] =
  [ func() : Nat { bot() },
    func() : (Nat, Nat) { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : Nat) {},
    func(_ : (Nat, Nat)) {},
  ];
let _ : [() -> Any] =
  [ func() : (Nat, Nat) { bot() },
    func() : (Nat, Nat, Nat) { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : (Nat, Nat)) {},
    func(_ : (Nat, Nat, Nat)) {},
  ];

// Objects
let _ : [() -> {a : Int}] =
  [ func() : {a : Nat; b : Nat} { bot() },
    func() : {a : Int; var c : Nat} { bot() },
  ];
let _ : [{a : Nat; b : Nat; var c : Nat} -> ()] =
  [ func(_ : {a : Nat; b : Nat}) {},
    func(_ : {a : Int; var c : Nat}) {},
  ];
let _ : [() -> Any] =
  [ func() : {a : Nat; b : Nat} { bot() },
    func() : {a : Nat; var b : Nat} { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : {a : Nat; b : Nat}) {},
    func(_ : {a : Nat; var b : Nat}) {},
  ];

// Variants
let _ : [() -> {#a : Int; #b : Nat; #c : Nat}] =
  [ func() : {#a : Nat; #b : Nat} { bot() },
    func() : {#a : Int; #c : Nat} { bot() },
  ];
let _ : [{#a : Nat} -> ()] =
  [ func(_ : {#a : Nat; #b : Nat}) {},
    func(_ : {#a : Int; #c : Nat}) {},
  ];
let _ : [() -> Any] =
  [ func() : {a : Nat} { bot() },
    func() : {#a : Nat} { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : {a : Nat}) {},
    func(_ : {#a : Nat}) {},
  ];

// Functions
let _ : [() -> (<A, B <: A>(Nat, B) -> (Int, A))] =
  [ func() : <A, B <: A>(Nat, A) -> (Int, A) { bot() },
    func() : <A, B <: A>(Int, B) -> (Nat, B) { bot() },
  ];
let _ : [(<A, B <: A>(Int, A) -> (Nat, B)) -> ()] =
  [ func(_ : <A, B <: A>(Nat, A) -> (Int, A)) {},
    func(_ : <A, B <: A>(Int, B) -> (Nat, B)) {},
  ];
let _ : [() -> Any] =
  [ func() : <A <: Nat>() -> () { bot() },
    func() : <A <: Int>() -> () { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : <A <: Int>() -> ()) {},
    func(_ : <A <: Nat>() -> ()) {},
  ];
let _ : [() -> Any] =
  [ func() : <A>() -> () { bot() },
    func() : <A, B>() -> () { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : <A>() -> ()) {},
    func(_ : <A, B>() -> ()) {},
  ];

// Abstract types
func f<A <: {a : Nat}, B <: {a : Int; b : Nat}, C <: A>() {
  let _ : [() -> {a : Int}] =
    [ func() : A { bot() },
      func() : B { bot() },
    ];
  let _ : [None -> ()] =
    [ func(_ : A) {},
      func(_ : B) {},
    ];

  let _ : [() -> A] =
    [ func() : A { bot() },
      func() : C { bot() },
    ];
  let _ : [C -> ()] =
    [ func(_ : A) {},
      func(_ : C) {},
    ];

  let _ : [() -> {a : Int}] =
    [ func() : B { bot() },
      func() : C { bot() },
    ];
  let _ : [None -> ()] =
    [ func(_ : B) {},
      func(_ : C) {},
    ];
};

// Recursive types
type A = {b : B};
type B = {a : A};
type C = {b : {a : C; c : C}};

let _ : [() -> A] =
  [ func() : A { bot() },
    func() : C { bot() },
  ];
let _ : [C -> ()] =
  [ func(_ : A) {},
    func(_ : C) {},
  ];

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

let _ : [() -> lubDE] =
  [ func() : D { bot() },
    func() : E { bot() },
  ];
let _ : [glbDE -> ()] =
  [ func(_ : D) {},
    func(_ : E) {},
  ];

// Type members
class T() {
  public type T = Nat;
};
class U() {
  public type T = Int;
};

let _ : [() -> T] =
  [ func() : T { bot() },
    func() : T { bot() },
  ];
let _ : [U -> ()] =
  [ func(_ : U) {},
    func(_ : U) {},
  ];
let _ : [() -> {}] =
  [ func() : T { bot() },
    func() : U { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : T) {},
    func(_ : U) {},
  ];
let _ : [() -> {}] =
  [ func() : T { bot() },
    func() : {T : Nat} { bot() },
  ];
let _ : [None -> ()] =
  [ func(_ : T) {},
    func(_ : {T : Nat}) {},
  ];
