module Map {

   public type Map<T, U> = {
      var size: Nat;
      var other: Nat
   };

   public func empty<T, U>() : Map<T, U> = { var size = 0 };

   public func size<T, U>(self : Map<T,U>) : Nat { self.size };

};


let m = Map.empty<Nat, Text>();

let n1 = m.size(); // call method
let n2 = m.size; // read field

let i1 = m.other; // read field
let i2 = m.other(); // call unavailable method
