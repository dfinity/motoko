module Map {

  public class Map<T, U>() {
  };

  public type Self<T,U> = Map<T, U>;

  public func get<T, U>(map: Self<T,U>, x : T, compare : (implicit : (T,T) -> Int)) : U {
    ignore compare;
    loop {};
  };
};

func compare(n : Nat, m: Nat) : Int { 0 };

let map : Map.Map<Nat,Text> = Map.Map();

func test() {

  let t : Nat = 0;

  let _ = map.get(t);

//  let _ : Text = map.get(t);
//  let _ = map.get t;
}



