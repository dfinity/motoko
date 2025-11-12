// Type inference on record update should be as strong as on record creation

// Stub for Map
module Map {
  public type Node<K, V> = {
    #leaf : Leaf<K, V>;
    #internal : Internal<K, V>;
  };

  public type Data<K, V> = {
    kvs : [var ?(K, V)];
    var count : Nat;
  };

  public type Internal<K, V> = {
    data : Data<K, V>;
    children : [var ?Node<K, V>];
  };

  public type Leaf<K, V> = {
    data : Data<K, V>;
  };

  public type Map<K, V> = {
    var root : Node<K, V>;
    var size : Nat;
  };
  public func empty<K, V>() : Map<K, V> = {
    var root = #leaf { data = { kvs = [var]; var count = 0 } };
    var size = 0;
  };
};

type Exercise = {
  name : Text;
  sets : Nat;
};

type Workout = {
  exercises : Map.Map<Nat, Exercise>;
  duration : Nat;
  timestamp : Int;
};

func createWorkout(duration : Nat) : Workout {
  {
    exercises = Map.empty();
    duration;
    timestamp = 0;
  }
};

func updateWorkout(workout : Workout, duration : Nat) : Workout {
  {
    workout with
    exercises = Map.empty(); // should typecheck without instantiation
    duration;
  }
};

let workout1 = createWorkout(30);
ignore updateWorkout(workout1, 45);
