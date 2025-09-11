/// I cannot write a migration function that migrates UserRegistry, because I don't have access to its value fields,
/// and I can't add a method that gives me that access, nor can I make an upgrade that makes them public.
/// This means I needed to plan ahead and make the constructor of UserRegistry accept all its internal fields, and also add a function that extracts all state from the class or make all fields public (breaking encapsulation).
persistent actor {
  // (Imagine this is core/Map)
  type Map<K, V> = {};
  func empty<K, V>() : Map<K, V> { {} };
  func add<K, V>(map : Map<K, V>, k : K, v : V) {};
  func get<K, V>(map : Map<K, V>, k : K) : V { loop {} };

  type UserId = Nat;
  class UserRegistry() {
    let users : Map<UserId, Text> = empty();
    var lastId : Nat = 0;

    public func addUser(name : Text) : UserId {
      lastId += 1;
      add(users, lastId, name);
      return lastId
    };

    public func getUser(id : UserId) : Text {
      get(users, id)
    };
  };

  var registry = UserRegistry();

  public func newUser(name : Text) : async UserId {
    registry.addUser(name)
  };

  public func getUser(id : UserId) : async Text {
    registry.getUser(id)
  };
}
