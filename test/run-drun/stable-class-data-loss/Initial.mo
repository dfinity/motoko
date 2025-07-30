persistent actor {
  // (Imagine this is core/Map)
  type Map<K, V> = {};
  func empty<K, V>() : Map<K, V> { {} };
  func add<K, V>(map : Map<K, V>, k : K, v : V) {};
  func remove<K, V>(map : Map<K, V>, k : K) {};

  type UserId = Nat;
  class UserRegistry() {
    let users : Map<UserId, Text> = empty();
    var lastId : Nat = 0;

    public func addUser(name : Text) : UserId {
      lastId += 1;
      add(users, lastId, name);
      return lastId
    };
  };

  var registry = UserRegistry();

  public func newUser(name : Text) : async UserId {
    registry.addUser(name)
  };
}
