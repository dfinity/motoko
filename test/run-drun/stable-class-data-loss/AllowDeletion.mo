/// I cannot write a migration function that migrates UserRegistry, because I don't have access to its value fields,
/// and I can't add a method that gives me that access.
/// This means I needed to plan ahead and make the constructor of UserRegistry accept all its internal fields (even the `lastId` counter), and also add a function that extracts all state from the class.
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

    public func deleteUser(id : UserId) {
      remove(users, id)
    };
  };

  var registry = UserRegistry();

  public func newUser(name : Text) : async UserId {
    registry.addUser(name)
  };

  public func deleteUser(id : UserId) {
    registry.deleteUser(id)
  };
}
