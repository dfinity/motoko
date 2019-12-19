import imported "test.did";

actor a {
  public func go() : async (imported.broker) {
    let id: Nat64 = await imported.addUser("name", 42);
    let name: Text = await imported.userName(id);
    let age: Nat8 = await imported.userAge(id);
    imported.deleteUser(id);
    return await imported.f({name="name"; _25_=42; _26_="reserved"});
  };
}
