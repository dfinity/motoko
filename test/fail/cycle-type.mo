actor {
  func _bad(a : actor { foo : () -> async () }) : async () {
    await (with cycles = 'C') a.foo();
    await (with cycles = "Can't") async ();
  }
}
