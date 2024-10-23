actor {
  func _bad(a : actor { foo : () -> async () }) : async () {
    await (with cycles = 'C') a.foo()
  }
}
