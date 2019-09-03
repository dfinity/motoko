actor {
  private var x = 0;
  inc () { x += 1 };
  get () : async Int { x };
}
