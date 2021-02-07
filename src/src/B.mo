module {
  public type B = Nat;

  public class D() {
    type B = Int;
    public fun f<B>() : B {
      10
    }
  }
}
