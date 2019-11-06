type List<A> = {
  head:A;
  tail:?List<A>;
};
actor {
 public func f(x:Nat):future List<Nat> {
   {head=x; tail=null};
 };
 public func g(x:Int):future List<Int> {
   {head=x; tail=null};
 };
}

