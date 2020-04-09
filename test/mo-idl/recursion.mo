type List<A> = {
  head:A;
  tail:?List<A>;
};
actor {
 public func f(x:Nat):async List<Nat> {
   {head=x; tail=null};
 };
 public func g(x:Int):async List<Int> {
   {head=x; tail=null};
 };
}

