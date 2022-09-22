// test handling of type components appearing in types of stable variables
actor {

  private type List<A> = ?(A,List<A>);

  private type Bound<A <: {}> = {};

  stable var v : [
    actor {
      type T = Int;
      type U<A> = (A, A);
      type List<A> = List<A>; // references List type above, not this List component (type components arent recursive, let alone mutually recursive
      type Pair<A,B> = (A,B);
      type Bound<A<:{}> = Bound<A>; // ditto
    }
  ] = [];


}