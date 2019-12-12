import P "prelude.mo";

module {
  // ropes, balanced with extra cat "level"s, chosen from special random distribution that ensures balance at all scales, for all operation sequences.

  public type Lev = Nat;
  public type Seq<X> = {
    #empty;
    #sing: X;
    #cat: (Lev, Seq<X>, Seq<X>)
  };

  public func empty<X>() : Seq<X> {
    #empty
  };

  public func cat<X>(s1: Seq<X>, s2:  Seq<X>): Seq<X> {
    /* todo: draw from this distribution: 
     flip coins while consecuative heads; 
     count the total of such consecuative outcomes as a natural number. */
    let lev = 0;
    /* todo:
     do balanced append step here, adding an additional level/cat node */
    #cat(lev, s1, s2)    
  };

  public func sing<X>(x:X) : Seq<X> {
    #sing(x) 
  };
  
  public func iter<X>() : Iter<X> {
    // todo: binary tree zipper here?
    P.nyi()
  };

}
