actor {
  type t_in = {
    #f0 : Null;
    #f1 : Any;
    #f2 : Any;
    #f3 : { foo : Int; bar : Text };
  };

  // different types to exercise subtyping
  type t_out = {
    #f0 : Null;
    #f1 : Any;
    #f2 : Text;
    #f3 : { aaa : Text; foo : Int; bar : Text; dad : Text; zzz : Text };
  };

  public func foo(n : t_in) : () {
    print("all izz well\n");
  };

  public func seed(n : Nat) : async t_out {
    switch n {
      case 0 (#f0 null);
      case 1 (#f1 null);
      case 2 (#f2 "Just a test");
      case 3 (#f3 { aaa = "A"; foo = 1; bar = "B"; dad = "C"; zzz = "Z"});

      case _ { print("No such seed"); #f0 null}
    }
  };
}
