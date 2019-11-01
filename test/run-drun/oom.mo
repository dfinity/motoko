actor Oom {

  private let N : Nat = 8;

  private func lp() : () {
    for (i in range(0,N)) {
      for (j in range(0,N)) {
        for (p in [-1,0,1].vals()) {
          for (q in [-1,0,1].vals()) {
            print "exists";
            printInt i;
            printInt j;
	    printInt p;
            printInt q;
          };
        };
      };
    };
  };

  public func doloop() : async () {
     lp();
  };
}

//CALL ingress doloop "DIDL\x00\x00"
//CALL ingress doloop "DIDL\x00\x00"
//CALL ingress doloop "DIDL\x00\x00"
//CALL ingress doloop "DIDL\x00\x00"
//CALL ingress doloop "DIDL\x00\x00"
//CALL ingress doloop "DIDL\x00\x00"
