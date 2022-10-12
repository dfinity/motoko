actor {


  var claimed = false;

  var count = 0 : Int;


  public shared func claim() : async () {
    var t = true;
    let f = false;
    if (not claimed) {
      claimed := true;
/*
      await async {
        count := 1;
      };
      */
    };
  };

}