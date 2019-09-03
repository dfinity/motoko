type O = shared { who : Text; what : Text; count : Int };

type RegionInfo = shared {
  id : Nat;
  short_name : Text;
  description : Text;
};


actor {
  hello (data : [O]) : async Text {
    var r = "Reporting this:\n";
    for (o in data.vals()) {
      r := r # o.who # " drank " # debug_show (o.count) # " bottles of " # o.what # ".\n"
    };
    return r;
  };

  returnRecord () : async [RegionInfo] {
    return [
      shared {
        id = 1;
        short_name = "C";
        description= "Central";
      }
    ];
  };
}
