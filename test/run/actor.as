/* test asynchronous construction of an actor */
let a = async {
  actor o {
    private a = "aa";
    private b = "ab";
	 get_a() : async Text { a };
    get_b() : async Text { b };
  };
  let (a, b) = (await o.get_a(), await o.get_b());
  print a;
  print b;
  print "\n";
};

let b = async {
  actor o {
    private a = await (async "ba");
    private b = "bb";
    get_a() : async Text { a };
    get_b() : async Text { b };
  };
  let (a,b) = (await o.get_a(), await o.get_b());
  print a;
  print b;
  print "\n";
};

let c = async {
  actor o {
    private a = await (async "ca");
    private b = await (async "cb");
    get_a() : async Text { a };
    get_b() : async Text { b };
  };
  let (a, b) = (await o.get_a(), await o.get_b());
  print a;
  print b;
  print "\n";
};

let d = async {
  actor o {
    private a = "da";
    private b = await (async "db");
    get_a() : async Text { a };
    get_b() : async Text { b };
  };
  let (a, b) = (await o.get_a(), await o.get_b());
  print a;
  print b;
  print "\n";
};


let e = async {
  let o = actor this {
    private a = "ea";
    private b = await (async "eb");
    get_a() : async Text { a };
    get_b(): async Text { b };
    get_ab(): async (Text, Text) {
	   (await this.get_a(), await this.get_b());
    };
  };
  let (a, b) = await(o.get_ab());
  print a;
  print b;
  print "\n";
};
