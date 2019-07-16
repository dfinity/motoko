/* Test asynchronous construction of an object */

let a = async {
  let o = object {
    let a = "aa";
    let b = "ab";
	  public func get_a() : Text { a };
	  public func get_b() : Text { b };
  };
  let (a, b) = (o.get_a(), o.get_b());
  print a;
  print b;
  print "\n";
};

let b = async {
  let o = object {
    let a = await (async "ba") ;
    let b = "bb";
		public func get_a() : Text { a };
		public func get_b() : Text { b };
  };
  let (a, b) = (o.get_a(), o.get_b());
  print a;
  print b;
  print "\n";
};

let c = async {
  let o = object {
    let a = await (async "ca") ;
    let b = await (async "cb");
    public func get_a() : Text { a };
    public func get_b() : Text { b };
  };
  let (a, b) = (o.get_a(), o.get_b());
  print a;
  print b;
  print "\n";
};

let d = async {
  let o = object {
    let a = "da";
    let b = await (async "db");
    public func get_a() : Text { a };
    public func get_b() : Text { b };
  };
  let (a, b) = (o.get_a(), o.get_b());
  print a;
  print b;
  print "\n";
};

let e = async {
  let o = object this {
    let a = "ea";
    let b = await (async "eb");
		public func get_a() : Text { a };
		public func get_b() : Text { b };
		public func get_ab() : (Text, Text) {
		  (this.get_a(), this.get_b());
		};
	};
  let (a, b) = o.get_ab();
  print a;
  print b;
  print "\n";
};
