/* Test asynchronous construction of an object */

let a = future {
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

let b = future {
  let o = object {
    let a = await (future "ba") ;
    let b = "bb";
    public func get_a() : Text { a };
    public func get_b() : Text { b };
  };
  let (a, b) = (o.get_a(), o.get_b());
  print a;
  print b;
  print "\n";
};

let c = future {
  let o = object {
    let a = await (future "ca") ;
    let b = await (future "cb");
    public func get_a() : Text { a };
    public func get_b() : Text { b };
  };
  let (a, b) = (o.get_a(), o.get_b());
  print a;
  print b;
  print "\n";
};

let d = future {
  let o = object {
    let a = "da";
    let b = await (future "db");
    public func get_a() : Text { a };
    public func get_b() : Text { b };
  };
  let (a, b) = (o.get_a(), o.get_b());
  print a;
  print b;
  print "\n";
};

let e = future {
  let o = object this {
    let a = "ea";
    let b = await (future "eb");
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
