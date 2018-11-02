type Object = { name : Text };

func f(objects : Object[]) {
  /* type error, expected iterable type, but expression has type Object[] */
  for (object in objects) {
    print(object.name);
  };
};

let xs : Object[] = [
  new { name = "a"; }
];

f(xs);
