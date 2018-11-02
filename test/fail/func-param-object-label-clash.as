type Object = { name : Text };

func f(name : Text) : Object {
  /* execution error, accessing identifier before its definition */
  new { name = name; };
};

let _ : Object = f("a");
