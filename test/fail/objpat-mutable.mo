type WithMut = { var x : Int; y : Text };

func reject (o : WithMut) =
  switch o {
    case { y = "good" } { print "good" };
    //case { x = -42 } { print "bad" }; // backend: Invalid_argument("unop")
    //case { x = (42 : Int) } { print "bad" }; // tc: cannot consume expected
    case { x } { printInt x }
  };
