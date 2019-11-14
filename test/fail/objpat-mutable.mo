type WithMut = { var x : Int; y : Text };

func reject (o : WithMut) =
  switch o {
    case { y = "good" } { Debug.print "good" };
    //case { x = -42 } { Debug.print "bad" }; // backend: Invalid_argument("unop")
    //case { x = (42 : Int) } { Debug.print "bad" }; // tc: cannot consume expected
    case { x } { Debug.printInt x }
  };
