type WithMut = { var x : Int; y : Text };

func reject (o : WithMut) =
  switch o {
    case { y = "good" } { debug_print "good" };
    //case { x = -42 } { debug_print "bad" }; // backend: Invalid_argument("unop")
    //case { x = (42 : Int) } { debug_print "bad" }; // tc: cannot consume expected
    case { x } { debug_print_Int x }
  };
