type WithMut = { var x : Int; y : Text };

func reject (o : WithMut) =
  switch o {
    case { y = "good" } { debugPrint "good" };
    //case { x = -42 } { debugPrint "bad" }; // backend: Invalid_argument("unop")
    //case { x = (42 : Int) } { debugPrint "bad" }; // tc: cannot consume expected
    case { x } { debugPrintInt x }
  };
