switch "duplicate field names" {
  case { len = l; chars; len } {
    for (x in chars()) { printInt (l()); }
  }
};

switch "duplicate bindings" {
  case { len; chars = len } {
    printInt (len())
  }
};

let exercise_coverage = new { x = true };
switch exercise_coverage {
  case { x = true; x = false } ()
};

// this is incorrectly analysed currently
// should be matched by the second case

switch (true, false) {
 case { x = (true,_); x = (_,true) } ();
 case { x = (true,_); x = (_,false) } ();
 case { x = (false,_); x = (_,true) } ();
 case { x = (false,_); x = (_,false) } ();
};
