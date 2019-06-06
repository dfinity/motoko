let a = ^3; // error
let b = ^-5; // error
let c : Nat = +3; // okay
let d : Nat = -3; // should be error

ignore (switch (1) { case (^1) "good"; case _ "hmmm" }); // error
ignore (switch (1) { case (+1) "good"; case _ "hmmm" }); // okay
ignore (switch (1) { case (-1 : Int) "good"; case _ "hmmm" }); // should be okay, crashes in interpreter

ignore (switch (-1) { case (^1) "good"; case _ "hmmm" }); // error
ignore (switch (-1) { case (+1) "good"; case _ "hmmm" });
ignore (switch (-1) { case (-1) "good"; case _ "hmmm" });


ignore (switch (1) { case (-1 : Int) "good"; case _ "hmmm" }); // should be okay
switch (1) { case (-1 : Nat) "good"; case _ "hmmm" }; // should be error
