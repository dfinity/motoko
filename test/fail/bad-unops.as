let a = ^3;
let b = ^-5;
let c : Nat = +3;
let d : Nat = -3;

//switch (1) { case (^1) "good"; case _ "hmmm" }; // Fatal error: exception Invalid_argument("unop")
ignore (switch (1) { case (+1) "good"; case _ "hmmm" });
//switch (1) { case (-1) "good"; case _ "hmmm" }; // Fatal error: exception Invalid_argument("unop")

//switch (-1) { case (^1) "good"; case _ "hmmm" }; // Fatal error: exception Invalid_argument("unop")
ignore (switch (-1) { case (+1) "good"; case _ "hmmm" });
ignore (switch (-1) { case (-1) "good"; case _ "hmmm" });
