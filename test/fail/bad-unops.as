let a = ^3;
let b = ^-5;
let c : Nat = +3;
let d : Nat = -3;

switch (1) { case (^1) "hmmm"; case _ "good" };
switch (1) { case (+1) "good"; case _ "hmmm" };
switch (1) { case (-1) "hmmm"; case _ "good" };

switch (-1) { case (^1) "hmmm"; case _ "good" };
ignore (switch (-1) { case (+1) "hmmm"; case _ "good" });
ignore (switch (-1) { case (-1) "good"; case _ "hmmm" });

ignore (switch (^1 : Word8) { case (^1) "good"; case _ "hmmm" });
//switch (+1 : Word8) { case (+1) "good"; case _ "hmmm" }; // both `+1` are not accepted
ignore (switch (-1 : Word8) { case (-1) "good"; case _ "hmmm" });

-- when testing the switch expression in the REPL, I either
--  * expect "good", or
--  * a type error (as stated in the corresponding .ok file)
--
-- test steps wrapped in ignore should type-check
