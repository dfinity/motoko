let s = "hello world!\n";

debugPrint "via `debugPrint`:\n";
debugPrint s;
debugPrint "\n";

debugPrint "via iteration and `debugPrintChar`: #1\n";
for (a in s.chars()) {
  debugPrintChar a;
};
debugPrint "\n";

debugPrint "via iteration and `debugPrintChar`: #2\n";
var x = 0;
for (a in s.chars()) {
  x += 1;
  debugPrintNat x;
  debugPrint ":";
  debugPrintChar '\'';
  debugPrintChar a;
  debugPrintChar '\'';
  debugPrint " ";
};
debugPrint "\n";

let russian = "Приветствую, мир!\n";
assert(russian.len() == 18);

debugPrint "via iteration and `debugPrintChar` (Unicode): #3\n";
x := 0;
for (a in russian.chars()) {
  x += 1;
  debugPrintNat x;
  debugPrint ":";
  debugPrintChar '\'';
  debugPrintChar a;
  debugPrintChar '\'';
  debugPrint " ";
};
debugPrint "\n";
assert(x == 18);

let emojis = "🙈🎸😋";
assert(emojis.len() == 3);

debugPrint "via iteration and `debugPrintChar` (Unicode): #4\n";
x := 0;
for (a in emojis.chars()) {
  x += 1;
  debugPrintNat x;
  debugPrint ":";
  debugPrintChar '\'';
  debugPrintChar a;
  debugPrintChar '\'';
  debugPrint " ";
};
debugPrint "\n";
assert(x == 3);

debugPrint russian; debugPrint "\n";
switch (russian.chars().next()) {
  case (?c) { debugPrintChar c; debugPrint "\n" };
  case _ {};
};

switch (emojis.chars().next()) {
  case (?c) { assert (c == '\u{1f648}'); debugPrintChar c; debugPrint "\n" };
  case _ {};
};
