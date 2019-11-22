let s = "hello world!";

debugPrint "via `debugPrint`:";
debugPrint s;
debugPrint "";

debugPrint "via iteration and `debugPrintChar`: #1";
for (a in s.chars()) {
  debugPrintChar a;
};
debugPrint "";

debugPrint "via iteration and `debugPrintChar`: #2";
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
debugPrint "";

let russian = "Приветствую, мир!\n";
assert(russian.len() == 18);

debugPrint "via iteration and `debugPrintChar` (Unicode): #3";
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
debugPrint "";
assert(x == 18);

let emojis = "🙈🎸😋";
assert(emojis.len() == 3);

debugPrint "via iteration and `debugPrintChar` (Unicode): #4";
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
debugPrint "";
assert(x == 3);

debugPrint russian; debugPrint "";
switch (russian.chars().next()) {
  case (?c) { debugPrintChar c; debugPrint "" };
  case _ {};
};

switch (emojis.chars().next()) {
  case (?c) { assert (c == '\u{1f648}'); debugPrintChar c; debugPrint "" };
  case _ {};
};
