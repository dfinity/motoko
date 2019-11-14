let s = "hello world!\n";

Debug.print "via `Debug.print`:\n";
Debug.print s;
Debug.print "\n";

Debug.print "via iteration and `Debug.printChar`: #1\n";
for (a in s.chars()) {
  Debug.printChar a;
};
Debug.print "\n";

Debug.print "via iteration and `Debug.printChar`: #2\n";
var x = 0;
for (a in s.chars()) {
  x += 1;
  Debug.printNat x;
  Debug.print ":";
  Debug.printChar '\'';
  Debug.printChar a;
  Debug.printChar '\'';
  Debug.print " ";
};
Debug.print "\n";

let russian = "Приветствую, мир!\n";
assert(russian.len() == 18);

Debug.print "via iteration and `Debug.printChar` (Unicode): #3\n";
x := 0;
for (a in russian.chars()) {
  x += 1;
  Debug.printNat x;
  Debug.print ":";
  Debug.printChar '\'';
  Debug.printChar a;
  Debug.printChar '\'';
  Debug.print " ";
};
Debug.print "\n";
assert(x == 18);

let emojis = "🙈🎸😋";
assert(emojis.len() == 3);

Debug.print "via iteration and `Debug.printChar` (Unicode): #4\n";
x := 0;
for (a in emojis.chars()) {
  x += 1;
  Debug.printNat x;
  Debug.print ":";
  Debug.printChar '\'';
  Debug.printChar a;
  Debug.printChar '\'';
  Debug.print " ";
};
Debug.print "\n";
assert(x == 3);

Debug.print russian; Debug.print "\n";
switch (russian.chars().next()) {
  case (?c) { Debug.printChar c; Debug.print "\n" };
  case _ {};
};

switch (emojis.chars().next()) {
  case (?c) { assert (c == '\u{1f648}'); Debug.printChar c; Debug.print "\n" };
  case _ {};
};
