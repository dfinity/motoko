let s = "hello world!\n";

print "via `print`:\n";
print s;
print "\n";

print "via iteration and `printChar`: #1\n";
for (a in s.chars()) {
  printChar a;
};
print "\n";

print "via iteration and `printChar`: #2\n";
var x = 0;
for (a in s.chars()) {
  x += 1;
  printNat x;
  print ":";
  printChar '\'';
  printChar a;
  printChar '\'';
  print " ";
};
print "\n";

let russian = "Приветствую, мир!\n";
assert(russian.len() == 18);

print "via iteration and `printChar` (Unicode): #3\n";
x := 0;
for (a in russian.chars()) {
  x += 1;
  printNat x;
  print ":";
  printChar '\'';
  printChar a;
  printChar '\'';
  print " ";
};
print "\n";
assert(x == 18);

let emojis = "🙈🎸😋";
assert(emojis.len() == 3);

print "via iteration and `printChar` (Unicode): #4\n";
x := 0;
for (a in emojis.chars()) {
  x += 1;
  printNat x;
  print ":";
  printChar '\'';
  printChar a;
  printChar '\'';
  print " ";
};
print "\n";
assert(x == 3);

print russian; print "\n";
switch (russian.chars().next()) {
  case (?c) { printChar c; print "\n" };
  case _ {};
};

switch (emojis.chars().next()) {
  case (?c) { assert (c == '\u{1f648}'); printChar c; print "\n" };
  case _ {};
};
