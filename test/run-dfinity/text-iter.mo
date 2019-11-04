let s = "hello world!\n";

debug_print "via `debug_print`:\n";
debug_print s;
debug_print "\n";

debug_print "via iteration and `debug_print_Char`: #1\n";
for (a in s.chars()) {
  debug_print_Char a;
};
debug_print "\n";

debug_print "via iteration and `debug_print_Char`: #2\n";
var x = 0;
for (a in s.chars()) {
  x += 1;
  debug_print_Nat x;
  debug_print ":";
  debug_print_Char '\'';
  debug_print_Char a;
  debug_print_Char '\'';
  debug_print " ";
};
debug_print "\n";

let russian = "Приветствую, мир!\n";
assert(russian.len() == 18);

debug_print "via iteration and `debug_print_Char` (Unicode): #3\n";
x := 0;
for (a in russian.chars()) {
  x += 1;
  debug_print_Nat x;
  debug_print ":";
  debug_print_Char '\'';
  debug_print_Char a;
  debug_print_Char '\'';
  debug_print " ";
};
debug_print "\n";
assert(x == 18);

let emojis = "🙈🎸😋";
assert(emojis.len() == 3);

debug_print "via iteration and `debug_print_Char` (Unicode): #4\n";
x := 0;
for (a in emojis.chars()) {
  x += 1;
  debug_print_Nat x;
  debug_print ":";
  debug_print_Char '\'';
  debug_print_Char a;
  debug_print_Char '\'';
  debug_print " ";
};
debug_print "\n";
assert(x == 3);

debug_print russian; debug_print "\n";
switch (russian.chars().next()) {
  case (?c) { debug_print_Char c; debug_print "\n" };
  case _ {};
};

switch (emojis.chars().next()) {
  case (?c) { assert (c == '\u{1f648}'); debug_print_Char c; debug_print "\n" };
  case _ {};
};
