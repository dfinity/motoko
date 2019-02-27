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
  printInt x;
  print ":";
  printChar '\'';
  printChar a;
  printChar '\'';
  print " ";
};
print "\n";
