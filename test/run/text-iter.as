let x = "hello";
let s = " ";
let y = "world";
let z = "!\n";
let c1 = x # s # (y # z);

print "via `print`:\n";
print c1;
print "\n";

print "via iteration and `printChar`:\n";
for (a in c1.chars()) {
  printChar a;
}