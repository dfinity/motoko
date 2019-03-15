var yyy : Nat = 10000000000000;
printInt 10000000000000; print "@@@@@@@ \n";
printInt yyy; print "@@@@@@@ \n";
yyy+=3;
printInt yyy; print "@@@@@@@ \n";


func intToNat(i : Int) : Nat = word64ToNat (intToWord64 i);

func And (x : Bool, y : Bool) : Bool = switch (x, y) { case (true, true) true; case _ false }; 

func refShowInt (acc : Text, n : Int) : Text {
    if (0 > n) "-" # (refShowInt("", -n))
    else if (And(0 == n, acc == ""))
        "0"
    else {
        let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];
        if (n == 0) acc else refShowInt(digits[intToNat(n % 10)] # acc, n / 10)
       }
};


print (refShowInt("", 0 : Int)); print "@@@@@@@ \n";
print (refShowInt("", -13 : Int)); print "@@@@@@@ \n";
print (refShowInt("", -123456789)); print "@@@@@@@ \n";
print (refShowInt("", yyy)); print "@@@@@@@ \n";
