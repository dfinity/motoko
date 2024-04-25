import { debugPrint; error } =  "mo:prim";

actor A {

    public func t0() : async () {
        try { debugPrint "IN" }
        case { debugPrint "OUT" };
    };
/*  nested `try` won't work
    public func t1() : async () {
        try {
            try {
                debugPrint "IN1";
                throw error "IN1";
            }
            case { debugPrint "OUT1" };
        }
        catch _ { debugPrint "CAUGHT1" }
    };
*/
    public func t2() : async () {
        try {
            debugPrint "IN2";
            throw error "IN2";
        }
        else catch _ { debugPrint "CAUGHT2" }
        case { debugPrint "OUT2" };
    };

    public func t3() : async () {
        try {
            debugPrint "IN3";
            return;
        }
        case { debugPrint "OUT3" };
    };

};

//SKIP run-low
//SKIP run-ir
//SKIP ic-ref-run


await A.t0();
//await A.t1();
await A.t2();
await A.t3();
