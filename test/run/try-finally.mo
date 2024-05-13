import { debugPrint; error } =  "mo:prim";

actor A {
    func m() : async () {
    };
    

    func t0() : async () {
        try { debugPrint "IN"; await m(); }
        case { debugPrint "OUT" };
    };

/*  nested `try` won't work
    func t1() : async () {
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
/*
    func t2() : async () {
        try {
            debugPrint "IN2";
            throw error "IN2";
        }
        else catch _ { debugPrint "CAUGHT2" }
        case { debugPrint "OUT2" };
    };

    //TODO: func t2t() : async Int { ... }

    func t3() : async () {
        try {
            debugPrint "IN3";
            return;
        }
        case { debugPrint "OUT3" };
    };

    // check that finally not running twice
    func t4() : async () {
        try {
            debugPrint "IN4";
        }
        case { debugPrint "OUT4" };
        return;
    };
*/
    func t5() : async () {
        label out try {
            debugPrint "IN5";
            await m();
            break out;
        }
        case { debugPrint "OUT5" };
    };

    // TODO: trap on happy/catch

    public func go() : async () {
        /*ignore*/ await t0();
        //await t1();
        /*await t2();
        await t3();
        await t4();*/
        await t5();
    };
};

//XSKIP comp
//SKIP ic-ref-run

A.go();
