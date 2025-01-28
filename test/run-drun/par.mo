import { call_raw; debugPrint; principalOfActor; replyDeadline; errorMessage; errorCode } = "mo:⛔";
import Cycles = "cycles/cycles";

actor A {

    func foo(next : () -> async ()) : async () {
        assert 0 != replyDeadline();
        debugPrint ("foo: " # debug_show(Cycles.available()));
        await (with cycles = 3000) next()
    };

    func bar(next : () -> async ()) : async () = async {
        await (with cycles = 4000) next()
    };

    public func oneshot() {
        debugPrint ("oneshot deadline set: " # debug_show(0 != replyDeadline()));
        debugPrint ("oneshot cycles: " # debug_show(Cycles.available()));
    };

    public func rawable() : async () {
        debugPrint ("rawable: " # debug_show(Cycles.available()));
    };

    public func test() : async () {
        debugPrint "test()";
        let message = "Hi!";

        func closA() : async Nat {
            message.size()
        };

        func closB() : async Nat = async {
            message.size()
        };
/*
         // this is ruled out
        func closC() : async Nat =
          if (1 == 2) async {
            message.size()
          } else async {
            message.size() + 1
          };
*/

/*      //Rule this out?
        func closD() : async Nat = (with cycles = 765) async {
            message.size()
        };    
*/

        assert 3 == (await (with cycles = 101) closA());
        assert 3 == (await (with cycles = 102) closB());

        await (with yeah = 8; timeout = 55; cycles = 1000)
        foo(func() : async () = async { assert message == "Hi!" });
        await (with cycles = 5000)
        bar(func() : async () = async { assert message == "Hi!" });
    };

    public func test2() : async () {
        debugPrint "test2()";
        await (with cycles = 1042) async { assert Cycles.available() == 1042 };
    };

    public func test3() : async () {
        debugPrint "test3()";
        oneshot();
        (with cycles = 3456) oneshot();
        (with timeout = 5) oneshot();
        (with timeout = 5; cycles = 4567) A.oneshot();
    };

    public func test4() : async () {
        debugPrint "test4()";
        ignore await call_raw(principalOfActor A, "rawable", "DIDL\00\00");
        ignore await (with cycles = 3456) call_raw(principalOfActor A, "rawable", "DIDL\00\00");
    };

    public func test5() : async () {
        await (with timeout = 3) async {
            debugPrint "test5()";
            assert 0 != replyDeadline();
        }
    };

    public func ext() : async () {
        assert 0 != replyDeadline();
        debugPrint ("ext: " # debug_show(Cycles.available()));
    };

    public func test6() : async () {
        await (with timeout = 3; cycles = 6543) A.ext()
    };

    public func test7() : async () {
        debugPrint "test7()";
        try
          await (with timeout = 1) async {
              assert 0 != replyDeadline();
              // busy loop
              loop ignore await (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand();
              ()
          }
        catch e {
            debugPrint("CAUGHT: " # debug_show errorCode e # " " # debug_show errorMessage e)
        }
    }
}

// testing
//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL ingress test "DIDL\x00\x00"
//CALL ingress test2 "DIDL\x00\x00"
//CALL ingress test3 "DIDL\x00\x00"
//CALL ingress test4 "DIDL\x00\x00"
//CALL ingress test5 "DIDL\x00\x00"
//CALL ingress test6 "DIDL\x00\x00"
//CALL ingress test7 "DIDL\x00\x00"
