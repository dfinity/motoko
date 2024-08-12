actor {

    func foo(next : () -> async ()) : async () {
        await (with cycles = 3000) next()
    };

    func bar(next : () -> async ()) : async () = async {
        await (with cycles = 4000) next()
    };

    public func oneshot() {
    };

    public func test(): async () {
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
        assert 42 == 42;
        assert 3 == (await (with cycles = 101) closA());
        assert 3 == (await (with cycles = 102) closB());

        await (with yeah = 8; timeout = 55; cycles = 1000)
        foo(func() : async () = async { assert message == "Hi!" });
        await (with cycles = 5000)
        bar(func() : async () = async { assert message == "Hi!" });
    };


    public func test2() : async () {
        await (with cycles = 1042) async { }
        
    }
}
