

actor Hey {
    import hi : <shared type>;
    import other : <shared type>;

    func worker : ty  { await hi() }

} : actor { worker : ty; import hi : <shared type>; import other : 

  Hey

module Fix {

    type BaseA = actor { beep : () -> async (); yell : shared () -> async () };
    type BaseC = actor { beep : () -> async (); yell : shared () -> async () };

    actor mixin Me (params) = this : BaseA {
        //import this : BaseA
        stable var eeek = 77;
        public shared func chime() : async Int {
            await this.beep();
            if false {
                await this.yell();
            };
            42

        };

        Me : (params) -> (BaseA -> actor { chime : () -> async Int } and BaseA);


    public func First(params : Int)(this : BaseA)  : actor mixin { chime : shared () -> async Int } = module {
        stable var eeek = 77;
        public shared func chime() : async Int {
            await this.beep();
            if false {
                await this.yell();
            };
            42
        }
    };

    public func Second<T <: actor { chime : () -> async Int }>(this : T) : module { yell : shared () -> async () } = module {
        private func foo(i : Int) : () {
            ignore i
        };
        public shared func yell() : async () {
            foo(await this.chime());
        }
    }

};

Fix.Me(42);

actor Third : actor { beep : () -> async (); chime : () -> async Int; yell : () -> async () } = {
    public func beep() : async () {};
    ///public let chime = Fix.First<actor { beep : () -> async (); yell : shared () -> async () }>(Third).chime;
    ///public let yell = Fix.Second<actor { chime : () -> async Int }>(Third).yell;

              } and Fix.First(42) and ;
