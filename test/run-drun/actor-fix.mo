module Fix {

    public func First<T <: actor { beep : () -> async (); yell : shared () -> async () }>(this : T) : module { chime : shared () -> async Int } = module {
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


actor Third : actor { beep : () -> async (); chime : () -> async Int; yell : () -> async () } = {
    public func beep() : async () {};
    public let chime = Fix.First<actor { beep : () -> async (); yell : shared () -> async () }>(Third).chime;
    public let yell = Fix.Second<actor { chime : () -> async Int }>(Third).yell;

};
