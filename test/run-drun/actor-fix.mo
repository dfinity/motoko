module Fix {

    /*actor class First<T <: actor { beep : () -> async () }>(this : T) = self {
        public func chime() : async Int {
            await this.beep();
        }
        }*/

    func First<T <: actor { beep : () -> async () }>(this : T) : actor { chime : () -> async Int } = actor {
        public func chime() : async Int {
            await this.beep();
            42
        }
    }

}
