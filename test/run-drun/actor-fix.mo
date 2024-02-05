module Fix {

    actor class First<T <: actor { beep : () -> async () }>(this : T) = self {
        public func chime() : async Int {
            await this.beep();
        }
    }

}
