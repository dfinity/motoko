actor Demo {
    actor otherActor {
        public func find(i : Int) : async Int {
            i + 25
        }
    };

    public func message() : async () {
        let r = comp();
        let h = await otherActor.find(r);
        process h
    };

    func comp() : Int = 42;
    func process(i : Int) = ()
}
