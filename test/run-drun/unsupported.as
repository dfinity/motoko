// top-level actor objects are supported
actor Counter {

    /* TODO
    func shared bad_shared() { }; // not  public
    */

    public func badactorarg(a:actor{}) : async () {};

    public func badfunctionarg(f:shared()->async ()) : async () {};

    /* TODO
    public func badoneway(){}; // unsupported oneway
    */

    public func ok() : async () {};

    public func ok_explicit() : async () = async {};

    public func bad_call() : async () {
        ignore (ok()); // unsupported intercanister messaging
    };

    public func bad_await_call() : async () {
        await (ok()); // unsupported intercanister messaging
    };

    public func bad_await() : async () {
        let t : async Int = loop {};
	await t; // unsupported general await
    };

    public func badasync() : async () {
        let a = async { 1; }; // unsupported async
    };

}
;

/* TODO
func shared bad_shared() { }; // not actor enclosed
*/

func local_spawn() {
  ignore(async ()); // not yet supported
};

{
    // shared function types aren't sharable
    type illformed_1 = shared (shared () -> ()) -> async ();
};

{
    // actors aren't shareable
    type illformed_2 = shared (actor {}) -> async ();
};

/* TODO
{
    // oneway functions aren't supported
    type illformed_3 = shared () -> ();
};
*/

