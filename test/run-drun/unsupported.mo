// top-level actor objects are supported
actor Counter {

    shared func bad_private_shared() { }; // not  public

    public func badactorarg(a:actor{}) : future () {};

    public func badfunctionarg(f:shared()->future ()) : future () {};

    /* TODO
    public func badoneway(){}; // unsupported oneway
    */

    public func ok() : future () {};

    public func ok_explicit() : future () = future {};

    public func bad_call() : future () {
        ignore (ok()); // unsupported intercanister messaging
    };

    public func bad_await_call() : future () {
        await (ok()); // unsupported intercanister messaging
    };

    public func bad_await() : future () {
        let t : future Int = loop {};
	await t; // unsupported general await
    };

    public func badfuture() : future () {
        let a = future { 1; }; // unsupported future
    };

}
;

shared func bad_shared() { }; // not actor enclosed

func local_spawn() {
  ignore(future ()); // not yet supported
};

{
    // shared function types aren't sharable
    type illformed_1 = shared (shared () -> ()) -> future ();
};

{
    // actors aren't shareable
    type illformed_2 = shared (actor {}) -> future ();
};

/* TODO
{
    // oneway functions aren't supported
    type illformed_3 = shared () -> ();
};
*/

{
  actor class BadActorClass () { }; // no actor classes
};

{
 let bad_non_top_actor : actor {} = if true actor {} else actor {};
};

{
  let bad_nested_actor =  { actor {}; ()};
};


actor BadSecondActor { };

func implicit_future() : future () { }; // future functions not supported
