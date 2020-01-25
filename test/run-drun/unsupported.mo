// top-level actor objects are supported
actor Counter {

    shared func bad_private_shared() { }; // not  public

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
        let t : async () = loop {};
	await t; // unsupported general await
    };

    public func badasync() : async () {
        let a = async { 1; }; // unsupported async
    };

}
;

shared func bad_shared() { }; // not actor enclosed

func local_spawn() {
  ignore(async ()); // no async capability
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

{
  actor class BadActorClass () { }; // no actor classes
};

{
  actor class BadActorClass (x : Int) { }; // no actor classes
};

{
 let bad_non_top_actor : actor {} = if true actor {} else actor {};
};

{
  let bad_nested_actor =  { let _ = actor {}; ()};
};


actor BadSecondActor { };

// async functions not supported (inference mode)
func implicit_async() : async () { };

// anonymous shared functions not supported (inference and checking mode)
let _ = shared func() : async () { };
(shared func() : async () { }) : shared () -> async ();
