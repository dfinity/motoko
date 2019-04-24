/**

 Result
 =========

 The result of a computation that may contain errors, exceptions, etc.
 
 ActorScript does not have exceptions, so we use a datatype to encode these outcomes.

 Rust does something analogous, for the same reason.  We use the Rust nomenclature for the datatype and its constructors.
 
 */

type Result<Ok,Err> = {
  #ok:Ok;
  #err:Err;
};

/**
 `assertUnwrap`
 ---------------
 assert that we can unwrap the result; should only be used in tests, not in canister implementations. This will trap.
*/
func assertUnwrap<Ok,Error>(r:Result<Ok,Error>):Ok {
  switch(r) {
    case (#err e) unreachable();
    case (#ok r) r;
  }
};

/**
 `assertUnwrapAny`
 ---------------
 */
func assertUnwrapAny<Ok>(r:Result<Ok,Any>):Ok {
  switch(r) {
    case (#err e) unreachable();
    case (#ok r) r;
  }
};

/**
 `assertOk`
 ---------------
*/
func assertOk(r:Result<Any,Any>) {
  switch(r) {
    case (#err _) unreachable();
    case (#ok _) ();
  }
};

/**
 `assertErr`
 ---------------
*/
func assertErr(r:Result<Any,Any>) {
  switch(r) {
    case (#err _) ();
    case (#ok _) unreachable();
  }
};

/**
 `bind`
 -------
 bind operation in result monad.
*/
func bind<R1,R2,Error>(
  x:Result<R1,Error>,
  y:R1 -> Result<R2,Error>) : Result<R2,Error> {
  switch x {
  case (#err e) (#err e);
  case (#ok r) (y r);  
  }
};

/**
 `option`
 -------
 create a result from an option, including an error value to handle the `null` case.
*/
func optionResult<R,E>(x:?R, err:E):Result<R,E> {
  switch x {
    case (? x) {#ok x};
    case null {#err err};
  }
}
