/*
 * Streams, a la functional programming, in ActorScript.
 *
 * Streams are lazy lists that may or may not end.
 * If non-empty, a stream contains a value "today",
 * and a thunk for the value "tomorrow", if any.
 *
 */

// Done:
//
//  - standard stream definition (well, two versions)
//  - standard higher-order combinator: mapfilter

// TODO-Matthew: Write:
//
//  - standard stream combinators: take, drop, merge, sort, etc...
//  - iterator objects, for use in 'for ... in ...' patterns
//  - streams+pairs: zip, split, etc
//  - regression tests for everything that is below

// TODO-Matthew: File issues:
//
//  - unhelpful error message around variable shadowing (search for XXX below)
//

// Thunks are not primitive in AS,
// ..but we can encode them as objects with a force method:
type Thk<T> = {force:() -> T};

// A "Stream Head" ("Sh") is the head of the stream, which _always_
// contains a value "today"; Its "tail" is a thunk that produces the
// next stream head ("tomorrow"), or `null`.
type Sh<T> = (T, ?Thk<Sh<T>>);

// "Optional Stream Head" (Osh) is the optional head of the stream.
// This type is related to Sh<T>, but is not equivalent.
type Osh<T> = ?(T, Thk<Osh<T>>);

// type Stream<T> =
//   ??? Sh<T> or Osh<T>
// Q: Which is more more "conventional?"
//

// map-and-filter; tail recursive.
// acts eagerly when the predicate fails,
// and lazily when it succeeds.
func mapfilter<T,S>(l : Osh<T>, f:T -> ?S) : Osh<S> = {
  func rec(l : Osh<T>) : Osh<S> {
    switch l {
      case null     { null };
      case (?(h,t)) {
        switch (f(h)) {
        case null { rec(t.force()) };
        case (?h_){
            // XXX -- When we shadow `t` we get a strange/wrong type error:
            //
            //  let t = new{force():Osh<S>{ rec(t.force()) }};
            //  ?(h_,t)
            //
            let s = new{force():Osh<S>{ rec(t.force()) }};
            ?(h_,s)
          };
        }
      };
    }
  };
  rec(l)
};
