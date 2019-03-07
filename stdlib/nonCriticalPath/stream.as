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
//  - standard higher-order combinators: map, mapfilter, merge

// TODO-Matthew: Write:
//
//  - (more) stream combinators: take, drop, sort, etc...
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

// stream map; trivially tail recursive. lazy.
func map<T,S>(l : Osh<T>, f:T -> S) : Osh<S> = {
  func rec(l : Osh<T>) : Osh<S> {
    switch l {
      case null     { null };
      case (?(h,t)) {
        let s = new{force():Osh<S>{ rec(t.force()) }};
        ?(f(h),s)
      };
    }
  };
  rec(l)
};

// stream merge (aka "collate"); trivially tail recursive. lazy.
func merge<T>(s1 : Osh<T>, s2 : Osh<T>, f:(T,T) -> Bool) : Osh<T> = {
  func rec(s1 : Osh<T>, s2 : Osh<T>) : Osh<T> {
    switch (s1, s2) {
      case (null, _) { s2 };
      case (_, null) { s1 };
      case (?(h1,t1), ?(h2,t2)) {
        if (f(h1,h2)) {
          // case: h1 is "today", h2 is "later"...
          let s = new{force():Osh<T>{ rec(t1.force(), s2) }};
          ?(h1,s)
        } else {
          // case: h2 is "today", h2 is "later"...
          let s = new{force():Osh<T>{ rec(s1, t2.force()) }};
          ?(h2,s)
        }
      }
    }
  };
  rec(s1, s2)
};

// stream map-and-filter; tail recursive.
// acts eagerly when the predicate fails,
// and lazily when it succeeds.
func mapfilter<T,S>(l : Osh<T>, f:T -> ?S) : Osh<S> = {
  func rec(s : Osh<T>) : Osh<S> {
    switch s {
    case null     { null };
    case (?(h,t)) {
        switch (f(h)) {
        case null { rec(t.force()) };
        case (?h_){
          // XXX -- When we shadow `t` we get a strange/wrong type error:
          //
          // type error, expression of type
          //    Osh<S/3> = ?(S/3, Thk<Osh<S/3>>)
          // cannot produce expected type
          //    ?(T/28, Thk<Osh<T/28>>)
          //
          // let t = new{force():Osh<S>{ rec(t.force()) }};
          // ?(h_,t)
          let s = new{force():Osh<S>{ rec(t.force()) }};
          ?(h_,s)
         };
        }
      };
    }
  };
  rec(l)
}
