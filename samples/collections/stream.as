/* 
 * Streams, a la functional programming, in ActorScript.
 */

// Thunks are not primitive in AS, 
// ..but we can encode them as objects with a force method:
type Thk<T> = {force:() -> T};

// "Stream head optional" is the (optional) head of the stream; The
// "tail" of the stream head is a thunk, that when forced, produces
// the next stream head, or `null`.
type Sho<T> = ?(T,Thk<Sho<T>>);

// A "stream" contains a value "today", and a thunk for the value
// "tomorrow".  These streams have optional endings, represented by
// the `Sho` type; they may or may not end.
type Stream<T> = (T,Thk<Sho<T>>);


