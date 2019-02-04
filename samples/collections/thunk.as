/* 
 * Thunks, a la functional programming, in ActorScript.
 */

// Thunks are not primitive in AS, 
// ..but we can encode them as objects with a force method:
type Thk<T> = {force:() -> T};

// lift a value into a "value-producing thunk"
func lift<T>(a:T) : Thk<T> = 
  new { force() : T { a } };

// apply a function to a thunk's value
func app<T,S>(f:T->S, x:Thk<T>) : Thk<S> {
  new { force() : S { f(x.force()) } }
};

// pair two thunks' values
/*
 // XXX I don't understand this type error:
 //
 // type error, expression of type
 // () -> (T/23, S/4)
 // cannot produce expected type
 // () -> ((T/23, S/4))

func pair<T,S>(x:Thk<T>, y:Thk<S>) : Thk<(T,S)> {
  new { force() : (T,S) { (x.force(), y.force()) } }
};
*/

// project first from a pair-valued thunk
func fst<T,S>(x:Thk<(T,S)>) : Thk<T> {
  new { force() : T { x.force().0 } }
};

// project second from a pair-valued thunk
func snd<T,S>(x:Thk<(T,S)>) : Thk<S> {
  new { force() : S { x.force().1 } }
};
