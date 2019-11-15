import E "../stdlib/env.mo";
import A "../stdlib/array.mo";

module {
  /*  
    A Cell can be thought of as a database cell that holds a single value which can be read and written. On top of that a cell:
    a) saves the entire history of changes made to the value, and
    b) saves environmental state with each write (e.g. time).

    Todo: This is currently implemented using arrays, but should probably be switched to lists at some point. 
  */
  public class Cell<T>(env : E.Env, init : T) {
    // The journal (history) of writes.
    var j : [E.Event<T>] = [env.wrap<T>(init)]; 
  
    // The write function.
    public func write(v : T) : () { 
      j := A.append<E.Event<T>>(j, [env.wrap<T>(v)]) 
    };
  
    // Read functions.
    // The current value (i.e. the last one written). 
    public func read() : T { 
      env.unwrap<T>(A.fromEnd<E.Event<T>>(j, 0)) 
    };

    // The entire raw journal.
    public func journal() : [E.Event<T>] = j ;

    // The history of written values as an array.
    public func values() : [T] { 
      A.map<E.Event<T>,T>(func(e){env.unwrap<T>(e)}, j) 
    };
  };
}
