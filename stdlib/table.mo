import E "../stdlib/env.mo";
import C "../stdlib/cell.mo";
import Array "../stdlib/array.mo";

module {
  /*  
    A Table is a two-colum table of cells of type A and B, respectively. 
  */
  type Row<A,B> = (C.Cell<A>, C.Cell<B>);

  public class Table<A,B>(env_ : E.Env, a_ : A, b_ : B) {
    let env = env_;

    func newRow(a : A, b : B) : Row<A,B> {
      (C.Cell<A>(env, a), C.Cell<B>(env, b));
    };

    // The table.
    // TODO: it might be better to allow for empty table after 
    // initialization. Ok for now. 
    var t : [Row<A,B>] = [newRow(a_,b_)];

    // Return the length
    public func len() : Nat = t.len();

    // The add function appends a row to the table.
    public func append(a : A, b : B) { 
      t := Array.append<Row<A,B>>(t,[newRow(a,b)]); 
    };

    // Two write functions for writing to first and second
    // column, respectively. Will trap if index i is out of range.
    public func write0(i : Nat, a : A) : () { 
      t[i].0.write(a);
    };
    public func write1(i : Nat, b : B) : () { 
      t[i].1.write(b);
    };

    // Two read functions for reading from first and second
    // column, respectively.
    public func read0(i : Nat) : A = t[i].0.read();
    public func read1(i : Nat) : B = t[i].1.read();

    // Return the raw table
    public func raw() : [Row<A,B>] = t;

    // Eval applies a user-provided function to each row.
    // The return value is a list. This allows to eliminate 
    // (i.e. filter) rows by returning an empty list.
    public func eval(f : (A,B) -> [A]) : [A] {
      func g(r : Row<A,B>) : [A] = f(r.0.read(), r.1.read());
      Array.bind<Row<A,B>,A>(t, g);
    }; 

    // Same as eval but more general.
    public func evalC<C>(f : (A,B) -> [C]) : [C] {
      func g(r : Row<A,B>) : [C] = f(r.0.read(), r.1.read());
      Array.bind<Row<A,B>,C>(t, g);
    }; 

    // Similar to raw() but returns types A,B directly instead of Cells.
    public func export() : [(A,B)] {
      evalC<(A,B)>(func(a,b){[(a,b)]}); 
    }; 
  };
}
