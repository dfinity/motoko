let p = new {x = 3; private y = 2; get_y() : Int = y};
let x = p.x;
let y = p.get_y();

type O = {self : () -> O};
let o = new this {self() : O = this};
let oo = o.self();

let tictac = new this {
  tic(n : Int) = if (n > 0) this.tac(n - 1);
  tac(n : Int) = if (n > 0) this.tic(n - 1);
};
let () = tictac.tic(10);

func ignore(_ : async ()) = ();
let tictac_async = new this {
  tic(n : Int) : async () { if (n > 0) ignore(this.tac(n - 1)) };
  tac(n : Int) : async () { if (n > 0) ignore(this.tic(n - 1)) };
};
let _ = tictac_async.tic(10);

let tictac_actor = actor this {
  tic(n : Int) : async () { if (n > 0) ignore(this.tac(n - 1)) };
  tac(n : Int) : async () { if (n > 0) ignore(this.tic(n - 1)) };
};
let _ = tictac_actor.tic(10);
