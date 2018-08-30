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

let tictac_actor = actor self {
  tic_msg(n : Int) { if (n > 0) self.tac_msg(n - 1) };
  tac_msg(n : Int) { if (n > 0) self.tic_msg(n - 1) };
};
let _ = tictac_actor.tic_msg(10);

func ignore(_ : async ()) = ();

let tictac_async = new this {
  tic_async(n : Int) : async () { if (n > 0) ignore(this.tac_async(n - 1)) };
  tac_async(n : Int) : async () { if (n > 0) ignore(this.tic_async(n - 1)) };
};
let _ = tictac_async.tic_async(10);

let tictac_actor_async = actor self {
  tic_msg_async(n : Int) : async () { if (n > 0) ignore(self.tac_msg_async(n - 1)) };
  tac_msg_async(n : Int) : async () { if (n > 0) ignore(self.tic_msg_async(n - 1)) };
};
let _ = tictac_actor_async.tic_msg_async(10);

let _ = async 1/0;
let _ = 1/0;
