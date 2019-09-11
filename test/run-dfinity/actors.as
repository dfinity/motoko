actor tictac_actor {
  public func tic_msg(n : Int) { if (n > 0) tictac_actor.tac_msg(n - 1) };
  public func tac_msg(n : Int) { if (n > 0) tictac_actor.tic_msg(n - 1) };
};
let _ = tictac_actor.tic_msg(10);

func ignore(_ : async ()) = ();

object tictac_async {
  public func tic_async(n : Int) : async () { if (n > 0) ignore(tictac_async.tac_async(n - 1)) };
  public func tac_async(n : Int) : async () { if (n > 0) ignore(tictac_async.tic_async(n - 1)) };
};
let _ = tictac_async.tic_async(10);

actor tictac_actor_async {
  public func tic_msg_async(n : Int) : async () { if (n > 0) ignore(tictac_actor_async.tac_msg_async(n - 1)) };
  public func tac_msg_async(n : Int) : async () { if (n > 0) ignore(tictac_actor_async.tic_msg_async(n - 1)) };
};
let _ = tictac_actor_async.tic_msg_async(10);

let _ = async 1/0;
let _ = 1/0;
