/* This tests checks if messages to actors are really asynchronous, and complete
   before delivery.
*/

let sync_object = new self {
  private var x : Bool = false;

  bump() : () { assert (x == false); x := true; assert (x == true); };

  test() : () { assert (x == false); self.bump(); assert (x == true); };
};

sync_object.test();

let async_actor = actor self {
  private var x : Bool = false;

  bump() { assert (x == false); x := true; assert (x == true); };

  test() : () { assert (x == false); self.bump(); assert (x == false); };
};

async_actor.test();
