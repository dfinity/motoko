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

let async2_actor = actor self {
  private var x : Bool = false;

  bump() { assert (x == false); x := true; assert (x == true);  };

  test()  { assert (x == false); bump(); assert (x == false); is_true(); };

  is_true() { assert (x == true); };
};

async2_actor.test();

let async_rec_actor = actor self {
  private var x : Bool = false;

  test(b : Bool)  {
    if (b) { assert (x == false); x := true; assert (x == true); }
    else   { assert (x == false); test(false); assert (x == false); is_true(); }
  };
  is_true() { assert (x == true); };
};

async_rec_actor.test(true);
