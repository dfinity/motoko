type Subscription = shared {
  post : shared Text -> ();  // revokable by Server
  cancel : shared () -> ();
};

type ClientData = {
  id : Nat;
  client : Client;
  var revoked : Bool;
};

