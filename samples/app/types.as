type Subscription = shared {
  post : shared Text -> ();  // revokable by Server
  cancel : shared () -> ();
};


