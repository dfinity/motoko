mixin(prefix : Text, suffix : Text) {

  let msg : Text = prefix # "Hello from Mixin" # suffix;
  public func mixinFunc() : async Text {
    msg
  };
};
