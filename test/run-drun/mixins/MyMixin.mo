mixin(prefix : Text, suffix : Text) {
  type MixinTy = Nat;
  let msg : Text = prefix # "Hello from Mixin" # suffix;
  public func mixinFunc() : async Text {
    msg
  };
};
