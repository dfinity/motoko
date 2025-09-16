import MyImport "MyImport";
import Text "Text2";

mixin(prefix : Text, suffix : Text) {
  type MixinTy = Nat;
  let t2 = Text.text2();
  let msg : Text = prefix # "Hello from Mixin" # suffix;
  public func mixinFunc() : async Text {
    MyImport.identity(msg)
  };
};
