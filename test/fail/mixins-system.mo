import Mixin "mixins/Mixin1";
// can't include mixin with system methods
persistent actor {
  include Mixin();
};

