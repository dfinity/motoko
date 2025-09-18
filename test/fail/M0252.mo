import Mixin "mixins/Counter";

module {
  class C() {
    include Mixin();
  };

  include Mixin();
}
