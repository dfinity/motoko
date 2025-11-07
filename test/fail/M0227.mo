import Mixin "mixins/Counter";

module {
  class C() {
    include Mixin(10);
  };

  include Mixin(10);
}
