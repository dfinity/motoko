import MyMixin "mixin/MyMixin";

persistent actor a {
  include MyMixin(10);
};

ignore (a.go()); //OR-CALL ingress go "DIDL\x00\x00"
