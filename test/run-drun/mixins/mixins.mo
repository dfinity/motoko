import MyMixin "MyMixin";

persistent actor a {
  include MyMixin();

  let myMsg = msg;
};
