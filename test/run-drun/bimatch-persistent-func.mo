persistent actor {
  persistent func g<T>(x : T) : persistent() -> T {
    persistent func h() : T { x };
    h;
  };
  stable let h = g<persistent() -> ()>(func() {});
};
