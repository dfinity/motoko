module {
  persistent func g<T>(x : T) : persistent() -> T {
    persistent func h() : T { x };
    h; // TODO: non-eop should still typecheck here
  };
  // don't allow `func() {}` to typecheck as `persistent() -> ()` for now
  let h = g<persistent() -> ()>(func() {});
};
