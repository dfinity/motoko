//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
persistent actor {
  persistent func g<T>(x : T) : persistent() -> T {
    persistent func h() : T { x };
    h;
  };
  let h = g<persistent() -> ()>(persistent func _anon() {});
  h();
};
