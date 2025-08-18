func foo<T, I>(t : T, f : I -> T) {};

func _main1() {
  let _ = foo(1, func x = -1);
};

func _main2() {
  let _ = foo(1, func x {
    let y = x + 1;
    y
  });
}
// `1` is used to infer `T`, however `I` cannot be inferred, error
