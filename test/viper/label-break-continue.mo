// @verify

actor LabelBreakContinue {
  func label_expressions() {
    let simple_label = label simple : Int break simple(42);
    assert:system simple_label == 42;

    let implicit_leave = label implicit : Int 42;
    assert:system implicit_leave == 42;

    let block_label_early_expr = label block : (Int, Int) {
      if (true) break block(42, 42);
      (24, 24)
    };
    assert:system block_label_early_expr.0 == 42 and block_label_early_expr.1 == 42;

    let block_label_expr = label block : (Int, Int) {
      if (false) break block(42, 42);
      (24, 24)
    };
    assert:system block_label_expr.0 == 24 and block_label_expr.1 == 24;

    var v = 0;
    let mut_label = label mutability : () {
      if (true) break mutability(v := 42);
      v := 100;
    };
    assert:system v == 42;

    v := 0;
    let mut_label_2 = label mutability : () {
      if (false) break mutability(v := 42);
      v := 100;
    };
    assert:system v == 100;
  };

  func loops() {
    var i = 0;
    label while_loop while (i < 5) {
      assert:loop:invariant (i < 3);
      i := i + 1;
      if (i == 3) break while_loop;
      continue while_loop;
      i := 100
    };

    // TODO: uncomment this when for loops are supported.
    /* let range = [0, 1, 2, 3, 4, 5];

    i := 0;
    label for_loop for(j in range.vals()) {
      assert:loop:invariant (j == i);
      assert:loop:invariant (j < 3);
      i := i + 1;
      if (j == 3) break for_loop;
      continue for_loop;
      i := 100;
    }; */

    // TODO: uncomment this when loops are supported.
    /* i := 0;
    label regular_loop loop {
      assert:loop:invariant (i < 5);
      i := i + 1;
      if (i == 4) break regular_loop;
      i := 100;
    }; */
  }
}
