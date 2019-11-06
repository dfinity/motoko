shared func call4(f : shared () -> future Int) : future Int = f();
