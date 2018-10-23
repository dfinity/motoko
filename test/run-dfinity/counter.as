let a = actor {
  private var c = 0;
  inc() {
    c += 1;
    printInt(c)
  };
  print () {
    printInt(c)
  }
};

a.inc();
a.inc();
a.inc();
a.print()
