let a = actor {
  private a = actor {
    private var c = 1;
    inc() {
      c += 1;
      printInt(c)
    };
    print () {
      printInt(c)
    };
  };
  inc() { a.inc() };
  print() { a.print() };
};

a.inc();
a.inc();
a.inc();
a.print()
