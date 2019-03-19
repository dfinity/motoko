let a = actor {
  private aa = actor {
    private var c = 1;
    inc() {
      c += 1;
      printInt(c)
    };
    print () {
      printInt(c)
    };
  };
  inc() { aa.inc() };
  print() { aa.print() };
};

a.inc();
a.inc();
a.inc();
a.print()
