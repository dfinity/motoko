actor {
  private var c = 1;
  inc() {
    c += 1;
    printInt c; print "\n";
  };
  printCounter () {
    printInt c; print "\n";
  }
}
//CALL inc
//CALL inc
//CALL inc
//CALL printCounter
