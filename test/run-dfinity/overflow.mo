// We have theses tests in run-dfinity because we want to check that certain
// traps are happening, and a good way to test this is if a message gets
// aborted.

ignore(async {
    ignore ((0-1):Int);
    print("This is reachable.\n");
});
ignore(async {
    ignore ((1-1):Nat);
    print("This is reachable.\n");
});
ignore(async {
    ignore ((0-1):Nat);
    print("This should be unreachable.\n");
});
ignore(async {
    ignore ((0-1):Nat);
    print("This should be unreachable.\n");
});
/*
ignore(async {
    ignore ((18446744073709551615 + 0):Nat);
    print("This is reachable.\n");
});
*/
ignore(async {
    ignore ((9223372036854775806 + 9223372036854775806 + 1):Nat);
    print("This is reachable.\n");
});
ignore(async {
    ignore ((9223372036854775806 + 9223372036854775806 + 2):Nat);
    print("This is reachable.\n");
});
