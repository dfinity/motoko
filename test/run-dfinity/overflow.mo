// We have theses tests in run-dfinity because we want to check that certain
// traps are happening, and a good way to test this is if a message gets
// aborted.

ignore(async {
    ignore ((0-1):Int);
    debug_print("This is reachable.\n");
});
ignore(async {
    ignore ((1-1):Nat);
    debug_print("This is reachable.\n");
});
ignore(async {
    ignore ((0-1):Nat);
    debug_print("This should be unreachable.\n");
});
ignore(async {
    ignore ((0-1):Nat);
    debug_print("This should be unreachable.\n");
});
/*
ignore(async {
    ignore ((18446744073709551615 + 0):Nat);
    debug_print("This is reachable.\n");
});
*/
ignore(async {
    ignore ((9223372036854775806 + 9223372036854775806 + 1):Nat);
    debug_print("This is reachable.\n");
});
ignore(async {
    ignore ((9223372036854775806 + 9223372036854775806 + 2):Nat);
    debug_print("This is reachable.\n");
});
