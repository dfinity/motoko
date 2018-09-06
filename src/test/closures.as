func add1 (x : Nat) : Nat = x + 1;

/* Storing functions */
let fs = [add1, add1, add1];

assert(fs[0](fs[1](fs[2](1))) == 4);

/* Passing a closure (scalar) */
var answer = 42;
func test_answer(x : Nat) { assert (answer == x) };
test_answer(42);

/* Passing a heap object (scalar) */
var answers = [43, 44];
func test_answers(x : Nat) { assert (answers[1] == x) };
test_answers(44);

/* Passing a closure (scalar, use-before-define) */
/* Does not work yet
func test_answer2() { assert (answer2 == 42) };
var answer2 = 42;
test_answer2();
*/


