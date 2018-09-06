func add1 (x : Nat) : Nat = x + 1;

/* Storing functions */
let fs = [add1, add1, add1];

assert(fs[0](fs[1](fs[2](1))) == 4);

/* Closure (scalar) */
var answer = 42;
func test_answer(x : Nat) { assert (answer == x) };
test_answer(42);

/* Closure (heap object) */
var answers = [43, 44];
func test_answers(x : Nat) { assert (answers[1] == x) };
test_answers(44);

/* Closure (scalar, use-before-define) */
func test_answer2() { assert (answer2 == 42) };
let answer2 = 42;
test_answer2();

/* Closure (mutable scalar, use-before-define) */
var answer3 = 50;
func test_answer3() { assert (answer3 == 51) };
answer3 := 51;
test_answer3();


