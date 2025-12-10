func main1() {
  var n = 0;
  var sum = 0;
  while (true) {
    n += 1;
    if (n == 2) continue;
    if (n == 5) break;
    sum += n;
  };
  assert (sum == 1 + 3 + 4);

  // Future work: consider supporting 'loop' without `while` or fix typing problem there
  // var hits = 0;
  // loop {
  //   hits += 1;
  //   if (hits == 3) continue;
  //   if (hits == 4) break;
  // };
  // assert (hits == 4);
};
// Future work: consider supporting 'loop' with `while`
// func main2() {
//   var n = 0;
//   var sum = 0;
//   loop {
//     n += 1;
//     if (n == 2) continue;
//     if (n == 5) break;
//     sum += n;
//   } while (true);
//   assert (sum == 1 + 3 + 4);
// };
func main3() {
  var n = 0;
  var sum = 0;
  for (i in [1, 2, 3, 4, 5, 6, 7].values()) {
    n += 1;
    if (n == 2) continue;
    if (n == 5) break;
    sum += n;
  };
  assert (sum == 1 + 3 + 4);
};
main1();
// main2();
main3();
