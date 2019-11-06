let normalise_test_case input expected =
  let actual = File_path.normalise input in
  String.equal actual expected ||
    (Printf.printf
       "\nExpected: %s\nActual: %s\n"
       expected
       actual;
     false)

let relative_to_test_case root contained expected =
  let actual = File_path.relative_to root contained in
  let show = function
    | None -> "None"
    | Some s -> "Some " ^ s in
  Lib.Option.equal String.equal actual expected ||
    (Printf.printf
       "\nExpected: %s\nActual: %s\n"
       (show expected)
       (show actual);
     false)

