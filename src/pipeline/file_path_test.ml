let normalise_test_case input expected =
  let actual = File_path.normalise input in
    if String.equal actual expected
    then true
    else (Printf.printf "\nExpected: %s\nActual: %s\n" expected actual; false)

let relative_to_test_case root contained expected =
  let actual = File_path.relative_to root contained in
  let show = function
    | None -> "None"
    | Some s -> "Some " ^ s in
    if Base.Option.equal Base.String.equal actual expected
    then true
    else
      (Printf.printf
         "\nExpected: %s\nActual: %s\n"
         (show expected)
         (show actual);
       false)

let%test "it removes leading `./` for relative paths" =
  normalise_test_case "./lib/foo" "lib/foo"

let%test "it removes duplicate `//`s" =
  normalise_test_case ".//lib/foo" "lib/foo"

let%test "it preserves trailing slashes" =
  normalise_test_case "lib/foo/" "lib/foo/"

let%test "it drops intermediate references to the `.` directory" =
  normalise_test_case "lib/./foo/" "lib/foo/"

let%test "it applies parent directory traversals" =
  normalise_test_case "lib/../foo/" "foo/"

let%test "it keeps parent directory references at the start of a path" =
  normalise_test_case "../foo/lib" "../foo/lib"

let%test "it does everything at once" =
  normalise_test_case "../foo//.././lib" "../lib"

let%test "it handles absolute paths" =
  normalise_test_case "/foo" "/foo"

let%test "it handles absolute directory paths" =
  normalise_test_case "/foo/./lib/" "/foo/lib/"

let%test "it makes one absolute path relative to another one" =
  relative_to_test_case
    "/home/project"
    "/home/project/src/main.as"
    (Some "src/main.as")

let%test "it's robust in the face of trailing slashes" =
  relative_to_test_case
    "/home/project/"
    "/home/project/src/main.as"
    (Some "src/main.as")

let%test "it makes a file path relative to a path" =
  relative_to_test_case
    "/home/project"
    "/home/project/main.as"
    (Some "main.as")

let%test "it preserves trailing slashes" =
  relative_to_test_case
    "/home/project/"
    "/home/project/src/"
    (Some "src/")

let%test "it handles directory traversals" =
  relative_to_test_case
    "/home/project"
    "/home/project/src/../lib/"
    (Some "lib/")

let%test "it fails to make disjoint paths relative to one another" =
  relative_to_test_case
    "/home/project"
    "/home/main.as"
    None

let%test "it handles relative paths" =
  relative_to_test_case
    "project/src"
    "project/src/Main.as"
    (Some "Main.as")
