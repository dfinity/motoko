let string_of_list f xs =
  List.map f xs
  |> String.concat "; "
  |> fun x -> "[ " ^ x ^ " ]"

let slice_test input expected =
  let actual = Imports.slice_imports input in
  let show (before, is, after) =
    string_of_list (fun x -> x) before ^ "\n"
    ^ string_of_list (fun (a, b) -> "(" ^ a ^ ", " ^ b ^ ")") is ^ "\n"
    ^ string_of_list (fun x -> x) after ^ "\n" in
  if not (actual = expected) then
    (Printf.printf "Expected:\n%sBut got:\n%s" (show expected) (show actual);
     false)
  else
    true

let build_file_test input expected =
  let actual = Imports.build_file input in
  if not (actual = expected) then
    (Printf.printf "Expected:\n%s\nBut got:\n%s\n" expected actual;
     false)
  else
    true


let%test "it slices a simple import section" =
  slice_test {|// I'm just a comment
// Another one
import List "lib/list";
module {}|}
    (["// I'm just a comment"; "// Another one"], [("List", "lib/list")] ,["module {}"])

let%test "it puts a file back together with the specified imports" =
  build_file_test
    (["// I'm just a comment"; "// Another one"], [("List", "lib/list")] ,["module {}"])
    {|// I'm just a comment
// Another one
import List "lib/list";
module {}|}

let%test "it puts a file back together with empty imports" =
  build_file_test
    (["// I'm just a comment"], [] ,["module {}"])
    {|// I'm just a comment
module {}|}
