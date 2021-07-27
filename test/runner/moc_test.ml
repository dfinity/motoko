(* A module with functions to test *)
module To_test = struct
  let lowercase = String.lowercase_ascii

  let capitalize = String.capitalize_ascii

  let str_concat = String.concat ""

  let list_concat = List.append
end

let drun_mo_test (file_path : string) : unit Alcotest.test_case =
  let open Alcotest in
  test_case (Printf.sprintf "drun: %s\n" file_path) `Quick (fun () -> ())

let drun_drun_test (file_path : string) : unit Alcotest.test_case =
  let open Alcotest in
  test_case (Printf.sprintf "drun: %s\n" file_path) `Quick (fun () -> ())

(* Scan directory drun/ for tests. Only the top-level files are tests. .drun
   files have .mo files in subdirectories. *)
let collect_drun_tests () : unit Alcotest.test_case list =
  let dir_name = "../run-drun" in

  let file_filter file_name =
    let extension = Filename.extension file_name in
    if extension = ".mo" then
      Some (drun_mo_test (Filename.concat dir_name file_name))
    else if extension = ".drun" then
      Some (drun_drun_test (Filename.concat dir_name file_name))
    else None
  in

  List.of_seq (Seq.filter_map file_filter (Array.to_seq (Sys.readdir dir_name)))

let collect_tests () : (string * unit Alcotest.test_case list) list =
  [ ("drun", collect_drun_tests ()) ]

(* The tests *)
let test_lowercase () =
  Alcotest.(check string) "same string" "hello!" (To_test.lowercase "hELLO!")

let test_capitalize () =
  Alcotest.(check string) "same string" "World." (To_test.capitalize "world.")

let test_str_concat () =
  Alcotest.(check string)
    "same string" "foobar"
    (To_test.str_concat [ "foo"; "bar" ])

let test_list_concat () =
  Alcotest.(check (list int))
    "same lists" [ 1; 2; 3 ]
    (To_test.list_concat [ 1 ] [ 2; 3 ])

(* Run it *)
let () =
  let open Alcotest in
  run "Motoko compiler tests" (collect_tests ())

(*
  run "Utils" [
      "string-case", [
          test_case "Lower case"     `Quick test_lowercase;
          test_case "Capitalization" `Quick test_capitalize;
        ];
      "string-concat", [ test_case "String mashing" `Quick test_str_concat  ];
      "list-concat",   [ test_case "List mashing"   `Slow  test_list_concat ];
    ]
*)
