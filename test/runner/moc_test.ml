(* A module with functions to test *)
module To_test = struct
  let lowercase = String.lowercase_ascii

  let capitalize = String.capitalize_ascii

  let str_concat = String.concat ""

  let list_concat = List.append
end

let read_file file_path =
  let ch = open_in file_path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

module StringSet = Set.Make (String)

(* Collect all .mo file names in a .drun file *)
let collect_drun_mo_files (drun_file_path : string) : StringSet.t =
  let mo_file_regex = Re.compile (Re.Perl.re "[^ ]*mo") in
  let file_contents = read_file drun_file_path in
  StringSet.of_list (Re.matches mo_file_regex file_contents)

(* Run a drun test specified as a .mo file *)
let drun_mo_test (mo_file_path : string) : unit Alcotest.test_case =
  let open Alcotest in
  test_case (Printf.sprintf "drun: %s\n" mo_file_path) `Quick (fun () -> ())

let rec print_list (strs : string list) =
  match strs with
  | [] -> ()
  | e :: l ->
      Printf.printf "%s\n" e;
      print_list l

(* Run a drun test specified as a .drun file *)
let drun_drun_test (drun_file_path : string) : unit Alcotest.test_case =
  let open Alcotest in
  test_case (Printf.sprintf "drun: %s\n" drun_file_path) `Quick (fun () ->
      let mo_files = collect_drun_mo_files drun_file_path in
      Printf.printf "Drun .mo files: ";
      print_list (List.of_seq (StringSet.to_seq mo_files));

      let test_name = Filename.basename drun_file_path in

      StringSet.iter
        (fun mo_file ->
          (* TODO: ../run-drun part should be gnoe *)
          let mo_file_path = Printf.sprintf "../run-drun/%s" mo_file in
          Printf.printf "Compiling mo file: %s\n" mo_file_path;

          let mo_base = Filename.basename mo_file in
          let out_dir = Printf.sprintf "_out/%s" test_name in
          let moc_cmd =
            Printf.sprintf
              "moc --hide-warnings -c %s -o %s/%s.drun.wasm\n" mo_file_path
              out_dir mo_base
          in

          Printf.printf "Compiling %s: `%s`" mo_file moc_cmd;

          let mkdir_exit = Sys.command (Printf.sprintf "mkdir -p %s" out_dir) in
          Alcotest.(check int) "mkdir exit code" 0 mkdir_exit;

          let moc_exit = Sys.command moc_cmd in
          Alcotest.(check int) "moc exit code" 0 moc_exit)
        mo_files)

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
