let extract_cursor input =
  let cursor_pos = ref (0, 0) in
  String.split_on_char '\n' input
  |> List.mapi (fun line_num line ->
         match String.index_opt line '|' with
         | Some column_num ->
             cursor_pos := (line_num, column_num);
             line |> String.split_on_char '|' |> String.concat ""
         | None -> line)
  |> String.concat "\n"
  |> fun f -> (f, !cursor_pos)

let prefix_test_case file expected =
  let file, (line, column) = extract_cursor file in
  let show = function
    | None -> "None"
    | Some (m, p) -> "Some (" ^ m ^ ", " ^ p ^ ")"
  in
  let actual = Completion.find_completion_prefix file line column in
  Option.equal ( = ) actual expected
  ||
  (Printf.printf "\nExpected: %s\nActual:   %s\n" (show expected) (show actual);
   false)

let import_relative_test_case root module_path import expected =
  let actual =
    Completion.import_relative_to_project_root root module_path import
  in
  let show = function None -> "None" | Some s -> "Some " ^ s in
  Option.equal String.equal actual expected
  ||
  (Printf.printf "\nExpected: %s\nActual:   %s\n" (show expected) (show actual);
   false)

let%test "it finds unqualified prefixes" =
  prefix_test_case "filt|" (Some ("", "filt"))

let%test "it understands whitespace" = prefix_test_case "filt |" None

let%test "it does find non-qualified idents after qualifiers" =
  prefix_test_case "List.filter we|" (Some ("", "we"))

let%test "it finds a simple prefix" =
  prefix_test_case "List.|" (Some ("List", ""))

let%test "it picks the qualified closest to the cursor" =
  prefix_test_case "Stack.some List.|" (Some ("List", ""))

let%test "it handles immediately following single character tokens" =
  prefix_test_case "List.|<" (Some ("List", ""))

let%test "it handles qualifier + partial identifier" =
  prefix_test_case "Stack.so|" (Some ("Stack", "so"))

let%test "it handles qualifiers following the cursor" =
  prefix_test_case "List.| Option" (Some ("List", ""))
  && prefix_test_case "List.fil| Option" (Some ("List", "fil"))

let%test "it handles multiline files" =
  prefix_test_case {|Stak.
List.|
|} (Some ("List", ""))

let%test "it handles a full module" =
  prefix_test_case
    {|module {
  private import List = "./ListLib";

  func singleton<T>(x: T): List.List<T> =
    List.cons<T>(x, Test.|<T>());

  func doubleton<T>(x: T): List.List<T> =
    List.cons<T>(x, List.cons<T>(x, List.nil<T>()));
 }|}
    (Some ("Test", ""))

let%test "it doesn't fall through to the next valid prefix" =
  prefix_test_case
    {|module {
private import List = "lib/ListLib"; // private, so we don't re-export List
private import ListFns = "lib/ListFuncs"; // private, so we don't re-export List
type Stack = List.List<Int>;
func push(x : Int, s : Stack) : Stack = List.cons<Int>(x, s);
func empty():Stack = List.nil<Int>();
func singleton(x : Int) : Stack =
  List.we|
  ListFns.singleton<Int>(x);
}|}
    (Some ("List", "we"))

let%test "it makes an import relative to the project root" =
  import_relative_test_case "/home/project" "/home/project/src/main.mo"
    "lib/List.mo" (Some "src/lib/List.mo")

let%test "it preserves trailing slashes for directory imports" =
  import_relative_test_case "/home/project" "/home/project/src/main.mo"
    "lib/List/" (Some "src/lib/List/")

let%test "it can handle parent directory relationships" =
  import_relative_test_case "/home/project" "/home/project/src/main.mo"
    "../lib/List.mo" (Some "lib/List.mo")
