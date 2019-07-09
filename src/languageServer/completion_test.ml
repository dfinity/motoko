let extract_cursor input =
  let cursor_pos = ref (0, 0) in
  Base.String.split_lines input
  |> List.mapi
       (fun line_num line ->
         match String.index_opt line '|' with
         | Some column_num ->
            cursor_pos := (line_num, column_num);
            line
            |> String.split_on_char '|'
            |> String.concat ""
         | None -> line
       )
  |> String.concat "\n"
  |> fun f -> (f, !cursor_pos)

let dummy_logger _ _ = ()

let prefix_test_case file expected =
  let (file, (line, column)) = extract_cursor file in
  let prefix =
    Completion.find_completion_prefix dummy_logger file line column in
  Base.Option.equal String.equal prefix expected

let import_relative_test_case root module_path import expected =
  let actual =
    Completion.import_relative_to_project_root root module_path import in
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

let parse_module_header_test_case project_root current_file file expected =
  let actual =
    Completion.parse_module_header
      project_root
      current_file file in
  let display_result (alias, path) = Printf.sprintf "%s => \"%s\"" alias path in
  let result = Base.List.equal
    actual
    expected
    ~equal:(fun (x, y) (x', y') ->
      String.equal x x' && String.equal y y' ) in
  if not result then
    Printf.printf
      "\nExpected: %s\nActual: %s"
      (Completion.string_of_list display_result expected)
      (Completion.string_of_list display_result actual) else ();
  result


let%test "it finds a simple prefix" =
  prefix_test_case "List.|" (Some "List")

let%test "it doesn't find non-qualified idents" =
  prefix_test_case "List.filter we|" None

let%test "it picks the qualified closest to the cursor" =
  prefix_test_case "Stack.some List.|" (Some "List")

let%test "it handles immediately following single character tokens" =
  prefix_test_case "List.|<" (Some "List")

let%test "TODO(Christoph): it doesn't handle qualifier + partial identifierf" =
  prefix_test_case "Stack.so|" (None)

let%test "it handles multiline files" =
  prefix_test_case
{|Stak.
List.|
|} (Some "List")

let%test "it handles a full module" =
  prefix_test_case
{|module {
  private import List = "./ListLib.as";

  func singleton<T>(x: T): List.List<T> =
    List.cons<T>(x, Test.|<T>());

  func doubleton<T>(x: T): List.List<T> =
    List.cons<T>(x, List.cons<T>(x, List.nil<T>()));
 }|} (Some "Test")

let%test "it doesn't fall through to the next valid prefix" =
  prefix_test_case
{|module {
private import List = "lib/ListLib.as"; // private, so we don't re-export List
private import ListFns = "lib/ListFuncs.as"; // private, so we don't re-export List
type Stack = List.List<Int>;
func push(x : Int, s : Stack) : Stack = List.cons<Int>(x, s);
func empty():Stack = List.nil<Int>();
func singleton(x : Int) : Stack =
  List.we|
  ListFns.singleton<Int>(x);
}|} None

let%test "it makes an import relative to the project root" =
  import_relative_test_case
    "/home/project"
    "/home/project/src/main.as"
    "lib/List.as"
    (Some "src/lib/List.as")

let%test "it preserves trailing slashes for directory imports" =
  import_relative_test_case
    "/home/project"
    "/home/project/src/main.as"
    "lib/List/"
    (Some "src/lib/List/")

let%test "it can handle parent directory relationships" =
  import_relative_test_case
    "/home/project"
    "/home/project/src/main.as"
    "../lib/List.as"
    (Some "lib/List.as")

let%test "it parses a simple module header" =
  parse_module_header_test_case
    "/project"
    "/project/src/Main.as"
    "import P \"lib/prelude.as\""
    ["P", "src/lib/prelude.as"]
