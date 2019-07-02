let extract_cursor input =
  let cursor_pos = ref (0,0) in
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


let%test "it finds a simple prefix" =
  prefix_test_case "List.|" (Some "List")

let%test "it doesn't find non-qualified idents" =
  prefix_test_case "List.filter we|" None

let%test "it picks the qualified closest to the cursor" =
  prefix_test_case "Stack.some List.|" (Some "List")

let%test "it handles immediately following single character tokens" =
  prefix_test_case "List.|<" (Some "List")

let%test "TODO: it doesn't handle qualifier + partial identifierf" =
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
