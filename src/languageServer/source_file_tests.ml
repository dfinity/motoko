module Lsp = Lsp.Lsp_t

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

let hovered_identifier_test_case file expected =
  let file, (line, column) = extract_cursor file in
  let show = function
    | Some (Source_file.CIdent i) -> i
    | Some (Source_file.CQualified (qualifier, ident)) ->
        qualifier ^ "." ^ ident
    | None -> "None"
  in
  let actual =
    Source_file.cursor_target_at_pos
      Lsp.{ position_line = line; position_character = column }
      file
  in
  Option.equal ( = ) actual expected
  ||
  (Printf.printf "\nExpected: %s\nActual:   %s\n" (show expected) (show actual);
   false)

let parse_module_header_test_case project_root current_file file expected =
  let actual = Source_file.parse_module_header project_root current_file file in
  let display_result = function
    | Source_file.ImportAlias (alias, path) ->
        Printf.sprintf "%s => \"%s\"" alias path
    | Source_file.ImportSymbol (symbol, path) ->
        Printf.sprintf "{ %s } => \"%s\"" symbol path
  in
  let result = Lib.List.equal ( = ) actual expected in
  if not result then
    Printf.printf "\nExpected: %s\nActual:   %s"
      (Completion.string_of_list display_result expected)
      (Completion.string_of_list display_result actual)
  else ();
  result

let%test "it finds an identifier" =
  hovered_identifier_test_case "f|ilter" (Some (Source_file.CIdent "filter"))

let%test "it ignores hovering over whitespace" =
  hovered_identifier_test_case "filter |" None

let%test "it finds a qualified identifier" =
  hovered_identifier_test_case "List.f|ilter"
    (Some (Source_file.CQualified ("List", "filter")))

let%test "it parses a simple module header" =
  parse_module_header_test_case "/project" "/project/src/Main.mo"
    "import P \"lib/prelude\""
    [ Source_file.ImportAlias ("P", "src/lib/prelude") ]

let%test "it parses a simple module header that contains a prim import" =
  parse_module_header_test_case "/project" "/project/src/Main.mo"
    "import Prim \"mo:⛔\""
    [ Source_file.ImportAlias ("Prim", "mo:⛔") ]

let%test "it parses a simple module header with package paths" =
  parse_module_header_test_case "/project" "/project/src/Main.mo"
    "import P \"mo:stdlib/prelude\""
    [ Source_file.ImportAlias ("P", "mo:stdlib/prelude") ]

let%test "it parses a simple module header" =
  parse_module_header_test_case "/project" "/project/Main.mo"
    {|
module {

private import List "lib/ListLib";
private import ListFuncs "lib/ListFuncs";

type Stack = List.List<Int>;

func push(x: Int, s: Stack): Stack =
  List.cons<Int>(x, s);

func empty(): Stack =
  List.nil<Int>();

func singleton(x: Int): Stack =
  ListFuncs.doubleton<Int>(x, x);
}
|}
    [
      Source_file.ImportAlias ("List", "lib/ListLib");
      Source_file.ImportAlias ("ListFuncs", "lib/ListFuncs");
    ]

let%test "it parses a simple module header with explicit symbol imports" =
  parse_module_header_test_case "/project" "/project/Main.mo"
    {|
import { List, nil, cons } "lib/ListLib";

module {

private import { doubleton } "lib/ListFuncs";

func push(x: Int, s: Stack): Stack =
  cons<Int>(x, s);

func empty(): Stack =
  nil<Int>();

func singleton(x: Int): Stack =
  doubleton<Int>(x, x);
}
|}
    [
      Source_file.ImportSymbol ("List", "lib/ListLib");
      Source_file.ImportSymbol ("nil", "lib/ListLib");
      Source_file.ImportSymbol ("cons", "lib/ListLib");
      Source_file.ImportSymbol ("doubleton", "lib/ListFuncs");
    ]
