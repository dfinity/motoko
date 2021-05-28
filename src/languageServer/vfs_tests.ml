module Lsp = Lsp.Lsp_t

let extract_cursors input =
  let cursor_start = ref (0, 0) in
  let start_found = ref false in
  let cursor_end = ref (0, 0) in
  String.split_on_char '\n' input
  |> List.mapi (fun line_num line ->
         match String.index_opt line '|' with
         | Some column_num ->
             if not !start_found then (
               start_found := true;
               cursor_start := (line_num, column_num);
               (* See if we can find a second pipe in the same line *)
               match String.index_from_opt line (column_num + 1) '|' with
               | Some column_num ->
                   (* Need to account for the previously found pipe,
                      so we subtract 1 from the column_num *)
                   cursor_end := (line_num, column_num - 1)
               | None -> ())
             else cursor_end := (line_num, column_num);
             line |> String.split_on_char '|' |> String.concat ""
         | None -> line)
  |> String.concat "\n"
  |> fun f -> (f, !cursor_start, !cursor_end)

let apply_change_test_case file replacement expected =
  let input, (sl, sc), (el, ec) = extract_cursors file in
  (* let _ = Printf.printf "start: %d:%d, end: %d:%d" sl sc el ec in *)
  let range =
    Lsp.
      {
        range_start = { position_line = sl; position_character = sc };
        range_end_ = { position_line = el; position_character = ec };
      }
  in
  let actual =
    String.concat "\n"
      (Vfs.apply_change
         (Lib.String.split input '\n')
         Lsp.
           {
             text_document_content_change_event_range = Some range;
             text_document_content_change_event_rangeLength = Some 0;
             text_document_content_change_event_text = replacement;
           })
  in
  String.equal actual expected
  ||
  (Printf.printf "\nExpected: %s\nActual:   %s\n" expected actual;
   false)

let%test "it applies the empty change" =
  apply_change_test_case "hi ||dude" "" "hi dude"

let%test "it applies a change" =
  apply_change_test_case "1\n|2|\n3" "4" "1\n4\n3"

let%test "it applies a two-line change" =
  apply_change_test_case "1\n|2|\n3" "4\n5" "1\n4\n5\n3"

let%test "it removes a line" = apply_change_test_case "a\n|b\n|c" "" "a\nc"

let%test "it inserts a newline" = apply_change_test_case "a\n||b" "\n" "a\n\nb"
