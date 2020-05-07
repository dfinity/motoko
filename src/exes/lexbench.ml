open Mo_frontend

let list_files_recursively dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f
        |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs
        |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] [ dir ]

let all_base_paths : string list =
  let base_path = "/home/creek/code/mo-libs/motoko-base" in
  list_files_recursively (base_path ^ "/src")

let rec drain_lexer lexer =
  let token, _, _ = lexer () in
  if token = Parser.EOF then () else drain_lexer lexer

let bench_lexer mk_lexer =
  List.iter
    (fun path ->
      let in_channel = open_in path in
      let lexbuf = Lexing.from_channel in_channel in
      drain_lexer (mk_lexer lexbuf);
      close_in in_channel)
    all_base_paths

let mk_new_lexer trivia lexbuf =
  Lexer.tokenizer
    (if trivia then Lexer.NormalWithTrivia else Lexer.Normal)
    lexbuf
  |> fst

let lexer_lexbuf_to_supplier lexer lexbuf () =
  let token = lexer lexbuf in
  (token, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

let mk_old_lexer lexbuf =
  let t = Lexer_old.token Lexer_old.Normal in
  lexer_lexbuf_to_supplier t lexbuf

open Core
open Core_bench.Std

let () =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"New"
      (fun () -> bench_lexer (mk_new_lexer false));
    Bench.Test.create ~name:"New with trivia"
      (fun () -> bench_lexer (mk_new_lexer true));
    Bench.Test.create ~name:"Old"
      (fun () -> bench_lexer mk_old_lexer);
  ])
