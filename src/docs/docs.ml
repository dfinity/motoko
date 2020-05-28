open Mo_frontend
open Extract

let adoc_docs : string -> string -> unit =
 fun in_file out_file ->
  let tokenizer, get_trivia_table =
    Lexer.tokenizer Lexer.NormalWithTrivia
      (Lexing.from_channel (open_in in_file))
  in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.parse_prog
  in
  let prog = parser tokenizer in_file in
  let trivia_table = get_trivia_table () in
  let module_docs, imports, docs = extract_docs prog trivia_table in
  let output = Adoc.render_docs module_docs docs in
  let oc = open_out out_file in
  Printf.fprintf oc "%s" output;
  flush oc;
  close_out oc

let plain_docs : string -> unit =
 fun file ->
  (* Printf.printf "Figuring out docs for %s:\n" file; *)
  let tokenizer, get_trivia_table =
    Lexer.tokenizer Lexer.NormalWithTrivia (Lexing.from_channel (open_in file))
  in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.parse_prog
  in
  let prog = parser tokenizer file in
  let trivia_table = get_trivia_table () in
  let module_docs, imports, docs = extract_docs prog trivia_table in
  (* Wasm.Sexpr.print 80 (Arrange.prog prog); *)
  print_endline module_docs;
  List.iter (fun (v, p) -> Printf.printf "import %s \"%s\";\n" v p) imports;
  List.iter (fun doc -> print_endline (Plain.render_doc_string doc)) docs

let html_docs : string -> string -> unit =
 fun in_file out_file ->
  (* Printf.printf "Figuring out docs for %s:\n" in_file; *)
  let tokenizer, get_trivia_table =
    Lexer.tokenizer Lexer.NormalWithTrivia
      (Lexing.from_channel (open_in in_file))
  in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.parse_prog
  in
  let prog = parser tokenizer in_file in
  let trivia_table = get_trivia_table () in
  let module_docs, imports, docs = extract_docs prog trivia_table in
  let html = Html.render_docs module_docs docs in
  let oc = open_out out_file in
  Printf.fprintf oc "%s" html;
  flush oc;
  close_out oc

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

let start_html source output =
  try Unix.mkdir output 0o777
  with _ ->
    ();
    (* Printf.printf "%s -> %s\n" source output; *)
    let all_files = list_files_recursively source in
    let all_files =
      List.filter (fun f -> Filename.extension f = ".mo") all_files
    in
    List.iter
      (fun file ->
        let rel_path =
          file
          |> Lib.FilePath.relative_to source
          |> Option.get
          |> Lib.String.chop_suffix "mo"
          |> Option.get
          |> fun f -> f ^ "html"
        in
        let out_path = Filename.concat output rel_path in
        (* Printf.printf "%s -> %s\n" file out_path; *)
        html_docs file out_path)
      all_files

let start_adoc source output =
  try Unix.mkdir output 0o777
  with _ ->
    ();
    (* Printf.printf "%s -> %s\n" source output; *)
    let all_files = list_files_recursively source in
    let all_files =
      List.filter (fun f -> Filename.extension f = ".mo") all_files
    in
    List.iter
      (fun file ->
        let rel_path =
          file
          |> Lib.FilePath.relative_to source
          |> Option.get
          |> Lib.String.chop_suffix "mo"
          |> Option.get
          |> fun f -> f ^ "adoc"
        in
        let out_path = Filename.concat output rel_path in
        (* Printf.printf "%s -> %s\n" file out_path; *)
        adoc_docs file out_path)
      all_files

(* let start = start_html *)
let start = start_adoc
