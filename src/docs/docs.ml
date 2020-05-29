open Mo_frontend
open Extract

type output_format = Plain | Adoc | Html

(* Renders a given module, and its module comment *)
type render = string -> doc list -> string

let write_file : string -> string -> unit =
 fun file output ->
  let oc = open_out file in
  Printf.fprintf oc "%s" output;
  flush oc;
  close_out oc

let process_source : render -> string -> string -> unit =
 fun render in_file out_file ->
  Printf.printf "Processing: %s\n" in_file;
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
  let output = render module_docs docs in
  write_file out_file output

let list_files_recursively : string -> string list =
 fun dir ->
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

let process_directory :
    (string -> string -> unit) -> string -> string -> string -> unit =
 fun processor extension source output ->
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
        |> fun f -> f ^ extension
      in
      let out_path = Filename.concat output rel_path in
      processor file out_path)
    all_files

let start : output_format -> string -> string -> unit =
 fun output_format src out ->
  (try Unix.mkdir out 0o777 with _ -> ());
  match output_format with
  | Plain -> process_directory (process_source Plain.render_docs) "txt" src out
  | Adoc -> process_directory (process_source Adoc.render_docs) "adoc" src out
  | Html ->
      write_file (Filename.concat out "styles.css") Styles.styles;
      process_directory (process_source Html.render_docs) "html" src out
