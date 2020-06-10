open Mo_frontend
open Extract

type output_format = Plain | Adoc | Html

let write_file : string -> string -> unit =
 fun file output ->
  let dirname = Filename.dirname file in
  (try Unix.mkdir dirname 0o777 with _ -> ());
  let oc = open_out file in
  Printf.fprintf oc "%s" output;
  flush oc;
  close_out oc

let extract : string -> string * doc list =
 fun in_file ->
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
  (module_docs, docs)

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

let list_files : string -> string -> (string * string * string) list =
 fun source output ->
  (* Printf.printf "%s -> %s\n" source output; *)
  let all_files = list_files_recursively source in
  let all_files =
    List.filter (fun f -> Filename.extension f = ".mo") all_files
  in
  List.map
    (fun file ->
      file
      |> Lib.FilePath.relative_to source
      |> Option.get
      |> Lib.String.chop_suffix ".mo"
      |> Option.get
      |> fun f -> (file, Filename.concat output f, f))
    all_files

let make_render_inputs : string -> string -> (string * Common.render_input) list
    =
 fun source output ->
  let all_files = List.sort compare (list_files source output) in
  let all_modules = List.map (fun (_, _, rel) -> rel) all_files in
  List.map
    (fun (input, output, current_path) ->
      let module_comment, declarations = extract input in
      ( output,
        Common.{ all_modules; current_path; module_comment; declarations } ))
    all_files

let start : output_format -> string -> string -> unit =
 fun output_format src out ->
  (try Unix.mkdir out 0o777 with _ -> ());
  match output_format with
  | Plain ->
      let inputs = make_render_inputs src out in
      List.iter
        (fun (out, input) ->
          write_file (out ^ ".txt") (Plain.render_docs input))
        inputs
  | Adoc ->
      let inputs = make_render_inputs src out in
      List.iter
        (fun (out, input) ->
          write_file (out ^ ".adoc") (Adoc.render_docs input))
        inputs
  | Html ->
      write_file (Filename.concat out "styles.css") Styles.styles;
      let inputs = make_render_inputs src out in
      List.iter
        (fun (out, input) ->
          write_file (out ^ ".html") (Html.render_docs input))
        inputs;
      write_file
        (Filename.concat out "index.html")
        (Html.make_index (List.map snd inputs))
