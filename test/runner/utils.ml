let read_file (file_path : string) : string =
  let ch = open_in file_path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* NB. adds newline *)
let write_file (file_path : string) (contents : string) : unit =
  let out = open_out file_path in
  Printf.fprintf out "%s\n" contents;
  close_out out

(** If `str` starts with `prefix`, returns the part after the prefix. Otherwise
    returns `None`. *)
let drop_prefix (str : string) (prefix : string) : string option =
  let prefix_len = String.length prefix in
  let str_len = String.length str in
  if str_len < prefix_len then None
  else
    let str_prefix = String.sub str 0 prefix_len in
    if str_prefix = prefix then
      Some (String.sub str prefix_len (str_len - prefix_len))
    else None

(** Returns lines starting with `prefix` with `prefix` part removed *)
let collect_lines_starting_with (contents : string) (prefix : string) :
    string list =
  (* TODO: What about "\r\n"? *)
  let lines = String.split_on_char '\n' contents in
  List.filter_map (fun line -> drop_prefix line prefix) lines

module StringSet = Set.Make (String)

let rec print_list (strs : string list) =
  match strs with
  | [] -> ()
  | e :: l ->
      Printf.printf "%s\n" e;
      print_list l
