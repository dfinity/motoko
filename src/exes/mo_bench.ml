open Core_bench

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

let all_base_files =
  list_files_recursively "/home/creek/code/mo-libs/motoko-base/src"

let parse_file = Pipeline.parse_file Source.no_region

let () =
  Core.Command.run (Bench.make_command [
    Bench.Test.create ~name:"parsing base"
      (fun () -> List.iter (fun f -> ignore (parse_file f)) all_base_files);
    Bench.Test.create ~name:"check base"
      (fun () -> Pipeline.check_files all_base_files);
  ])
