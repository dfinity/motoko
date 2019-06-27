open As_frontend
open As_types
open As_values
open Printf

let print_list f xs =
  List.map f xs
  |> String.concat "; "
  |> fun x -> "[" ^ x ^ "]"

let lib_files (_: unit) : string list =
  let lib_dir = "lib" in
  Sys.readdir lib_dir
  |> Array.to_list
  |> List.filter (fun file -> String.equal (Filename.extension file) ".as")
  |> List.map (fun file -> Filename.concat lib_dir file)

let complete () =
  lib_files ()
  |> print_list Base__Fn.id
  |> printf "%s"

(* let is_module_export (exp: Syntax.exp): bool = *)
