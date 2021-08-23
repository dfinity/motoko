let shorten_test pkg_map aliases base path expected =
  let actual =
    Declaration_index.shorten_import_path
      (Mo_config.Flags.M.of_seq (List.to_seq pkg_map))
      (Mo_config.Flags.M.of_seq (List.to_seq aliases))
      base path
  in
  if not (actual = expected) then (
    Printf.printf "Expected:\n%s\nBut got:\n%s\n" expected actual;
    false)
  else true

let%test "it shortens to a relative path" =
  shorten_test [ ("std", "/pkgs/std") ] [] "/project/src/file.mo"
    "/project/src/lib/hello.mo" "lib/hello"

let%test "it shortens to a package path" =
  shorten_test [ ("std", "/pkgs/std") ] [] "/project/src/file.mo"
    "/pkgs/std/list.mo" "mo:std/list"
