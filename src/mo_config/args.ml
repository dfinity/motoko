(* This module contains some argument parsing that is common between
multiple executables *)

let string_map flag r desc =
  let key_ref = ref "DEADBEEF" in
  flag,
  Arg.Tuple [
    Arg.Set_string key_ref ;
    Arg.String (fun value ->
      let key = !key_ref in
      if Flags.M.mem key !r
      then (Printf.eprintf "duplicate %s %s" flag key ; exit 1)
      else r := Flags.M.add key value !r
    )
  ],
  desc

(* Everything related to imports, packages, aliases *)
let package_args = [
  string_map "--package" Flags.package_urls "<args> Specify a package-name-package-URL pair, separated by a space";
  "--actor-idl", Arg.String (fun fp -> Flags.actor_idl_path := Some fp), " path to actor IDL files";
  string_map "--actor-alias" Flags.actor_aliases " actor import alias"
  ]

let error_args = [
  "--error-detail", Arg.Set_int Flags.error_detail, " set error message detail for syntax errors"
  (* TODO move --hide-warnings here? *)
  ]

let inclusion_args = [
    (* generic arg inclusion from file *)
  "--args", Arg.Expand Arg.read_arg,
    "<file> Read additional newline separated command line arguments \n\
    \      from <file>";
  "--args0", Arg.Expand Arg.read_arg0,
    "<file> Read additional NUL separated command line arguments from \n\
    \      <file>"
  ]
