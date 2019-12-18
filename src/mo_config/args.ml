(* This module contains some argument parsing that is common between
multiple executables *)

(* Everything related to imports, packages, aliases *)
let package_args = [ "--package",
  (let package_name_ref = ref "DEADBEEF" in
   Arg.Tuple [
       Arg.Set_string package_name_ref ;
       Arg.String begin fun package_url ->
         (* push (package_name, package_url) onto the list. *)
         Flags.package_urls := (
           !package_name_ref,
           package_url
         ) :: ! Flags.package_urls
         end
  ]), "<args> Specify a package-name-package-URL pair, separated by a space"
; "--actor-idl", Arg.String (fun fp -> Flags.actor_idl_path := Some fp), " path to actor IDL files"
  ]
