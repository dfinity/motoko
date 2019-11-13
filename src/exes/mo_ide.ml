let entry_point : string option ref = ref None
let debug = ref false

let set_debug () = debug := true
let set_entry_point ep = entry_point := Some ep
let usage = "LSP server for motoko"

let argspec =
  Arg.align
    [ "--debug",
      Arg.Unit set_debug,
      " outputs logging information to a lsp.log file"
    ; "--canister-main",
      Arg.String set_entry_point,
      " specifies the entry point for the current project"
    ; "--package",
      (let package_name_ref = ref "DEADBEEF" in
       Arg.Tuple [
           Arg.Set_string package_name_ref ;
           Arg.String begin fun package_url ->
             (* push (package_name, package_url) onto the list. *)
             Mo_config.Flags.package_urls := (
               !package_name_ref,
               package_url
             ) :: ! Mo_config.Flags.package_urls
             end
      ]), "<args> Specify a package-name-package-URL pair, separated by a space" ;
    ]

let () =
  Arg.parse argspec ignore usage;
  match !entry_point with
  | None ->
     Printf.eprintf "--canister-main needs to be specified";
     exit 1
  | Some ep ->
     LanguageServer.start ep !debug
