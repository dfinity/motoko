open Mo_config

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
    ]
    @ Args.error_args
    @ Args.package_args

let () =
  Arg.parse argspec ignore usage;
  match !entry_point with
  | None ->
     Printf.eprintf "--canister-main needs to be specified";
     exit 1
  | Some ep ->
     LanguageServer.start ep !debug
