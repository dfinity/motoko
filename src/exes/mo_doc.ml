open Mo_config

let source : string option ref = ref None
let output : string ref = ref "docs"

let set_source s = source := Some s
let set_output o = output := o
let usage = "Documentation generator for Motoko"

let argspec =
  Arg.align
    [ "--source",
      Arg.String set_source,
      " specifies what directory to search for source files."
    ; "--output",
      Arg.String set_output,
      " specifies where the documentation will be generated."
    ]

let () =
  Arg.parse argspec ignore usage;
  match !source with
  | None ->
     Printf.eprintf "--source needs to be specified";
     exit 1
  | Some source ->
     Docs.start source !output
