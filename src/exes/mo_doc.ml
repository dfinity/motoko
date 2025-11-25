let source : string ref = ref "src"
let output : string ref = ref "docs"
let format : Docs.output_format ref = ref Docs.Html
let package : string option ref = ref None

let set_source s = source := s
let set_output o = output := o
let set_format = function
  | "html" -> format := Docs.Html
  | "adoc" -> format := Docs.Adoc
  | "plain" -> format := Docs.Plain
  | s -> Printf.eprintf "Unknown output format: %s" s
let set_package s = package := Some s

let usage = "Documentation generator for Motoko"

let argspec = [
      "--source",
      Arg.String set_source,
      "<path>  specifies what directory to search for source files. Defaults to `src`"
    ; "--output",
      Arg.String set_output,
      "<path>  specifies where the documentation will be generated. Defaults to `docs`"
    ; "--format",
      Arg.String set_format,
      "<format>  specifies the generated format. One of `html`, `adoc`, or `plain` Defaults to `html`"
    ; "--package",
      Arg.String set_package,
      "<package>  specifies the optional package name, e.g. `core`."
    ]

let invalid s =
    Printf.printf "Unexpected positional argument: %s\n\n" s;
    Arg.usage argspec usage;
    exit 1

let () =
  Arg.parse argspec invalid usage;
  Docs.start !format !source !output !package
