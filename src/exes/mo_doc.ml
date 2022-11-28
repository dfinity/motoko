let source : string ref = ref "src"
let output : string ref = ref "docs"
let name : string option ref = ref None
let format : Docs.output_format ref = ref Docs.Html

let set_source s = source := s
let set_output o = output := o
let set_name n = name := Some n
let set_format = function
  | "html" -> format := Docs.Html
  | "adoc" -> format := Docs.Adoc
  | "plain" -> format := Docs.Plain
  | s -> Printf.eprintf "Unknown output format: %s" s
let usage = "Documentation generator for Motoko"

let argspec = [
      "--source",
      Arg.String set_source,
      "<path>  specifies what directory to search for source files. Defaults to `src`"
      ; "--output",
        Arg.String set_output,
        "<path>  specifies where the documentation will be generated. Defaults to `docs`"
      ; "--name",
        Arg.String set_name,
        "<name>  specifies the Vessel package name for generated code examples"
      ; "--format",
      Arg.String set_format,
      "<format>  specifies the generated format. One of `html`, `adoc`, or `plain` Defaults to `html`"
    ]

let () =
  Arg.parse argspec ignore usage;
  Docs.start !format Docs.{ source = !source; output = !output; name = !name }
