type output_format = Plain | Adoc | Html

type config = {
  source : string;
  output : string;
  name : string option;
}

(** Generates documentation for all Motoko source files in the _input_ directory
 ** inside the _output_ directory using the specified _output format_. *)
val start : output_format -> config -> unit
