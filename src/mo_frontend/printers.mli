open Parser.MenhirInterpreter

(* This module offers the functionality required by the functor
   [MenhirLib.Printers.Make]. *)

val string_of_symbol : xsymbol -> string

val print: string -> unit
val print_symbol: xsymbol -> unit
val print_element: (element -> unit) option
val to_string : unit -> string
