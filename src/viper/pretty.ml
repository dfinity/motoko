open Source
open Syntax

open Format

let pp_info ppf NoInfo = ()

let pp_prog ppf p =
  match p.it with
  | Prog ->
    fprintf ppf "Prog %a" pp_info p.note


let prog p =
  Lib.Format.with_str_formatter (fun ppf ->
    pp_prog ppf) p

