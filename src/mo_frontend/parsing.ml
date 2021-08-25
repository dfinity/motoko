
module P =
  MenhirLib.Printers.Make
    (Parser.MenhirInterpreter)
    (Printers)

(* Instantiate [ErrorReporting] for our parser. This requires
   providing a few functions -- see [CalcErrorReporting]. *)

module E =
  Menhir_error_reporting.Make
    (Parser.MenhirInterpreter)
    (Error_reporting)

(* Define a printer for explanations. We treat an explanation as if it
   were just an item: that is, we ignore the position information that
   is provided in the explanation. Indeed, this information is hard to
   show in text mode. *)

let uniq xs = List.fold_right (fun x ys -> if List.mem x ys then ys else x::ys) xs []

let abstract_symbols explanations =
  let symbols = List.sort Parser.MenhirInterpreter.compare_symbols
    (List.map (fun e -> List.hd (E.future e))  explanations) in
  let ss = List.map Printers.string_of_symbol symbols in
  String.concat "\n  " (uniq ss)

let abstract_future future =
  let ss = List.map Printers.string_of_symbol future  in
  String.concat " " ss

let rec lex_compare_futures f1 f2 =
  match f1,f2 with
  | [], [] -> 0
  | s1::ss1,s2::ss2 ->
    (match Parser.MenhirInterpreter.compare_symbols s1 s2 with
     | 0 -> lex_compare_futures ss1 ss2
     | c -> c)
  | _ -> assert false

let compare_futures f1 f2 = match compare (List.length f1) (List.length f2) with
      | 0 -> lex_compare_futures f1 f2
      | c -> c

let abstract_futures explanations =
  let futures = List.sort compare_futures (List.map E.future explanations) in
  let ss = List.map abstract_future futures in
  String.concat "\n  " (uniq ss)

let abstract_item item =
  P.print_item item;
  Printers.to_string()

let abstract_items explanations =
  let items = List.sort Parser.MenhirInterpreter.compare_items (List.map E.item explanations) in
  let ss = List.map abstract_item items in
  String.concat "  " (uniq ss)

type error_detail = int

exception Error of string * Lexing.position * Lexing.position

(* The lexbuf is a 1024 byte wide window, we need to compute offsets before
   accessing it, because token positions are absolute to the whole input *)
let slice_lexeme lexbuf i1 i2 =
  let open Lexing in
  let offset = i1.pos_cnum - lexbuf.lex_abs_pos in
  let len = i2.pos_cnum - i1.pos_cnum in
  if offset < 0 || len < 0
  then "<unknown>" (* Too rare to care *)
  else Bytes.sub_string lexbuf.lex_buffer offset len

let parse error_detail checkpoint lexer lexbuf =
  Diag.with_message_store (fun m ->
    try
      (* Temporary hack! *)
      Parser_lib.msg_store := Some m;
      Some (E.entry checkpoint lexer)
    with E.Error ((start, end_), explanations) ->
      let at =
        Source.{left = Lexer.convert_pos start; right = Lexer.convert_pos end_}
      in
      let lexeme = slice_lexeme lexbuf start end_ in
      let token =
        if lexeme = "" then "end of input" else
        "token '" ^ String.escaped lexeme ^ "'"
      in
      let msg =
        match error_detail with
        | 1 ->
          Printf.sprintf
            "unexpected %s, expected one of token or <phrase>:\n  %s"
            token (abstract_symbols explanations)
        | 2 ->
          Printf.sprintf
            "unexpected %s, expected one of token or <phrase> sequence:\n  %s"
            token (abstract_futures explanations)
        | 3 ->
          Printf.sprintf
            "unexpected %s in position marked . of partially parsed item(s):\n%s"
            token (abstract_items explanations)
        | _ ->
          Printf.sprintf "unexpected %s" token
      in
      Diag.add_msg m (Diag.error_message at "M0001" "syntax" msg);
      None
  )
