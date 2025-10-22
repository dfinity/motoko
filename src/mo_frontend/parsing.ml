
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

module S = Source.Region_set

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
  let ss = List.map Printers.string_of_symbol future in
  String.concat " " ss

let abstract_future_with_example future =
  let ss      = String.concat " " @@ List.map Printers.string_of_symbol future in
  let example = String.concat " " @@ List.map Printers.example_of_symbol future in
  if String.compare ss example != 0 then
    ss ^ " (e.g. '" ^ example ^ "')"
  else
    ss

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

let abstract_futures_with_examples explanations =
  let futures = List.sort compare_futures (List.map E.future explanations) in
  let ss = List.map abstract_future_with_example futures in
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

module I = Parser.MenhirInterpreter

(* For debug *)
module Debug = struct
  module Printer = MenhirRecoveryLib.MakePrinter (
    struct
      module I = Parser.MenhirInterpreter
      let print s = Printf.eprintf "%s" s
      let print_symbol s = print (Printers.string_of_symbol s)
      let print_element = None
      let print_token t = print (Source_token.string_of_parser_token t)
    end)

  let print_checkpoint = function
    | I.InputNeeded env ->
      Printf.eprintf "InputNeeded";
      Printer.print_env env
    | I.Shifting (env1, env2, _) ->
      Printf.eprintf "Shifting";
      Printer.print_env env1;
      Printf.eprintf " -> ";
      Printer.print_env env2
    | I.HandlingError env ->
      Printf.eprintf "HandlingError";
      Printer.print_env env
    | I.AboutToReduce (env, _) ->
      Printf.eprintf "AboutToReduce";
      Printer.print_env env
    | I.Accepted _ ->
      Printf.eprintf "Accepted"
    | I.Rejected ->
      Printf.eprintf "Rejected"

  let string_of_production (prod : I.production) =
    Printf.sprintf "%s -> %s"
      (Printers.string_of_symbol (I.lhs prod))
      (String.concat " . " (List.map Printers.string_of_symbol (I.rhs prod)))

  let inspect_state_items st =
    let symbol = I.incoming_symbol st in
    Printer.print_symbol (I.X symbol);
    Printf.eprintf "\n";
    I.items st |> List.iter (fun (prod, _dot) ->
      Printf.eprintf "  %s\n" (string_of_production prod))

  let rec inspect_env (env : 'a I.env) =
    match I.top env, I.pop env with
    | Some (I.Element (st, _, _, _)), Some env' ->
      inspect_state_items st; inspect_env env'
    | _ -> Printf.eprintf "inspect_env done\n"
end

module RecoveryTracer = MenhirRecoveryLib.DummyPrinter (I)

module RecoveryConfig = struct
  include Recover_parser

  (* Adapt [default_value region] to MenhirRecoverLib interface ([default_value loc]) *)
  let default_value (loc: Custom_compiler_libs.Location.t) sym =
      let open Custom_compiler_libs.Location in
      let open Lexing in
      let open Source in
      let file = loc.loc_start.pos_fname in
      let region_loc : region = {
        left  : pos = {file; line = loc.loc_start.pos_lnum; column = loc.loc_start.pos_bol};
        right : pos = {file; line = loc.loc_end.pos_lnum; column = loc.loc_end.pos_bol};
      } in
      default_value region_loc sym (* [default_value] is included from Recover_parser *)

  let guide _ = false
  let use_indentation_heuristic = false
  let is_eof  = function Parser.EOF -> true | _ -> false
end

module R = MenhirRecoveryLib.Make (Parser.MenhirInterpreter) (RecoveryConfig) (RecoveryTracer)

let region_of_pos (start, end_) =
  Source.{left = Lexer.convert_pos start; right = Lexer.convert_pos end_}

let handle_generic_error error_detail msg_store ((startp, _) as pos) lexeme inputneeded_cp =
  let at = region_of_pos pos in
  let token =
    if lexeme = "" then "end of input" else
      "token '" ^ String.escaped lexeme ^ "'"
  in
  let explanations = E.investigate startp inputneeded_cp in
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
    | 4 ->
      Printf.sprintf
        "unexpected %s, expected one of token or <phrase> sequence:\n  %s"
        token (abstract_futures_with_examples explanations)
    | _ ->
      Printf.sprintf "unexpected %s" token
  in
  Diag.add_msg msg_store (Diag.error_message at "M0001" "syntax" msg)

let is_lcurly (type a) (symbol : a I.symbol) = match symbol with
  | I.T (I.T_LCURLY) -> true
  | _ -> false

(** Find closest '{' on the stack and check if it comes from the 'block' production. *)
let rec inside_block (env : 'a I.env) =
  match I.top env, I.pop env with
  | Some (I.Element (st, _, start_pos, end_pos)), Some env' ->
    if is_lcurly (I.incoming_symbol st) then
      match I.items st with
      | (prod, _dot) :: _ when I.lhs prod = I.X (I.N I.N_block) -> Some (start_pos, end_pos)
      | _ -> None
    else
      inside_block env'
  | _ -> None

(** Try adding a custom error instead of the generic one. *)
let try_add_custom_error env lexeme reported msg_store =
  match lexeme with
  | "=" | "with" ->
    (match inside_block env with
    | Some block_pos ->
      let at = region_of_pos block_pos in
      if not (S.mem at !reported) then begin
        reported := S.add at !reported;
        Diag.add_msg msg_store (Diag.error_message at "M0001" "syntax"
          (Printf.sprintf "expected block but got record; wrap the record in braces: { { … } }"))
      end;
      true
    | None -> false)
  | _ -> false

let handle_error env lexbuf reported msg_store inputneeded_cp error_detail = 
  let (startp, endp) as positions = I.positions env in
  let lexeme = slice_lexeme lexbuf startp endp in
  if not (try_add_custom_error env lexeme reported msg_store) then
    handle_generic_error error_detail msg_store positions lexeme inputneeded_cp

(* We drive the parser in the usual way, but records the last [InputNeeded]
   checkpoint. If a syntax error is detected, we go back to this checkpoint
   and analyze it in order to produce a meaningful diagnostic. *)

let parse ?(recovery = false) mode error_detail start lexer lexbuf =
  Diag.with_message_store ~allow_errors:recovery (fun m ->
    Parser_lib.msg_store := Some m;
    Parser_lib.mode := Some mode;
    (* Avoid repeated custom errors for the same position *)
    let reported = ref S.empty in
    let save_error (inputneeded_cp : 'a I.checkpoint) (fail_cp : 'a I.checkpoint) : unit =
    (* The parser signals a syntax error. Note the position of the
         problematic token, which is useful. Then, go back to the
         last [InputNeeded] checkpoint and investigate. *)
      match fail_cp with
      | I.HandlingError env ->
        handle_error env lexbuf reported m inputneeded_cp error_detail
      | _ -> assert false
    in
    let fail cp = None in
    let succ e = Some e in
    R.loop_handle_recover succ fail save_error lexer start
  )
