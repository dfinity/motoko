module ST = Source_token
include Lexer_lib

type pos = { line : int; column : int }

let pos_of_lexpos : Lexing.position -> pos =
 fun lexpos ->
  Lexing.{ line = lexpos.pos_lnum; column = lexpos.pos_cnum - lexpos.pos_bol }

type trivia_info = {
  leading_trivia : ST.line_feed ST.trivia list;
  trailing_trivia : ST.void ST.trivia list;
}

let doc_comment_of_trivia_info : trivia_info -> string option =
 fun info ->
  let lines =
    List.filter_map
      (function
        | Source_token.Comment s -> (
            match Lib.String.chop_prefix "///" s with
            | Some "" -> Some ""
            | Some line_comment ->
                (* We expect a documentation line comment to start with a space
                 *  (which we remove here) *)
                Lib.String.chop_prefix " " line_comment
            | None ->
                Option.bind
                  (Lib.String.chop_prefix "/**" s)
                  (Lib.String.chop_suffix "*/")
                |> Option.map String.trim )
        | _ -> None)
      info.leading_trivia
  in
  if lines = [] then None else Some (String.concat "\n" lines)

module PosHash = struct
  type t = pos

  let equal i j = i = j

  let hash { line; column } = column lor 20 land line
end

module PosHashtbl = Hashtbl.Make (PosHash)

(* type triv_table = trivia_info IntHashtbl.t *)
type triv_table = trivia_info PosHashtbl.t

let empty_triv_table = PosHashtbl.create 0

type source_token = ST.token * Lexing.position * Lexing.position

type parser_token = Parser.token * Lexing.position * Lexing.position

let first (t, _, _) = t

let opt_is_whitespace : 'a ST.trivia option -> bool =
 fun x -> Option.fold ~none:false ~some:ST.is_whitespace x

let tokenizer (mode : Lexer_lib.mode) (lexbuf : Lexing.lexbuf) :
    (unit -> parser_token) * (unit -> triv_table) =
  let trivia_table : triv_table = PosHashtbl.create 1013 in
  let lookahead : source_token option ref = ref None in
  (* We keep the trailing whitespace of the previous token
     around so we can disambiguate operators *)
  let last_trailing : ST.line_feed ST.trivia list ref = ref [] in
  let next () : source_token =
    match !lookahead with
    | Some t ->
        lookahead := None;
        t
    | None ->
        let token = Source_lexer.token mode lexbuf in
        let start = Lexing.lexeme_start_p lexbuf in
        let end_ = Lexing.lexeme_end_p lexbuf in
        (token, start, end_)
  in
  let peek () : source_token =
    match !lookahead with
    | None ->
        let token = next () in
        lookahead := Some token;
        token
    | Some t -> t
  in
  let next_parser_token () : parser_token =
    let rec eat_leading acc =
      let token, start, end_ = next () in
      match ST.to_parser_token token with
      (* A semicolon immediately followed by a newline gets a special token for the REPL *)
      | Ok Parser.SEMICOLON when ST.is_line_feed (first (peek ())) ->
          (List.rev acc, (Parser.SEMICOLON_EOL, start, end_))
      (* >> can either close two nested type applications, or be a shift
         operator depending on whether it's prefixed with whitespace *)
      | Ok Parser.GT
        when opt_is_whitespace (Lib.List.hd_opt (acc @ List.rev !last_trailing))
             && first (peek ()) = ST.GT ->
          let _, _, end_ = next () in
          (acc, (Parser.USHROP, start, end_))
      | Ok t -> (List.rev acc, (t, start, end_))
      | Error t -> eat_leading (t :: acc)
    in
    let rec eat_trailing acc =
      match ST.is_lineless_trivia (first (peek ())) with
      | Some t ->
          ignore (next ());
          eat_trailing (t :: acc)
      | None -> List.rev acc
    in
    let leading_trivia, (token, start, end_) = eat_leading [] in
    let trailing_trivia = eat_trailing [] in
    let leading_ws () =
      opt_is_whitespace (Lib.List.last_opt (!last_trailing @ leading_trivia))
    in
    let trailing_ws () =
      opt_is_whitespace (Lib.List.hd_opt trailing_trivia)
      || (trailing_trivia = [] && ST.is_line_feed (first (peek ())))
    in
    (* Disambiguating operators based on whitespace *)
    let token =
      match token with
      | Parser.GT when leading_ws () && trailing_ws () -> Parser.GTOP
      | Parser.LT when leading_ws () && trailing_ws () -> Parser.LTOP
      | _ -> token
    in
    last_trailing := List.map (ST.map_trivia ST.absurd) trailing_trivia;
    if mode.with_trivia then
      PosHashtbl.add trivia_table (pos_of_lexpos start)
        { leading_trivia; trailing_trivia };
    (token, start, end_)
  in
  (next_parser_token, fun () -> trivia_table)
