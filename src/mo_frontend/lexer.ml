module ST = Source_token
open Trivia
include Lexer_lib

type source_token = ST.token * Lexing.position * Lexing.position

type parser_token = Parser.token * Lexing.position * Lexing.position

let first (t, _, _) = t

let opt_is_whitespace : 'a trivia option -> bool =
 fun x -> Option.fold ~none:false ~some:ST.is_whitespace x

let tokenizer (mode : Lexer_lib.mode) (lexbuf : Lexing.lexbuf) :
    (unit -> parser_token) * triv_table =
  let trivia_table : triv_table = PosHashtbl.create 1013 in
  let lookahead : source_token list ref = ref [] in
  (* We keep the trailing whitespace of the previous token
     around so we can disambiguate operators *)
  let last_trailing : line_feed trivia list ref = ref [] in
  let next () : source_token =
    match !lookahead with
    | t :: ts ->
        lookahead := ts;
        t
    | [] ->
        let token = Source_lexer.token mode lexbuf in
        let start = Lexing.lexeme_start_p lexbuf in
        let end_ = Lexing.lexeme_end_p lexbuf in
        (token, start, end_)
  in
  let peek () : source_token =
    match !lookahead with
    | [] ->
        let token = next () in
        lookahead := [token];
        token
    | t :: _ -> t
  in
  let next_parser_token () : parser_token =
    let rec eat_leading acc =
      let token, start, end_ = next () in
      match ST.to_parser_token token with
      (* A semicolon immediately followed by a newline gets a special token for the REPL *)
      | Ok Parser.SEMICOLON when ST.is_line_feed (first (peek ())) ->
          (List.rev acc, (Parser.SEMICOLON_EOL, start, end_))
      (* ?? is either null coalesce, or two questions marks depending on whitespace *)
      | Ok Parser.QUEST when first (peek ()) = ST.QUEST ->
          let (_, _, end2) as token2 = next () in (* consume second question mark *)
          if ST.is_whitespace_token (first (peek ())) then
            (* ?? followed by space: return NULLCOALESCE spanning both question marks *)
            (List.rev acc, (Parser.NULLCOALESCE, start, end2))
          else begin
            lookahead := token2 :: !lookahead; (* revert consumption of second question mark *)
            (List.rev acc, (Parser.QUEST, start, end_))
          end
      (* >> can either close two nested type applications, or be a shift
         operator depending on whether it's prefixed with whitespace *)
      | Ok Parser.GT
        when opt_is_whitespace (Lib.List.hd_opt (acc @ List.rev !last_trailing))
             && first (peek ()) = ST.GT ->
          let _, _, end_ = next () in
          (acc, (Parser.SHROP, start, end_))
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
    last_trailing := List.map (map_trivia absurd) trailing_trivia;
    PosHashtbl.add trivia_table (pos_of_lexpos start)
      { leading_trivia; trailing_trivia };
    (token, start, end_)
  in
  (next_parser_token, trivia_table)
