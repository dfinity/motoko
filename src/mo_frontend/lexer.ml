module ST = Source_token
include Lexer_lib

type pos = { line : int; column : int }

let pos_of_lexpos : Lexing.position -> pos =
 fun lexpos ->
  Lexing.{ line = lexpos.pos_lnum; column = lexpos.pos_cnum - lexpos.pos_bol }

module TrivTable = Map.Make (struct
  type t = pos

  let compare p1 p2 = compare p1 p2
end)

type trivia_info = {
  leading_trivia : ST.line_feed ST.trivia list;
  trailing_trivia : ST.void ST.trivia list;
}

type triv_table = trivia_info TrivTable.t

type source_token = ST.token * Lexing.position * Lexing.position

type parser_token = Parser.token * Lexing.position * Lexing.position

let first (t, _, _) = t

let is_gt = function ST.GT -> true | _ -> false

let tokenizer (mode : Lexer_lib.mode) (lexbuf : Lexing.lexbuf) :
    (unit -> parser_token) * (unit -> triv_table) =
  let trivia_table : triv_table ref = ref TrivTable.empty in
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
        let tkn = Source_lexer.token mode lexbuf in
        let start = Lexing.lexeme_start_p lexbuf in
        let end_ = Lexing.lexeme_end_p lexbuf in
        (tkn, start, end_)
  in
  let peek () : source_token =
    match !lookahead with
    | None ->
        let tkn = next () in
        lookahead := Some tkn;
        tkn
    | Some t -> t
  in
  let next_source_token () : parser_token =
    let has_whitespace triv =
      List.find_opt ST.is_whitespace triv |> Option.is_some
    in
    let rec eat_leading acc =
      let tkn, start, end_ = next () in
      match ST.to_parser_token tkn with
      (* A semicolon immediately followed by a newline gets a special token for the REPL *)
      | Ok Parser.SEMICOLON when ST.is_line_feed (first (peek ())) ->
          (List.rev acc, (Parser.SEMICOLON_EOL, start, end_))
      (* >> can either close two nested type applications, or be a shift
         operator depending on whether it's prefixed with whitespace *)
      | Ok Parser.GT
        when has_whitespace (!last_trailing @ acc)
             && is_gt (first (peek ())) ->
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
    let leading_ws () = has_whitespace (!last_trailing @ leading_trivia) in
    let trailing_ws () =
      has_whitespace trailing_trivia || ST.is_line_feed (first (peek ()))
    in
    (* Disambiguating operators based on whitespace *)
    let token =
      match token with
      | Parser.GT when leading_ws () && trailing_ws () -> Parser.GTOP
      | Parser.LT when leading_ws () && trailing_ws () -> Parser.LTOP
      | _ -> token
    in
    last_trailing := List.map (ST.map_trivia ST.absurd) trailing_trivia;
    trivia_table :=
      TrivTable.add (pos_of_lexpos start)
        { leading_trivia; trailing_trivia }
        !trivia_table;
    (token, start, end_)
  in
  (next_source_token, fun () -> !trivia_table)
