module L = Lexer_new
module ST = Source_token

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

(* Horrible to use a global table here *)
type source_token_triple = ST.token * Lexing.position * Lexing.position

type source_token = Parser.token * Lexing.position * Lexing.position

let un_triple (t, _, _) = t

let tokenizer (mode : L.mode) (lexbuf : Lexing.lexbuf) :
    (int * int -> trivia_info option) * (unit -> source_token) =
  let trivia_table : triv_table ref = ref TrivTable.empty in
  let lookup_trivia (line, column) =
    TrivTable.find_opt { line; column } !trivia_table
  in
  let lookahead : source_token_triple option ref = ref None in
  let last_trailing : ST.line_feed ST.trivia list ref = ref [] in
  let next () : source_token_triple =
    match !lookahead with
    | Some t ->
        lookahead := None;
        t
    | None ->
        let tkn = L.token mode lexbuf in
        let start = Lexing.lexeme_start_p lexbuf in
        let end_ = Lexing.lexeme_end_p lexbuf in
        (tkn, start, end_)
  in
  let peek () : source_token_triple =
    match !lookahead with
    | None ->
        let tkn = next () in
        lookahead := Some tkn;
        tkn
    | Some t -> t
  in
  let next_source_token () : source_token =
    let has_leading_ws = function
      | [] -> false
      | x :: _ -> ST.is_whitespace x
    in
    let has_trailing_ws xs = has_leading_ws (List.rev xs) in
    let rec eat_leading acc =
      let tkn, start, end_ = next () in
      match ST.to_parser_token tkn with
      | Ok Parser.SEMICOLON when ST.is_line_feed (un_triple (peek ())) ->
        (List.rev acc, (Parser.SEMICOLON_EOL, start, end_))
      | Ok t -> (List.rev acc, (t, start, end_))
      | Error t -> eat_leading (t :: acc)
    in
    let rec eat_trailing acc =
      match ST.is_lineless_trivia (un_triple (peek ())) with
      | Some t ->
          ignore (next ());
          eat_trailing (t :: acc)
      | None -> List.rev acc
    in
    let leading_trivia, (token, start, end_) = eat_leading [] in
    let leading_ws = has_trailing_ws (!last_trailing @ leading_trivia) in
    let trailing_trivia = eat_trailing [] in
    let trailing_ws =
      if trailing_trivia = [] then
        peek ()
        |> un_triple
        |> ST.to_parser_token
        |> Result.fold ~ok:(Fun.const false) ~error:ST.is_whitespace
      else has_leading_ws trailing_trivia
    in
    last_trailing := List.map (ST.map_trivia ST.absurd) trailing_trivia;
    (* Disambiguate Lexer hacks *)
    let token =
      match token with
      | Parser.GT when leading_ws && trailing_ws -> Parser.GTOP
      | Parser.LT when leading_ws && trailing_ws -> Parser.LTOP
      | _ -> token
    in
    trivia_table :=
      TrivTable.add (pos_of_lexpos start)
        { leading_trivia; trailing_trivia }
        !trivia_table;
    (token, start, end_)
  in
  (lookup_trivia, next_source_token)
