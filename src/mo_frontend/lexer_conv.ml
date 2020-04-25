module L = Lexer_new
module ST = Source_token

let source_to_parser_token :
    ST.token -> (Parser.token, ST.line_feed ST.trivia) result = function
  | ST.EOF -> Ok Parser.EOF
  | ST.LET -> Ok Parser.LET
  | ST.VAR -> Ok Parser.VAR
  | ST.LPAR -> Ok Parser.LPAR
  | ST.RPAR -> Ok Parser.RPAR
  | ST.LBRACKET -> Ok Parser.LBRACKET
  | ST.RBRACKET -> Ok Parser.RBRACKET
  | ST.LCURLY -> Ok Parser.LCURLY
  | ST.RCURLY -> Ok Parser.RCURLY
  | ST.AWAIT -> Ok Parser.AWAIT
  | ST.ASYNC -> Ok Parser.ASYNC
  | ST.BREAK -> Ok Parser.BREAK
  | ST.CASE -> Ok Parser.CASE
  | ST.CATCH -> Ok Parser.CATCH
  | ST.CONTINUE -> Ok Parser.CONTINUE
  | ST.LABEL -> Ok Parser.LABEL
  | ST.DEBUG -> Ok Parser.DEBUG
  | ST.IF -> Ok Parser.IF
  | ST.IGNORE -> Ok Parser.IGNORE
  | ST.IN -> Ok Parser.IN
  | ST.ELSE -> Ok Parser.ELSE
  | ST.SWITCH -> Ok Parser.SWITCH
  | ST.LOOP -> Ok Parser.LOOP
  | ST.WHILE -> Ok Parser.WHILE
  | ST.FOR -> Ok Parser.FOR
  | ST.RETURN -> Ok Parser.RETURN
  | ST.TRY -> Ok Parser.TRY
  | ST.THROW -> Ok Parser.THROW
  | ST.ARROW -> Ok Parser.ARROW
  | ST.ASSIGN -> Ok Parser.ASSIGN
  | ST.FUNC -> Ok Parser.FUNC
  | ST.TYPE -> Ok Parser.TYPE
  | ST.OBJECT -> Ok Parser.OBJECT
  | ST.ACTOR -> Ok Parser.ACTOR
  | ST.CLASS -> Ok Parser.CLASS
  | ST.PUBLIC -> Ok Parser.PUBLIC
  | ST.PRIVATE -> Ok Parser.PRIVATE
  | ST.SHARED -> Ok Parser.SHARED
  | ST.QUERY -> Ok Parser.QUERY
  | ST.SEMICOLON -> Ok Parser.SEMICOLON
  | ST.SEMICOLON_EOL -> Ok Parser.SEMICOLON_EOL
  | ST.COMMA -> Ok Parser.COMMA
  | ST.COLON -> Ok Parser.COLON
  | ST.SUB -> Ok Parser.SUB
  | ST.DOT -> Ok Parser.DOT
  | ST.QUEST -> Ok Parser.QUEST
  | ST.AND -> Ok Parser.AND
  | ST.OR -> Ok Parser.OR
  | ST.NOT -> Ok Parser.NOT
  | ST.IMPORT -> Ok Parser.IMPORT
  | ST.MODULE -> Ok Parser.MODULE
  | ST.DEBUG_SHOW -> Ok Parser.DEBUG_SHOW
  | ST.ASSERT -> Ok Parser.ASSERT
  | ST.ADDOP -> Ok Parser.ADDOP
  | ST.SUBOP -> Ok Parser.SUBOP
  | ST.MULOP -> Ok Parser.MULOP
  | ST.DIVOP -> Ok Parser.DIVOP
  | ST.MODOP -> Ok Parser.MODOP
  | ST.POWOP -> Ok Parser.POWOP
  | ST.ANDOP -> Ok Parser.ANDOP
  | ST.OROP -> Ok Parser.OROP
  | ST.XOROP -> Ok Parser.XOROP
  | ST.SHLOP -> Ok Parser.SHLOP
  | ST.USHROP -> Ok Parser.USHROP
  | ST.SSHROP -> Ok Parser.SSHROP
  | ST.ROTLOP -> Ok Parser.ROTLOP
  | ST.ROTROP -> Ok Parser.ROTROP
  | ST.EQOP -> Ok Parser.EQOP
  | ST.NEQOP -> Ok Parser.NEQOP
  | ST.LEOP -> Ok Parser.LEOP
  | ST.LTOP -> Ok Parser.LTOP
  | ST.GTOP -> Ok Parser.GTOP
  | ST.GEOP -> Ok Parser.GEOP
  | ST.HASH -> Ok Parser.HASH
  | ST.EQ -> Ok Parser.EQ
  | ST.LT -> Ok Parser.LT
  | ST.GT -> Ok Parser.GT
  | ST.PLUSASSIGN -> Ok Parser.PLUSASSIGN
  | ST.MINUSASSIGN -> Ok Parser.MINUSASSIGN
  | ST.MULASSIGN -> Ok Parser.MULASSIGN
  | ST.DIVASSIGN -> Ok Parser.DIVASSIGN
  | ST.MODASSIGN -> Ok Parser.MODASSIGN
  | ST.POWASSIGN -> Ok Parser.POWASSIGN
  | ST.CATASSIGN -> Ok Parser.CATASSIGN
  | ST.ANDASSIGN -> Ok Parser.ANDASSIGN
  | ST.ORASSIGN -> Ok Parser.ORASSIGN
  | ST.XORASSIGN -> Ok Parser.XORASSIGN
  | ST.SHLASSIGN -> Ok Parser.SHLASSIGN
  | ST.USHRASSIGN -> Ok Parser.USHRASSIGN
  | ST.SSHRASSIGN -> Ok Parser.SSHRASSIGN
  | ST.ROTLASSIGN -> Ok Parser.ROTLASSIGN
  | ST.ROTRASSIGN -> Ok Parser.ROTRASSIGN
  | ST.NULL -> Ok Parser.NULL
  | ST.DOT_NUM s -> Ok (Parser.DOT_NUM s)
  | ST.NAT s -> Ok (Parser.NAT s)
  | ST.FLOAT s -> Ok (Parser.FLOAT s)
  | ST.CHAR u -> Ok (Parser.CHAR u)
  | ST.BOOL b -> Ok (Parser.BOOL b)
  | ST.ID s -> Ok (Parser.ID s)
  | ST.TEXT s -> Ok (Parser.TEXT s)
  | ST.PRIM -> Ok Parser.PRIM
  | ST.UNDERSCORE -> Ok Parser.UNDERSCORE
  (*ST. Trivia *)
  | ST.SPACE n -> Error (ST.Space n)
  | ST.LINEFEED lf -> Error (ST.Line lf)
  | ST.TAB n -> Error (ST.Tab n)
  | ST.COMMENT c -> Error (ST.Comment c)

type source_token_triple = ST.token * Lexing.position * Lexing.position

type token_triple = Parser.token * Lexing.position * Lexing.position

let un_triple (t, _, _) = t

let source_to_parser_triple (t, s, e) = (source_to_parser_token t, s, e)

let token : L.mode -> Lexing.lexbuf -> unit -> ST.token_ann * Parser.token =
 fun mode lexbuf ->
  let lookahead : source_token_triple option ref = ref None in
  let next () : source_token_triple =
    match !lookahead with
    | Some t ->
        lookahead := None;
        t
    | None ->
        let tkn = L.token mode lexbuf in
        (tkn, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
  in
  let peek () : source_token_triple =
    match !lookahead with
    | None ->
        let tkn = next () in
        lookahead := Some tkn;
        tkn
    | Some t -> t
  in
  let next_token () : ST.token_ann * Parser.token =
    let rec eat_leading acc =
      let tkn, start, end_ = next () in
      match source_to_parser_token tkn with
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
    (* List.iter (fun t -> print_endline (ST.string_of_trivia_lf t)) leading_trivia; *)
    let trailing_trivia = eat_trailing [] in
    (* List.iter
     *   (fun t -> print_endline (ST.string_of_trivia ST.absurd t))
     *   trailing_trivia; *)
    (ST.{ range = (start, end_); leading_trivia; trailing_trivia }, token)
  in next_token
