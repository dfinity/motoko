type line_feed = LF | CRLF

type token =
  | EOF
  | LET
  | VAR
  | LPAR
  | RPAR
  | LBRACKET
  | RBRACKET
  | LCURLY
  | RCURLY
  | AWAIT
  | ASYNC
  | BREAK
  | CASE
  | CATCH
  | CONTINUE
  | LABEL
  | DEBUG
  | IF
  | IGNORE
  | IN
  | ELSE
  | SWITCH
  | LOOP
  | WHILE
  | FOR
  | RETURN
  | TRY
  | THROW
  | ARROW
  | ASSIGN
  | FUNC
  | TYPE
  | OBJECT
  | ACTOR
  | CLASS
  | PUBLIC
  | PRIVATE
  | SHARED
  | QUERY
  | SEMICOLON
  | SEMICOLON_EOL
  | COMMA
  | COLON
  | SUB
  | DOT
  | QUEST
  | AND
  | OR
  | NOT
  | IMPORT
  | MODULE
  | DEBUG_SHOW
  | ASSERT
  | ADDOP
  | SUBOP
  | MULOP
  | DIVOP
  | MODOP
  | POWOP
  | ANDOP
  | OROP
  | XOROP
  | SHLOP
  | USHROP
  | SSHROP
  | ROTLOP
  | ROTROP
  | EQOP
  | NEQOP
  | LEOP
  | LTOP
  | GTOP
  | GEOP
  | HASH
  | EQ
  | LT
  | GT
  | PLUSASSIGN
  | MINUSASSIGN
  | MULASSIGN
  | DIVASSIGN
  | MODASSIGN
  | POWASSIGN
  | CATASSIGN
  | ANDASSIGN
  | ORASSIGN
  | XORASSIGN
  | SHLASSIGN
  | USHRASSIGN
  | SSHRASSIGN
  | ROTLASSIGN
  | ROTRASSIGN
  | NULL
  | DOT_NUM of string
  | NAT of string
  | FLOAT of string
  | CHAR of Mo_values.Value.unicode
  | BOOL of bool
  | ID of string
  | TEXT of string
  | PRIM
  | UNDERSCORE
  (* Trivia *)
  | LINEFEED of line_feed
  | SPACE of int
  | TAB of int (* shudders *)
  | COMMENT of string

type void = { next : void }

let rec absurd : void -> 'a = fun { next } -> absurd next

type 'l trivia = Comment of string | Space of int | Tab of int | Line of 'l

let map_trivia : ('a -> 'b) -> 'a trivia -> 'b trivia =
 fun f -> function
  | Comment str -> Comment str
  | Space n -> Space n
  | Tab n -> Tab n
  | Line l -> Line (f l)

let string_of_line_feed = function LF -> "LF" | CRLF -> "CRLF"

let string_of_trivia : ('a -> string) -> 'a trivia -> string =
 fun f t ->
  match t with
  | Comment str -> str
  | Space n -> Printf.sprintf "Space(%d)" n
  | Tab n -> Printf.sprintf "Tab(%d)" n
  | Line l -> Printf.sprintf "Line(%s)" (f l)

let string_of_trivia_lf : line_feed trivia -> string =
  string_of_trivia string_of_line_feed

let is_lineless_trivia : token -> void trivia option = function
  | SPACE n -> Some (Space n)
  | TAB n -> Some (Tab n)
  | COMMENT c -> Some (Comment c)
  | _ -> None

let is_trivia : token -> line_feed trivia option = function
  | LINEFEED lf -> Some (Line lf)
  | t -> Option.map (map_trivia absurd) (is_lineless_trivia t)

type annotation = {
  range : Lexing.position * Lexing.position;
  leading_trivia : line_feed trivia list;
  trailing_trivia : void trivia list;
}

let dummy_annotation = {
   range = Lexing.dummy_pos, Lexing.dummy_pos;
   leading_trivia = [];
   trailing_trivia = [];
}
