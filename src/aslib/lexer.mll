{
open Parser
module Utf8 = Wasm.Utf8

type mode = Normal | Privileged

exception Error of Source.region * string

let convert_pos pos =
  { Source.file = pos.Lexing.pos_fname;
    Source.line = pos.Lexing.pos_lnum;
    Source.column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
  }

let region lexbuf =
  let left = convert_pos (Lexing.lexeme_start_p lexbuf) in
  let right = convert_pos (Lexing.lexeme_end_p lexbuf) in
  {Source.left = left; Source.right = right}

let error lexbuf msg = raise (Error (region lexbuf, msg))
let error_nest start lexbuf msg =
  lexbuf.Lexing.lex_start_p <- start;
  error lexbuf msg


let utf8 s i =
  let len = if s.[!i] < '\xe0' then 1 else if s.[!i] < '\xf0' then 2 else 3 in
  i := !i + len;
  List.hd (Utf8.decode (String.sub s (!i - len) (1 + len)))

let codepoint lexbuf s i =
  let u =
    if s.[!i] >= '\x80' then utf8 s i else
    if s.[!i] <> '\\' then Char.code s.[!i] else
    match (incr i; s.[!i]) with
    | 'n' -> Char.code '\n'
    | 'r' -> Char.code '\r'
    | 't' -> Char.code '\t'
    | '\\' -> Char.code '\\'
    | '\'' -> Char.code '\''
    | '\"' -> Char.code '\"'
    | 'u' ->
      let j = !i + 2 in
      i := String.index_from s j '}';
      (try
        let n = int_of_string ("0x" ^ String.sub s j (!i - j)) in
        if 0 <= n && n < 0x110000 then n else raise (Failure "")
      with Failure _ -> error lexbuf "unicode escape out of range")
    | h ->
      incr i;
      int_of_string ("0x" ^ String.make 1 h ^ String.make 1 s.[!i])
  in incr i; u

let char lexbuf s =
  codepoint lexbuf s (ref 1)

let text lexbuf s =
  let b = Buffer.create (String.length s) in
  let i = ref 1 in
  while !i < String.length s - 1 do
    let bs = Utf8.encode [codepoint lexbuf s i] in
    Buffer.add_substring b bs 0 (String.length bs)
  done;
  Buffer.contents b
}

let sign = '+' | '-'
let digit = ['0'-'9']
let hexdigit = ['0'-'9''a'-'f''A'-'F']
let num = digit ('_'? digit)*
let hexnum = hexdigit ('_'? hexdigit)*

let letter = ['a'-'z''A'-'Z']
let symbol =
  ['+''-''*''/''\\''^''~''=''<''>''!''?''@''#''$''%''&''|'':''`''.''\'']

let space = [' ''\t''\n''\r']
let ascii = ['\x00'-'\x7f']
let ascii_no_nl = ['\x00'-'\x09''\x0b'-'\x7f']
let utf8cont = ['\x80'-'\xbf']
let utf8enc =
    ['\xc2'-'\xdf'] utf8cont
  | ['\xe0'] ['\xa0'-'\xbf'] utf8cont
  | ['\xed'] ['\x80'-'\x9f'] utf8cont
  | ['\xe1'-'\xec''\xee'-'\xef'] utf8cont utf8cont
  | ['\xf0'] ['\x90'-'\xbf'] utf8cont utf8cont
  | ['\xf4'] ['\x80'-'\x8f'] utf8cont utf8cont
  | ['\xf1'-'\xf3'] utf8cont utf8cont utf8cont
let utf8 = ascii | utf8enc
let utf8_no_nl = ascii_no_nl | utf8enc

let escape = ['n''r''t''\\''\'''\"']
let character =
    [^'"''\\''\x00'-'\x1f''\x7f'-'\xff']
  | utf8enc
  | '\\'escape
  | '\\'hexdigit hexdigit 
  | "\\u{" hexnum '}'

let nat = num | "0x" hexnum
let frac = num
let hexfrac = hexnum
let float =
    num '.' frac?
  | num ('.' frac?)? ('e' | 'E') sign? num
  | "0x" hexnum '.' hexfrac?
  | "0x" hexnum ('.' hexfrac?)? ('p' | 'P') sign? num
let char = '\'' character '\''
let text = '"' character* '"'
let id = letter ((letter | digit | '_')*)
let privileged_id = "@" id

let reserved = ([^'\"''('')'';'] # space)+  (* hack for table size *)

rule token mode = parse
  | "(" { LPAR }
  | ")" { RPAR }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LCURLY }
  | "}" { RCURLY }
  | ";" { SEMICOLON }
  | ";\n" { Lexing.new_line lexbuf; SEMICOLON_EOL }
  | "," { COMMA }
  | ":" { COLON }
  | "<:" { SUB }
  | "." { DOT }
  | "?" { QUEST }
  | "=" { EQ }
  | "<" { LT }
  | ">" { GT }
  | "+" { ADDOP }
  | "-" { SUBOP }
  | "*" { MULOP }
  | "/" { DIVOP }
  | "%" { MODOP }
  | "**" { POWOP }
  | "&" { ANDOP }
  | "|" { OROP }
  | "^" { XOROP }
  | "<<" { SHLOP }
  | space">>" { USHROP } (*TBR*)
  | "+>>" { SSHROP }
  | "<<>" { ROTLOP }
  | "<>>" { ROTROP }
  | "#" { HASH }

  | "==" { EQOP }
  | "!=" { NEQOP }
  | ">=" { GEOP }
  | "<=" { LEOP }
  | ":=" { ASSIGN }

  | "+=" { PLUSASSIGN }
  | "-=" { MINUSASSIGN }
  | "*=" { MULASSIGN }
  | "/=" { DIVASSIGN }
  | "%=" { MODASSIGN }
  | "**=" { POWASSIGN }
  | "&=" { ANDASSIGN }
  | "|=" { ORASSIGN }
  | "^=" { XORASSIGN }
  | "<<=" { SHLASSIGN }
  | ">>=" { USHRASSIGN }
  | "+>>=" { SSHRASSIGN }
  | "<<>=" { ROTLASSIGN }
  | "<>>=" { ROTRASSIGN }
  | "#=" { CATASSIGN }

  | space">"space { GTOP } (*TBR*)
  | space"<"space { LTOP } (*TBR*)
  | "->" { ARROW }
  | "_" { UNDERSCORE }

  | '.' (num as s) { DOT_NUM s }
  | nat as s { NAT s }
  | float as s { FLOAT s }
  | char as s { CHAR (char lexbuf s) }
  | text as s { TEXT (text lexbuf s) }
  | '"'character*('\n'|eof)
    { error lexbuf "unclosed text literal" }
  | '"'character*['\x00'-'\x09''\x0b'-'\x1f''\x7f']
    { error lexbuf "illegal control character in text literal" }
  | '"'character*'\\'_
    { error_nest (Lexing.lexeme_end_p lexbuf) lexbuf "illegal escape" }

  | "actor" { ACTOR }
  | "and" { AND }
  | "async" { ASYNC }
  | "assert" { ASSERT }
  | "await" { AWAIT }
  | "break" { BREAK }
  | "case" { CASE }
  | "class" { CLASS }
  | "continue" { CONTINUE }
  | "label" { LABEL }
  | "else" { ELSE }
  | "false" { BOOL false }
  | "for" { FOR }
  | "func" { FUNC }
  | "if" { IF }
  | "in" { IN }
  | "import" { IMPORT }
  | "module" { MODULE }
  | "new" { NEW }
  | "not" { NOT }
  | "null" { NULL }
  | "object" { OBJECT }
  | "or" { OR }
  | "let" { LET }
  | "loop" { LOOP }
  | "private" { PRIVATE }
  | "return" { RETURN }
  | "shared" { SHARED }
  | "debug_show" { DEBUG_SHOW }
  | "switch" { SWITCH }
  | "true" { BOOL true }
  | "type" { TYPE }
  | "var" { VAR }
  | "while" { WHILE }

  | "prim" as s { if mode = Privileged then PRIM else ID s }
  | id as s { ID s }
  | privileged_id as s { if mode = Privileged then ID s else error lexbuf "privileged identifier" }

  | "//"utf8_no_nl*eof { EOF }
  | "//"utf8_no_nl*'\n' { Lexing.new_line lexbuf; token mode lexbuf }
  | "//"utf8_no_nl* { token mode lexbuf (* causes error on following position *) }
  | "/*" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; token mode lexbuf }
  | space#'\n' { token mode lexbuf }
  | '\n' { Lexing.new_line lexbuf; token mode lexbuf }
  | eof { EOF }

  | utf8 { error lexbuf "malformed operator" }
  | _ { error lexbuf "malformed UTF-8 encoding" }

and comment start = parse
  | "*/" { () }
  | "/*" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; comment start lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment start lexbuf }
  | eof { error_nest start lexbuf "unclosed comment" }
  | utf8 { comment start lexbuf }
  | _ { error lexbuf "malformed UTF-8 encoding" }
