
(* 0,_ : unexpected token *)
(* 1   :  and expected symbol *)
(* 2   :  and expected symbols *)
(* 3   :  and parsed items *)

type error_detail = int

exception Error of (string * Lexing.position * Lexing.position)

val parse : error_detail ->
            'a Parser.MenhirInterpreter.checkpoint ->
            Parser.MenhirInterpreter.supplier ->
            Lexing.lexbuf ->
            'a
