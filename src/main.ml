module Run = Wasm.Run
module I32 = Wasm.I32
module Lexer = Lexer
let of_string = I32.of_string
let foo s = Run.run_string s

let token lb = let tok = Lexer.token lb in
               Printf.printf "%s" (Lexer.token_to_string(tok));
	       tok

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
(* Run the parser on this line of input. *)
   Printf.printf ">%s%!" line ;
   let prog = Parser.prog token linebuf in
   Printf.printf "ok" 
with
  | _ ->  Printf.printf "noway" 
(*
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)
*)
let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel
  
(* let () =
  repeat (Lexing.from_channel stdin) *)


let main () =
    let filename = Sys.argv.(1) in
    let is = open_in filename in
    let lexer = Lexing.from_channel is in
    try 
       let prog = Parser.prog token lexer in 
       Typing.check_prog prog;
       Printf.printf "ok" ;
       close_in is
    with _ -> 
       Printf.printf "noway" ;
       close_in is

let() = main()


