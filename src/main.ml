
let token lb = let tok = Lexer.token lb in
               (* Printf.printf "%s" (Lexer.token_to_string(tok)); *)
	       tok
let main () =
    let filename = Sys.argv.(1) in
    let is = open_in filename in 
    let lexer = Lexing.from_channel is in

    (* I can't seem to get the lexer to use filename for pos_fname, so we update the filename later instead *)
    let string_of_region (r:Source.region)  =
    	let r = {Source.left = {r.left with file = filename};
	         Source.right = {r.right with file = filename}} in
        Source.string_of_region r
    in
    (try
       let prog = Parser.prog token lexer in 
       Typing.check_prog prog;
       Printf.printf "typechecked %s" filename;
    with 
    | Lexer.Syntax (r,m) ->
       let r = string_of_region r in
       Printf.printf "Syntax Error %s:%s!" r m;
    | Parser.Error ->
       let r = string_of_region (Lexer.region lexer) in
       Printf.printf "Syntax Error %s!" r;
    | Typing.TypeError (r,m)  -> 
       let r = string_of_region r in
       Printf.printf "Type Error %s:%s!" r m;
    | Typing.KindError (r,m) ->
       let r = string_of_region r in
       Printf.printf "Kind Error %s:%s!" r m;
    | e ->
       Printf.printf "exception %s!" (Printexc.to_string e))
    ;
    print_newline();
    close_in is
       

let() = main()


