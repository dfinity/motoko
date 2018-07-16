
open Typing

let token lb = let tok = Lexer.token lb in
               (* for debugging: *)
	       (* Printf.printf "%s" (Lexer.token_to_string(tok)); *)
	       tok
let main () =
    let filename = Sys.argv.(1) in
    let is = open_in filename in 
    let lexer = Lexing.from_channel is in

    (* I can't seem to get the lexer to use filename for pos_fname, so we update the filename later instead *)
    let string_of_region (r:Source.region)  =
    	let r = {Source.left = {r.left with Source.file = filename};
	         Source.right = {r.right with Source.file = filename}} in
        Source.string_of_region r
    in
    (try
       let prog = Parser.prog token lexer in 
       let (ve,ce,ke) = Typing.check_prog prog in
       Printf.printf "typechecked %s" filename;
       print_newline();
       Env.iter (fun v con -> Printf.printf "\n %s -> %s" v (Con.to_string con)) ce;
       Env.iter (fun v (t,mut) -> Printf.printf "\n %s : %s" v (typ_to_string t)) ve;
       ConEnv.iter (fun (con:con) k -> Printf.printf "\n %s %s" (Con.to_string con) (kind_to_string k)) ke;
       let context = Typing.union_kinds (Typing.union_constructors (Typing.union_values Typing.prelude  ve) ce) ke in
       let _ = Interpret.interpret_prog prog (fun dyn_ve ->
					  Env.iter (fun v (t,mut) -> Printf.printf "\n %s -> %s" v (
						        Interpret.Values.val_to_string context t (Interpret.Values.derefV (Env.find v dyn_ve)))) ve;
					  Interpret.Values.unitV)

	 in
       ()
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
       Printf.printf "exception %s:" (Printexc.to_string e));
       Printf.printf "%s" (Printexc.get_backtrace())
    ;
    print_newline();
    close_in is
       

let() = main()


