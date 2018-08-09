open Type
open Typing


let token lb =
  let tok = Lexer.token lb in
	(* Printf.printf "%s" (Lexer.token_to_string(tok)); *)
	tok

let main () =
  let filename = Sys.argv.(1) in 
  let is = open_in filename in 
  let lexer = Lexing.from_channel is in

  (* I can't seem to get the lexer to use filename for pos_fname, so we update the filename later instead *)
  let string_of_region (r : Source.region) =
  	let r = {Source.left = {r.Source.left with Source.file = filename};
         Source.right = {r.Source.right with Source.file = filename}} in
      Source.string_of_region r
  in
  try
    let prog = Parser.prog token lexer in 
    let ve, te, ke = Typing.check_prog prog in
    Printf.printf "\nChecking %s:\n" filename;
    Env.iter (fun v con -> Printf.printf "  %s := %s\n" v (Con.to_string con)) te;
    Env.iter (fun v (t, mut) -> Printf.printf "  %s : %s\n" v (string_of_typ t)) ve;
    Con.Env.iter (fun con k -> Printf.printf "  %s %s\n" (Con.to_string con) (string_of_kind k)) ke;
    let context = Typing.adjoin_cons (Typing.adjoin_typs (Typing.adjoin_vals Typing.empty_context ve) te) ke in
    Printf.printf "\nInterpreting %s (tracing function calls):\n" filename;
    ignore (Interpret.interpret_prog prog (fun dyn_ve ->
			Printf.printf "\nFinal state %s:\n" filename;
			Type.Env.iter (fun v (t, mut) ->
				let w = Interpret.unrollV (Value.Env.find v dyn_ve) in
				let w =
          match mut with
					| ConstMut -> Value.as_val_bind w
					| VarMut -> !(Value.as_var_bind w)
				in Printf.printf "  %s = %s\n" v (Value.string_of_val context.cons t w)
      ) ve;
		  Value.unitV
    ))
  with
  | Lexer.Syntax (r, m) ->
    let r = string_of_region r in
    Printf.printf "%s: syntax error, %s" r m;
  | Parser.Error ->
    let r = string_of_region (Lexer.region lexer) in
    Printf.printf " %s: syntax error" r;
  | Typing.TypeError (r, m)  -> 
    let r = string_of_region r in
    Printf.printf "%s: type error, %s" r m;
  | Typing.KindError (r, m) ->
    let r = string_of_region r in
    Printf.printf "%s: type error, %s" r m;
  | e ->
     let r = string_of_region !Interpret.last_region in
     let context = !Interpret.last_context in
     let ve = context.Interpret.vals in
     Value.Env.iter (fun v w -> 
    	 Printf.printf "  %s = %s\n" v (Value.debug_string_of_recbind w)) ve;
     Printf.printf "%s: %s" r
       (match e with
       | Operator.Overflow -> "arithmetic overflow"
       | _ -> Printexc.to_string e);
     Printf.printf "%s" (Printexc.get_backtrace ())
  ;
  close_in is


let () = main ()
