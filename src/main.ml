open Printf

let name = "asc"
let version = "0.1"
let banner = "ActorScript " ^ version ^ " interpreter"
let usage = "Usage: " ^ name ^ " [option] [file ...]"

(*
let print_te =
  Type.Env.iter (fun x c ->
    printf "  type %s := %s\n" x (Con.to_string con)
  )
*)

let print_ce =
  Con.Env.iter (fun c k ->
    printf "  type %s %s\n" (Con.to_string c) (Type.string_of_kind k)
  )

let print_ve =
  Type.Env.iter (fun x (t, m) ->
    printf "  %s%s : %s\n" (Type.string_of_mut m) x (Type.string_of_typ t)
  )

let print_dyn_ve ce ve dyn_ve =
  Type.Env.iter (fun x (t, m) ->
    let v = Value.read_rec_bind (Value.Env.find x dyn_ve) in
    Printf.printf "  %s%s = %s\n"
      (Type.string_of_mut m) x (Value.string_of_val ce t v)
  ) ve

let print_debug_ve =
  Value.Env.iter (fun x b ->
    printf "  %s = %s\n" x (Value.debug_string_of_rec_bind b)
  )

let run filename =
  let ic = open_in filename in 
  let lexer = Lexing.from_channel ic in
  lexer.Lexing.lex_curr_p <-
    {lexer.Lexing.lex_curr_p with Lexing.pos_fname = filename};
  let success =
    try
      let prog = Parser.prog Lexer.token lexer in 
      printf "\nChecking %s:\n" filename;
      let ve, te, ce = Typing.check_prog prog in
      print_ce ce;
      print_ve ve;
      printf "\nInterpreting %s (tracing function calls):\n" filename;
      ignore (Interpret.interpret_prog prog (fun dyn_ve ->
  			Printf.printf "\nFinal state %s:\n" filename;
        print_dyn_ve ce ve dyn_ve;
  		  Value.unit
      ));
      true
    with exn ->
      let r, msg, dump =
        match exn with
        | Lexer.Error (at, msg) -> at, "syntax error, " ^ msg, false
        | Parser.Error -> Lexer.region lexer, "syntax error", false
        | Typing.TypeError (at, msg) -> at, "type error, " ^ msg, false
        | Typing.KindError (at, msg) -> at, "type error, " ^ msg, false
        | Operator.Overflow ->
          !Interpret.last_region, "arithmetic overflow", false
        | _ ->
          !Interpret.last_region, "fatal error " ^ Printexc.to_string exn, true
      in
      if dump then print_debug_ve (!Interpret.last_context).Interpret.vals;
      printf "%s: %s\n" (Source.string_of_region r) msg;
      if dump then printf "%s" (Printexc.get_backtrace ());
      false
  in
  close_in ic;
  success


let args = ref []
let add_arg source = args := !args @ [source]

let argspec = Arg.align
[
  "-v", Arg.Unit (fun () -> printf "%s\n" banner), " show version"
]

let () =
  Arg.parse argspec add_arg usage;
  if !args = [] then printf "%s\n" usage else
  List.iter (fun arg -> if not (run arg) then exit 1) !args
