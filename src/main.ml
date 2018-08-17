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
  Type.Env.iter (fun x t ->
    printf "  %s : %s\n" x (Type.string_of_typ t)
  )

let print_dyn_ve ce ve dyn_ve =
  Type.Env.iter (fun x t ->
    let v = Value.read_rec_bind (Value.Env.find x dyn_ve) in
    Printf.printf "  %s = %s\n" x (Value.string_of_val ce t v)
  ) ve

let print_debug_ve =
  Value.Env.iter (fun x b ->
    printf "  %s = %s\n" x (Value.debug_string_of_rec_bind b)
  )

let trace heading filename =
  printf "\n%s %s:\n" heading filename

let run filename =
  let ic = open_in filename in 
  let lexer = Lexing.from_channel ic in
  lexer.Lexing.lex_curr_p <-
    {lexer.Lexing.lex_curr_p with Lexing.pos_fname = filename};
  let success =
    try
      let prog = Parser.parse_prog Lexer.token lexer in 
      trace "Checking" filename;
      let ve, te, ce = Typing.check_prog prog in
      print_ce ce;
      print_ve ve;
      trace "Interpreting" filename;
      Interpret.interpret_prog prog (fun dyn_ve ->
  			trace "Finished" filename;
        print_dyn_ve ce ve dyn_ve;
  		  Value.unit
      );
      true
    with exn ->
      let r, sort, msg, dump =
        match exn with
        | Lexer.Error (at, msg) -> at, "syntax", msg, false
        | Parser.Error -> Lexer.region lexer, "syntax", "unexpected token", false
        | Typing.TypeError (at, msg) -> at, "type", msg, false
        | Typing.KindError (at, msg) -> at, "type", msg, false
        | Interpret.Trap (at, msg) -> at, "execution", msg, false
        | _ ->
          Interpret.get_last_region(), "fatal", Printexc.to_string exn, true
      in
      if dump then print_debug_ve (Interpret.get_last_context ()).Interpret.vals;
      printf "%s: %s error, %s\n" (Source.string_of_region r) sort msg;
      if dump then Printexc.print_backtrace stderr;
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
  Printexc.record_backtrace true;
  Arg.parse argspec add_arg usage;
  if !args = [] then printf "%s\n" usage else
  List.iter (fun arg -> if not (run arg) then exit 1) !args
