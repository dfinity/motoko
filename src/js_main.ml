open Printf

let rec map_fold_left f x = function
  | [] -> (x, [])
  | (y::ys) ->
      let (x1, z)  = f x y in
      let (x2, zs) = map_fold_left f x1 ys in
      (x2, z::zs)

let print_ce =
  Con.Env.iter (fun c k ->
    let eq, params, typ = Type.strings_of_kind k in
    printf "type %s%s %s %s\n" (Con.to_string c) params eq typ
  )

let print_stat_ve =
  Type.Env.iter (fun x t ->
    let t' = Type.as_immut t in
    printf "%s %s : %s\n"
      (if t == t' then "let" else "var") x (Type.string_of_typ t')
  )

let print_dyn_ve env =
  Value.Env.iter (fun x d ->
    let t = Type.Env.find x env.Typing.vals in
    let t' = Type.as_immut t in
    printf "%s %s : %s = %s\n"
      (if t == t' then "let" else "var") x
      (Type.string_of_typ t') (Value.string_of_def d)
  )

let print_dyn_ve_untyped =
  Value.Env.iter (fun x d ->
    printf "%s = %s\n" x (Value.string_of_def d)
  )

let print_scope env (ve, te, ce) dyn_ve =
  print_ce ce;
  print_dyn_ve env dyn_ve

let print_val env v t =
  printf "%s : %s\n" (Value.string_of_val v) (Type.string_of_typ t)

let phase heading filename =
  if !Flags.verbose then printf "-- %s %s:\n%!" heading filename


(* Typechecking *)

let typecheck to_stdout env lexer parse infer name =
  lexer.Lexing.lex_curr_p <-
    {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
  try
    let prog = parse Lexer.token lexer in
    phase "Checking" name;
    let t, ((ve, te, ce) as stat_scope) = infer env prog in
    let env' = Typing.adjoin env stat_scope in
    if !Flags.trace then begin
      print_ce ce;
      print_stat_ve ve
    end;
    Some (env', (name, t, env', stat_scope, prog))
  with exn ->
    let r, sort, msg, dump =
      match exn with
      | Lexer.Error (at, msg) -> at, "syntax", msg, false
      | Parser.Error -> Lexer.region lexer, "syntax", "unexpected token", false
      | Typing.Error (at, msg) -> at, "type", msg, false
      | End_of_file -> raise exn
      | _ ->
        Interpret.get_last_region (), "fatal", Printexc.to_string exn, true
    in
    if dump then printf "\n";
    if to_stdout then begin
      printf "%s: %s error, %s\n" (Source.string_of_region r) sort msg;
      if !Flags.verbose then printf "\n";
    end else begin
      eprintf "%s: %s error, %s\n" (Source.string_of_region r) sort msg;
      if !Flags.verbose then eprintf "\n";
    end;
    None

let typecheck_string env s name =
  let lexer = Lexing.from_string s in
  let parse = Parser.parse_prog in
  let infer env prog = Type.unit, Typing.check_prog env prog in
  typecheck false env lexer parse infer name

let compile_mod (name, _t, _stat_env, _stat_scope, prog) : string =
  phase "Compiling" name;
  let m = Compile.compile prog in
  Compile.string_of_wat m

let js_compile source =
  Flags.privileged := true;
  match typecheck_string Typing.empty_env Prelude.prelude "prelude" with
  | Some (env1, prel) ->
      begin match typecheck_string env1 source "js-input" with
      | Some (_, tc'ed_mod) -> compile_mod tc'ed_mod
      | None -> ""
      end
 | None -> ""

let _ = Js.export "asc"
    (object%js
       method compile s = Js.string (js_compile (Js.to_string s))
     end);
