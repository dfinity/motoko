%{

let verification_syntax_error at code msg =
  Diag.add_msg (Option.get !Parser_lib.msg_store)
    (Diag.error_message at code "verification syntax" msg)

(* Verification mode only *)

let (&&&) cond (action : Mo_def.Syntax.exp) =
  if not cond then
    verification_syntax_error
      action.Source.at
      "M0181" "verification assertions not permitted in normal mode";
  action

let is_verification () =
  match !Parser_lib.mode with
  | None -> assert false
  | Some mode -> mode.Lexer_lib.verification

%}

(* Viper-only tokens and productions *)

%token INVARIANT
%token IMPLIES
(*
%nonassoc IMPLIES  (* see parser.mly *)
*)
%token OLD

%%

%public exp_bin(B) :
  | e1=exp_bin(B) IMPLIES e2=exp_bin(ob)
    { ImpliesE(e1, e2) @? at $sloc }

%public exp_nondec(B) :
  | ASSERT COLON SYSTEM e=exp_nest
    { is_verification () &&& AssertE(Static, e) @? at $sloc }
  | ASSERT COLON INVARIANT e=exp_nest
    { is_verification () &&& AssertE(Invariant, e) @? at $sloc }
  | ASSERT COLON LOOP COLON INVARIANT e=exp_nest
    { is_verification () &&& AssertE(Loop_invariant, e) @? at $sloc }
  | ASSERT COLON FUNC e=exp_nest
    { is_verification () &&& AssertE(Precondition, e) @? at $sloc }
  | ASSERT COLON RETURN e=exp_nest
    { is_verification () &&& AssertE(Postcondition, e) @? at $sloc }
  | ASSERT COLON s=NAT COLON ASYNC e=exp_nest
    { is_verification () &&& AssertE(Concurrency s, e) @? at $sloc }
  | OLD e=exp_nest
    { is_verification () &&& OldE e @? at $sloc }

%%
