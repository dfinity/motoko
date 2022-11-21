(* Viper only tokens and productions *)

%token INVARIANT
%token IMPLIES
(*
%nonassoc IMPLIES  (* see parser.mly *)
*)

%%

%public exp_bin(B) :
  | e1=exp_bin(B) IMPLIES e2=exp_bin(ob)
    { ImpliesE(e1, e2) @? at $sloc }

%public exp_nondec(B) :
  | ASSERT COLON SYSTEM e=exp_nest
    { is_verification () &&& AssertE(Static, e) @? at $sloc }
  | ASSERT COLON INVARIANT e=exp_nest
    { is_verification () &&& AssertE(Invariant, e) @? at $sloc }
  | ASSERT COLON FUNC e=exp_nest
    { is_verification () &&& AssertE(Precondition, e) @? at $sloc }
  | ASSERT COLON RETURN e=exp_nest
    { is_verification () &&& AssertE(Postcondition, e) @? at $sloc }
  | ASSERT COLON s=NAT COLON ASYNC e=exp_nest
    { is_verification () &&& AssertE(Concurrency s, e) @? at $sloc }

%%
