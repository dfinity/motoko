%token INVARIANT
(* I get
> Error: 54 states have shift/reduce conflicts.
when I write this here (instead of in parser.mly)

%token IMPLIES
%nonassoc IMPLIES
*)
%%

%public exp_bin(B) :
  | e1=exp_bin(B) IMPLIES e2=exp_bin(ob)
    { ImpliesE(e1, e2) @? at $sloc }

%public exp_nondec(B) :
  | ASSERT COLON INVARIANT e=exp_nest
    { AssertE(Invariant, e) @? at $sloc }
  | ASSERT COLON FUNC e=exp_nest
    { AssertE(Precondition, e) @? at $sloc }
  | ASSERT COLON RETURN e=exp_nest
    { AssertE(Postcondition, e) @? at $sloc }
  | ASSERT COLON s=NAT COLON ASYNC e=exp_nest
    { AssertE(Concurrency s, e) @? at $sloc }

%%
