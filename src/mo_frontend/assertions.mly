%%

%public exp_nondec(B) :
  | ASSERT LBRACKET STABLE RBRACKET e=exp_nest
    { AssertE(e) @? at $sloc }
  | ASSERT LBRACKET FUNC RBRACKET e=exp_nest
    { AssertE(e) @? at $sloc }
 (* | ASSERT LBRACKET RETURN RBRACKET e=exp_nest
    { AssertE(e) @? at $sloc } *)

%%
