%%

%public exp_nondec(B) :
  | ASSERT COLON STABLE e=exp_nest
    { AssertE(Invariant, e) @? at $sloc }
  | ASSERT COLON FUNC e=exp_nest
    { AssertE(Precondition, e) @? at $sloc }
  | ASSERT COLON RETURN e=exp_nest
    { AssertE(Postcondition, e) @? at $sloc }

%%
