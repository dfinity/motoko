%%

%public exp_nondec(B) :
  | ASSERT COLON STABLE e=exp_nest
    { AssertE(e) @? at $sloc }
  | ASSERT COLON FUNC e=exp_nest
    { AssertE(e) @? at $sloc }
  | ASSERT COLON RETURN e=exp_nest
    { AssertE(e) @? at $sloc }

%%
