%%

%public exp_nondec(B) :
  | ASSERT STABLE e=exp_nest
    { AssertE(e) @? at $sloc }

%%
