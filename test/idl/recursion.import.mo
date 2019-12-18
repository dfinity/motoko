import imported "recursion.did";

ignore (imported.g(?{head=42; tail=null}));
imported.f(imported)
