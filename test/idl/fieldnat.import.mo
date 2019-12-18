import imported "fieldnat.did";

ignore (imported.foo({_2_=42}));
ignore (imported.bab(42, 42));
ignore (imported.bar({_50_=42}));
ignore (imported.baz({_50_=42; _2_=42}));
