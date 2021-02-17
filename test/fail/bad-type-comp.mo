ignore ((module {}) : module { type T = Null });
ignore ((module { public type T = Int}) : module { type T = Null });
ignore ((module { public type T = Null}) : module { type T = Null; type T = Null });
