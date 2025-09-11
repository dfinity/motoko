// test warning suppression

(with migration = func ({f = _ : ()}) : {} = {}) // warnon dropped field
actor a1 { };

(with migration = func ({f = _ : (nowarn: ()) }) : {} = {}) // no-warning, named type
actor a2 { };

(with migration = func ({f = _ : () }) : {} = {}) // warn on re-initialized field
actor b1 { stable let f = (); f };

(with migration = func ({f = _ : (nowarn: ()) }) : {} = {}) // no-warning, named type
actor b2 { stable let f = (); f };

