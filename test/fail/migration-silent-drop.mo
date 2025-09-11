// test warning suppression

(with migration = func (_ : {f : ()}) : {} = {}) // warn on dropped field
actor a1 { };

(with migration = func (_ : {f : (nowarn: ())}) : {} = {}) // no-warning, named type
actor a2 { };

(with migration = func (_ : {f : ()}) : {} = {}) // warn on re-initialized field
actor b1 { stable let f = (); f };

(with migration = func (_ : {f : (nowarn: ())}) : {} = {}) // no-warning, named type
actor b2 { stable let f = (); f };


(with migration = func (_ : {var f : ()}) : {} = {}) // warn on dropped field
actor c1 { };

(with migration = func (_ : {var f : (nowarn: ())}) : {} = {}) // no-warning, named type
actor c2 { };

(with migration = func (_ : {var f : ()}) : {} = {}) // warn on re-initialized field
actor c3 { stable let f = (); f };

(with migration = func (_ : {var f : (nowarn: ())}) : {} = {}) // no-warning, named type
actor c4 { stable let f = (); f };



