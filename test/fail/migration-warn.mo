// Warn about fields that are consumed, not produced and re-declared.
// These fields will be re-initialized, not retained.
// This may be intentional or a sign of data loss, hence just a warning.
actor (with migration =
  func({ data : Nat }) : { } {
    ignore data;
    {  }
   })
   A {

   stable var data = 0;

   ignore data;

};

// fix 1: remove from domain
actor (with migration =
  func({ }) : { } {
    {  }
   })
   B {

   stable var data = 0;

   ignore data;

};

// fix 2: add to range
actor
  (with migration =
     func({ data : Nat }) : { data : Nat } {
       { data }
     })
   C {

   stable var data = 0;

   ignore data;

};
