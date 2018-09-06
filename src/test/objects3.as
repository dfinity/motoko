type Q = {var this : Q?; x : Nat};
let q : Q = new {var this = null; x = 4};
q.this := q;
/* how to access this?
assert(q.this.this.x == 1);
*/
