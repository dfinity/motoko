(with migration =
   func({
     }) :
   { } =
   { }
)
persistent actor {
  var g : Int = 0; // reject, a supertype
  assert false;
}
