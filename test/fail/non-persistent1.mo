//MOC-FLAG --require-persistent-actors
actor {
  transient let _x = #x;
  var _y = #y; // error
  stable let _z = #z;
}
