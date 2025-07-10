//MOC-FLAG --require-persistent-actors
actor {
  let _x = #x; // error
  var _y = #y; // error
  stable let _z = #z;
}
