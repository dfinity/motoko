//MOC-FLAG --non-persistent
actor {
  let _x = #x; // error
  var _y = #y; // error
  stable let _z = #z;
}
