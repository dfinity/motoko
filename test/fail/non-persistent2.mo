//MOC-FLAG --require-persistent-actors
actor { // error
  transient let x = #x;
  transient var y = #y;
  stable let z = #z;
}
