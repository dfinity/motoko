//MOC-FLAG --non-persistent
actor { // error
  transient let x = #x;
  transient var y = #y;
  stable let z = #z;
}
