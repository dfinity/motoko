//MOC-FLAG --default-persistent-actors
persistent actor { // warn
  transient let _x = #x;
  transient var _y = #y;
  stable let _z = #z; // warn
}
