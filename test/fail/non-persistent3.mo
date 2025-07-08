//MOC-FLAG --non-persistent
persistent actor {
  transient let _x = #x;
  transient var _y = #y;
  stable let _z = #z; // warn
}
