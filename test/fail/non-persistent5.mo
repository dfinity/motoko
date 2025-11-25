//MOC-FLAG --default-persistent-actors
persistent actor { // warn
  transient let _x = #x;
  transient var _y = #y;
  let _z = #z;
}
