import _Unused1 "mo:pkg/unused3"; // don't report package imports usage warning
import _Unused2 "unused4"; // do report relative imports usage warning

module {

  // a private, unused definition
  let unused2 = 1;

}
