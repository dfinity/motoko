/**
 * Module     : format.mo
 * Copyright  : 2020 DFINITY Stiftung
 * License    : Apache 2.0 with LLVM Exception
 * Stability  : Stable
 */

import Common "common";
import Galois "galois";
import List "list";
import Nat "nat";
import Util "util";

module {

  type ErrorCorrection = Common.ErrorCorrection;
  type List<T> = List.List<T>;

  public func encode(level : ErrorCorrection, maskRef : List<Bool>) : List<Bool> {
    let input = List.append<Bool>(Common.eciBits(level), maskRef);
    let poly1 = Galois.polyFromBits(Util.padRight(10, input));
    let poly2 = Galois.polyFromBits(Nat.natToBits(1335));
    Util.padLeftTo(15, Nat.natToBits(Nat.natXor(Nat.natFromBits(Galois.polyToBits(Galois.polyAdd(poly1, Galois.polyDivMod(poly1, poly2).1))), 21522)))
  };

}
