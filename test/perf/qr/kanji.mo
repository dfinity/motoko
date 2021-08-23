/**
 * Module     : kanji.mo
 * Copyright  : 2020 DFINITY Stiftung
 * License    : Apache 2.0 with LLVM Exception
 * Stability  : Experimental
 */

import List "list";
import Prelude "prelude";
import Version "version";

module {

  type List<T> = List.List<T>;
  type Version = Version.Version;

  public func encode(version : Version, text : Text) : ?List<Bool> {
    Prelude.printLn("Error: Kanji mode is not yet implemented!");
    Prelude.unreachable()
  };

}
