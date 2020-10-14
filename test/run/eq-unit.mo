// equality on singletons should not hide side effects
import Prim "mo:prim";
ignore (Prim.debugPrint "Look, ma!" == ());
