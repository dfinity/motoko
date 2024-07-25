//MOC-FLAG -dl
import { debugPrint; error; errorMessage; call_raw; principalOfActor } =  "mo:⛔";

actor {
    public func foo() {
        try {
            try {
                try {}
                finally { debugPrint "OUT2" }
            }
            finally { debugPrint "OUT1" }
        }
        finally { debugPrint "OUT0" }
    }
}
