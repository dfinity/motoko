//MOC-FLAG -dl
import { debugPrint; error; errorMessage; call_raw; principalOfActor } =  "mo:⛔";

actor {
    public func foo() {
        try {
            try {
                try { await async (); if true return else return }
                finally { debugPrint "OUT2" }
            }
            finally { debugPrint "OUT1" }
        }
        finally { debugPrint "OUT0" }
    }
}
