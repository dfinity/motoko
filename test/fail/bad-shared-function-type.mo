type t = shared() -> async* (); // reject (shared function can't return async*)
