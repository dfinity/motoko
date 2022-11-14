object has_var {
    public var v = 42
};

object no_var {
    public let w = 25
};

// analysis
let _ : {var v : Nat; w : Nat } = { has_var and no_var };

//synthesis
let _ = { has_var and no_var }
