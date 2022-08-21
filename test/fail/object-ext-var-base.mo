object has_var {
    public var v = 42
};

object no_var {
    public let w = 25
};

let _ = { has_var and no_var }
