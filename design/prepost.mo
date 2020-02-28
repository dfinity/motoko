let a = actor {
};

// Pre(a} = {};
// Post(a) = {};

let a1 = actor {
    post var x = 0;
    public func get_x() :async Int { x };
    public func set_x(v:Int) { x := v};
},

// Pre(a1) = {};
// Post(a1) = {x : int};

actor a3 {
    pre x: int ;
    post var x = pre.x;
    post var y = 0;
    public func x() :async Int { x };
    public func set_x(v:Int) { x };
    public func y() :async Int { y };
    public func set_y(v:Int) { y = v};
},

// Pre(a2) = {x: int};
// Post(a2) = {x: int, y: int}   

};

//Note {} <: Pre{a}
//    Post{a} <: Pre{a1} 
//    Post{a1} <: Pre{a2}