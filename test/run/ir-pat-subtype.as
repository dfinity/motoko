type L = {a:{}};
type U = {};

let l = new {a = new {}};

func f(l:L):U = switch l {
    case (u:U) u;
};

let {} = f(l);

// tuples
func t1(tl:(L,L)) : (U,U) = switch tl {
    case (tu: (U,U)) tu;
};

let ({},{}) = t1((l,l));

func t2(tl:(L,L)) : (U,U) = switch tl {
    case (u1:U,u2:U) (u1,u2);
};

let ({},{}) = t2((l,l));


// options

func o1(ol:?L) : ? U = switch ol {
    case (null: ?U) null;
    case (ou: ?U) ou;
};

let ?{} = o1(?l);

func o2(tl:?L) : ?U = switch tl {
    case (null: ?U) null;
    case (? u) (? u);
};

let ?{} = o2(?l);

// records

func r1(rl:object{a:L}) : object {a:U} = switch rl {
    case (ru:{a:U}) ru;
};

let {a={}}:object{a:U} = r1(object{a=l});

func r2(rl:object{a:L}) : object {a:U} = switch rl {
    case ({a=u:U}) object {a=u};
};

let {a={}}:object{a:U} = r2(object{a=l});


// variants

func v1(vl:{#a:L}) : {#a:U} = switch vl {
    case (vu:{#a:U}) vu;
};

let #a {} = v1(#a l);

func v2(vl:{#a:L}) : {#a:U} = switch vl {
    case (#a u) #a u;
};

let #a {} = v2(#a l);

// alternative patterns

func a(l:L):U = switch l {
    case ((_:U) or (_:L))  l;
};

let {} = a(l);
