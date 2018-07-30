type T<a,b> = a;

actor class A<t>() {};

func test0(a : T<Int, Bool>, b : T<Int, Nat>) : T<Int, Word8> = (if true a else b) : Int;

func test1(a : <t> t->t, b : <u> u->u) : <c> c->c = if true a else b;

func test2(a : <t, u> t->u, b : <t, u> t->u) : <t, u> t->u = if true a else b;

func test3(a : A<Int>, b : A<Int>) : A<Int> = if true a else b;

func test4<t>(a : A<t>, b : A<t>) : A<t> = if true a else b;

func test5<t>(a : A<(T<t, Int>)>, b : A<(T<t, Bool>)>) : A<(T<t, Word8>)> = if true a else b;

func test6<t>(a : A<T<t, Int>>, b : A<T<t, Bool>>) : A<T<t, Word8>> = if true a else b;

// func wrong7<t, u>(a : A<t>, b : A<u>) : A<t> = if true then a else b;

// func test3(a : <t,u> t->u, b : <t, u> u->t) : <t, u> t->u = if true then a else b;
