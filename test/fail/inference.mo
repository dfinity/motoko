func id<T>(x:T):T { x };
ignore id(1);
ignore id<Nat>(1);

func swap<T,U>(x:T,y:U):(U,T){(y,x)};
ignore swap(1,true);
ignore swap(true,1);

type N = <T>(T->T,T)->T;

func zero<T>(f:T->T,z:T):T{z};

func succ(n:N):N {
  func <T>(f:T->T,z:T) : T { f(n<T>(f,z))}
};

ignore zero;
ignore succ(zero);
ignore succ(succ(zero));

func app<T,U>(f: T-> U, x:T) : U { f(x); };
func compose<T,U,V>(f: T -> U, g : U-> V) : T->V { func x { g (f(x))} };

ignore compose(succ,succ);