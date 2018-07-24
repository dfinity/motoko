

type T<a,b> = a;

actor class  A<t>(){
};

func Test0(a:T<Int,Bool>,b:T<Int,Nat>): T<Int,Word8> = (if true a else b): Int;

func Test1(a:<t>t->t,b:<u>u->u):<c>c->c = if true a else b;

func Test2(a:<t,u>t->u,b:<t,u>t->u):<t,u>t->u = if true a else b;

func Test3(a:A<Int>,b:A<Int>):A<Int> = if true a else b;

func Test4<t>(a:A<t>,b:A<t>):A<t> = if true a else b;

func Test5<t>(a:A<(T<t,Int>)> , b:A<(T<t,Bool>)>):A<(T<t,Word8>)> = if true a else b;

func Test6<t>(a:A<T<t,Int>> , b:A<T<t,Bool>>):A<T<t,Word8>> = if true a else b;

/* func Wrong7<t,u>(a:A<t> , b:A<u>):A<t> = if true then a else b; */


/* func Test3(a:<t,u>t->u,b:<t,u>u->t):<t,u>t->u = if true then a else b; */




