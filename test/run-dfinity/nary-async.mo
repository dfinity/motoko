/* test n-ary future/await */

/* n-ary args */
{
let t = "0_0\n";
shared func f0_0() : future () {};
let _ : future () = future {
  await f0_0();
  print t;
};
};

{
let t = "1_0\n";
shared func f1_0(x:Int) : future () {
  assert(x == 1);
};
let _ : future () = future {
  await f1_0(1);
  print t;
};
};

{
let t = "2_0\n";
shared func f2_0(x:Int,y:Bool) : future () {
  assert(x==1);
  assert(y==true);
};
let _ : future () = future {
  await f2_0(1,true);
  print t;
};
};

{
let t = "3_0\n";
shared func f3_0(x:Int,y:Bool,z:Text) : future () {
  assert(x == 1);
  assert(y == true);
  assert(z == "a");
};
let _ : future () = future {
  await f3_0(1,true,"a");
  print t;
};
};

/* n-ary returns */

{
let t = "0_0\n";
shared func f0_0() : future () {};
let _ : future () = future {
  await f0_0();
  print t;
};
};

{
let t = "0_1\n";
shared func f0_1() : future Int {
   1;
};

let _ : future Int = future {
  let x = await f0_1();
  assert(x == 1);
  print t;
  x;
};
};

{
let t = "0_2\n";
shared func f0_2() : future (Int,Bool) {
   (1,true);
};
let _ : future (Int,Bool) = future {
  let (x,y) = await f0_2();
  assert(x==1);
  assert(y==true);
  print t;
  (x,y);
};
};


{
let t = "0_3\n!!";
shared func f0_3() : future (Int,Bool,Text) {
   (1,true,"a");
};
let _ : future (Int,Bool,Text)  = future {
  let (x,y,z) = await f0_3();
  assert(x==1);
  assert(y==true);
  assert(z=="a");
  print t;
  (x,y,z);
};
};



/* special case: unary tuples */
/*
{
let t = "(1)-(1)\n";
shared func fu_u(a:Int,) : future (Int,) {
   return (2*a,);
};

let _ : future (Int,)  = future {
  let (x,) = await fu_u(1);
  assert(x==2);
  print t;
  return (x,);
};
};
*/


/* Disabled: No generic messages are supported
func Generic<T <: Shared>(t:Text, x:T,eq:(T,T)->Bool)  {

shared func fu_u(x:T) : future T {
   return x;
};

let _ : future T  = future {
  let y = await fu_u(x);
  assert(eq(x,y));
  print t;
  return y;
};
};


Generic<Int>("<Int>\n", 1, func eq(i:Int,j:Int) : Bool = i == j);

Generic<()>("<()>\n", (), func eq(i:(),j:()) : Bool = true);

Generic<(Int,Bool)>("<(Int,Bool)>\n", (1,true),
	            func eq((i,b):(Int,Bool),
		            (j,c):(Int,Bool)) : Bool = i == j and b == c);
*/
