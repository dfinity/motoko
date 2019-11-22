/* test n-ary async/await */

/* n-ary args */
{
let t = "0_0";
shared func f0_0() : async () {};
let _ : async () = async {
  await f0_0();
  debugPrint t;
};
};

{
let t = "1_0";
shared func f1_0(x:Int) : async () {
  assert(x == 1);
};
let _ : async () = async {
  await f1_0(1);
  debugPrint t;
};
};

{
let t = "2_0";
shared func f2_0(x:Int,y:Bool) : async () {
  assert(x==1);
  assert(y==true);
};
let _ : async () = async {
  await f2_0(1,true);
  debugPrint t;
};
};

{
let t = "3_0";
shared func f3_0(x:Int,y:Bool,z:Text) : async () {
  assert(x == 1);
  assert(y == true);
  assert(z == "a");
};
let _ : async () = async {
  await f3_0(1,true,"a");
  debugPrint t;
};
};

/* n-ary returns */

{
let t = "0_0";
shared func f0_0() : async () {};
let _ : async () = async {
  await f0_0();
  debugPrint t;
};
};

{
let t = "0_1";
shared func f0_1() : async Int {
   1;
};

let _ : async Int = async {
  let x = await f0_1();
  assert(x == 1);
  debugPrint t;
  x;
};
};

{
let t = "0_2";
shared func f0_2() : async (Int,Bool) {
   (1,true);
};
let _ : async (Int,Bool) = async {
  let (x,y) = await f0_2();
  assert(x==1);
  assert(y==true);
  debugPrint t;
  (x,y);
};
};


{
let t = "0_3!!";
shared func f0_3() : async (Int,Bool,Text) {
   (1,true,"a");
};
let _ : async (Int,Bool,Text)  = async {
  let (x,y,z) = await f0_3();
  assert(x==1);
  assert(y==true);
  assert(z=="a");
  debugPrint t;
  (x,y,z);
};
};



/* special case: unary tuples */
/*
{
let t = "(1)-(1)";
shared func fu_u(a:Int,) : async (Int,) {
   return (2*a,);
};

let _ : async (Int,)  = async {
  let (x,) = await fu_u(1);
  assert(x==2);
  debugPrint t;
  return (x,);
};
};
*/


/* Disabled: No generic messages are supported
func Generic<T <: Shared>(t:Text, x:T,eq:(T,T)->Bool)  {

shared func fu_u(x:T) : async T {
   return x;
};

let _ : async T  = async {
  let y = await fu_u(x);
  assert(eq(x,y));
  debugPrint t;
  return y;
};
};


Generic<Int>("<Int>", 1, func eq(i:Int,j:Int) : Bool = i == j);

Generic<()>("<()>", (), func eq(i:(),j:()) : Bool = true);

Generic<(Int,Bool)>("<(Int,Bool)>", (1,true),
	            func eq((i,b):(Int,Bool),
		            (j,c):(Int,Bool)) : Bool = i == j and b == c);
*/
