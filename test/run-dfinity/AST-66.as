// test cps conversion of async blocks with type decs
let _ = async{
  type T = Null;
  await { async (null:T) };
};

let _ = async{
  type T = U;
  let _ = await { async (null:T) };
  type U = Null;
  await { async (null:T) };
};
