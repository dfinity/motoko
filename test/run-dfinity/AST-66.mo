// test cps conversion of future blocks with type decs

let _ = future{
  type T = Null;
  await { future (null:T) };
};

let _ = future{
  type T = U;
  let _ = await { future (null:T) };
  type U = Null;
  await { future (null:T) };
};
