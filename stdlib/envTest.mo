import E "env.mo";
import P "prelude.mo";

P.printLn("Env");

{
  P.printLn("  Env.time");

  let env = E.Env();        // time is initialized to 0
  assert(env.currentTime() == 10); // time advances before reading it 
  assert(env.currentTime() == 20);
  assert(env.currentTime() == 30);
};

{
  P.printLn("  Event");
 
  let e = E.Event<Bool>(true, 42);
  assert(e.time() == 42); 
  assert(e.value() == true); 
  assert(e.rep().0 == 42); 
  assert(e.rep().1 == true); 
}; 

{
  P.printLn("  (un)wrap");
 
  let env = E.Env();
  let e1 = env.wrap<Bool>(true);
  assert(env.unwrap<Bool>(e1) == true); 
  assert(e1.time() == env.currentTime()-10); 
  assert(e1.value() == true); 

  let e2 = env.wrap<Bool>(false);
  assert(env.unwrap<Bool>(e2) == false); 
  assert(e2.time() == env.currentTime()-10); 
  assert(e2.value() == false); 
};
