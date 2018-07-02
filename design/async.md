# Lowering Async (following C\# async/await)

Assuming a promise like interface:

```
class Async<T>(){
  setresult(t:T):T;
  continuewith(k:()->()):();
  result(): T;
}
```

## Example 1 (no control flow)

```
async {
let a = await f();
let b = await g();
a+b;
}  : Async<int>
```

Our approach is inspired by the C# translation (which does, however, assume closures are stateful and can capture l-values).

The basic idea is to hoist all locals (mutable or not) to mutable, outermost variables.

The async expression (typically a block) is lowered to a block that
1. defines a mutable async value `a : Async<T>` to return the result of type `T`.
2. declares all locals as mutable globals (global to the block) (to save the values of locals in the state machine).
3. declares a mutable integral state variable, `s`, initially `0`, (to save the next state of the state machine).
4. defines a (recursive) side-effecting function, `SM:()->()`, encoding a state machine that acts as a call-back to each enclosed await.
5. runs the state machine, SM(), until the first await (if any, with `a.result` unset) or completion (with a.result set).
  * note that SM() must have access to `a` in its closure.
6. returns the task `a` (whether complete or not).

The state machine is function `SM():()` that:
- enters a loop that switches on the current value of `s` to resume the machine from the next state.
- pause the state machine (at an `await`) by first advancing `s` to the next logical state and returning from `SM()` (thus  exiting the loop, too).
- returning a value `t` from the async block (whether implicitly  or explicity), sets the result of `a` (`a.setresult(t)`)

Here's a manual translation of the example above.


```
{
let a0 = Async<int>();
let mutable a : Int;
let mutable b : Int;
let mutable s : Int = 0; // state of the machine, one state for entry plus each await (for now)
func SM() {
  do loop
  switch s {
  case 0 :
    s = 1;
    let a1 = f();
    a1.continuewith(SM);
    return;
  case 1 :
    a = a_1.result;
    s = 2;
    let a2 = g();
    a2.continuewith(SM);
    return;
  case 2:
    b := a_2.result;
    a0.setresult(b1+a2);
    return;
  }
SM();
a_0;
}
```

C# actually has an optimization that allows one to  test whether an awaited async  is already complete (Async<T>.IsCompleted:()->Bool) and continue the loop,
avoiding the cost of scheduling a continuation, but that's a local optimization we could do too.

## Example 2 (with strutured control flow)

With control constructs, the C# approach is to first lower structured control flow to explicit labels and jumps.
Each control point now has a label that can then be interpreted as an additional state of the state machine.
Implicit flow is lowered to *Jumping* to a label. In turn jumping to a label is just setting the state, s, accordingly and re-entering the m/c interpreter (without pausing) with a ```continue loop```.

Roughly:

```
async () {
 while (await f())
   await g()   
}
```

First, lower control flow to labelled loops (see l):

```
async () {
 loop l {
 if (await f()) then {
   await g();
   continue l;
 }
 else break l;
}
```
Then a-normalize awaits (i.e. name the awaited values, so we can reference their ```.continuewith``` and ```.result``` fields)

```
async () {
 loop l {
   let b = await f();
   if (b) then {
     await g();
     continue l;
   }
   else break l;
 }
}
```

Finally, translate as in the previous example, using ```s = ```*nextstate*```; continue loop```
to interpret inserted jumps appropriately.

Roughly, the result might look something like this:

```
{
let a0 = Async<int>();
let mutable s = 0;
func SM():(){
  do loop
  switch s {
  case 0 :
    let a1 = f();
    a1.continuewith(SM);
    return;
  case 1 :
    b = a_1.result;
    if (b) then { state = 2; continue loop };
    else {state = 3; continue loop;} 
    s = 2;
    let a2 = g();
    a2.continuewith(SM);
    return;
  case 2:
    {state = 0;
     continue loop;} 
  case 3:
    {
     state = 4;
     continue loop;
    }
  case 4;
    {
      a_0.setresult(());
      return;
    }
  }
SM();
a_0;
}
```





