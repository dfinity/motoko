# Async Await Translation

Based on 

"A Selective CPS Transformation",
Lasse R.Nielsen, BRICS1
Department of Computer Science, University of Aarhus, Building 540, Ny Munkegade, DK-8000 Aarhus C, Denmark

https://doi.org/10.1016/S1571-0661(04)80969-1

We start with following (typed) term language:

```
e = x
  | c
  | e @ e
  | \x.e
  | let x = e in e
  | if e then e else e
  | async e
  | await e
  | while e do e
  | label l e
  | break l e
  | return e
```

The aim of game is to remove async and await by a source-to-source translation, leaving as much code as possible in direct-style.

Terms have effect `T` (trivial) or `A` (await) with `T` < `A`. A term has effect `A` if any subterm not enclosed by `async` is `await`. 

The only term that introduce effect `A` is `await` - the effect is masked by its innermost enclosing async (if any).

The body of every lambda must be trivial.

We write `t` for trivial terms and `e` otherwise. Every trival term is also a non-trivial term since `T` < `A`.

Term 't' are await free terms (in the sense that await can only occur within a nested async (with no intervening lambdas)).

Trivial terms can be compiled in direct style by translation T[t].
Non-trivial terms must be cps-converted by translations C[e] and CPS[e] and their translations expect a continutation.

```
CPS[ e ] = \\k.let kret = k in C[e] k

where

C[ t ] =
   \\k. k @ T[ t ]
C[ t1 e2 ] =
   \\k. T[ t1 ] @ (\\f.C[ e2 ] @ \\v. k @ (f @ v))
C[ e1 e2 ] =
   \\k. C[ t1 ] @ (\\f.C[ e2 ] @ \\v. k @ (f @ v))
C[ let x = t1 in e1 ] =
   \\k.let x = T[t1] in C[e2] @ k
C[ let x = t1 in e1 ] =
   \\k.C[ t1 ] @ (\\x. C[e2] @ k)
C[ await t ] =
   \\k.await(T[t1],k)
C[ await e ] =
   \\k.C[e] (\\v.await(v,k))
C[ if e1 then e2 else e3 ]  =
   \\k.C[e1](\\b.if b then C[e1] k else C[e2] k)
C[ if t1 then e2 else e3 ]  =
   \\k.if T[t1] then C[e1] k else C[e2] k)
C[ if e1 then t2 else t3 ]  =
   \\k.C[e1](\\b.k @ (if b then T[e1] else T[e2]))
C[ while t1 do e2 ] =
   \\k.let rec l = \u.k (while T[t1] do C[e2] l) in
       l()
C[ while e1 do t2] =
   let rec l = \\k. C[e1](\\v.if v then T[t2]; l@() else k@()) in
   l k
C[ label l e ] = \\l. C[e] @ l	   // we use label l to name the continuation
C[ break l e ] = \\k. C[e] @ l     // discard k, continue from l
C[ return e ] = \\_. C[e] @ kret   // discard k, return
```

The translation of trivial term, `T[ _ ]`, is the identity on all but ```async``` terms, at which point we switch to the CPS[-] translation.
Note `T[await _]` is (deliberately) undefined.

```
T[ async e ] = spawn (\t.CPS(e) @ (\v.complete(t,v))
T[ x ]= x
T[ c ] = c
T[ \x.t ] = \x.T[t]
T[ t1 t1 ] = T(t1) T(t2)
T[ let x = t1 in t2 ] = let x = T[t1]  in T[t2] 
T[ if t1 then t2 else t3] =
   if T[t1] then T[t2] else T[t3]
T[ while t1 do t1 ] =
   while T[t1] do T[t2]
T[ break l t ] =
   break l T[t]
T[ label l t ] =
   label l T[t]
T[ return T[t] ] =
   return T[t]
```

We use the following primitives for scheduling actions (that complete tasks).

```               
spawn(f) = let t = task{result=None;waiters=[]} in
           schedule (\u.f(t));
           t

await(t,k) = match t with
             | {result=Some v} -> k v
	     | {result=None} -> t.waiters <- k::t.waiters; yield()

complete(t,v) = match t with
             | {result=None;waiters} ->
               t.result = Some v;
               foreach waiter in waiters do
                 Schedule(\u.waiter(v))
             | {result=Some _ } -> assert(false) 



yield() = schedule.Next()
```
