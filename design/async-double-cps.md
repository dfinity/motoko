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
  | await e                // may throw
  | while e do e
  | label l e
  | break l e
  | return e
  | try e with x -> e     // evaluate e, handling error x using e.
  | throw e               // throw error e to nearest enclosing handler or async block
```


The aim of the game is to remove async and await and try catch/throw  by a source-to-source translation, leaving as much code as possible in direct-style.

Terms have effect `T` (trivial) or `A` (await) with `T` < `A`. A term has effect `A` if any subterm not enclosed by `async` is `await`.

The only terms that introduce effect `A` is `await`, `try` or `throw` - the effect is masked by its innermost enclosing `async` (if any).

Note `await`, `try` and `throw` may only occur within an `async` block.

The body of every lambda must be trivial.

We write `t` for trivial terms and `e` otherwise. Every trival term is also a non-trivial term since `T` < `A`.

Terms `t` are `await/try/throw` free terms (in the sense that await can only occur within a nested async (with no intervening lambdas)).

Trivial terms can be compiled in direct style by translation `T[t]`.

Non-trivial terms must be cps-converted by translations `C r [e]` and `CPS[e] r`.
Their translations expect a pair `r = (reply,reject)` and `reply` and `reject` continuations (for interpreting early `return` and `throw`).
The `reply` continuation only changes when we enter an async block. 
The `reject` continuation changes when we enter a `async` or `try`.
The translation `C r [e]` produces a term in cps, taking a single continuation argument.

Since `async` and `try catch` are block structured, we can record the
`reply/reject` continuation in an environment argument, simplifying
the translation by not threading two continuations throughout.


```JS
CPS[ e ] = \\(reply,reject).C (reply,reject) [e] k

where

C r [ t ] =
   \\k. k @ T[ t ]
C r [ t1 e2 ] =
   \\k. let f = T[ t1 ] in C r [ e2 ] @ \\v. k @ (f @ v))
C r [ e1 e2 ] =
   \\k. C r [ e1 ] @ (\\f.C r [ e2 ] @ (\\v. k @ (f @ v))
C r [ let x = t1 in e2 ] =
   \\k.let x = T[t1] in C r [e2] @ k
C r [ let x = e1 in e2 ] =
   \\k.C r [ t1 ] @ (\\x. C r [e2] @ k)
C r [ await t ] =
   \\k.await(T[t1], (k, r) )
C r [ await e ] =
   \\(k,r).C r [e] @ (\\v.await(v,(k,r))
C r [ if e1 then e2 else e3 ]  =
   \\k.C r [e1] @ (\\b.if b then C r [e1]@k else C r [e2]@k)
C r [ if t1 then e2 else e3 ]  =
   \\k.if T[t1] then C r [e1] @ k else C r [e2] @ k
C r [ if e1 then t2 else t3 ]  =
   \\k.C r [e1] @ (\\b.k @ (if b then T[t1] else T[t2])))
C r [ while t1 do e2 ] =
  \\k.
    let rec l = \u. if T[t1] then C r [e2]@l else k@u in
    l@()
C r [ while e1 do t2 ] =
  \\k.
    let rec l = \u. C r [e1](\\v.if v then T[t2] ; l@u else k@u) in
    l@()
C r [ while e1 do e2 ] =
  \\k.
    let rec l = \u. C r [e1] (\\v.if v then C r [e2]@l else k@()) in
    l@()
C r [ label l e ] = \\l. C r [e] @ l                 // we use label l to name the success continuation
C r [ break l e ] = \\k. C r [e] @ l                 // discard k, continue from l
C (reply,_) [ return e ] = \\k. C r [e] @ reply      // discard (k,r), exit via reply
C (reply,reject) [ try e1 with x -> e2 ] =
  let reply' = \\x. C r [e2] @ k in
  \\k. C (reply,reject') [e1] @ k
C (_,reject) [ throw e] = \\k. C r [e] @ reject      // discard k, exit async or try via reject
```

The translation of trivial terms, `T[ _ ]`, is  homomorpic on all terms but `async _`, at which point we switch to the `CPS[-]` translation.
Note `T[await _]`, `T[throw _]` and `T[try _ with _ -> _]`, are (deliberately) undefined.

```JS
T[ async e ] = spawn (\t.CPS(e) @ ((\v.complete(t,v),(\v.reject(t,e))))
T[ x ]= x
T[ c ] = c
T[ \x.t ] = \x.T[t]
T[ t1 t1 ] = T[t1] T[t2]
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

We use the following primitives for scheduling actions (that complete asyncs).

```JS
type 'a cont = 'a -> unit;

type 'a result =
    Pending
  | Rejected of error
  | Completed of 'a

type error = ...;

type 'a async = { mutable result : 'a result; mutable waiters: ('a cont * error cont) list }

spawn(f) = let t = async { result = Pending; waiters = [] } in
           schedule (\u.f(t));
           t

await(t,reply,reject) = match t with
             | {result = Completed v} -> reply v
             | {result = Rejected e} -> reject e
             | {result = Pending} ->
			   t.waiters <- (k,r)::t.waiters;
			   yield()

complete(t,v) = match t with
             | {result = Pending; waiters} ->
               t.result <- Completed v;
               foreach (reply,_reject) in waiters do
                 schedule(\u.reply(v))
             | {result = _ } -> assert(false)

reject(t,v) = match t with
             | { result = Pending; waiters} ->
               t.result <- Rejected v;
               foreach (_reply,reject) in waiters do
                 schedule(\u.reject(v))
             | { result = _ } -> assert(false)

yield() = schedule.Next()
```

The above translations are flawed:

Consider:

```
async {
  label l
  let x = break l 1 in
    break l (await {2})
}
```

The first call to break occurs in a trivial sub-expression and is compiled to `break l T[1]`
However, the second call to `break l (await{2})` is a non-trivial expression (due to the await()) and compiled as the application to a continuation `C[await{2}] @ l`.

Our remedy is to track the target representation of a source label as either target label (Label) or target continuation (Cont) and
translate accordingly. To handle the translation of return to reply or return, we use a distinguished label `l_ret`.

Thus extend the translations `C[]`, `CPS[]` and `T[]` with an implicit environment argument,`env`, classifying labels and
consulted and modified in select rules. The other cases are unchanged (apart from propagating the `env` argument).

```
C env r [ label l e ] =
  let env' = env[l->Cont] in
  \\l. C env' r [e] @ l        // we use label l to name the success continuation

C env r [ break l e ] =
  assert(env[l] = Cont)
  \\k. C env r [e] @ l        // discard k, continue from l

C env (reply,reject) [ return e ] =
  assert(env[kret] = Cont)
  \\k. C env r [e] @ reply   // discard k, using reply

T env [ async e ] =
  let env' = [l_ret->Cont] in
  spawn (\t.CPS env' [e] @ ((\v.complete(t,v),(\e.reject(t,e))))

T env [\x.t] =
  let env' = [l_ret->Label]
  \x.T env' [t]

T env [ break l t ] =
  match env[t] with
  | Cont ->  l @ (T env [t])
  | Label -> break l (T env [t])

T env [ label l t ] =
  let env' = env[l->Label]
  label l (T env' [t])

T env [ return t ] =
  match env[l_ret] with
  | Cont ->  l_ret@(T env [t])
  | Label -> return (T env [t])
```


Returning to the problematic example, we should now have that label `l` is compiled and used as a continuation in both cases:

```
async {
  label l
  let x = break l 1 in
  break l (await {2})
}
```


AS syntax

```
<exp> ::=
  throw <exp>
  try <exp> { (catch <pat> <exp>)+ }
```
