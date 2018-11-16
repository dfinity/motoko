Type-Directed marshalling between DFN and ActorScript types.
------------------------------------------------------------

For now, I'm totally ignoring polymorphism to sketch a type directed translation that yields natural interop types. I'm also ignoring the fact that closure contain existentially typed environments which we need to serialize or tokenize in some way.

Assuming DFN types are given by:

```
D = i32
  | i64
  | databuf
  | elembuf
  | funcref
  | actorref
  | modref

let scalar(d:D) = d == i32 or d == i64
```

And Actorscript (shared) types (the values of which are *sendable*) 

```
S1 =
  | Null  (* ? *)
  | Bool
  | WordX
  | Int
  | Nat
  | Char
  | Text
  | shared (S1,...,Sn) -> ()
  | S1[]
  | (S1,...,SN)
  | actor {f1:S1,...,fn:Sn}
  | shared object {f1:S1,...,fn:Sn}
```

We define two translations:

```
d[Null] = Int32
d[Text]=databuf
d[Nat] = databuf
d[Int] = databuf
d[Word8|Word16|Word32] = i32
d[Word64] = i64
d[shared (S1,..,SN) -> ()] = funcref
d[a[]] = databuf if scalar d[a]
       | elembuf otherwise
d[actor ...] = actorref
d[shared object (f1:S1,...fn:Sn)] = d[(S1,...,Sn)] (f1,...,fn in sort order)
d[a[]] = databuf if scalar d[a]
       | elembuf otherwise
d[(S1,..,S2)] = elembuf
```

Assuming `v:T`, we define the AS to DFN value translation, `d(v):d(T)`, as follows:

```
d(null):d[Null] = 0
d(v) : d[T]
d(s) : d[Text] = databuf [[c | c = s[i] in len[c] |]
d(n) : d[Nat] = databuf (bytes(n))
d(i) : d[Int] = databuf (bytes(i))
d(w) : d[Word8|Word16|Word32] = word32(w)
d(w) : d[Word64] = word64(w)
d(func f(v1,...,vn) = e) : d[shared (S1,...,Sn) -> ())] =
       funcref ((w1:d(S1),...,wn:d(Sn)) -> let vi = a(wi) in e)
d(null,S?) = databuf{} if scalar(d[S])
           | elembuf{} otherwise
d(v?,S?) = databuf{bytes(d(v))} if scalar(d[S])
	 | elembuf{d(v)}        otherwise
d({v1,...,vn}) : d[S1[]] = databuf {bytes{v1,...,vn}} if scalar[d(S1)]
d({v1,...,vn}) : d[S1[]] = elembuf {d(v1),...,d(vn)}  otherwise
d(actor (actorref,...)) : d[actor ...] = actorref
d((v1,...,vn)] = elembuf { databuf { bytes(vi) | i in 1 .. n, scalar(d(Si)) }} (* roughly *)
                           elembuf { d(vi) | i in 1 .. n, !scalar(d(Si)) }}

d(shared object {f1 = v1,..,fn=vn}) (assuming f1,...,fn in sort order)
  d[shared object {f1 :S1, ..., fn: S2n}] = d((v1,...,vn)) 
```  

Assuming we expect a value of type `T`,
we define the (dependently typed)
DFN to AS value translation, `a:{T:Type)->{w:d(T)}->T`, as follows:

```
a (Null) w = match w with 0 -> none (* else trap? *)
a (Text) w = text_of_databuf w
a (Nat) w = nat_of_databuf w
a (Int) w = int_of_databuf w
a (Word8) w = word8_of_I32 w
a (Word16) w = word15_of_I32 w
a (Word32) w = word32_of_I32 w
a (Word64) w = word64_of_I64 w
a (shared (S1,...,Sn) -> ()) (funref f) =
   fun shared (v1,...,vn) -> f (d(v1:S1),...,d(vn:Sn)))
a (S?) = if databuf.len w = 0 then null else Some a(S)(w[o]) , where scalar(S)
a (S?) = if elembuf.len w = 0 then null else Some a(S)(w[o]) otherwise
a (actor {fi:shared function (Si1,...,Sin) -> ()}) actorref =
   actor (actorref, {fi = a (shared function (Si1,...,Sin) -> ()) (actor.internalize fi) })
a (S1,...,Sn) (elembuf{eb,db}) =
   (* roughly, we'd need to be more precise for scalar sizes*)
   let buf(Si) = if scalar then eb else db
   let ind(Si) = |{Sj | scalar(Sj)=scalar(Si) and j<i}| in
   (a(S1)(buf(S1)(w1)(ind(S1))), ...
    a(Sn)(buf(Sn)(w1)(ind(Sn))))
a (shared object {f:S1,...,fn:Sn}) w = (assuming f1,...,fn in sort order)
  let (v1,...,vn) = a (S1,...,Sn) w in
  new object (f1 = v1,...,fn=vn) 
```

But what should we do about polymorphism? Can we rule it out,
or just transition to uniform serialization for polymorpic values? Or should we bite the bullet and do type-passing... what would that look like?




