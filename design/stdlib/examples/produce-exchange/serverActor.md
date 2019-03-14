

 PESS: Server Actor
 =======================================

 As part of the PESS definition, we define an interface that gives the
 format of each message sent by a participant, and response received in
 return.

 In ActorScript, this format specification corresponds to the
 public-facing signature of the Server actor, defined below.

 This server actor gives a collection of ingress messages and
 corresponding response types for each participant in the exchange,
 using only types defined in the PESS (e.g., no collection types, no
 standard library types, and no higher-order ActorScript types).

 As explained in the `README.md` file,
 this actor also gives a behavioral spec of the
 exchange's semantics, by giving a prototype implementation of this
 behavior, whose functional behavior, not implementation details, are
 part of the formal PESS.

```
...
```


   PESS: Registrar-based ingress messages
   ================================================
   Add/remove support across various mostly-static tables:

   - truck types, produce (types) and region information.
   - participants: producers, retailers and transporters.

   For each of the six entities listed above, we have an add
   (`Add`) and remove (`Rem`) function below, prefixed by
   `registrar`-, and suffixed by one of the entities in `TruckType`,
   `Region`, `Produce`, `Producer`, `Retailer`, `Transporter`.

   To do: more registrar ingress messages:
   =======================================================

   - Get a list of all ids for each entity class in the registry:
   ids of all truck types, all regions, all produce, all transporters, all producers, all retailers.

   - For each id kind, provide a server message to get back the other registry info
   that the registrar stores in association with it (short_name, description, etc.).

   - not now, but eventually, may need a cursor-message sub-system for going through extremely long lists of ids.

`reigstrarTruckType`
-------------------
```
...
```
`registrarRemTruckType`
---------------------
returns `?()` on success, and `null` on failure.
```
...
```
`registrarTruckType`
---------------------
adds the truck type to the system; fails if the given information is
invalid in any way.
```
...
```
`registrarRemProduce`
---------------------
returns `?()` on success, and `null` on failure.
```
...
```
`registrarAddProduce`
---------------------
adds the produce to the system; fails if the given information is invalid in any way.
```
...
```
`registrarRemProduce`
---------------------
returns `?()` on success, and `null` on failure.
```
...
```
`registrarAddProducer`
---------------------
adds the producer to the system; fails if the given region is non-existent.
```
...
```
`registrarRemProducer`
---------------------
returns `?()` on success, and `null` on failure.
```
...
```
`registrarAddRetailer`
---------------------
adds the producer to the system; fails if the given region is non-existent.
```
...
```
`registrarRemRetailer`
---------------------
returns `?()` on success, and `null` on failure.
```
...
```
`registrarAddTransporter`
---------------------
```
...
```
`registrarRemTransporter`
---------------------
```
...
```
PESS: Producer-based ingress messages:
==========================================
`producerAddInventory`
---------------------------
```
...
```
`producerRemInventory`
---------------------------
```
...
```
`producerReservations`
---------------------------
```
...
```
PESS: Transporter-based ingress messages:
===========================================
`transporterAddRoute`
---------------------------
```
...
```
`transporterRemRoute`
---------------------------
```
...
```
`transporterReservations`
---------------------------
```
...
```
PESS: Retailer-based ingress messages:
======================================
`retailerQueryAll`
---------------------------


   TODO-Cursors (see above).

```
...
```
`retailerReserve`
---------------------------
```
...
```
`retailerReserveCheapest`
---------------------------
Like `retailerReserve`, but chooses cheapest choice among all
feasible produce inventory items and routes, given a grade,
quant, and delivery window.
?? This may be an example of what Mack described to me as
wanting, and being important -- a "conditional update"?
```
...
```
`retailerReservations`
---------------------------


   TODO-Cursors (see above).

```
...
```
PESS: (Producer/Transporter/Retailer) ingress messages:
========================================================
`reservationInfo`
---------------------------
