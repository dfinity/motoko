//MOC-FLAG --implicit-lib-vals
//MOC-FLAG --package core ../core-stub/src

import Map "mo:core/Map";

// The mo:core/Text import should not be necessary for the implicit Text.compare to be resolved

let map = Map.empty<Text, Nat>();
map.add("abc", 3)

//SKIP run-low
//SKIP run
//SKIP run-ir