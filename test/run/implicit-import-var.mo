//MOC-FLAG --package core ../core-stub/src
//MOC-FLAG --package base ../base-stub/src
//MOC-FLAG --implicit-package core
// Make sure that the `base` package does not interfere with the implicit resolution from `core`

ignore Map.empty<Text, Nat>(); // auto-import Map

ignore Nat; //auto-=import Nat




