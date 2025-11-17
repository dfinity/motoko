//MOC-FLAG --package core ../core-stub/src
//MOC-FLAG --package base ../base-stub/src
//MOC-FLAG --implicit-package core
// Make sure that the `base` package does not interfere with the implicit resolution from `core`

ignore Nat; //auto-import Nat

ignore Map.empty<Text, Nat>(); // suggest core/Map or core/pure/Map
