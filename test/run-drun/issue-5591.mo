//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY

let opt : ?Blob = from_candid (to_candid ("blob": Blob));
assert opt == ?"blob";
