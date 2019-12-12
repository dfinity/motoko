// actor references

let _ = actor "";  // missing scheme
let _ = actor "CI";  // missing colon
let _ = actor "https://cern.ch";  // wrong scheme
let _ = actor "ic:C0FEFED00D";  // does not validate
let _ = actor "ic:C0FEFED00D41";  // cannot infer
