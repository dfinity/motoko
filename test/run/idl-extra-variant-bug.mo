// crash with IDL error: byte tag not 0 or 1
// due to failure to skip payload of unknown variant
let b : Blob = to_candid ([(?(#b "abc"), true)]);

let o = (from_candid b) : ?[(?{#a},Bool)];

assert(o == [(null,true)]);

//SKIP run
//SKIP run-ir
//SKIP run-low
