// actor references

ignore (actor "");  // missing scheme

ignore (actor "CI");  // missing colon

ignore (actor "https://cern.ch");  // wrong scheme

ignore (actor "ic:");  // empty principal

ignore (actor "ic:c0fefed00d41");  // lowercase not allowed

ignore (actor "ic:ABCDEFGH");  // not hex

ignore (actor "ic:C0FEFED00DE");  // not even number of digits

ignore (actor "ic:C0FEFED00D42");  // does not validate

ignore (actor "IC:C0FEFED00D41" : ());  // must be actor type
