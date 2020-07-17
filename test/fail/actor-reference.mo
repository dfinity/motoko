// actor references

ignore (actor "");  // missing scheme

ignore (actor "CI");  // missing colon

ignore (actor "https://cern.ch");  // wrong scheme

ignore (actor "ic:");  // empty principal

ignore (actor "ic:BFOZS-KWA73-7NADI");  // lowercase not allowed

ignore (actor "ic:bfozskwa737nadi");  // missing dashes

ignore (actor "ic:vpgq");  // too short

ignore (actor "ic:5h74t-uga73-7nadi");  // wrong checksum

ignore (actor "IC:bfozs-kwa73-7nadi" : ());  // must be actor type

ignore (actor (45));  // argument must be text type
