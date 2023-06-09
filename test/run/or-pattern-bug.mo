func ok1(((#a : {#a;#b}) or (#b : {#a;#b})) : {#a;#b}) {}; // accepted
func ok2(((#a : {#a;#b}) or (#b : {#a;#b})) : {#}) {}; // accepted

//func wrong(((#a : {#a}) or (#b : {#b})) : {#a;#b}) {}; // rejected

func bad(#a or #b) {};


func bad1(#a or #b : {#a}) {}; // accepted
func bad2(#a or #b : {#a;#b}) {}; // accepted
func bad3(#a or #b : {#a;#b;#c}) {}; //accepted
func bad4(#a or #b : {#d}) {}; //accepted
