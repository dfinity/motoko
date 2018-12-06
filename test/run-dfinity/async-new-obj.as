/* test asynchronous construction of an object */
let a = async {
   let o = new {private a = "aa" ;
                private b = "ab";
	        get_a():Text {a;};
	        get_b():Text {b;};}; 
   let (a,b) = (o.get_a(),o.get_b());
   print a;
   print b;
   print "\n";
};

let b = async {
   let o = new {private a = await (async "ba") ;
                private b = "bb";
		get_a():Text {a;};
		get_b():Text {b;};}; 
   let (a,b) = (o.get_a(),o.get_b());
   print a;
   print b;
   print "\n";   
};

let c = async {
   let o = new {private a = await (async "ca") ;
                private b = await (async "cb");
	        get_a():Text {a;};
	        get_b():Text {b;};};
   let (a,b) = (o.get_a(),o.get_b());		  
   print a;
   print b;
   print "\n";
};

let d = async {
   let o = new {private a = "da";
                private b = await (async "db");
                get_a(): Text {a;};
	  	get_b(): Text {b;};};
   let (a,b) = (o.get_a(),o.get_b());		  
   print a;
   print b;
   print "\n";
};


let e = async {
   let o = new this {
                  private a = "ea";
                  private b = await (async "eb");
		  get_a() : Text {a;};
		  get_b(): Text {b;};
		  get_ab():(Text,Text) {
		    (this.get_a(), this.get_b());
		  };
		  };
   let (a,b) = o.get_ab();
   print a;
   print b;
   print "\n";
};
