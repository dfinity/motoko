/* adapted from ../run/actor.as */
let f = async {
   let o = actor this {
                  private a = "fe";
                  private b = "fb";
		  private get_a = await (async (func get_a() : async Text {a;}));
		  private get_b = await (async (func get_b() : async Text {b;}));
		  get_ab(): async (Text,Text) {
		    let _ = (await this.get_a(), await this.get_b());  
   		    (await get_a(), await get_b())
		  };
		  };
   let (a,b) = await(o.get_ab());
   print a;
   print b;
   print "\n";
};
