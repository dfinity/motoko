/* test tail-position calculation; error would stack overflow in dvm*/

let bound:Int = 100000;

{
    func Loop(n:Int){
	if (n >= bound) {
            print "done 1\n";
            return;
	};
	Loop(n+1);
    };
    Loop(0);
};

{
    func Loop(n:Int){
	if (n >= bound) {
            print "done 2\n";
            return;
	};
	if (true)
            Loop(n+1)
	else
         Loop(n+1);
    };
    Loop(0);
};


{
    func Loop(n:Int){
	if (n >= bound) {
            print "done 3\n";
            return;
	};
	switch (n % 2) {
	case 0 Loop(n+1);
	case 1 Loop(n+1);
	case _ assert(false);
	};
    };

    Loop(0);
};

{
    func Loop(n:Int){
	if (n >= bound) {
            print "done 4\n";
            return;
	};
	{ let m = n;
	  Loop(m +1);
	};
    };
    Loop(0);
};


{
    func Loop(n:Int){
	if (n >= bound) {
            print "done 5\n";
            return;
	};
	let _ = (return Loop(n+1)) + 1;
    };
    Loop(0);
};


{
    func Loop(n:Int):Bool{
	if (n >= bound) {
            print "done 6\n";
            return true;
	};
	true and Loop(n+1);
    };
    assert(Loop(0));
};

{
    func Loop(n:Int):Bool {
	if (n >= bound) {
            print "done 7\n";
            return true;
	};
	false or Loop(n+1);
    };
    assert(Loop(0));
};
