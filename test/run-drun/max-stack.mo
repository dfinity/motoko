//MOC-FLAG --compacting-gc --rts-stack-pages 32
import { errorMessage; performanceCounter; rts_heap_size; rts_max_stack_size; debugPrint; } = "mo:⛔";

actor stack {

    var log : Text = "";

    func trace(t : Text) {
        log #= t;
        log #= "\n";
        debugPrint t
    };

    func counters() : (Int, Nat64, Nat) =
      (rts_heap_size(),
       performanceCounter(0),
       rts_max_stack_size());

    public func ser() : async () { await go(false) };
    public func deser() : async () { await go(true) };

    public func go(deserialize : Bool) : async () {
        log := "";
        let (m0, n0, s0) = counters();
        var i = 0;
        type List = ?((), List);
        var l : List = null;
        var done = false;
        while (not done) {
          try {
            await async {
              var c = 0;
              while (c < 1024) {
                l := ?((),l);
                i += 1;
                c += 1
              };
              let b = to_candid(l);

              let o : ?(List) =
               if deserialize
                 from_candid(b)
               else null;

              trace(debug_show {
                length = i;
                bytes = b.size();
//              heap = rts_heap_size();
                stack = rts_max_stack_size();
                stack_pages = (rts_max_stack_size()+65535)/65536
              });
            }
          } catch e {
            trace (errorMessage(e));
            done := true
          }
        };
        trace(debug_show{ alloced = i });
        let b = to_candid(l);
        trace("serialized");

        let o : ?(List) =
          if deserialize
            from_candid(b)
          else null;

        if deserialize trace("deserialized");
        let (m1, n1, s1) = counters();
        trace(debug_show {
          length = i;
          bytes = b.size();
//          heap = m1 - m0;
//          cycles = n1 - n0;
          stack = s1-s0;
          stack_pages = (s1+65535)/65536}
        );

    }


}
//SKIP run-low
//SKIP run
//SKIP run-ir
//SKIP ic-ref-run
//CALL ingress ser 0x4449444C0000
//CALL ingress deser 0x4449444C0000
