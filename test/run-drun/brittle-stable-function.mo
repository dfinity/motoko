//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
//SKIP ic-ref-run
import Clamper "Clamper";

persistent actor {
  let clamper : Clamper.Clamper = Clamper.Clamper(0, 5);

  public query func clamp10() : async Nat {
    return clamper.clamp(10);
  };

}

//CALL query clamp10 0x4449444c0000
