/// IC principals (user and canister smart contract IDs)

import Prim "mo:⛔";
import Blob "Blob";
import Hash "Hash";
module {

  /// Internet Computer principal identifiers.
  /// Convert to `Blob` for access to bytes.
  public type Principal = Prim.Types.Principal;

  /// Conversion.
  public let fromActor : (a : actor {}) -> Principal = Prim.principalOfActor;

  /// Conversion.
  public let toBlob : (p : Principal) -> Blob = Prim.blobOfPrincipal;

  /// Conversion.
  public let fromBlob : (b : Blob) -> Principal = Prim.principalOfBlob;
  
  /// Conversion.
  public func toText(p : Principal) : Text = debug_show(p);

  private let anonymousPrincipal : Blob = "\04";

  public func isAnonymous(p : Principal) : Bool =
    Prim.blobOfPrincipal p == anonymousPrincipal;

  public func hash(principal : Principal) : Hash.Hash =
    Blob.hash (Prim.blobOfPrincipal(principal));

  public func fromText(t : Text) : Principal = fromActor(actor(t));

  /// Returns `x == y`.
  public func equal(x : Principal, y : Principal) : Bool { x == y };

  /// Returns `x != y`.
  public func notEqual(x : Principal, y : Principal) : Bool { x != y };

  /// Returns `x < y`.
  public func less(x : Principal, y : Principal) : Bool { x < y };

  /// Returns `x <= y`.
  public func lessOrEqual(x : Principal, y : Principal) : Bool { x <= y };

  /// Returns `x > y`.
  public func greater(x : Principal, y : Principal) : Bool { x > y };

  /// Returns `x >= y`.
  public func greaterOrEqual(x : Principal, y : Principal) : Bool { x >= y };

  /// Returns the order of `x` and `y`.
  public func compare(x : Principal, y : Principal) : { #less; #equal; #greater } {
    if (x < y) { #less }
    else if (x == y) { #equal }
    else { #greater }
  };
}
