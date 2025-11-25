import Result "mo:core/Result";

module ic_sig_verifier {
  public func verifyBlsSig(signature : Blob, message : Blob, public_key : Blob) : Result.Result<(), Text> =
      ((prim "component:ic-sig-verifier:verify-bls-sig") : (Blob, Blob, Blob)  -> Result.Result<(), Text>) (signature, message, public_key);
  public func verifyCanisterSig(signature_cbor : Blob, message: Blob, public_key_der : Blob, ic_root_public_key_raw : Blob) : Result.Result<(), Text> =
      ((prim "component:ic-sig-verifier:verify-canister-sig") : (Blob, Blob, Blob, Blob) -> Result.Result<(), Text>)(signature_cbor, message, public_key_der, ic_root_public_key_raw);
  public func verifyCanisterSigMainnet(args_serialized : Blob) : Result.Result<(), Text> =
      ((prim "component:ic-sig-verifier:verify-canister-sig-mainnet") : Blob -> Result.Result<(), Text>) args_serialized;
};
