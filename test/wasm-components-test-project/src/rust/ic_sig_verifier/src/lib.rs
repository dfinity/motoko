wit_bindgen::generate!({
    path: "ic_sig_verifier.wit",
    world: "ic-sig-verifier",
});

use candid::{CandidType, Decode, Deserialize};
use ic_signature_verification::verify_canister_sig;
use ic_verify_bls_signature::verify_bls_signature;

struct IcSigVerifier;
export!(IcSigVerifier);

use crate::exports::api::Guest;

#[derive(CandidType, Deserialize)]
struct CanisterSigVerifierArgs {
    message: Vec<u8>,
    signature_cbor: Vec<u8>,
    public_key_der: Vec<u8>,
}
impl Guest for IcSigVerifier {
    // The signature must be exactly 48 bytes (compressed G1 element)
    // The key must be exactly 96 bytes (compressed G2 element)
    fn verify_bls_sig(signature: Vec<u8>, message: Vec<u8>, public_key: Vec<u8>) -> Result<(), String> {
        verify_bls_signature(&signature, &message, &public_key).map_err(|_| "BLS signature verification failed".to_string())
    }

    fn verify_canister_sig(
        signature_cbor: Vec<u8>,
        message: Vec<u8>,
        public_key_der: Vec<u8>,
        ic_root_public_key_raw: Vec<u8>,
    ) -> Result<(), String> {
        verify_canister_sig(
            &message,
            &signature_cbor,
            &public_key_der,
            &ic_root_public_key_raw,
        )
    }

    fn verify_canister_sig_mainnet(args_serialized: Vec<u8>) -> Result<(), String> {
        let args = match Decode!(&args_serialized, CanisterSigVerifierArgs) {
            Ok(args) => args,
            Err(_) => {
                return Err("failed parsing arguments of verify_canister_sig".to_string());
            }
        };
        verify_canister_sig(
            &args.message,
            &args.signature_cbor,
            &args.public_key_der,
            &ic_canister_sig_creation::IC_ROOT_PUBLIC_KEY,
        )
    }
}
