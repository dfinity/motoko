// measure cost of repeated candid subtype checks
import { Array_tabulate; performanceCounter; debugPrint; rts_heap_size } = "mo:⛔";

actor {

  func counters() : (Int, Nat64) = (rts_heap_size(), performanceCounter(0));

  public func go() : async () {
    let a0 : [Self] = Array_tabulate<Self>(1024, func _ { actor "aaaaa-aa" : Self } );
    let b = to_candid (a0);
    let (m0, n0) = counters();
    let o : ?[Self] = from_candid b;
    let (m1, n1) = counters();
    assert (?a0 == o);
    debugPrint(debug_show {heap_bytes = m1 - m0; cycles = n1 - n0});
  };

  // stolen from https://github.com/krpeacock/ic-management-canister

  public type bitcoin_address = Text;
  public type bitcoin_network = { #mainnet; #testnet };
  public type block_hash = [Nat8];
  public type canister_id = Principal;
  public type canister_settings = {
    freezing_threshold : ?Nat;
    controllers : ?[Principal];
    memory_allocation : ?Nat;
    compute_allocation : ?Nat;
  };
  public type definite_canister_settings = {
    freezing_threshold : Nat;
    controllers : [Principal];
    memory_allocation : Nat;
    compute_allocation : Nat;
  };
  public type ecdsa_curve = { #secp256k1 };
  public type get_balance_request = {
    network : bitcoin_network;
    address : bitcoin_address;
    min_confirmations : ?Nat32;
  };
  public type get_current_fee_percentiles_request = {
    network : bitcoin_network;
  };
  public type get_utxos_request = {
    network : bitcoin_network;
    filter : ?{ #page : [Nat8]; #min_confirmations : Nat32 };
    address : bitcoin_address;
  };
  public type get_utxos_response = {
    next_page : ?[Nat8];
    tip_height : Nat32;
    tip_block_hash : block_hash;
    utxos : [utxo];
  };
  public type http_header = { value : Text; name : Text };
  public type http_response = {
    status : Nat;
    body : [Nat8];
    headers : [http_header];
  };
  public type millisatoshi_per_byte = Nat64;
  public type outpoint = { txid : [Nat8]; vout : Nat32 };
  public type satoshi = Nat64;
  public type send_transaction_request = {
    transaction : [Nat8];
    network : bitcoin_network;
  };
  public type user_id = Principal;
  public type utxo = { height : Nat32; value : satoshi; outpoint : outpoint };
  public type wasm_module = [Nat8];
  public type Self = actor {
    bitcoin_get_balance : shared get_balance_request -> async satoshi;
    bitcoin_get_current_fee_percentiles : shared get_current_fee_percentiles_request -> async [
        millisatoshi_per_byte
      ];
    bitcoin_get_utxos : shared get_utxos_request -> async get_utxos_response;
    bitcoin_send_transaction : shared send_transaction_request -> async ();
    canister_status : shared { canister_id : canister_id } -> async {
        status : { #stopped; #stopping; #running };
        memory_size : Nat;
        cycles : Nat;
        settings : definite_canister_settings;
        idle_cycles_burned_per_day : Nat;
        module_hash : ?[Nat8];
      };
    create_canister : shared { settings : ?canister_settings } -> async {
        canister_id : canister_id;
      };
    delete_canister : shared { canister_id : canister_id } -> async ();
    deposit_cycles : shared { canister_id : canister_id } -> async ();
    ecdsa_public_key : shared {
        key_id : { name : Text; curve : ecdsa_curve };
        canister_id : ?canister_id;
        derivation_path : [[Nat8]];
      } -> async { public_key : [Nat8]; chain_code : [Nat8] };
    http_request : shared {
        url : Text;
        method : { #get; #head; #post };
        max_response_bytes : ?Nat64;
        body : ?[Nat8];
        transform : ?{
          function : shared query {
              context : [Nat8];
              response : http_response;
            } -> async http_response;
          context : [Nat8];
        };
        headers : [http_header];
      } -> async http_response;
    install_code : shared {
        arg : [Nat8];
        wasm_module : wasm_module;
        mode : { #reinstall; #upgrade; #install };
        canister_id : canister_id;
      } -> async ();
    provisional_create_canister_with_cycles : shared {
        settings : ?canister_settings;
        specified_id : ?canister_id;
        amount : ?Nat;
      } -> async { canister_id : canister_id };
    provisional_top_up_canister : shared {
        canister_id : canister_id;
        amount : Nat;
      } -> async ();
    raw_rand : shared () -> async [Nat8];
    sign_with_ecdsa : shared {
        key_id : { name : Text; curve : ecdsa_curve };
        derivation_path : [[Nat8]];
        message_hash : [Nat8];
      } -> async { signature : [Nat8] };
    start_canister : shared { canister_id : canister_id } -> async ();
    stop_canister : shared { canister_id : canister_id } -> async ();
    uninstall_code : shared { canister_id : canister_id } -> async ();
    update_settings : shared {
        canister_id : Principal;
        settings : canister_settings;
      } -> async ();
  }

}

//CALL ingress go 0x4449444C0000
