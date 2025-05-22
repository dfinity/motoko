use pocket_ic::PocketIc;

fn main() {
    println!("Hello from test-runner!");
    // We can add some basic pocket-ic usage here to verify it works
    let server = PocketIc::new();
    let canister = server.create_canister();
    println!("Created canister: {}", canister);
}
