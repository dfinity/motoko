fn main() {
    println!("cargo:rustc-link-search=native=../_build");
    println!("cargo:rustc-link-lib=static=tommath");
}
