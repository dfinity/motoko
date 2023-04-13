use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, token::Extern, Abi, ItemFn, LitStr, Signature};

/// Feature macro for conveniently exporting functions to the compiler.
///
/// ```
/// #[export]
/// pub unsafe fn my_function() {
///     ...
/// }
/// ```
///
/// Expansion:
/// ```
/// #[no_mangle]
/// pub unsafe extern "C" fn my_function() { ... }
/// ```
///
/// Conditional compilation with ic-mode:
///
/// ```
/// #[export(ic_only)]
/// fn my_function() { ... }
/// ```
///
/// Expansion:
///
/// ```
/// #[cfg(feature = "ic")]
/// #[no_mangle]
/// extern "C" fn my_function() { ... }
/// ```
#[proc_macro_attribute]
pub fn export(attr: TokenStream, input: TokenStream) -> TokenStream {
    let ic_only = if attr.is_empty() {
        false
    } else if attr.to_string() == "ic_only" {
        true
    } else {
        panic!("Unknown attribute: {:?}", attr.to_string());
    };

    let original_function = parse_macro_input!(input as ItemFn);
    let original_signature = original_function.sig;
    assert!(
        original_signature.abi.is_none(),
        "Functions with #[export] attribute cannot have ABI annotations"
    );
    assert!(
        original_signature.asyncness.is_none(),
        "Exported functions cannot be async"
    );
    assert!(
        original_signature.variadic.is_none(),
        "Exported functions cannot have variadic arguments"
    );

    let new_abi = Some(Abi {
        extern_token: Extern::default(),
        name: Some(LitStr::new("C", Span::call_site())),
    });
    let new_signature = Signature {
        abi: new_abi,
        ..original_signature
    };
    let new_function = ItemFn {
        sig: new_signature,
        ..original_function
    };

    let ic_attribute = if ic_only {
        quote!(#[cfg(feature = "ic")])
    } else {
        quote!()
    };

    quote!(
        #ic_attribute
        #[no_mangle]
        #new_function
    )
    .into()
}

/// Feature macro for ic-only features, not used during RTS unit testing.
/// Equivalent to using the attribute `#[cfg(feature = "ic")]`.
#[proc_macro_attribute]
pub fn ic_only(attr: TokenStream, input: TokenStream) -> TokenStream {
    assert!(attr.is_empty());
    let block = syn::parse_macro_input!(input as syn::Item);
    quote!(
        #[cfg(feature = "ic")]
        #block
    )
    .into()
}

/// Feature macro for testing-only features, not used by the compiler (IC mode).
/// Equivalent to using the attribute `#[cfg(not(feature = "ic"))]`.
#[proc_macro_attribute]
pub fn testing_only(attr: TokenStream, input: TokenStream) -> TokenStream {
    assert!(attr.is_empty());
    let block = syn::parse_macro_input!(input as syn::Item);
    quote!(
        #[cfg(not(feature = "ic"))]
        #block
    )
    .into()
}
