use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_attribute]
pub fn ic_fn(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let fun = syn::parse_macro_input!(input as syn::ItemFn);
    let fun_sig = &fun.sig;

    // Some sanity checks
    assert!(fun_sig.asyncness.is_none(), "IC functions cannot be async");
    assert!(
        fun_sig.generics.params.is_empty(),
        "IC functions cannot have generic arguments"
    );
    assert!(
        fun_sig.abi.is_none(),
        "Functions with #[ic_fn] attribute cannot have ABI annotations"
    );
    assert!(
        fun_sig.variadic.is_none(),
        "IC functions cannot have variadic arguments"
    );

    let fn_ident = &fun_sig.ident;
    let wrapper_ret = fun_sig.output.clone();
    let wrapper_args: Vec<(syn::Ident, syn::Type)> = fun_sig
        .inputs
        .iter()
        .enumerate()
        .map(|(i, arg)| match arg {
            syn::FnArg::Receiver(_) => {
                panic!("IC functions can't have recievers (`&self`, `&mut self`, etc.)")
            }
            syn::FnArg::Typed(pat) => (
                syn::Ident::new(&format!("arg{}", i), proc_macro2::Span::call_site()),
                (*pat.ty).clone(),
            ),
        })
        .collect();

    // Parameters of the wrapper function
    let wrapper_params_syn: Vec<proc_macro2::TokenStream> = wrapper_args
        .iter()
        .map(|(ident, ty)| quote!(#ident: #ty))
        .collect();

    // Arguments passed to the original function
    let wrapper_args_syn: Vec<&syn::Ident> = wrapper_args.iter().map(|(ident, _)| ident).collect();

    let module_ident = syn::Ident::new(&format!("export_{}", fn_ident), fn_ident.span());

    quote!(
        #fun

        mod #module_ident {
            use super::*;

            #[cfg(feature = "ic")]
            #[no_mangle]
            unsafe extern "C" fn #fn_ident(#(#wrapper_params_syn,)*) #wrapper_ret {
                super::#fn_ident(#(#wrapper_args_syn,)*)
            }
        }

    )
    .into()
}

#[proc_macro_attribute]
pub fn ic_heap_fn(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let fun = syn::parse_macro_input!(input as syn::ItemFn);
    let fun_sig = &fun.sig;

    // Some sanity checks
    assert!(fun_sig.asyncness.is_none(), "IC functions cannot be async");
    assert_eq!(
        fun_sig.generics.params.len(),
        1,
        "IC heap functions should have one generic argument for the heap implementation"
    );
    assert!(
        fun_sig.abi.is_none(),
        "Functions with #[ic_fn] attribute cannot have ABI annotations"
    );
    assert!(
        fun_sig.variadic.is_none(),
        "IC functions cannot have variadic arguments"
    );

    let fn_ident = &fun_sig.ident;
    let wrapper_ret = fun_sig.output.clone();
    let wrapper_args: Vec<(syn::Ident, syn::Type)> = fun_sig
        .inputs
        .iter()
        .enumerate()
        .filter_map(|(i, arg)| match arg {
            syn::FnArg::Receiver(_) => {
                panic!("IC functions can't have recievers (`&self`, `&mut self`, etc.)")
            }
            syn::FnArg::Typed(pat) => {
                if i == 0 {
                    // First argument should be `heap`, skip
                    None
                } else {
                    Some((
                        syn::Ident::new(&format!("arg{}", i), proc_macro2::Span::call_site()),
                        (*pat.ty).clone(),
                    ))
                }
            }
        })
        .collect();

    // Parameters of the wrapper function
    let wrapper_params_syn: Vec<proc_macro2::TokenStream> = wrapper_args
        .iter()
        .map(|(ident, ty)| quote!(#ident: #ty))
        .collect();

    // Arguments passed to the original function
    let wrapper_args_syn: Vec<&syn::Ident> = wrapper_args.iter().map(|(ident, _)| ident).collect();

    let module_ident = syn::Ident::new(&format!("export_{}", fn_ident), fn_ident.span());

    quote!(
        #fun

        mod #module_ident {
            use super::*;

            #[cfg(feature = "ic")]
            #[no_mangle]
            unsafe extern "C" fn #fn_ident(#(#wrapper_params_syn,)*) #wrapper_ret {
                super::#fn_ident(&mut crate::heap::ic::IcHeap, #(#wrapper_args_syn,)*)
            }
        }
    )
    .into()
}
