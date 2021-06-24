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
    let fn_wrapper_ident = syn::Ident::new(&format!("ic_{}", fn_ident), fn_ident.span());
    let fn_name = fn_ident.to_string();
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

    quote!(
        #fun

        #[cfg(feature = "ic")]
        #[export_name = #fn_name]
        unsafe extern "C" fn #fn_wrapper_ident(#(#wrapper_params_syn,)*) #wrapper_ret {
            #fn_ident(#(#wrapper_args_syn,)*)
        }
    )
    .into()
}

#[proc_macro_attribute]
pub fn ic_mem_fn(attr: TokenStream, input: TokenStream) -> TokenStream {
    let ic_only = if attr.is_empty() {
        false
    } else if attr.to_string() == "ic_only" {
        true
    } else {
        panic!("Unknown attribute: {:?}", attr.to_string());
    };

    let fun = syn::parse_macro_input!(input as syn::ItemFn);
    let fun_sig = &fun.sig;

    // Some sanity checks
    assert!(fun_sig.asyncness.is_none(), "IC functions cannot be async");
    assert_eq!(
        fun_sig.generics.params.len(),
        1,
        "IC memory functions should have one generic argument for the memory implementation"
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
    let fn_wrapper_ident = syn::Ident::new(&format!("ic_{}", fn_ident), fn_ident.span());
    let fn_name = fn_ident.to_string();
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
                    // First argument should be `memory`, skip
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

    let fun_attr = if ic_only {
        quote!(#[cfg(feature = "ic")])
    } else {
        quote!()
    };

    quote!(
        #fun_attr
        #fun

        #[cfg(feature = "ic")]
        #[export_name = #fn_name]
        unsafe extern "C" fn #fn_wrapper_ident(#(#wrapper_params_syn,)*) #wrapper_ret {
            #fn_ident(&mut crate::memory::ic::IcMemory, #(#wrapper_args_syn,)*)
        }
    )
    .into()
}
