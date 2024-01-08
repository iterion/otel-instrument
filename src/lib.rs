use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    Attribute, ItemFn, Signature, Visibility,
};
mod attr;
mod expand;

#[proc_macro_attribute]
pub fn instrument(
    args: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = syn::parse_macro_input!(args as attr::InstrumentArgs);
    // Cloning a `TokenStream` is cheap since it's reference counted internally.
    instrument_precise(args.clone(), item.clone())
        .unwrap_or_else(|_err| instrument_speculative(args, item))
}

/// Instrument the function, without parsing the function body (instead using the raw tokens).
fn instrument_speculative(
    args: attr::InstrumentArgs,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as MaybeItemFn);
    let instrumented_function_name = input.sig.ident.to_string();
    expand::gen_function(
        input.as_ref(),
        args,
        instrumented_function_name.as_str(),
        None,
    )
    .into()
}

/// Instrument the function, by fully parsing the function body,
/// which allows us to rewrite some statements related to async-like patterns.
fn instrument_precise(
    args: attr::InstrumentArgs,
    item: proc_macro::TokenStream,
) -> Result<proc_macro::TokenStream, syn::Error> {
    let input = syn::parse::<ItemFn>(item)?;
    let instrumented_function_name = input.sig.ident.to_string();

    if input.sig.constness.is_some() {
        return Ok(quote! {
            compile_error!("the `#[instrument]` attribute may not be used with `const fn`s")
        }
        .into());
    }

    // check for async_trait-like patterns in the block, and instrument
    // the future instead of the wrapper
    if let Some(async_like) = expand::AsyncInfo::from_fn(&input) {
        return async_like.gen_async(args, instrumented_function_name.as_str());
    }

    let input = MaybeItemFn::from(input);

    Ok(expand::gen_function(
        input.as_ref(),
        args,
        instrumented_function_name.as_str(),
        None,
    )
    .into())
}

/// This is a more flexible/imprecise `ItemFn` type,
/// which's block is just a `TokenStream` (it may contain invalid code).
#[derive(Debug, Clone)]
struct MaybeItemFn {
    outer_attrs: Vec<Attribute>,
    inner_attrs: Vec<Attribute>,
    vis: Visibility,
    sig: Signature,
    block: TokenStream,
}

impl MaybeItemFn {
    fn as_ref(&self) -> MaybeItemFnRef<'_, TokenStream> {
        MaybeItemFnRef {
            outer_attrs: &self.outer_attrs,
            inner_attrs: &self.inner_attrs,
            vis: &self.vis,
            sig: &self.sig,
            block: &self.block,
        }
    }
}

/// This parses a `TokenStream` into a `MaybeItemFn`
/// (just like `ItemFn`, but skips parsing the body).
impl Parse for MaybeItemFn {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let outer_attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let sig: Signature = input.parse()?;
        let inner_attrs = input.call(Attribute::parse_inner)?;
        let block: TokenStream = input.parse()?;
        Ok(Self {
            outer_attrs,
            inner_attrs,
            vis,
            sig,
            block,
        })
    }
}

impl From<ItemFn> for MaybeItemFn {
    fn from(
        ItemFn {
            attrs,
            vis,
            sig,
            block,
        }: ItemFn,
    ) -> Self {
        let (outer_attrs, inner_attrs) = attrs
            .into_iter()
            .partition(|attr| attr.style == syn::AttrStyle::Outer);
        Self {
            outer_attrs,
            inner_attrs,
            vis,
            sig,
            block: block.to_token_stream(),
        }
    }
}

/// A generic reference type for `MaybeItemFn`,
/// that takes a generic block type `B` that implements `ToTokens` (eg. `TokenStream`, `Block`).
#[derive(Debug, Clone)]
struct MaybeItemFnRef<'a, B: ToTokens> {
    outer_attrs: &'a Vec<Attribute>,
    inner_attrs: &'a Vec<Attribute>,
    vis: &'a Visibility,
    sig: &'a Signature,
    block: &'a B,
}
