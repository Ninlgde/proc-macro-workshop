use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input};
use syn::spanned::Spanned;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as syn::DeriveInput);

    match do_expend(&ast) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into()
    }
}

/// 为struct扩展builder
fn do_expend(ast: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let original_struct_name = ast.ident.to_string();
    let builder_struct_name = format!("{}Builder", original_struct_name);
    let builder_struct_ident = syn::Ident::new(&builder_struct_name, ast.span());

    let original_struct_ident = &ast.ident;

    let fields = get_fields_from_derive_input(&ast)?;
    let generated_fields = generate_builder_struct_fields_def(&fields)?;
    let generated_init_fields = generate_builder_struct_factory_init_clauses(&fields)?;

    let generated_setters = generate_setter_functions(&fields)?;
    let generated_builder_functions = generate_build_function(fields, original_struct_ident)?;

    let ret = quote! {
        pub struct #builder_struct_ident {
            #generated_fields
        }

        impl #original_struct_ident {
            pub fn builder() -> #builder_struct_ident {
                #builder_struct_ident {
                    #(#generated_init_fields),*
                }
            }

        }

        impl #builder_struct_ident {
            #generated_setters

            #generated_builder_functions
        }
    };

    syn::Result::Ok(ret)
}

/// type of struct's fields
type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;

/// 从DeriveInput中获取struct的fields
fn get_fields_from_derive_input(d: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
                                 fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
                                 ..
                             }) = d.data {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(d, "Must define on a Struct, not Enum".to_string()))
}

/// 根据struct的fields生成builder的fields
fn generate_builder_struct_fields_def(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| { &f.ident }).collect();
    let types: syn::Result<Vec<_>> = fields.iter().map(|f| {
        if let Some(inner_ty) = get_generic_inner_type(&f.ty, "Option") {
            Ok(quote!(std::option::Option<#inner_ty>))
        } else if get_user_specified_ident_for_vec(f)?.is_some() {
            let origin_ty = &f.ty;
            Ok(quote!(#origin_ty))
        } else {
            let origin_ty = &f.ty;
            Ok(quote!(std::option::Option<#origin_ty>))
        }
    }).collect();

    let types = types?;
    let token_stream = quote! {
        #(#idents: #types),*
    };
    Ok(token_stream)
}

/// 根据struct的fields生成builder的工厂方法
fn generate_builder_struct_factory_init_clauses(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let init_clauses: syn::Result<Vec<_>> = fields.iter().map(|f| {
        let ident = &f.ident;
        if get_user_specified_ident_for_vec(f)?.is_some() {
            Ok(quote! {
                #ident: std::vec::Vec::new()
            })
        } else {
            Ok(quote! {
                #ident: std::option::Option::None
            })
        }
    }).collect();

    Ok(init_clauses?)
}

/// 根据struct的fields生成builder的setter方法
fn generate_setter_functions(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| { &f.ident }).collect();
    let types: Vec<_> = fields.iter().map(|f| { &f.ty }).collect();

    // 创建一个空的TokenStream
    let mut final_tokenstream = proc_macro2::TokenStream::new();

    for (idx, (ident, type_)) in idents.iter().zip(types.iter()).enumerate() {
        let mut tokenstream_piece;
        if let Some(inner_ty) = get_generic_inner_type(type_, "Option") {
            tokenstream_piece = quote! {
                fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            };
        } else if let Some(ref user_specified_ident) = get_user_specified_ident_for_vec(&fields[idx])? {
            let inner_ty = get_generic_inner_type(type_, "Vec").ok_or(syn::Error::new(fields[idx].span(), "each field must be specified with Vec field"))?;
            tokenstream_piece = quote! {
                fn #user_specified_ident(&mut self, #user_specified_ident: #inner_ty) -> &mut Self {
                    self.#ident.push(#user_specified_ident);
                    self
                }
            };
            if user_specified_ident != ident.as_ref().unwrap() {
                tokenstream_piece.extend(quote! {
                    fn #ident(&mut self, #ident: #type_) -> &mut Self {
                        self.#ident = #ident.clone();
                        self
                    }
                })
            }
        } else {
            tokenstream_piece = quote! {
                fn #ident(&mut self, #ident: #type_) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            };
        }
        // 不断追加新的TokenStream片段到一个公共的TokenStream上
        final_tokenstream.extend(tokenstream_piece);
    }

    Ok(final_tokenstream)
}

/// 为builder生成build方法
fn generate_build_function(fields: &StructFields, origin_struct_ident: &syn::Ident) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| { &f.ident }).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut checker_code_pieces = Vec::new();
    for idx in 0..idents.len() {
        let ident = idents[idx];
        if get_generic_inner_type(&types[idx], "Option").is_none() && get_user_specified_ident_for_vec(&fields[idx])?.is_none() {
            checker_code_pieces.push(quote! {
                if self.#ident.is_none() {
                    let err = format!("{} field missing", stringify!(#ident));
                    return std::result::Result::Err(err.into())
                }
            });
        }
    }

    let mut fill_result_clauses = Vec::new();
    for idx in 0..idents.len() {
        let ident = idents[idx];
        if get_user_specified_ident_for_vec(&fields[idx])?.is_some() {
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone()
            })
        } else if get_generic_inner_type(&types[idx], "Option").is_none() {
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone().unwrap()
            })
        } else {
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone()
            });
        }
    }


    let token_stream = quote! {
        pub fn build(&mut self) -> std::result::Result<#origin_struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#checker_code_pieces)*
                               //  ^--注意，由于我们要重复的是一组if判断代码块，它们之间不需要用逗号分隔，所以这里的重复模式是`*`，而不是之前重复结构体字段时用到的`,*`
            let ret = #origin_struct_ident{
                #(#fill_result_clauses),*
            };
            std::result::Result::Ok(ret)
        }
    };
    Ok(token_stream)
}

/// 判断type是否Option<T>
/// 如果是返回Some(T)
/// 否则返回None
fn get_generic_inner_type<'a>(ty: &'a syn::Type, outer_ident_name: &'a str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = ty {
        // 这里我们取segments的最后一节来判断是不是`Option<T>`，这样如果用户写的是`std:option:Option<T>`我们也能识别出最后的`Option<T>`
        if let Some(seg) = path.segments.last() {
            if seg.ident == outer_ident_name {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                                                              ref args,
                                                              ..
                                                          }) = seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(inner_ty)) = args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
}

/// 获取修饰vec的builder(each = "xxx")的xxx
fn get_user_specified_ident_for_vec(field: &syn::Field) -> syn::Result<Option<syn::Ident>> {
    for attr in &field.attrs {
        if let Ok(syn::Meta::List(syn::MetaList {
                                      ref path,
                                      ref nested,
                                      ..
                                  })) = attr.parse_meta()
        {
            if let Some(p) = path.segments.first() {
                if p.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) = nested.first() {
                        if kv.path.is_ident("each") {
                            if let syn::Lit::Str(ref ident_str) = kv.lit {
                                return Ok(Some(syn::Ident::new(
                                    ident_str.value().as_str(),
                                    attr.span(),
                                )));
                            }
                        } else {
                            if let Ok(syn::Meta::List(ref list)) = attr.parse_meta() {
                                return Err(syn::Error::new_spanned(list, r#"expected `builder(each = "...")`"#));
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}
