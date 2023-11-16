mod err;
mod hir;
mod jit;
mod typ;

use std::sync::Arc;

pub use ast::Span;
pub use err::{Diag, Result};

type Set<K> = indexmap::IndexSet<K, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;
type Map<K, V> = indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

fn default<T: Default>() -> T {
    T::default()
}

fn main() {
    let src = Arc::new(std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap());
    let mut print_diag = |diag| {
        eprintln!(
            "{:?}",
            miette::Error::new(diag).with_source_code(src.clone())
        )
    };
    let result = run(&src, &mut print_diag);
    if let Some(result) = result {
        println!("Return: {result}");
    }
}

fn run(src: &str, diag: &mut dyn FnMut(Diag)) -> Option<f64> {
    let (ast, parse_errors) = ast::parse(&src).into_output_errors();
    err::ast_errors_to_report(parse_errors, diag);
    let ast = ast?;

    // println!("{ast:?}");

    let hir = hir::build(src, &ast, diag);

    // println!("{hir:?}");

    let entrypoint = jit::compile(&hir);
    let entrypoint = unsafe { std::mem::transmute::<_, fn() -> f64>(entrypoint) };
    Some(entrypoint())
}
