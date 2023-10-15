mod err;
mod hir;
mod jit;

pub use ast::Span;
pub use err::{Err, Result};
use jit::Jit;

type Set<K> = indexmap::IndexSet<K, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;
type Map<K, V> = indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

fn default<T: Default>() -> T {
    T::default()
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let ast = match ast::parse(&src).into_result() {
        Ok(ast) => ast,
        Err(parse_errs) => {
            parse_errs
                .into_iter()
                .for_each(|e| println!("Parse error: {}", e));
            std::process::exit(1)
        }
    };
    // println!("{ast:?}");

    let hir = match hir::build(&src, &ast) {
        Ok(hir) => hir,
        Err(err) => {
            eprintln!("{err:?}");
            std::process::exit(2)
        }
    };

    let main = hir.functions.last().unwrap();
    let main_ptr = Jit::default().compile(main);
    let main_ptr = unsafe { std::mem::transmute::<_, fn() -> f64>(main_ptr) };
    println!("Return: {}", main_ptr());
}
