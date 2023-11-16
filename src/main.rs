mod err;
mod hir;
mod jit;

pub use ast::Span;
pub use err::{Err, Result};

type Set<K> = indexmap::IndexSet<K, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;
type Map<K, V> = indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

fn default<T: Default>() -> T {
    T::default()
}

fn main() -> miette::Result<()> {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    run(&src).map_err(|error| error.with_source_code(src))
}

fn run(src: &str) -> miette::Result<()> {
    let span_to_line_pos = |mut pos| {
        let mut line_num = 1;
        for line in src.lines() {
            if line.len() < pos {
                pos -= line.len() + 1;
                line_num += 1;
            } else {
                break;
            }
        }
        (line_num, pos + 1)
    };
    let ast = match ast::parse(&src).into_result() {
        Ok(ast) => ast,
        Err(parse_errs) => {
            for e in parse_errs {
                let (line, pos) = span_to_line_pos(e.span().start);
                println!("{line}:{pos} Parse error: {}", e);
            }
            std::process::exit(1)
        }
    };
    // println!("{ast:?}");

    let hir = hir::build(src, &ast)?;

    // println!("{hir:?}");

    let main_ptr = jit::compile(&hir);
    let main_ptr = unsafe { std::mem::transmute::<_, fn() -> f64>(main_ptr) };
    println!("Return: {}", main_ptr());

    Ok(())
}
