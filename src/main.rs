use crate::jit::Jit;

mod ast;
mod jit;

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let ast = match ast::parse(&src).into_result() {
        Ok(ast) => ast,
        Err(parse_errs) => {
            parse_errs
                .into_iter()
                .for_each(|e| println!("Parse error: {}", e));
            return;
        }
    };
    println!("{ast:?}");

    let fn_ptr = Jit::default().compile(&ast);
    let fn_ptr = unsafe { std::mem::transmute::<_, fn(f64, f64) -> f64>(fn_ptr) };
    println!("Return: {}", fn_ptr(10., 4.));
}
