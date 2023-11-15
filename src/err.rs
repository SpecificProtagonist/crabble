use crate::Span;

// Aborting on first error is good enough for now (maybe fix later)

pub type Result<T> = std::result::Result<T, Err>;

#[derive(Debug)]
pub enum Err {
    NotFound { ident: Span },
    Immutable { ident: Span, decl: Span },
    ConstRedefinition { ident: Span, existing: Span },
}

// TODO: Ariadne
