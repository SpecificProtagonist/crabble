use crate::Span;

// Aborting on first error is good enough for now (maybe fix later)

pub type Result<T> = std::result::Result<T, Err>;

#[derive(Debug)]
pub enum Err {
    Redefinition { ident: Span, previous: Span },
    NotFound { ident: Span },
}

// TODO: Ariadne
