use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::Span;

// Aborting on first error is good enough for now (maybe fix later; use #[related])

pub type Result<T> = std::result::Result<T, Err>;

pub fn miette(span: Span) -> SourceSpan {
    SourceSpan::new(span.start.into(), (span.end - span.start).into())
}

#[derive(Error, Debug, Diagnostic)]
#[diagnostic(severity = "error")]
pub enum Err {
    #[error("Identifier not found")]
    NotFound {
        #[label("unknown identifier")]
        ident: SourceSpan,
    },
    #[error("Mutating immutable variable")]
    Immutable {
        #[label("mutation occurs here")]
        ident: SourceSpan,
        #[label("defined here")]
        decl: SourceSpan,
    },
    #[error("Cannot redefine constant")]
    ConstRedefinition {
        #[label("redefinition")]
        ident: SourceSpan,
        #[label("originally defined here")]
        existing: SourceSpan,
    },
}
