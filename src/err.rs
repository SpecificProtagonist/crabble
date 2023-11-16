use chumsky::{error::RichReason, prelude::Rich};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::Span;

// Aborting on first error is good enough for now (maybe fix later; use #[related])

pub type Result<T> = std::result::Result<T, Diag>;

// TODO: Warnings
#[derive(Error, Debug, Diagnostic)]
#[diagnostic(severity = "error")]
pub enum Diag {
    #[error("{reason}")]
    SyntaxError {
        // TODO: wait for miette to have underline-only errors
        #[label("error occured here")]
        loc: SourceSpan,
        reason: String,
    },
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

#[derive(Diagnostic, Debug, Error)]
#[error("{message}")]
#[diagnostic()]
pub struct Diags {
    message: String,
    #[source_code]
    src: String,
    #[related]
    diags: Vec<Diag>,
}

pub fn miette(span: Span) -> SourceSpan {
    SourceSpan::new(span.start.into(), (span.end - span.start).into())
}

// Diag is a fn so diags get printed even in case of an ICE, but can also be collected for lib usage
pub fn ast_errors_to_report(syntax_errs: Vec<Rich<char>>, diag: &mut dyn FnMut(Diag)) {
    fn report(loc: SourceSpan, err: RichReason<char>, diag: &mut dyn FnMut(Diag)) {
        match err {
            RichReason::ExpectedFound { expected, found } => diag(Diag::SyntaxError {
                loc,
                reason: {
                    let expected = if expected.len() == 1 {
                        format!("{}", expected[0])
                    } else {
                        format!(
                            "one of {}",
                            expected
                                .iter()
                                .map(|e| e.to_string())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    };
                    if let Some(found) = found {
                        format!("found {found:?}, expected {expected}")
                    } else {
                        format!("expected {expected}")
                    }
                },
            }),
            RichReason::Custom(reason) => diag(Diag::SyntaxError { loc, reason }),
            RichReason::Many(errs) => errs.into_iter().for_each(|err| report(loc, err, diag)),
        }
    }
    for err in syntax_errs {
        report(miette(*err.span()), err.into_reason(), diag);
    }
}
