use crate::hir::Hir;
use crate::Result;

// TODO: niche
#[derive(Debug)]
pub struct Typ(pub u32);

#[derive(Debug)]
pub enum TypV {
    Error,
    Empty,
    F64,
    Fun { args: Vec<Typ>, ret: Typ },
}

pub fn annotate_types(hir: &mut Hir) -> Result<()> {
    Ok(())
}
