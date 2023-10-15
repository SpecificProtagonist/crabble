type Set<K> = indexmap::IndexSet<K, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

#[derive(Default)]
pub struct Strings(Set<String>);

pub struct Name(usize);

impl Strings {
    pub fn intern() {}
}
