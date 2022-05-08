pub mod scavenger;

pub struct SpaceUsage {
    pub capacity_in_words: usize,
    pub used_in_words: usize,
    pub external_in_words: usize,
}
