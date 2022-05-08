#![allow(dead_code)]
use super::SpaceUsage;

pub struct NewPage {
    next: *mut Self,
    top: usize,
    end: usize,
    survivor_end: usize,
    resolved_top: usize,
}

pub struct SemiSpace {
    capacity_in_words: usize,
    max_capacity_in_words: usize,
    head: *mut NewPage,
    tail: *mut NewPage,
}

pub struct ScavengeStats {
    start_micros: i64,
    end_micros: i64,
    before: SpaceUsage,
    after: SpaceUsage,
    promo_candidates_in_words: usize,
    promoted_in_words: usize,
    abandoned_in_words: usize,
}

pub struct Scavenger {
    pub scavenging: bool,
    pub early_tenure: bool,
    pub failed_to_promote: bool,
    pub abort: bool,
    pub to: *mut SemiSpace,
}
