use std::iter::IntoIterator;
use std::ops::{Index, IndexMut};
use std::slice::Iter as SliceIter;

pub struct SegmentedVec<T> {
    chunks: Vec<Vec<T>>,
    chunk_size: usize,
}

impl<T> SegmentedVec<T> {
    /// Constructs a new, empty SegmentedVec<T> with a default chunk size of 256.
    ///
    /// The segmented vector will not allocate until elements are pushed onto it.
    pub fn new() -> Self {
        SegmentedVec::with_chunk_size(256)
    }

    /// Constructs a new, empty SegmentedVec<T> with the provided chunk size.
    ///
    /// The segmented vector will not allocate until elements are pushed onto it.
    pub fn with_chunk_size(chunk_size: usize) -> Self {
        SegmentedVec {
            chunks: Vec::new(),
            chunk_size,
        }
    }

    /// Appends an element to the back of a collection.
    pub fn push(&mut self, val: T) {
        let mut new_chunk = true;
        if let Some(chunk) = self.chunks.last() {
            new_chunk = chunk.len() >= self.chunk_size;
        }

        if new_chunk {
            self.chunks.push(Vec::with_capacity(self.chunk_size));
        }

        self.chunks.last_mut().unwrap().push(val)
    }

    /// Removes the last element from a vector and returns it, or `None` if it is empty.
    pub fn pop(&mut self) -> Option<T> {
        loop {
            match self.chunks.last_mut() {
                Some(chunk) => {
                    let popped = chunk.pop();
                    if popped.is_some() {
                        return popped;
                    }
                }
                None => {
                    return None;
                }
            }
            self.chunks.pop();
        }
    }

    /// Clears the vector, removing all values.
    ///
    /// This method deallocates the chunks of the segmented vector.
    pub fn clear(&mut self) {
        self.chunks.clear();
    }

    /// Returns the number of elements in the segmented vector, also referred to as its 'length'.
    pub fn len(&self) -> usize {
        match self.chunks.last() {
            Some(chunk) => (self.chunks.len() - 1) * self.chunk_size + chunk.len(),
            None => 0,
        }
    }

    /// Returns a reference to an element at the provided index if it exists.
    pub fn get(&self, idx: usize) -> Option<&T> {
        let c = idx / self.chunk_size;
        let sub_idx = idx % self.chunk_size;
        if let Some(chunk) = self.chunks.get(c) {
            return chunk.get(sub_idx);
        }

        None
    }

    /// Returns a mutable reference to an element at the provided index if it exists.
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        let c = idx / self.chunk_size;
        let sub_idx = idx % self.chunk_size;
        if let Some(chunk) = self.chunks.get_mut(c) {
            return chunk.get_mut(sub_idx);
        }

        None
    }

    /// Return size of the `nth` allocated chunk in the segmented vector.
    pub fn chunk(&self, nth: usize) -> Option<&[T]> {
        self.chunks.get(nth).map(|chunk| &chunk[..])
    }

    /// Returns an iterator over the segmented vector.
    pub fn iter<'l>(&'l self) -> Iter<'l, T> {
        Iter {
            chunks: self.chunks.iter(),
            current: [].iter(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn pop_back(&mut self) -> Option<T> {
        self.pop()
    }

    pub fn push_back(&mut self, val: T) {
        self.push(val);
    }
}

impl<T> Index<usize> for SegmentedVec<T> {
    type Output = T;
    fn index(&self, idx: usize) -> &T {
        self.get(idx).unwrap()
    }
}

impl<T> IndexMut<usize> for SegmentedVec<T> {
    fn index_mut(&mut self, idx: usize) -> &mut T {
        self.get_mut(idx).unwrap()
    }
}

/// An iterator over a `SegmentedVector<T>`
pub struct Iter<'l, T> {
    chunks: SliceIter<'l, Vec<T>>,
    current: SliceIter<'l, T>,
}

impl<'l, T> Iterator for Iter<'l, T> {
    type Item = &'l T;
    fn next(&mut self) -> Option<&'l T> {
        if let Some(v) = self.current.next() {
            return Some(v);
        }

        if let Some(chunk) = self.chunks.next() {
            self.current = chunk.iter();
        } else {
            return None;
        }

        self.next()
    }
}

impl<'l, T> IntoIterator for &'l SegmentedVec<T> {
    type Item = &'l T;
    type IntoIter = Iter<'l, T>;
    fn into_iter(self) -> Iter<'l, T> {
        self.iter()
    }
}

#[test]
fn test_basic() {
    let mut v = SegmentedVec::with_chunk_size(8);
    let n = 100usize;
    for i in 0..n {
        v.push(i);
    }
    assert_eq!(v.len(), 100);

    for i in 0..n {
        assert_eq!(*v.get(i).unwrap(), i);
    }

    let mut i = 0;
    for val in &v {
        assert_eq!(*val, i);
        i += 1;
    }
    assert_eq!(i, n);

    assert!(v.get(n).is_none());

    for i in 0..(n + 10) {
        if i < n {
            assert_eq!(v.pop(), Some(n - 1 - i));
            assert_eq!(v.len(), n - i - 1);
        } else {
            assert_eq!(v.pop(), None);
            assert_eq!(v.len(), 0);
        }
    }

    assert_eq!(v.len(), 0);
}

#[macro_export]
macro_rules! bitfield {
    // Generate new bitfield with getters and setters
    ($(#[$attributes:meta])* $visibility:vis struct $name:ident($type:ty); $($fields:tt)*) => {
        $(#[$attributes])*
        $visibility struct $name(pub $type);

        $crate::bitfield! {@impl_range struct $name($type)}
        impl $name {
            $crate::bitfield! {@fields @getter $($fields)*}
            $crate::bitfield! {@fields @setter $($fields)*}
        }
    };

    // Impl: Implement BitRange<T> and BitRangeMut<T> for struct(pub T)
    (@impl_range struct $name:ident($type:ty)) => {
        impl<T> const $crate::memory::support::BitRange<T> for $name
        where
            $type: ~const $crate::memory::support::BitRange<T>
        {
            #[inline]
            fn bits(&self, msb: usize, lsb: usize) -> T {
                self.0.bits(msb, lsb)
            }
        }

        impl<T> const $crate::memory::support::BitRangeMut<T> for $name
        where
            $type: ~const $crate::memory::support::BitRange<T> + ~const $crate::memory::support::BitRangeMut<T>
        {
            #[inline]
            fn set_bits(&mut self, msb: usize, lsb: usize, value: T) -> &mut Self {
                self.0.set_bits(msb, lsb, value);
                self
            }
        }
    };

    // Parse Fields: Process regular fields without from/into conversion
    (@fields @$variant:tt $(#[$attributes:meta])* $visibility:vis $type:ty, $getter:tt, $setter:tt: $($exprs:expr),*; $($rest:tt)*) => {
        $crate::bitfield! {@fields @$variant $(#[$attributes])* $visibility $type, _, _, $getter, $setter: $($exprs),*; $($rest)*}
    };

    // Parse Fields: Process fields with from conversion
    (@fields @$variant:tt $(#[$attributes:meta])* $visibility:vis $type:ty, from $from:ty, $getter:tt, $setter:tt: $($exprs:expr),*; $($rest:tt)*) => {
        $crate::bitfield! {@fields @$variant $(#[$attributes])* $visibility $type, $from, $type, $getter, $setter: $($exprs),*; $($rest)*}
    };

    // Parse Fields: Process fields with into conversion
    (@fields @$variant:tt $(#[$attributes:meta])* $visibility:vis $type:ty, into $into:ty, $getter:tt, $setter:tt: $($exprs:expr),*; $($rest:tt)*) => {
        $crate::bitfield! {@fields @$variant $(#[$attributes])* $visibility $type, $type, $into, $getter, $setter: $($exprs),*; $($rest)*}
    };

    // Parse Fields: Process fields with from and into conversion for same type
    (@fields @$variant:tt $(#[$attributes:meta])* $visibility:vis $type:ty, from into $from_into:ty, $getter:tt, $setter:tt: $($exprs:expr),*; $($rest:tt)*) => {
        $crate::bitfield! {@fields @$variant $(#[$attributes])* $visibility $type, $from_into, $from_into, $getter, $setter: $($exprs),*; $($rest)*}
    };

    // Parse Fields: Process fields with from and into conversion for different types
    (@fields @$variant:tt $(#[$attributes:meta])* $visibility:vis $type:ty, from $from:ty, into $into:ty, $getter:tt, $setter:tt: $($exprs:expr),*; $($rest:tt)*) => {
        $crate::bitfield! {@fields @$variant $(#[$attributes])* $visibility $type, $from, $into, $getter, $setter: $($exprs),*; $($rest)*}
    };

    // Fields: Process each field one-by-one by splitting list head off
    (@fields @$variant:tt $(#[$attributes:meta])* $visibility:vis $type:ty, $from:tt, $into:tt, $getter:tt, $setter:tt: $($exprs:expr),*; $($rest:tt)*) => {
        $crate::bitfield! {@field @$variant $(#[$attributes])* $visibility $type, $from, $into, $getter, $setter: $($exprs),*}
        $crate::bitfield! {@fields @$variant $($rest)*}
    };

    // Fields: Stop case once all fields are processed
    (@fields @$variant:tt) => {};

    // Field: Propagate field with getter and setter to individual macros
    (@field @$variant:tt $(#[$attributes:meta])* $visibility:vis $type:ty, $from:tt, $into:tt, $getter:ident, $setter:ident: $($exprs:expr),*) => {
        $crate::bitfield! {@field @$variant $(#[$attributes])* $visibility $type, $from, $into, $getter, _: $($exprs),*}
        $crate::bitfield! {@field @$variant $(#[$attributes])* $visibility $type, $from, $into, _, $setter: $($exprs),*}
    };

    // Field Getter: Bit Range (without conversion)
    (@field @getter $(#[$attributes:meta])* $visibility:vis $type:ty, _, _, $getter:ident, _: $msb:expr, $lsb:expr) => {
        $(#[$attributes])*
        $visibility const fn $getter(&self) -> $type {
            use $crate::memory::support::BitRange;
            self.bits($msb, $lsb)
        }
    };

    // Field Getter: Bit Range (with conversion)
    (@field @getter $(#[$attributes:meta])* $visibility:vis $type:ty, $from:ty, $into:ty, $getter:ident, _: $msb:expr, $lsb:expr) => {
        $(#[$attributes])*
        $visibility const fn $getter(&self) -> $into
            where $into: ~const ::core::convert::From<$type>
        {
            use $crate::BitRange;
            let raw_value: $type = self.bits($msb, $lsb);
            let value: $into = <$into>::from(raw_value);
            value
        }
    };

    // Field Getter: Single Bit (without conversion)
    (@field @getter $(#[$attributes:meta])* $visibility:vis $type:ty, _, _, $getter:ident, _: $bit:expr) => {
        $(#[$attributes])*
        $visibility const fn $getter(&self) -> bool {
            use $crate::memory::support::Bit;
            self.bit($bit)
        }
    };

    // Field Getter: Single Bit (with conversion)
    (@field @getter $(#[$attributes:meta])* $visibility:vis $type:ty, $from:ty, $into:ty, $getter:ident, _: $bit:expr) => {
        $(#[$attributes])*
        $visibility const fn $getter(&self) -> $into
            where $into: ~const ::core::convert::From<$type>
        {
            use $crate::memory::support::Bit;
            let raw_value: $type = self.bit($bit);
            let value: $into = <$into>::from(raw_value);
            value
        }
    };

    // Field Getter: Disabled
    (@field @getter $(#[$attributes:meta])* $visibility:vis $type:ty, $from:tt, $into:tt, _, $setter:ident: $($exprs:expr),*) => {};

    // Field Setter: Bit Range (without conversion)
    (@field @setter $(#[$attributes:meta])* $visibility:vis $type:ty, _, _, _, $setter:ident: $msb:expr, $lsb:expr) => {
        $(#[$attributes])*
        $visibility const fn $setter(&mut self, value: $type) -> &mut Self {
            use $crate::memory::support::BitRangeMut;
            self.set_bits($msb, $lsb, value)
        }
    };

    // Field Setter: Bit Range (with conversion)
    (@field @setter $(#[$attributes:meta])* $visibility:vis $type:ty, $from:ty, $into:ty, _, $setter:ident: $msb:expr, $lsb:expr) => {
        $(#[$attributes])*
        $visibility const fn $setter(&mut self, value: $from) -> &mut Self
            where $type: ~const ::core::convert::From<$from>
        {
            use $crate::memory::support::BitRangeMut;
            let raw_value: $type = <$type>::from(value);
            self.set_bits($msb, $lsb, raw_value)
        }
    };

    // Field Setter: Single Bit (without conversion)
    (@field @setter $(#[$attributes:meta])* $visibility:vis $type:ty, _, _, _, $setter:ident: $bit:expr) => {
        $(#[$attributes])*
        $visibility const fn $setter(&mut self, value: $type) -> &mut Self {
            use $crate::memory::support::BitMut;
            self.set_bit($bit, value)
        }
    };

    // Field Setter: Single Bit (with conversion)
    (@field @setter $(#[$attributes:meta])* $visibility:vis $type:ty, $from:ty, $into:ty, _, $setter:ident: $bit:expr) => {
        $(#[$attributes])*
        $visibility const fn $setter(&mut self, value: $from) -> &mut Self
            where $type: ~const ::core::convert::From<$from>
        {
            use $crate::memory::support::BitMut;
            let raw_value: $type = <$type>::from(value);
            self.set_bit($bit, raw_value)
        }
    };

    // Field Setter: Disabled
    (@field @setter $(#[$attributes:meta])* $visibility:vis $type:ty, $from:tt, $into:tt, $getter:ident, _: $($exprs:expr),*) => {};
}

/// A trait to retrieve a range of bits as type `V`.
pub trait BitRange<V> {
    /// Get a range of bits between `lsb..=msb` and return as type `V`.
    fn bits(&self, msb: usize, lsb: usize) -> V;
}

/// A trait to set a range of bits with the type `V`.
pub trait BitRangeMut<V>: BitRange<V> {
    /// Set a range of bits between `lsb..=msb` using value `V`.
    fn set_bits(&mut self, msb: usize, lsb: usize, value: V) -> &mut Self;
}

/// A trait to retrieve a single bit as a boolean.
pub trait Bit {
    /// Get a single bit and return as boolean. (`true` = set, `false` = clear)
    fn bit(&self, bit: usize) -> bool;
}

/// A trait to set a single bit as a boolean.
pub trait BitMut: Bit {
    /// Set a single bit using a boolean. (`true` = set, `false` = clear)
    fn set_bit(&mut self, bit: usize, value: bool) -> &mut Self;
}

impl<T: ~const BitRange<u8>> const Bit for T {
    fn bit(&self, bit: usize) -> bool {
        self.bits(bit, bit) != 0
    }
}

impl<T: ~const BitRange<u8> + ~const BitRangeMut<u8>> const BitMut for T {
    fn set_bit(&mut self, bit: usize, value: bool) -> &mut Self {
        self.set_bits(bit, bit, value as u8)
    }
}

macro_rules! impl_bitrange {
    // implement given range types for each storage type
    ($variant:tt, ($storage_type:ty, $($rest:ty),*), ($($range_type:ty),*)) => {
        impl_bitrange! {$variant, ($storage_type), ($($range_type),*)}
        impl_bitrange! {$variant, ($($rest),*), ($($range_type),*)}
    };

    // implement given range types for storage type
    ($variant:tt, ($storage_type:ty), ($($range_type:ty),*)) => {
        $(impl_bitrange! {$variant, $storage_type, $range_type})*
    };

    // implement bit range for uint-based storage type
    (uint, $storage_type:ty, $range_type:ty) => {
        impl const BitRange<$range_type> for $storage_type {
            #[inline]
            fn bits(&self, msb: usize, lsb: usize) -> $range_type {
                // treat both range bounds as inclusive
                let msb = msb + 1;

                // determine number of bits
                let storage_bits = ::core::mem::size_of::<$storage_type>() * 8;
                let range_bits = ::core::mem::size_of::<$range_type>() * 8;

                // check input range boundaries
                debug_assert!(lsb < storage_bits, "lsb is out of bounds for bit range");
                debug_assert!(msb <= storage_bits, "msb is out of bounds for bit range");
                debug_assert!(lsb <= msb, "lsb must not be greater than msb for bit range");
                debug_assert!((msb - lsb) <= range_bits, "value truncated in bit range operation");

                // shift away unnecessary high and low bits
                (*self << (storage_bits - msb) >> (storage_bits - msb) >> lsb) as $range_type
            }
        }

        impl const BitRangeMut<$range_type> for $storage_type {
            #[inline]
            fn set_bits(&mut self, msb: usize, lsb: usize, value: $range_type) -> &mut Self {
                // treat both range bounds as inclusive
                let msb = msb + 1;

                // determine number of bits
                let storage_bits = ::core::mem::size_of::<$storage_type>() * 8;

                // check range boundaries
                debug_assert!(lsb < storage_bits, "lsb is out of bounds for bit range");
                debug_assert!(msb <= storage_bits, "msb is out of bounds for bit range");
                debug_assert!(lsb < msb, "lsb must not be greater than msb for bit range");

                // ensure value does not get truncated
                let new_value = value as $storage_type;
                let dropped_bits = storage_bits - (msb - lsb);
                debug_assert!(
                    (new_value << dropped_bits >> dropped_bits) as $range_type == value,
                    "value truncated in bit range operation"
                );

                // calculate mask for clearing bits
                let mask = !((!0 as $storage_type) << (storage_bits - msb) >> (storage_bits - msb) >> lsb << lsb);

                // clear bits and OR with new value
                *self = (*self & mask) | (new_value << lsb);
                self
            }
        }
    };
}

impl_bitrange! {uint, (u8, u16, u32, u64, u128), (u8, u16, u32, u64, u128)}
impl_bitrange! {uint, (u8, u16, u32, u64, u128), (i8, i16, i32, i64, i128)}

use std::fmt;
pub struct FormattedSize {
    pub size: usize,
}

impl fmt::Display for FormattedSize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ksize = (self.size as f64) / 1024f64;

        if ksize < 1f64 {
            return write!(f, "{}B", self.size);
        }

        let msize = ksize / 1024f64;

        if msize < 1f64 {
            return write!(f, "{:.1}K", ksize);
        }

        let gsize = msize / 1024f64;

        if gsize < 1f64 {
            write!(f, "{:.1}M", msize)
        } else {
            write!(f, "{:.1}G", gsize)
        }
    }
}

pub fn formatted_size(size: usize) -> FormattedSize {
    FormattedSize { size }
}
