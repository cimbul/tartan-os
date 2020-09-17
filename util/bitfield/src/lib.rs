//! Define structures that wrap a number with accessors for certain bit ranges.
//!
//! # Example
//!
//! ```
//! # use tartan_bitfield::bitfield;
//! #
//! bitfield! {
//!     // The structure will be a wrapper for a u32 value
//!     pub struct Example(u32) {
//!         // Accessors for field `a` will refer to the first four least significant
//!         // bytes of the wrapped value, bits 0, 1, 2, and 3. Note that like normal
//!         // Rust ranges, the end of the range is *exclusive*.
//!         //
//!         // The accessors will be public, and will take/return the four bits as a `u8`.
//!         [0..4] pub a: u8,
//!
//!         // No accessors cover bits `4..6`. This is legal and can be used for reserved
//!         // bits. However, these bits will still affect equality for the struct as a
//!         // whole.
//!
//!         // Accessors for field `b` will refer to the twelve bits starting at bit 6,
//!         // but they will not be public. They will take/return the 12 bits as a `u16`.
//!         [6..18] b: u16,
//!
//!         // Note that this bit range overlaps with `b`. This is allowed.
//!         [16..20] pub c: u8,
//!
//!         // Accessors for field `d` will take/return a boolean and refer to a single
//!         // bit. Note that the `bool` is implied and not specified after the name.
//!         [25] pub d,
//!
//!         // This will cover the 6 most significant bits of the wrapped value, but
//!         // the getters will take/return a `SubFields` struct instead of `u8`. This is
//!         // useful for nested bitfields, but the `A as B` syntax works for any `B`
//!         // which implements `Into<A>` and `From<A>`.
//!         [26..32] pub e: u8 as SubFields,
//!     }
//! }
//!
//! bitfield! {
//!     // All accessors on this field use booleans and refer to single bits
//!     pub struct SubFields(u8) {
//!         [0] pub zero,
//!         [1] pub one,
//!         [2] pub two,
//!         [3] pub three,
//!         [4] pub four,
//!         [5] pub five,
//!     }
//! }
//!
//!
//! // The struct can be initialized with a u32 value
//! let x = Example(0xfa84_9e1b);
//! assert_eq!(x.a(), 0xb_u8);
//! assert_eq!(x.b(), 0x278_u16);  // Private, but still can be used within the module
//! assert_eq!(x.c(), 0x4_u8);
//! assert_eq!(x.d(), true);
//! assert_eq!(x.e(), SubFields(0x3e_u8));
//! assert_eq!(x.e().zero(), false);
//! assert_eq!(x.e().five(), true);
//!
//! // It can also be converted Into and From its underlying representation
//! let n: u32 = x.into();
//! let y: Example = n.into();
//! assert_eq!(n, 0xfa84_9e1b);
//! assert_eq!(x, y);
//!
//! // Setters are all prefixed with `set_`. They have the same visibility as the getters.
//! let mut z = Example::default();
//! z.set_a(0xb);
//! z.set_b(0x278);
//! z.set_c(0x4);
//! z.set_d(true);
//! z.set_e(SubFields(0x3e));
//! assert_eq!(z, Example(0xfa04_9e0b));
//!
//! // Reserved ranges influence equality, and they are all zero on `z`.
//! assert_ne!(z, x);
//! ```

#![no_std]
#![warn(missing_docs)]
#![warn(clippy::pedantic)]
#![allow(clippy::inline_always)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::similar_names)]

use core::convert::From;
use core::default::Default;
use core::ops;

// Must be re-exported so that crates that use these macros will be able to resolve it
#[doc(hidden)]
pub use paste::paste;


/// Define a structure that wraps a number with accessors for certain bit ranges.
///
/// See the crate documentation for an example.
///
/// The structure will implement these traits, where `T` is the underlying type defined
/// in parentheses immediately after the struct name.
///   * [`Debug`]
///   * [`Default`]
///   * [`Clone`]
///   * [`Copy`]
///   * [`PartialEq`]
///   * [`Into<T>`](Into)
///   * [`From<T>`](From)
#[macro_export]
macro_rules! bitfield {
    [
        $( #[$struct_meta:meta] )*
        $struct_vis:vis struct $struct:ident($underlying_type:ty) {
            $(
                $( #[$field_meta:meta] )*
                [ $field_lsb:literal $( .. $field_msb:literal )? ]
                $field_vis:vis $field:ident
                $( : $field_underlying_type:ty $( as $field_interface_type:ty )? )?
            ),*
            $(,)?
        }
    ] => {
        $( #[$struct_meta] )*
        #[repr(transparent)]
        #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
        $struct_vis struct $struct($underlying_type);

        impl $struct {
            $(
                $crate::bitfield! {
                    @field
                    $( #[$field_meta] )*
                    [ $field_lsb $( .. $field_msb )? ]
                    $field_vis $field
                    $( : $field_underlying_type $( as $field_interface_type )? )?
                }
            )*
        }

        impl ::core::convert::From<$underlying_type> for $struct {
            fn from(val: $underlying_type) -> Self { Self(val) }
        }

        impl ::core::convert::From<$struct> for $underlying_type {
            fn from(val: $struct) -> Self { val.0 }
        }
    };

    // Special case for single-bit boolean fields
    [
        @field
        $( #[$meta:meta] )*
        [ $bit:literal ]
        $vis:vis $field:ident
    ] => {
        $crate::paste! {
            $( #[$meta] )*
            $vis fn $field(&self) -> bool {
                $crate::get_bit(self.0, $bit)
            }

            $( #[$meta] )*
            $vis fn [< set_ $field >](&mut self, value: bool) {
                self.0 = $crate::set_bit(self.0, $bit, value);
            }
        }
    };

    // A field type and both range bounds are required in all other cases.
    // When no explicit interface type is given, use the underlying type.
    [
        @field
        $( #[$meta:meta] )*
        [ $lsb:literal .. $msb:literal ]
        $vis:vis $field:ident
        : $field_type:ty
    ] => {
        $crate::bitfield! {
            @field
            $( #[$meta] )*
            [$lsb..$msb] $vis $field: $field_type as $field_type
        }
    };

    [
        @field
        $( #[$meta:meta] )*
        [ $lsb:literal .. $msb:literal ]
        $vis:vis $field:ident
        : $underlying_type:ty as $interface_type:ty
    ] => {
        $crate::paste! {
            $( #[$meta] )*
            $vis fn $field(&self) -> $interface_type {
                use $crate::TruncateInto;
                let underlying: $underlying_type =
                    $crate::get_bits(self.0, $lsb, $msb).truncate_into();
                underlying.into()
            }

            $( #[$meta] )*
            $vis fn [< set_ $field >](&mut self, value: $interface_type) {
                let underlying: $underlying_type = value.into();
                self.0 =
                    $crate::set_bits(self.0, $lsb, $msb, underlying.into());
            }
        }
    };
}


/// Get a boolean reflecting a single bit of the value.
///
/// `bit_num` starts as zero for the least significant bit.
///
/// ```
/// # use tartan_bitfield::get_bit;
/// assert_eq!(get_bit(0b0000_0100_u8, 2), true);
/// assert_eq!(get_bit(0b0000_0100_u8, 3), false);
/// ```
#[must_use]
pub fn get_bit<T>(val: T, bit_num: u8) -> bool
where
    T: Default
        + PartialEq
        + From<bool>
        + ops::BitAnd<T, Output = T>
        + ops::Shl<u8, Output = T>,
{
    let position_mask = T::from(true) << bit_num;
    (val & position_mask) != T::default()
}


/// Create a copy of the value with a single bit modified.
///
/// `bit_num` starts as zero for the least significant bit.
///
/// ```
/// # use tartan_bitfield::set_bit;
/// assert_eq!(set_bit(0b0000_0000_u8, 5, true), 0b0010_0000);
/// assert_eq!(set_bit(0b1111_1111_u8, 0, false), 0b1111_1110);
/// ```
#[must_use]
pub fn set_bit<T>(val: T, bit_num: u8, bit_val: bool) -> T
where
    T: From<bool>
        + ops::BitAnd<Output = T>
        + ops::BitOr<Output = T>
        + ops::Shl<u8, Output = T>
        + ops::Not<Output = T>,
{
    let value_mask = T::from(bit_val) << bit_num;
    let position_mask = T::from(true) << bit_num;
    val & position_mask.not() | value_mask
}


/// Extract a range of bits from the value, shifted so the first bit of the subset is the
/// least significant bit of the result.
///
/// Bits are numbered starting with zero for the least significant bit. The range of bits
/// in the result is `lsb..msb`, **exclusive** of `msb`.
///
/// ```
/// # use tartan_bitfield::get_bits;
/// assert_eq!(get_bits(0b1100_1110_u8, 3, 7), 0b1001);
/// assert_eq!(get_bits(0b1010_0101_u8, 6, 8), 0b10);
/// ```
#[must_use]
pub fn get_bits<T>(packed_val: T, lsb: u8, msb: u8) -> T
where
    T: Default
        + OverflowingShl
        + OverflowingShr
        + ops::Not<Output = T>
        + ops::BitAnd<T, Output = T>,
{
    let field_width = msb - lsb;
    // e.g., 0b0000_0111 for U with a width 3 bytes from its MSB to LSB
    let field_width_mask = T::default().not().saturating_shl(field_width.into()).not();
    packed_val.saturating_shr(lsb.into()) & field_width_mask
}


/// Create a copy of the value with a subset of bits updated based on the passed value.
///
/// Bits are numbered starting with zero for the least significant bit. The range of
/// updated bits is `lsb..msb`, **exclusive** of `msb`. `field_val` is shifted left `lsb`
/// bytes before being combined with `packed_val`.
///
/// ```
/// # use tartan_bitfield::set_bits;
/// assert_eq!(set_bits(0b0000_0000_u8, 6, 8, 0b11), 0b1100_0000);
/// assert_eq!(set_bits(0b1111_1111_u8, 1, 5, 0b0000), 0b1110_0001);
/// assert_eq!(set_bits(0b1010_0110_u8, 2, 6, 0b1110), 0b1011_1010);
/// ```
#[must_use]
pub fn set_bits<T>(packed_val: T, lsb: u8, msb: u8, field_val: T) -> T
where
    T: Default
        + Copy
        + OverflowingShl
        + ops::Shl<u8, Output = T>
        + ops::Not<Output = T>
        + ops::BitAnd<T, Output = T>
        + ops::BitOr<T, Output = T>,
{
    // e.g., 0b1110_0000 for MSB = 5 (exclusive)
    let msb_mask = T::default().not().saturating_shl(msb.into());
    // e.g., 0b0000_0011 for LSB = 2
    let lsb_mask = T::default().not().saturating_shl(lsb.into()).not();
    // e.g., 0b1110_0011 for MSB = 5, LSB = 2
    let position_mask = msb_mask | lsb_mask;
    let value_mask = field_val.saturating_shl(lsb.into()) & position_mask.not();
    packed_val & position_mask | value_mask
}


/// A type whose values can be truncated into another type. This is more explicit than
/// `x as T`.
pub trait TruncateInto<T> {
    /// Truncate the value to fit in the destination type
    fn truncate_into(self) -> T;
}

macro_rules! truncate_into_impl {
    ($source:ty, $dest:ty) => {
        impl TruncateInto<$dest> for $source {
            #[inline(always)]
            fn truncate_into(self) -> $dest {
                self as $dest
            }
        }
    };
}

truncate_into_impl!(u128, u128);
truncate_into_impl!(u128, u64);
truncate_into_impl!(u128, u32);
truncate_into_impl!(u128, u16);
truncate_into_impl!(u128, u8);

truncate_into_impl!(u64, u64);
truncate_into_impl!(u64, u32);
truncate_into_impl!(u64, u16);
truncate_into_impl!(u64, u8);

truncate_into_impl!(u32, u32);
truncate_into_impl!(u32, u16);
truncate_into_impl!(u32, u8);

truncate_into_impl!(u16, u16);
truncate_into_impl!(u16, u8);

truncate_into_impl!(u8, u8);


/// A type with an overflowing left shift operation. Also adds a saturating version.
///
/// All basic numeric types have this operation, but there is no corresponding trait in
/// [`core::ops`].
pub trait OverflowingShl
where
    Self: Sized + Default,
{
    /// Shift the value left by `n mod m` bits, where `m` is the number of bits in the
    /// type. Return the shifted value along with a boolean indicating whether the shift
    /// count was wrapped.
    ///
    /// Since this behavior is unintuitive and practically useless, see
    /// [`saturating_shl`](Self::saturating_shl) for an alternative that behaves the way
    /// you probably expect.
    fn overflowing_shl(self, n: u32) -> (Self, bool);

    /// Shift the value left by `n` bits. If `n` is greater than or equal to the number
    /// of bits in this type, the result will be zero.
    #[inline(always)]
    fn saturating_shl(self, n: u32) -> Self {
        match self.overflowing_shl(n) {
            (_, true) => Self::default(),
            (x, _) => x,
        }
    }
}

macro_rules! overflowing_shl_impl {
    ($type:ty) => {
        impl OverflowingShl for $type {
            #[inline(always)]
            fn overflowing_shl(self, n: u32) -> (Self, bool) {
                self.overflowing_shl(n)
            }
        }
    };
}

overflowing_shl_impl!(u8);
overflowing_shl_impl!(u16);
overflowing_shl_impl!(u32);
overflowing_shl_impl!(u64);
overflowing_shl_impl!(u128);


/// A type with an overflowing right shift operation. Also adds a saturating version.
///
/// All basic numeric types have this operation, but there is no corresponding trait in
/// [`core::ops`].
pub trait OverflowingShr
where
    Self: Sized + Default,
{
    /// Shift the value right by `n mod m` bits, where `m` is the number of bits in the
    /// type. Return the shifted value along with a boolean indicating whether the shift
    /// count was wrapped.
    ///
    /// Since this behavior is unintuitive and practically useless, see
    /// [`saturating_shr`](Self::saturating_shr) for an alternative that behaves the way
    /// you probably expect.
    fn overflowing_shr(self, n: u32) -> (Self, bool);

    /// Shift the value right by `n` bits. If `n` is greater than or equal to the number
    /// of bits in this type, the result will be zero.
    #[inline(always)]
    fn saturating_shr(self, n: u32) -> Self {
        match self.overflowing_shr(n) {
            (_, true) => Self::default(),
            (x, _) => x,
        }
    }
}

macro_rules! overflowing_shr_impl {
    ($type:ty) => {
        impl OverflowingShr for $type {
            #[inline(always)]
            fn overflowing_shr(self, n: u32) -> (Self, bool) {
                self.overflowing_shr(n)
            }
        }
    };
}

overflowing_shr_impl!(u8);
overflowing_shr_impl!(u16);
overflowing_shr_impl!(u32);
overflowing_shr_impl!(u64);
overflowing_shr_impl!(u128);
