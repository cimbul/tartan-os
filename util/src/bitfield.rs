use core::convert::From;
use core::default::Default;
use core::ops;

// Must be re-exported so that crates that use these macros will be able to resolve it
pub use paste::paste;

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
                bitfield! {
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
        $crate::bitfield::paste! {
            $( #[$meta] )*
            $vis fn $field(&self) -> bool {
                $crate::bitfield::get_bit(self.0, $bit)
            }

            $( #[$meta] )*
            $vis fn [< set_ $field >](&mut self, value: bool) {
                self.0 = $crate::bitfield::set_bit(self.0, $bit, value);
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
            $( #[$meta:meta] )*
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
        $crate::bitfield::paste! {
            $( #[$meta] )*
            $vis fn $field(&self) -> $interface_type {
                use $crate::bitfield::TruncateInto;
                let underlying: $underlying_type =
                    $crate::bitfield::get_bits(self.0, $lsb, $msb).truncate_into();
                underlying.into()
            }

            $( #[$meta] )*
            $vis fn [< set_ $field >](&mut self, value: $interface_type) {
                let underlying: $underlying_type = value.into();
                self.0 =
                    $crate::bitfield::set_bits(self.0, $lsb, $msb, underlying.into());
            }
        }
    };
}


#[inline(always)]
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


#[inline(always)]
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


#[inline]
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


#[inline]
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


pub trait TruncateInto<T> {
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


pub trait OverflowingShl
where
    Self: Sized + Default,
{
    fn overflowing_shl(self, n: u32) -> (Self, bool);

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


pub trait OverflowingShr
where
    Self: Sized + Default,
{
    fn overflowing_shr(self, n: u32) -> (Self, bool);

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
