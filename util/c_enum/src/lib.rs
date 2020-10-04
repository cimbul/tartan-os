//! Support for C-style enums that support unknown values.
//!
//! ```
//! # use tartan_c_enum::c_enum;
//! #
//! c_enum! {
//!     pub enum Example(u16) {
//!         Foo,
//!         Bar = 2,
//!         Quux,
//!         Baz = 0xffff,
//!     }
//! }
//!
//! // Known value
//! let x = Example::Bar;
//! assert_eq!(x, Example::from(2));
//! assert_eq!(u16::from(x), 2);
//! assert_eq!(x.name(), Some("Bar"));
//!
//! // Omitted variant values assigned in sequence
//! assert_eq!(u16::from(Example::Foo), 0);
//! assert_eq!(u16::from(Example::Quux), 3);
//!
//! // Unknown value
//! let y = Example::from(0xcafe);
//! assert_eq!(u16::from(y), 0xcafe);
//! assert_eq!(y.name(), None);
//!
//! // Use in an FFI-safe struct
//! #[repr(C)]
//! #[derive(Debug, PartialEq)]
//! pub struct Quux(Example, u8, u8);
//! unsafe {
//!     assert_eq!(
//!         core::mem::transmute::<[u8; 4], Quux>([0xff, 0xff, 0x8c, 0xf2]),
//!         Quux(Example::Baz, 0x8c, 0xf2),
//!     );
//!     assert_eq!(
//!         core::mem::transmute::<[u8; 4], Quux>([0xab, 0xab, 0x05, 0x3b]),
//!         Quux(Example::from(0xabab), 0x05, 0x3b),
//!     );
//! }
//! ```
//!
//! Rust's `enum` types trigger undefined behavior when they are assigned unknown
//! discriminant values (e.g., through a pointer cast or transmutation). While this
//! enables useful complier optimizations, it also means that `enum`s are not safe for use
//! in FFI, since C treats enums as integral types that can take any value within range of
//! the underlying integer type.
//!
//! This crate offers an alternative kind of enumeration which is more similar to C.
//! Enumerations defined with the `c_enum` macro are simple wrappers for an integer type.
//! Known variants are defined as constants, and can be associated with their names
//! defined in code (e.g., for `Debug` output), but unknown values are fully supported.
//! Since they have transparent representations, they do not trigger undefined behavior
//! when transmuting from arbitrary values (as long as you use a built-in integer type)
//! and are safe to use in FFI structs and functions.

#![no_std]

/// Trait implemented by all [`c_enum`] types.
///
/// This mainly exists for documentation, to show which traits and methods are available
/// on all C-style enums defined with this crate. (Click **"Show Declaration"** in Rustdoc
/// to see the supertraits.)
pub trait CEnum<T>
where
    Self: core::fmt::Debug
        + Clone
        + Copy
        + PartialEq
        + core::hash::Hash
        + PartialOrd
        + Ord
        + core::convert::From<T>,
    T: core::convert::From<Self>,
{
    /// The name of the enum variant in code, if one is defined for this value.
    fn name(self) -> Option<&'static str>;
}

/// Define a struct that wraps an integer type and acts like a C-style enum.
///
/// The new struct will be a transparent newtype wrapper for the underlying integral type.
/// It will have an associated constant for each variant listed in the definition, and it
/// will implement [`CEnum`] and all its supertraits.
///
/// See crate documentation for examples.
#[macro_export]
macro_rules! c_enum {
    [
        $( #[$meta:meta] )*
        $vis:vis enum $name:ident($repr_type:ty) {
            $(
                $( #[$variant_meta:meta] )*
                $variant_name:ident $( = $variant_value:expr )?
            ),*
            $(,)?
        }
    ] => {
        $(#[$meta])*
        #[repr(transparent)]
        #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name($repr_type);

        impl $name {
            $crate::c_enum! {
                @variants
                default = 0;
                $(
                    $( #[$variant_meta] )*
                    $variant_name $( = $variant_value )?
                ),*
            }

            /// The name of the enum variant in code, if one is defined for this value.
            ///
            /// An identical method is available through the `CEnum` trait, but this
            /// version is declared `const` (which is currently unstable on traits).
            pub const fn name(self) -> Option<&'static str> {
                match self {
                    $(
                        $(#[$variant_meta])*
                        Self::$variant_name => Some(stringify!($variant_name)),
                    )*
                    _ => None,
                }
            }
        }

        impl $crate::CEnum<$repr_type> for $name {
            fn name(self) -> Option<&'static str> {
                // Delegate to const method in inherent impl
                self.name()
            }
        }

        impl core::fmt::Debug for $name {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                match self.name() {
                    Some(name) => core::write!(
                        f,
                        concat!(stringify!($name), "::{}"),
                        name,
                    ),
                    None => f.debug_tuple(stringify!($name))
                        .field(&self.0)
                        .finish(),
                }
            }
        }

        impl core::convert::From<$repr_type> for $name {
            fn from(repr: $repr_type) -> Self {
                Self(repr)
            }
        }

        impl core::convert::From<$name> for $repr_type {
            fn from(enum_value: $name) -> Self {
                enum_value.0
            }
        }
    };

    // No variants
    [
        @variants
        default = $default_value:expr;
    ] => {
        // Done
    };

    // Explicit value
    [
        @variants
        default = $default_value:expr;
        $( #[$current_meta:meta] )*
        $current_name:ident = $current_value:expr
        $(
            ,
            $( #[$rest_meta:meta] )*
            $rest_name:ident $( = $rest_value:expr )?
        )*
    ] => {
        $(#[$current_meta])*
        #[allow(non_upper_case_globals)]
        pub const $current_name: Self = Self($current_value);

        $crate::c_enum! {
            @variants
            default = $current_value + 1;
            $(
                $(#[$rest_meta])*
                $rest_name $( = $rest_value )?
            ),*
        }
    };

    // Default value
    [
        @variants
        default = $default_value:expr;
        $( #[$current_meta:meta] )*
        $current_name:ident
        $(
            ,
            $( #[$rest_meta:meta] )*
            $rest_name:ident $( = $rest_value:expr )?
        )*
    ] => {
        $(#[$current_meta])*
        #[allow(non_upper_case_globals)]
        pub const $current_name: Self = Self($default_value);

        $crate::c_enum! {
            @variants
            default = $default_value + 1;
            $(
                $(#[$rest_meta])*
                $rest_name $( = $rest_value )?
            ),*
        }
    };
}
