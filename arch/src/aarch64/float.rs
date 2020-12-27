//! Floating-point control and status.

use crate::system_register_access;
use tartan_bitfield::bitfield;
use tartan_c_enum::c_enum;


bitfield! {
    /// `FPCR`: Influences floating-point instruction execution.
    pub struct ControlRegister(u64) {
        /// `AHP`: Use an alternative format for half-precision floats when converting
        /// to/from other formats. Otherwise, use the IEEE half-precision format.
        [26] pub alternative_half_precision_format,
        /// `DN`: Always use the default encoding for NaN results. Otherwise, use the
        /// encoding from an input operand.
        [25] pub default_nan,
        /// `FZ`: When a result would be denormal, yield zero instead. Otherwise, use the
        /// IEEE 754 behavior.
        [24] pub flush_to_zero,
        /// `RMode`: The IEEE 754 rounding mode in use.
        [22..24] pub rounding_mode: u8 as RoundingMode,
        /// `FZ16`: Counterpart to [`flush_to_zero`](Self::flush_to_zero) for
        /// half-precision calculations.
        ///
        /// Requires `FEAT_FP16`.
        [19] pub flush_to_zero_half_precision,
        /// For each type of floating-point exception, defines whether the error will be
        /// trapped. If false, the corresponding flag in [`StatusRegister::exceptions`]
        /// will be set instead.
        [ 8..16] pub trapped_exceptions: u8 as Exceptions,
    }
}

system_register_access!(ControlRegister, "FPCR");


bitfield! {
    /// `FPSR`: Indicates non-trapped floating-point exceptions.
    pub struct StatusRegister(u64) {
        /// Indicates any non-trapped exceptions that have been detected since these flags
        /// were last reset.
        [ 0.. 8] pub exceptions: u8 as Exceptions,
    }
}

system_register_access!(StatusRegister, "FPSR");


c_enum! {
    /// Floating-point rounding mode, as defined by IEEE 754.
    pub enum RoundingMode(u8) {
        /// Round to the nearest number, with ties toward even numbers.
        Nearest,
        /// Round toward positive infinity.
        PlusInfinity,
        /// Round toward negative infinity.
        MinusInfinity,
        /// Round toward zero (truncate).
        Zero,
    }
}

bitfield! {
    /// Status/mask bits for each type of floating-point exception.
    pub struct Exceptions(u8) {
        /// `DN`: An operand was a denormal number.
        [7] denormal_input,
        /// `IX`: A result was rounded.
        [4] inexact,
        /// `UF`: A result was too small to be represented accurately.
        [3] underflow,
        /// `OF`: A result was too large to be represented accurately.
        [2] overflow,
        /// `DZ`: Attempted to divide a number by zero (other than zero or infinity, which
        /// raises an [`invalid_operation`] instead).
        [1] divide_by_zero,
        /// `IO`: Attempted a mathematically undefined operation, such as infinity minus
        /// infinity.
        [0] invalid_operation,
    }
}
