//! Exception and interrupt handling.

use crate::{cp15_register_get, cp15_register_set};

// Must be re-exported so that crates that use these macros will be able to resolve it
#[doc(hidden)]
pub use paste::paste;


/// Define an exception vector table that forwards all exceptions to the designated
/// handler.
///
/// The first argument is the name of the vector table, which will be exported as an
/// unmangled symbol and exposed to Rust code as an immutable static of type
/// [`VectorTable`].
///
/// The second argument is the name of the handler function, which should have
/// signature `func(Kind)`.
///
/// # Example
/// ```ignore
/// # use tartan_arch::arm_exception_vector_table;
/// # use tartan_arch::arm::ExceptionLevel;
/// # use tartan_arch::arm::interrupt::{Kind, VectorBaseAddressRegister};
/// #
/// arm_exception_vector_table!(exception_table, handle_exception);
///
/// fn handle_exception(kind: Kind) {
///     panic!("Exception {:?}", kind);
/// }
///
/// fn main() {
///     VectorBaseAddressRegister::set(unsafe { &exception_table });
/// }
/// ```
#[macro_export]
macro_rules! arm_exception_vector_table {
    [$table:ident, $handler:path] => {
        extern "C" {
            #[no_mangle]
            static $table: $crate::arm::interrupt::VectorTable;
        }

        core::arch::global_asm!(concat!("
            .arm  // Exception handlers always use 32-bit (Arm-mode) instructions
            .balign 0x10
        ", stringify!($table), ":

        . = ", stringify!($table), " + 0x00
            // Reset
            ldr pc, ", stringify!($table), "_reset

        . = ", stringify!($table), " + 0x04
            // Undefined instruction
            ldr pc, ", stringify!($table), "_undefined

        . = ", stringify!($table), " + 0x08
            // Software interrupt
            ldr pc, ", stringify!($table), "_software

        . = ", stringify!($table), " + 0x0c
            // Prefetch abort
            ldr pc, ", stringify!($table), "_prefetch

        . = ", stringify!($table), " + 0x10
            // Data abort
            ldr pc, ", stringify!($table), "_data

        . = ", stringify!($table), " + 0x18
            // Hardware interrupt (IRQ)
            ldr pc, ", stringify!($table), "_interrupt

        . = ", stringify!($table), " + 0x1c
            // Fast hardware interrupt (FIQ)
            ldr pc, ", stringify!($table), "_fast_interrupt
        "));

        $crate::arm::interrupt::paste! {
            #[no_mangle]
            fn [< $table _reset >]() {
                $handler($crate::arm::interrupt::Kind::Reset);
            }

            #[no_mangle]
            fn [< $table _undefined >]() {
                $handler($crate::arm::interrupt::Kind::UndefinedInstruction);
            }

            #[no_mangle]
            fn [< $table _software >]() {
                $handler($crate::arm::interrupt::Kind::SoftwareInterrupt);
            }

            #[no_mangle]
            fn [< $table _prefetch >]() {
                $handler($crate::arm::interrupt::Kind::PrefetchAbort);
            }

            #[no_mangle]
            fn [< $table _data >]() {
                $handler($crate::arm::interrupt::Kind::DataAbort);
            }

            #[no_mangle]
            fn [< $table _interrupt >]() {
                $handler($crate::arm::interrupt::Kind::HardwareInterrupt);
            }

            #[no_mangle]
            fn [< $table _fast_interrupt >]() {
                $handler($crate::arm::interrupt::Kind::FastHardwareInterrupt);
            }
        }
    };
}


/// The kind of exception being handled
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Kind {
    Reset,
    UndefinedInstruction,
    SoftwareInterrupt,
    PrefetchAbort,
    DataAbort,
    HardwareInterrupt,
    FastHardwareInterrupt,
}


/// Exception vector table that contains code to handle each exception [`Kind`].
#[repr(align(0x10))]
pub struct VectorTable([u8; 0x20]);


/// Contains the address of the [`VectorTable`] that the processor should use.
pub enum VectorBaseAddressRegister {}

impl VectorBaseAddressRegister {
    /// Retrieve the current value of this register for the specified exception level.
    ///
    /// Only defined for exception levels 1–3. The register for a given exception level is
    /// only accessible from that level or higher.
    pub fn get() -> *const VectorTable {
        let usize_value = cp15_register_get!(0, "c12", "c0", 0);
        usize_value as *const VectorTable
    }

    /// Update the register for the specified exception level with the given value.
    ///
    /// Address must be aligned to 11 bits (2048).
    pub fn set(value: *const VectorTable) {
        unsafe {
            cp15_register_set!(0, "c12", "c0", 0, value as usize);
        }
    }
}
