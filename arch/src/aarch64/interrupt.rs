//! Exception and interrupt handling.

use super::ExceptionLevel;
use crate::system_register_access;
use core::arch::asm;
use tartan_bitfield::bitfield;
use tartan_c_enum::c_enum;

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
/// signature `func(Kind, Source)`.
///
/// # Example
/// ```ignore
/// # use tartan_arch::aarch64_exception_vector_table;
/// # use tartan_arch::aarch64::ExceptionLevel;
/// # use tartan_arch::aarch64::interrupt::{Kind, Source, VectorBaseAddressRegister};
/// #
/// aarch64_exception_vector_table!(exception_table_el1, handle_exception);
///
/// fn handle_exception(kind: Kind, source: Source) {
///     panic!("Exception {:?} from {:?}", kind, source);
/// }
///
/// fn main() {
///     VectorBaseAddressRegister::set(
///         ExceptionLevel::One,
///         unsafe { &exception_table_el1 }
///     );
/// }
/// ```
#[macro_export]
macro_rules! aarch64_exception_vector_table {
    [$table:ident, $handler:path] => {
        extern "C" {
            #[no_mangle]
            static $table: $crate::aarch64::interrupt::VectorTable;
        }

        core::arch::global_asm!(concat!(
            "
            //
            // Exceptions from current level using SP_EL0 (thread mode)

            .balign 0x800
        ", stringify!($table), ":
            // Synchronous exception
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_sync_exception_from_thread
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // Interrupt
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_interrupt_from_thread
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // Fast interrupt
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_fast_interrupt_from_thread
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // System error
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_system_error_from_thread
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            //
            // Exceptions from current level using current-level SP (handler mode)

            .balign 0x80
            // Synchronous exception
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_sync_exception_from_handler
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // Interrupt
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_interrupt_from_handler
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // Fast interrupt
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_fast_interrupt_from_handler
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // System error
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_system_error_from_handler
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            //
            // Exceptions from lower level in 64-bit mode

            .balign 0x80
            // Synchronous exception
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_sync_exception_from_lower
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // Interrupt
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_interrupt_from_lower
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // Fast interrupt
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_fast_interrupt_from_lower
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // System error
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_system_error_from_lower
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            //
            // Exceptions from lower level in 32-bit mode

            .balign 0x80
            // Synchronous exception
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_sync_exception_from_lower_32bit
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // Interrupt
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_interrupt_from_lower_32bit
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // Fast interrupt
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_fast_interrupt_from_lower_32bit
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret

            .balign 0x80
            // System error
            stp lr, fp, [sp, #-16]!
            bl ", stringify!($table), "_save
            bl ", stringify!($table), "_system_error_from_lower_32bit
            bl ", stringify!($table), "_restore
            ldp lr, fp, [sp], #16
            eret


        ", stringify!($table), "_save:
            stp x0, x1, [sp, #-16]!
            stp x2, x3, [sp, #-16]!
            stp x4, x5, [sp, #-16]!
            stp x6, x7, [sp, #-16]!
            stp x8, x9, [sp, #-16]!
            stp x10, x11, [sp, #-16]!
            stp x12, x13, [sp, #-16]!
            stp x14, x15, [sp, #-16]!

        ", stringify!($table), "_restore:
            ldp x14, x15, [sp], #16
            ldp x12, x13, [sp], #16
            ldp x10, x11, [sp], #16
            ldp x8, x9, [sp], #16
            ldp x6, x7, [sp], #16
            ldp x4, x5, [sp], #16
            ldp x2, x3, [sp], #16
            ldp x0, x1, [sp], #16
            "
        ));

        $crate::aarch64::interrupt::paste! {
            #[no_mangle]
            fn [< $table _sync_exception_from_thread >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::Synchronous,
                    $crate::aarch64::interrupt::Source::CurrentLevelThread,
                )
            }

            #[no_mangle]
            fn [< $table _interrupt_from_thread >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::Interrupt,
                    $crate::aarch64::interrupt::Source::CurrentLevelThread,
                )
            }

            #[no_mangle]
            fn [< $table _fast_interrupt_from_thread >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::FastInterrupt,
                    $crate::aarch64::interrupt::Source::CurrentLevelThread,
                )
            }

            #[no_mangle]
            fn [< $table _system_error_from_thread >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::SystemError,
                    $crate::aarch64::interrupt::Source::CurrentLevelThread,
                )
            }

            #[no_mangle]
            fn [< $table _sync_exception_from_handler >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::Synchronous,
                    $crate::aarch64::interrupt::Source::CurrentLevelHandler,
                )
            }

            #[no_mangle]
            fn [< $table _interrupt_from_handler >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::Interrupt,
                    $crate::aarch64::interrupt::Source::CurrentLevelHandler,
                )
            }

            #[no_mangle]
            fn [< $table _fast_interrupt_from_handler >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::FastInterrupt,
                    $crate::aarch64::interrupt::Source::CurrentLevelHandler,
                )
            }

            #[no_mangle]
            fn [< $table _system_error_from_handler >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::SystemError,
                    $crate::aarch64::interrupt::Source::CurrentLevelHandler,
                )
            }

            #[no_mangle]
            fn [< $table _sync_exception_from_lower >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::Synchronous,
                    $crate::aarch64::interrupt::Source::LowerLevel,
                )
            }

            #[no_mangle]
            fn [< $table _interrupt_from_lower >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::Interrupt,
                    $crate::aarch64::interrupt::Source::LowerLevel,
                )
            }

            #[no_mangle]
            fn [< $table _fast_interrupt_from_lower >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::FastInterrupt,
                    $crate::aarch64::interrupt::Source::LowerLevel,
                )
            }

            #[no_mangle]
            fn [< $table _system_error_from_lower >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::SystemError,
                    $crate::aarch64::interrupt::Source::LowerLevel,
                )
            }

            #[no_mangle]
            fn [< $table _sync_exception_from_lower_32bit >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::Synchronous,
                    $crate::aarch64::interrupt::Source::LowerLevel32Bit,
                )
            }

            #[no_mangle]
            fn [< $table _interrupt_from_lower_32bit >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::Interrupt,
                    $crate::aarch64::interrupt::Source::LowerLevel32Bit,
                )
            }

            #[no_mangle]
            fn [< $table _fast_interrupt_from_lower_32bit >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::FastInterrupt,
                    $crate::aarch64::interrupt::Source::LowerLevel32Bit,
                )
            }

            #[no_mangle]
            fn [< $table _system_error_from_lower_32bit >]() {
                $handler(
                    $crate::aarch64::interrupt::Kind::SystemError,
                    $crate::aarch64::interrupt::Source::LowerLevel32Bit,
                )
            }
        }
    };
}

/// The kind of exception being handled: sync, IRQ, FIQ, SError.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    /// A standard exception traceable to a specific instruction.
    Synchronous,
    /// An external interrupt (IRQ).
    Interrupt,
    /// An external interrupt through the higher-priority fast interrupt (FIQ) mechanism.
    FastInterrupt,
    /// An internal error with the processor (SError).
    SystemError,
}

/// The exception level that generated an exception.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Source {
    /// Exception from current level using SP_EL0 (thread mode)
    CurrentLevelThread,
    /// Exception from current level using current-level SP (handler mode)
    CurrentLevelHandler,
    /// Exception from lower level (e.g., EL0) in 64-bit mode
    LowerLevel,
    /// Exception from lower level (e.g., EL0) in 32-bit mode
    LowerLevel32Bit,
}


/// A single entry in the exception [`VectorTable`].
///
/// The bytes of the entry are interpreted as instructions and executed when an exception
/// of the corresponding [`Kind`] + [`Source`] is encountered. Consequently, it isn't
/// useful to instantiate this type directly. Use [`aarch64_exception_vector_table`]
/// instead.
#[repr(align(0x80))]
#[allow(dead_code)]
pub struct VectorEntry([u8; 0x80]);


/// Exception vector table that contains code to handle exceptions from each combination
/// of [`Kind`] and [`Source`].
#[repr(align(0x800))]
#[allow(dead_code)]
pub struct VectorTable([VectorEntry; 16]);


/// `VBAR_ELx`: Contains the address of the [`VectorTable`] that the processor should use
/// at a given exception level.
pub enum VectorBaseAddressRegister {}

impl VectorBaseAddressRegister {
    /// Retrieve the current value of this register for the specified exception level.
    ///
    /// Only defined for exception levels 1–3. The register for a given exception level is
    /// only accessible from that level or higher.
    pub fn get(level: ExceptionLevel) -> *const VectorTable {
        let mut value: *const VectorTable;
        unsafe {
            match level {
                ExceptionLevel::Zero => panic!("This register does not exist for EL0"),
                ExceptionLevel::One => asm!("mrs {}, vbar_el1", out(reg) value),
                ExceptionLevel::Two => asm!("mrs {}, vbar_el2", out(reg) value),
                ExceptionLevel::Three => asm!("mrs {}, vbar_el3", out(reg) value),
            }
        }
        value
    }

    /// Update the register for the specified exception level with the given value.
    ///
    /// Address must be aligned to 11 bits (2048).
    pub fn set(level: ExceptionLevel, value: *const VectorTable) {
        unsafe {
            match level {
                ExceptionLevel::Zero => panic!("This register does not exist for EL0"),
                ExceptionLevel::One => asm!("msr vbar_el1, {}", in(reg) value),
                ExceptionLevel::Two => asm!("msr vbar_el2, {}", in(reg) value),
                ExceptionLevel::Three => asm!("msr vbar_el3, {}", in(reg) value),
            }
        }
    }
}


bitfield! {
    /// `DAIF`: Controls masking of different kinds of exceptions.
    pub struct MaskRegister(u64) {
        /// `F`: Mask fast (FIQ) interrupts.
        [6] pub fast_interrupts_masked,
        /// `I`: Mask regular (IRQ) interrupts.
        [7] pub interrupts_masked,
        /// `A`: Mask system error (SError) exceptions.
        [8] pub system_error_masked,
        /// `D`: Mask debugging-related exceptions (breakpoints, watchpoints, stepping)
        /// for the current exception level.
        [9] pub debug_masked,
    }
}

system_register_access!(MaskRegister, "DAIF");


bitfield! {
    /// `ESR_ELx`: Holds information about the cause of the exception currently being
    /// handled.
    pub struct SyndromeRegister(u64) {
        /// `EC`: The class of exception that was triggered.
        [26..32] pub class: u8 as Class,
        /// `IL`: When set, indicates that the instruction that triggered this exception
        /// was a 32-bit opcode (A64 or classic Arm). Otherwise, the opcode was 16 bits
        /// (Thumb).
        [25] pub length_32bit,
        /// `ISS`: Additional data about the exception in a format that is specific to
        /// the [`class`](Self::class) value.
        [ 0..25] pub class_data: u32,
    }
}

impl SyndromeRegister {
    /// Retrieve the current value of this register for the specified exception level.
    ///
    /// Only defined for exception levels 1–3. The register for a given exception level is
    /// only accessible from that level or higher.
    pub fn get(level: ExceptionLevel) -> Self {
        let mut value = Self(0);
        unsafe {
            match level {
                ExceptionLevel::Zero => panic!("This register does not exist for EL0"),
                ExceptionLevel::One => asm!("mrs {}, esr_el1", out(reg) value.0),
                ExceptionLevel::Two => asm!("mrs {}, esr_el2", out(reg) value.0),
                ExceptionLevel::Three => asm!("mrs {}, esr_el3", out(reg) value.0),
            }
        }
        value
    }

    /// Update the register for the specified exception level with the given value.
    pub fn set(level: ExceptionLevel, value: Self) {
        unsafe {
            match level {
                ExceptionLevel::Zero => panic!("This register does not exist for EL0"),
                ExceptionLevel::One => asm!("msr esr_el1, {}", in(reg) value.0),
                ExceptionLevel::Two => asm!("msr esr_el2, {}", in(reg) value.0),
                ExceptionLevel::Three => asm!("msr esr_el3, {}", in(reg) value.0),
            }
        }
    }
}


c_enum! {
    /// Classifies different exception causes.
    ///
    /// Note that the Arm documentation defines more variants than are defined in this
    /// type. This only includes exception classes that can be triggered from Aarch64
    /// state and taken to EL1.
    pub enum Class(u8) {
        /// Tried to execute an undefined/unsupported opcode, or for any other reason that
        /// does not fall under another exception class.
        Unknown                     = 0b00_0000,
        /// Trapped execution of a wait instruction (`WFE`/`WFI`).
        Wait                        = 0b00_0001,
        /// Trapped access to a FPU/vector register or instruction.
        FPUAccess                   = 0b00_0111,
        /// Tried to execute instructions after an illegal return.
        IllegalState                = 0b00_1110,
        /// Triggered a system call via the `SVC` instruction.
        SupervisorCall              = 0b01_0101,
        /// Trapped access to a system register or instruction.
        SystemInstruction           = 0b01_1000,
        /// Trapped access to a vector register or instruction.
        VectorAccess                = 0b01_1001,
        /// Improperly authenticated pointer detected. Requires `FEAT_FPAC`.
        PointerAuthFailure          = 0b01_1100,
        /// Instruction abort triggered at lower exception level.
        InstructionAbortFromLower   = 0b10_0000,
        /// Instruction abort triggered at current exception level.
        InstructionAbortFromCurrent = 0b10_0001,
        /// Program counter not properly aligned.
        PCAlignment                 = 0b10_0010,
        /// Data abort triggered at lower exception level.
        DataAbortFromLower          = 0b10_0100,
        /// Data abort triggered at current exception level.
        DataAbortFromCurrent        = 0b10_0101,
        /// Stack pointer not properly aligned.
        SPAlignment                 = 0b10_0110,
        /// Trapped floating point exception.
        FloatException              = 0b10_1100,
        /// System error (SError).
        SystemError                 = 0b10_1111,
        /// Breakpoint triggered at lower exception level.
        BreakpointFromLower         = 0b11_0000,
        /// Breakpoint triggered at current exception level.
        BreakpointFromCurrent       = 0b11_0001,
        /// Software step at lower exception level.
        StepFromLower               = 0b11_0010,
        /// Software step at current exception level.
        StepFromCurrent             = 0b11_0011,
        /// Watchpoint triggered at lower exception level.
        WatchpointFromLower         = 0b11_0100,
        /// Watchpoint triggered at current exception level.
        WatchpointFromCurrent       = 0b11_0101,
        /// Executed a `BRK` breakpoint instruction.
        BreakpointInstruction       = 0b11_1100,
    }
}
