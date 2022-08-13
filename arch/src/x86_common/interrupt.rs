//! Support for managing interrupts.

use crate::model_specific_register_access;
use core::arch::asm;
use tartan_bitfield::bitfield;
use tartan_c_enum::c_enum;

#[cfg(doc)]
use super::FlagRegister;


c_enum! {
    /// An interrupt/exception number.
    pub enum InterruptVector(u8) {
        /// `#DE`: Division by zero or overflow in division.
        DivideError = 0,
        /// `#DB`: Breakpoints or other debugging-related traps/faults.
        DebugException = 1,
        /// `NMI`: Severe external interrupt that cannot be ignored.
        NonMaskable = 2,
        /// `#BP`: Breakpoint via `INT3` instruction.
        Breakpoint = 3,
        /// `#OF`: Overflow condition was detected with `INTO` instruction.
        Overflow = 4,
        /// `#BR`: Out-of-bounds index detected with `BOUND` instruction.
        Bound = 5,
        /// `#UD`: Unrecognized or reserved instruction opcode.
        InvalidOpcode = 6,
        /// `#NM`: Tried to execute FPU instruction with no coprocessor present.
        DeviceNotAvailable = 7,
        /// `#DF`: A fault was triggered while handling another interrupt.
        DoubleFault = 8,
        /// `#TS`: An error was found in the TSS while task switching.
        InvalidTaskSegment = 10,
        /// `#NP`: Tried to use a segment without a descriptor defined. Does not apply to
        /// the stack segment, which has its [own exception](Self::SegmentNotPresent).
        SegmentNotPresent = 11,
        /// `#SS`: The stack overflowed its segment or the segment was invalid.
        StackFault = 12,
        /// `#GP`: Memory protection or other miscellaneous error.
        ProtectionFault = 13,
        /// `#PF`: Tried to load a page that was not present, or used a page in a way that
        /// was not allowed by its attributes.
        PageFault = 14,
        /// `#MF`: Unmasked floating-point error that was *not* part of a SIMD operation.
        FloatingPointError = 16,
        /// `#AC`: Improperly aligned memory access in user mode while alignment checks
        /// were enabled.
        ///
        /// See [`FlagRegister::alignment_check_or_access_control`].
        AlignmentCheck = 17,
        /// `#MC`: Internal processor/bus error.
        MachineCheck = 18,
        /// `#XM`: Unmasked floating-point error during SIMD operation.
        SIMDFloatingPointError = 19,
        /// `#VE`: Improper use of virtualization extensions.
        ///
        /// See [`ControlRegister4::virtual_machine_extensions`].
        VirtualizationException = 20,
        /// `#CP`: Improper branching detected by control-flow guard.
        ///
        /// See [`ControlRegister4::control_flow_enforcement`].
        ControlProtectionException = 21,
    }
}

impl InterruptVector {
    /// Minimum user-defined interrupt vector.
    const MIN_USER: InterruptVector = InterruptVector(32);

    /// Indicates that this is interrupt vector is reserved by the system.
    pub fn reserved(self) -> bool {
        self < Self::MIN_USER
    }
}


/// `IDTR`: Contains the memory range of the interrupt descriptor table.
#[repr(C, packed)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::module_name_repetitions)]
pub struct InterruptDescriptorTableRegister {
    /// The inclusive maximum address offset (i.e., size - 1) of the descriptor table.
    pub limit: u16,
    /// Base address of the descriptor table.
    pub address: usize,
}

impl InterruptDescriptorTableRegister {
    /// Retrieve the current value of this register
    pub fn get() -> Self {
        let mut value = Self::default();
        unsafe {
            asm!(
                "sidt [{0}]",
                in(reg) &mut value,
            );
        }
        value
    }

    /// Update the register to the given value.
    ///
    /// # Safety
    /// This register can have fundamental affects on how programs execute.
    pub unsafe fn set(value: &Self) {
        asm!(
            "lidt [{0}]",
            in(reg) value,
        );
    }
}


bitfield! {
    /// `IA32_APIC_BASE`: A model-specific register that allows relocating an advanced
    /// programmable interrupt controller (APIC)'s control structure.
    ///
    /// See Intel ISA volume 2 §10.4.3–10.4.4.
    pub struct APICBaseRegister(u64) {
        /// Can be cleared to disable the APIC. However, it cannot be re-enabled once
        /// disabled with this mechanism.
        [11] pub enabled,
        /// Enable the APIC's
        [10] pub x2_interface,
        /// Indicates that this core was designated as the bootstrap processor for
        /// single-threaded operation on startup.
        [8] pub bootstrap_processor,
    }
}

impl APICBaseRegister {
    /// The default APIC base address when the processor is reset.
    pub const DEFAULT_ADDRESS: usize = 0xfee0_0000;
    const ADDRESS_MASK: u64 = 0xffff_ffff_ffff_f000;

    /// Base address of the APIC's registers.
    pub fn address(self) -> usize {
        #![allow(clippy::cast_possible_truncation)]
        (self.0 & Self::ADDRESS_MASK) as usize
    }

    /// Update the base address of the APIC's registers.
    pub fn set_address(&mut self, address: usize) {
        self.0 &= !Self::ADDRESS_MASK;
        self.0 |= Self::ADDRESS_MASK & address as u64;
    }
}

model_specific_register_access!(APICBaseRegister, 0x1b);
