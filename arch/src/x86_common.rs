//! Architecture-specific bindings common to 32-bit and 64-bit x86 processors.

use tartan_bitfield::bitfield;

#[cfg(doc)]
use features::BasicFeatures;

pub mod features;
pub mod io;
pub mod paging;


bitfield! {
    /// `EFLAGS`/`RFLAGS`: General flags, including control, status, and basic system
    /// flags.
    ///
    /// Getters and setters for this structure only access a value in memory, not the
    /// register itself. Use the [`get`](Self::get) and [`set`](Self::set) methods to work
    /// with the actual register.
    pub struct FlagRegister(usize) {
        /// `CF`: Indicates an arithmetic instruction generated a carry/borrow (unsigned
        /// overflow).
        [0] pub carry,
        /// `PF`: Indicates that the least-significant byte of the result has *even*
        /// parity.
        [2] pub parity,
        /// `AF`: Indicates a carry/borrow/overflow out of bit 3 in binary-coded decimal
        /// (BCD) arithmetic.
        [4] pub aux_carry,
        /// `ZF`: Indicates that the result of an instruction is zero.
        [6] pub zero,
        /// `SF`: Indicates that the most-significant bit of a result is 1.
        [7] pub sign,
        /// `TF`: Enable single-step debugging.
        [8] pub trap,
        /// `IF`: Enable non-maskable interrupts. Non-maskable interrupts are always
        /// enabled.
        [9] pub interrupt_enabled,
        /// `DF`: String instructions work on addresses high-to-low when set, low-to-high
        /// when clear.
        [10] pub direction,
        /// `OF`: Indicates that the result overflowed for signed arithmetic (carry/borrow
        /// for the second-most-significant bit).
        [11] pub signed_overflow,
        /// `IOPL`: Sets the privilege threshold for a task to access I/O address space.
        /// Smaller numbers are higher privilege.
        [12..14] pub io_privilege_level: u8,
        /// `NT`: Indicates that the processor should switch back to a parent task when it
        /// executes an `IRET` instruction.
        ///
        /// Only supported in 32-bit mode. If this is set in 64-bit mode, `IRET` will
        /// trigger an exception.
        [14] pub nested_task,
        /// `RF`: Disable instruction breakpoints.
        [16] pub resume,
        /// `VM`: Enable virtual real mode.
        [17] pub virtual_8086_mode,
        /// `AC`: Enable strict alignment checks for memory accesses in privilege level 3.
        /// In privilege levels 0–2, allow access to pages assigned to lower privilege
        /// levels.
        ///
        /// Alignment checking requires [`ControlRegister0::alignment_check_mask`]. Access
        /// protection requires [`ControlRegister4::supervisor_access_prevention`].
        [18] pub alignment_check_or_access_control,
        /// `VIF`: Virtual counterpart to the `interrupt_enabled` flag, used with
        /// [VME](ControlRegister4::virtual_8086_extensions) or
        /// [PVI](ControlRegister4::protected_virtual_interrupts).
        [19] pub virtual_interrupt_enabled,
        /// `VIP`: Indicates an interrupt is pending for
        /// [VME](ControlRegister4::virtual_8086_extensions) or
        /// [PVI](ControlRegister4::protected_virtual_interrupts).
        [20] pub virtual_interrupt_pending,
        /// `ID`: Indicates `CPUID` support when the flag is modifiable.
        [21] pub identification,
    }
}

impl FlagRegister {
    /// Retrieve the current value of the `EFLAGS` register.
    pub fn get() -> Self {
        let mut value = Self(0);
        unsafe {
            #[cfg(target_arch = "x86")]
            asm!(
                "
                pushfd
                pop {0:e}
                ",
                out(reg) value.0,
            );

            #[cfg(target_arch = "x86_64")]
            asm!(
                "
                pushfq
                pop {0:r}
                ",
                out(reg) value.0,
            );
        }
        value
    }

    /// Update the `EFLAGS` register with the values in this struct, as permission level
    /// allows.
    ///
    /// Some flags will be unaffected depending on the processor mode and permission level
    /// flags. See the reference for the POPF instruction in the _Intel 64 and IA-32
    /// Architectures Software Developer's Manual_, volume 2.
    ///
    /// # Safety
    /// Altering certain system flags can have dramatic effects on the execution of this
    /// and other programs, including memory safety. See volume 1 §3.4.3 ("EFLAGS
    /// Register") and volume 3 §2.3 ("System Flags and Fields in the EFLAGS Register") of
    /// the _Intel 64 and IA-32 Architectures Software Developer's Manual_.
    pub unsafe fn set(self) {
        #[cfg(target_arch = "x86")]
        asm!(
            "
            push {0:e}
            popfd
            ",
            in(reg) self.0,
        );

        #[cfg(target_arch = "x86_64")]
        asm!(
            "
            push {0:r}
            popfq
            ",
            in(reg) self.0,
        );
    }
}


#[macro_export]
#[doc(hidden)]
macro_rules! simple_register_access {
    [$struct:ident, $register:literal] => {
        impl $struct {
            /// Retrieve the current value of this register
            pub fn get() -> Self {
                let mut value = Self(0);
                unsafe {
                    asm!(
                        concat!("mov {0}, ", $register),
                        out(reg) value.0,
                    );
                }
                value
            }

            /// Update the register with the value in this struct.
            ///
            /// # Safety
            /// Altering certain system flags can have dramatic effects on the execution
            /// of this and other programs, including memory safety.
            pub unsafe fn set(self) {
                asm!(
                    concat!("mov ", $register, ", {0}"),
                    in(reg) self.0,
                );
            }
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! indexed_register_access {
    [$struct:ident, $index:literal, $read_instr:literal, $write_instr:literal] => {
        impl $struct {
            /// Retrieve the current value of this register
            pub fn get() -> Self {
                let lower: u32;
                let upper: u32;
                unsafe {
                    asm!(
                        $read_instr,
                        in("ecx") $index,
                        out("eax") lower,
                        out("edx") upper,
                    );
                }
                let mut value = Self(0);
                value.0 |= u64::from(lower);
                value.0 |= u64::from(upper) << 32;
                value
            }

            /// Update the register with the value in this struct.
            ///
            /// # Safety
            /// Altering certain system flags can have dramatic effects on the execution
            /// of this and other programs, including memory safety.
            pub unsafe fn set(self) {
                #![allow(clippy::cast_possible_truncation)]
                let lower = self.0 as u32;
                let upper = (self.0 >> 32) as u32;
                asm!(
                    $write_instr,
                    in("ecx") $index,
                    in("eax") lower,
                    in("edx") upper,
                );
            }
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! extended_register_access {
    [$struct:ident, $index:literal] => {
        $crate::indexed_register_access!($struct, $index, "xgetbv", "xsetbv");
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! model_specific_register_access {
    [$struct:ident, $index:literal] => {
        $crate::indexed_register_access!($struct, $index, "rdmsr", "wrmsr");
    }
}


bitfield! {
    /// `CR0`: System control register with flags affecting protection, paging, and FPU
    /// behavior.
    ///
    /// Getters and setters for this structure only access a value in memory, not the
    /// register itself. Use the [`get`](Self::get) and [`set`](Self::set) methods to
    /// work with the actual register.
    pub struct ControlRegister0(usize) {
        /// `CR0.PG`: Enable paging. Requires [`protected_mode`](Self::protected_mode).
        [31] pub paging,
        /// `CR0.CD`: Disable all memory caching.
        [30] pub cache_disabled,
        /// `CR0.NW`: Disable write-back/write-through caching.
        [29] pub cache_not_write_through,
        /// `CR0.AM`: Enables strict alignment checks for memory access, in combination
        /// with [`FlagRegister::alignment_check_or_access_control`].
        [18] pub alignment_check_mask,
        /// `CR0.WP`: Enforce read-only pages even in privilege levels 0–2. They are
        /// always enforced in level 3.
        [16] pub write_protect,
        /// `CR0.NE`: Use internal error mechanism for FPU errors, rather than DOS-style.
        [ 5] pub native_fpu_error,
        /// `CR0.ET`: On 386/486, 387 FPU instructions are supported if set. Always set
        /// on modern processors.
        [ 4] pub fpu_extension_type,
        /// `CR0.TS`: Set by processor when task was switched but FPU context has not been
        /// saved yet.
        ///
        /// Used to save work when the new task does not alter the FPU state. When an FPU
        /// instruction is executed with this flag set, the processor raises an exception
        /// that allows the OS to save the FPU state. This behavior can be altered by
        /// other flags. See [`monitor_fpu_state`](Self::monitor_fpu_state).
        [ 3] pub task_switched_without_fpu_state,
        /// `CR0.EM`: Trigger an exception on all FPU instructions. Used to support
        /// software emulation.
        [ 2] pub fpu_emulation,
        /// `CR0.MP`: Enable exception behavior described for the `CR0.TS` flag for the
        /// (`F`)`WAIT` instruction.
        [ 1] pub monitor_fpu,
        /// Enable protected mode. Does not enable paging on its own. See
        /// [`paging`](Self::paging).
        [ 0] pub protected_mode,
    }
}

simple_register_access!(ControlRegister0, "cr0");

impl ControlRegister0 {
    /// Directly clear the
    /// [`task_switched_without_fpu_state`](Self::task_switched_without_fpu_state) flag in
    /// this register using a single instruction.
    ///
    /// # Safety
    /// Clearing this flag when inappropriate may clobber FPU state and potentially affect
    /// memory safety.
    pub unsafe fn clear_task_switched_without_fpu_state() {
        asm!("clts");
    }
}


bitfield! {
    /// `CR4`: Miscellaneous system control flags.
    ///
    /// Getters and setters for this structure only access a value in memory, not the
    /// register itself. Use the [`get`](Self::get) and [`set`](Self::set) methods to
    /// work with the actual register.
    pub struct ControlRegister4(usize) {
        /// `CR4.VME`: Enable interrupts and exception handling in [virtual
        /// real-mode](EFLAGS::virtual_8086_mode).
        ///
        /// Requires [`BasicFeatures::virtual_8086_extensions`].
        [0] pub virtual_8086_extensions,

        /// `CR4.PVI`: Enable virtual interrupts in protected mode.
        ///
        /// Requires [`BasicFeatures::virtual_8086_extensions`].
        [1] pub protected_virtual_interrupts,

        /// `CR4.TSD`: Disable access to processor timestamp counter except in privilege
        /// level 0.
        ///
        /// Requires [`BasicFeatures::timestamp_counter`].
        [2] pub timestamp_disabled,

        /// `CR4.DE`: Enable newer debug register scheme where `DR4` and `DR5` are
        /// unavailable.
        ///
        /// When this flag is clear, they are equivalent to `DR6` and `DR7`.
        ///
        /// Requires [`BasicFeatures::debugging_extensions`].
        [3] pub debugging_extensions,

        /// `CR4.PSE`: Support large pages (4MB). Applies to 32-bit mode only.
        ///
        /// When this flag is clear in 32-bit mode, pages are always 4KB. Large pages are
        /// always enabled in 64-bit mode.
        ///
        /// Requires [`BasicFeatures::page_size_extensions`].
        [4] pub page_size_extensions,

        /// `CR4.PAE`: Enable pages to map to physical addresses larger than 32-bits.
        ///
        /// Required for 64-bit mode.
        ///
        /// Requires [`BasicFeatures::physical_address_extension`].
        [5] pub physical_address_extension,

        /// `CR4.MCE`: Enable machine-check exception.
        ///
        /// Requires [`BasicFeatures::machine_check_exception`].
        [6] pub machine_check_exception,

        /// `CR4.PGE`: Enable global pages, which are shared across task contexts.
        ///
        /// Requires [`BasicFeatures::global_pages`].
        [7] pub global_pages,

        /// `CR4.PCE`: Allow access to performance monitoring counter in privilege levels
        /// 1–3 (always accessible in level 0).
        [8] pub performance_counter,

        /// `CR4.OSFXSR`: Enable the `FXSAVE`/`FXRSTOR` and SSE instructions, if present.
        ///
        /// These instructions require special support from the operating system.
        ///
        /// Requires [`BasicFeatures::fpu_save`].
        [9] pub sse_and_fpu_save,

        /// `CR4.OSXMMEXCPT`: Enable unmasked SIMD floating-point exception handling for
        /// SSE instructions.
        ///
        /// This requires special support from the operating system.
        [10] pub simd_exceptions,

        /// `CR4.UMIP`: Prevent access to instructions that allow reads from
        /// descriptor/task registers, except in privilege level 0.
        [11] pub restrict_user_mode_instructions,

        /// `CR4.LA57`: Support 57-bit addresses using 5-level paging in 64-bit mode.
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [12] pub five_level_paging,

        /// `CR4.VMX` (**Intel-only**): Enable virtual machine extensions.
        ///
        /// Requires [`BasicFeatures::virtual_machine_extensions`].
        [13] pub virtual_machine_extensions,

        /// `CR4.SME` (**Intel-only**): Enable safer-mode extensions.
        ///
        /// Requires [`BasicFeatures::safer_mode_extensions`].
        [14] pub safer_mode_extensions,

        /// `CR4.FSGSBASE`: Enable instructions to load/store the `FS` and `GS` base
        /// registers with 32/64-bit values in 64-bit mode.
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [16] pub extended_base_registers,

        /// `CR4.PCIDE`: Enable process-context identifiers (PCID) in 64-bit mode.
        ///
        /// Requires [`BasicFeatures::process_context_ids`].
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [17] pub process_context_ids,

        /// `CR4.OSXSAVE`: Enable instructions for saving and restoring extended processor
        /// state (FPU/MMX/SSE/AVX).
        ///
        /// These instructions require special support from the operating system.
        ///
        /// Requires [`BasicFeatures::extended_state_save`].
        [18] pub extended_state_save,

        /// `CR4.SMEP`: Enable execution prevention in privilege levels 0–2.
        [20] pub supervisor_execution_prevention,

        /// `CR4.SMAP`: Enable access prevention in privilege levels 0–2.
        [21] pub supervisor_access_prevention,

        /// `CR4.PKE`: Use page protection keys in 64-bit mode to control access from
        /// privilege level 3.
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [22] pub user_protection_keys,

        /// `CR4.CET` (**Intel-only**): Enable control-flow enforcement technology.
        /// Requires [`ControlRegister0::write_protect`].
        [23] pub control_flow_enforcement,

        /// `CR4.PKS` (**Intel-only**): Use page protection keys in 64-bit mode to control
        /// access from privilege levels 0-2.
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [24] pub supervisor_protection_keys,
    }
}

simple_register_access!(ControlRegister4, "cr4");


bitfield! {
    /// `XCR0`: System control flags that indicate OS support for context management for
    /// various registers with the `XSAVE` feature.
    ///
    /// Getters and setters for this structure only access a value in memory, not the
    /// register itself. Use the [`get`](Self::get) and [`set`](Self::set) methods to
    /// work with the actual register.
    ///
    /// Requires [`BasicFeatures::extended_state_save`].
    pub struct ExtendedControlRegister0(u64) {
        /// `XCR0.X87`: Hardcoded to 1.
        [0] pub fpu,

        /// `XCR0.SSE`: Manage SSE state with `XSAVE`, including the `XMM` registers.
        [1] pub sse,

        /// `XCR0.AVX`: Manage 256-bit AVX state in upper halves of the `YMM` registers
        /// with `XSAVE`.
        ///
        /// Requires [`sse`](Self::sse). The lower halves of these registers are
        /// equivalent to `XMM` and are covered by that flag.
        [2] pub avx_256,

        /// `XCR0.BNDREG`: Manage MPX bounds registers with `XSAVE`.
        ///
        /// Requires [`mpx_bound_config_status`](Self::mpx_bound_config_status).
        [3] pub mpx_bounds,

        /// `XCR0.BNDCSR`: Manage MPX config and status registers with `XSAVE`.
        ///
        /// Requires [`mpx_bounds`](Self::mpx_bounds).
        [4] pub mpx_bound_config_status,

        /// `XCR0.OPMASK`: Manage AVX-512 opmask registers with `XSAVE`.
        ///
        /// Requires the other `avx_512_*` flags.
        [5] pub avx_512_opmask,

        /// `XCR0.ZMM_Hi256`: Manage 512-bit AVX state in the upper halves of the `ZMM`
        /// registers up to `ZMM15` with `XSAVE`.
        ///
        /// Registers `ZMM8`–`ZMM15` are available in 64-bit mode only, so this only
        /// applies up to `ZMM7` in 32-bit mode.
        ///
        /// Requires [`avx_256`](Self::avx_256) and the other `avx_512_*` flags. The lower
        /// halves of these registers are equivalent to `YMM` and are covered by
        /// `avx_256`(Self::avx_256).
        [6] pub avx_512,

        /// `XCR0.Hi16_ZMM`: Manage AVX-512 state in `ZMM16`–`ZMM31` with `XSAVE`.
        ///
        /// These registers are only available in 64-bit mode.
        ///
        /// Requires the other `avx_512_*` flags.
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [7] pub avx_512_extended,

        /// `XCR0.PKRU`: Manage protection key rights registers with `XSAVE`.
        [8] pub protection_key_rights,
    }
}

extended_register_access!(ExtendedControlRegister0, 0);
