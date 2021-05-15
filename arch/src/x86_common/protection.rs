//! Support for protected mode operation.
//!
//! This includes the minimal support for segmented memory and hardware task management
//! that is required to operate in protected mode with a flat memory model.

use core::fmt;
use core::num::NonZeroU16;
use static_assertions::const_assert_eq;
use tartan_bitfield::{
    bitfield, bitfield_accessors, bitfield_without_debug, get_bit, set_bit, Bitfield,
};
use tartan_c_enum::c_enum;

#[cfg(doc)]
use super::FlagRegister;
#[cfg(doc)]
use crate::x86_64::protection::TaskStateSegmentHeader;
#[cfg(doc)]
use crate::x86_64::ExtendedFeatureEnableRegister;


/// `GDTR`: Points to the memory range of the global descriptor table (GDT).
#[repr(C, packed)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct GlobalDescriptorTableRegister {
    /// The inclusive maximum address offset (i.e., size - 1) of the descriptor table.
    pub limit: u16,
    /// Base address of the descriptor table
    pub address: usize,
}

impl GlobalDescriptorTableRegister {
    /// Retrieve the current value of this register
    pub fn get() -> Self {
        let mut value = Self::default();
        unsafe {
            asm!(
                "sgdt [{0}]",
                in(reg) &mut value,
            );
        }
        value
    }

    /// Update the register to the given value.
    ///
    /// # Safety
    /// This register fundamentally affects memory accesses and can have an impact on
    /// memory safety.
    pub unsafe fn set(value: &Self) {
        asm!(
            "lgdt [{0}]",
            in(reg) value,
        );
    }

    /// Update the global descriptor table pointer and all segment registers.
    ///
    /// # Safety
    /// This register fundamentally affects memory accesses and can have an impact on
    /// memory safety.
    pub unsafe fn set_with_segments(
        gdt_pointer: &Self,
        code_selector: Selector,
        data_selector: Selector,
    ) {
        // TODO: Disable interrupts?

        #[cfg(target_arch = "x86")]
        asm!(
            "
            // Set the global descriptor table register
            lgdt [{gdt_pointer}]

            // Update the data segment selectors
            mov ds, {data_selector:e}
            mov es, {data_selector:e}
            mov fs, {data_selector:e}
            mov gs, {data_selector:e}
            mov ss, {data_selector:e}

            // Update the code segment selector. Directly loading the CS register doesn't
            // work, so we use a far return. The 'return' is really a jump to the next
            // instruction using a far pointer we push onto the stack, consisting of:
            //   * [rsp]     = 32-bit offset
            //   * [rsp + 4] = 32-bit segment selector (zero-extended from 16 bits)
            push {code_selector:e}
            mov eax, offset .dummy_target
            push eax
            retf
        .dummy_target:
            ",
            gdt_pointer = in(reg) gdt_pointer,
            data_selector = in(reg) u16::from(data_selector),
            code_selector = in(reg) u16::from(code_selector),
            out("eax") _,
        );

        #[cfg(target_arch = "x86_64")]
        asm!(
            "
            // Set the global descriptor table register
            lgdt [{gdt_pointer}]

            // Update the data segment selectors
            mov ds, {data_selector:r}
            mov es, {data_selector:r}
            mov fs, {data_selector:r}
            mov gs, {data_selector:r}
            mov ss, {data_selector:r}

            // Update the code segment selector. Directly loading the CS register doesn't
            // work, so we use a far return. The 'return' is really a jump to the next
            // instruction using a far pointer we push onto the stack, consisting of:
            //   * [rsp]     = 64-bit offset
            //   * [rsp + 8] = 64-bit segment selector (zero-extended from 16 bits)
            push {code_selector:r}
            lea rax, [rip + .dummy_target]
            push rax
            rex64 retf
        .dummy_target:
            ",
            gdt_pointer = in(reg) gdt_pointer,
            data_selector = in(reg) u16::from(data_selector),
            code_selector = in(reg) u16::from(code_selector),
            out("rax") _,
        );
    }
}


/// `LDTR`: Contains a [`Selector`] referencing a [`SegmentDescriptor`] that points to the
/// the local descriptor table (LDT).
///
/// Note that this type cannot be instantiated. It simply serves as a namespace for the
/// [`get`](Self::get) and [`set`](Self::set) methods, which work on a [`Selector`]
/// instance.
#[allow(clippy::empty_enum)]
pub enum LocalDescriptorTableRegister {}

impl LocalDescriptorTableRegister {
    /// Retrieve the current value of this register
    pub fn get() -> Selector {
        let mut value = Selector(0);
        unsafe {
            asm!(
                "sldt {0:x}",
                out(reg) value.0,
            );
        }
        value
    }

    /// Update the register with the value in this struct.
    ///
    /// The value must point to an entry in the global descriptor table
    /// ([`local`](Selector::local) == `false`) with the
    /// [`SystemDescriptorType::LocalDescriptorTable`] type.
    ///
    /// # Safety
    /// This register fundamentally affects memory accesses and can have an impact on
    /// memory safety.
    pub unsafe fn set(selector: Selector) {
        asm!(
            "lldt {0:x}",
            in(reg) selector.0,
        );
    }

    /// Get a pointer to the [`SegmentDescriptor`] for the currently-loaded local
    /// descriptor table, using the value of this register.
    ///
    /// # Safety
    /// The LDTR must not loaded with a selector for a valid LDR segment descriptor. The
    /// returned pointer is only valid until the LDTR is modified.
    pub unsafe fn current_descriptor() -> *const SegmentDescriptor {
        let address = LocalDescriptorTableRegister::get().descriptor_address();
        address as *const SegmentDescriptor
    }
}


/// `TR`: Contains a [`Selector`] referencing a [`SegmentDescriptor`] that points to the
/// current task state segment (TSS).
///
/// Note that this type cannot be instantiated. It simply serves as a namespace for the
/// [`get`](Self::get) and [`set`](Self::set) methods, which work on a [`Selector`]
/// instance.
#[allow(clippy::empty_enum)]
pub enum TaskRegister {}

impl TaskRegister {
    /// Retrieve the current value of this register
    pub fn get() -> Selector {
        let mut value = Selector(0);
        unsafe {
            asm!(
                "str {0:x}",
                out(reg) value.0,
            );
        }
        value
    }

    /// Update the register with the provided selector value.
    ///
    /// The value must point to an entry in the global descriptor table
    /// ([`local`](Selector::local) == `false`) with one of the
    /// `SystemDescriptorType::TaskState*` types.
    ///
    /// # Safety
    /// This register fundamentally affects memory accesses and can have an impact on
    /// memory safety.
    pub unsafe fn set(selector: Selector) {
        asm!(
            "ltr {0:x}",
            in(reg) selector.0,
        );
    }
}


/// Standard segment registers (`CS`, `DS`, `SS`, etc.), which contain [`Selector`]s.
pub enum SegmentRegister {
    /// `CS` register, which controls instruction loading
    Code,
    /// `DS` register, which controls the default segment for load/store instructions
    Data,
    /// `SS` segment register, which controls the location of the stack pointer and stack
    /// push and pop instructions.
    Stack,
    /// `ES` segment register, which can be used as an additional data segment.
    Extra,
    /// `FS` segment register, which can be used as an additional data segment.
    ExtraF,
    /// `GS` segment register, which can be used as an additional data segment.
    ExtraG,
}

impl SegmentRegister {
    /// Retrieve the current value of this register
    pub fn get(self) -> Selector {
        let mut value = Selector(0);
        unsafe {
            match self {
                Self::Code => asm!("mov {0:x}, cs", out(reg) value.0),
                Self::Data => asm!("mov {0:x}, ds", out(reg) value.0),
                Self::Stack => asm!("mov {0:x}, ss", out(reg) value.0),
                Self::Extra => asm!("mov {0:x}, es", out(reg) value.0),
                Self::ExtraF => asm!("mov {0:x}, fs", out(reg) value.0),
                Self::ExtraG => asm!("mov {0:x}, gs", out(reg) value.0),
            }
        }
        value
    }

    /// Update the register with the provided selector value.
    ///
    /// # Safety
    /// This register fundamentally affects memory accesses and can have an impact on
    /// memory safety.
    pub unsafe fn set(self, selector: Selector) {
        match self {
            Self::Code => asm!("mov cs, {0:x}", in(reg) selector.0),
            Self::Data => asm!("mov ds, {0:x}", in(reg) selector.0),
            Self::Stack => asm!("mov ss, {0:x}", in(reg) selector.0),
            Self::Extra => asm!("mov es, {0:x}", in(reg) selector.0),
            Self::ExtraF => asm!("mov fs, {0:x}", in(reg) selector.0),
            Self::ExtraG => asm!("mov gs, {0:x}", in(reg) selector.0),
        }
    }
}


bitfield! {
    /// A reference to an entry in a segment descriptor table.
    ///
    /// Used as the value of the segment registers `CS`, `DS`, etc. as well as the
    /// [`LocalDescriptorTableRegister`] and [`TaskRegister`].
    pub struct Selector(u16) {
        /// `RPL`: The privilege level "requested" when accessing the referenced segment.
        ///
        /// This may be different than the current privilege level (CPL), in which case
        /// the access must allowed for *both* the CPL and RPL. This is intended to allow
        /// OS code to limit its privileges when executing on behalf of user code.
        [0..2] pub privilege_level: u8,

        /// Indicates that this selector references a descriptor in the local descriptor
        /// table (LDT). Otherwise, it references the global descriptor table (GDT).
        [2] pub local,
    }
}

impl Selector {
    const OFFSET_MASK: u16 = 0xfff8;

    /// Create a new selector with the given field values
    pub const fn new(offset: u16, privilege_level: u8, local: bool) -> Self {
        // TODO: Rework bitfield crate to allow creating in const contexts
        let value = offset & Self::OFFSET_MASK
            | (privilege_level & 0b11) as u16
            | (local as u16) << 2;
        Self(value)
    }

    /// Create a null selector
    pub const fn null() -> Self {
        Self(0)
    }

    /// The offset of the referenced segment entry in the descriptor table.
    pub fn offset(self) -> u16 {
        self.0 & Self::OFFSET_MASK
    }

    /// Update the offset of the referenced entry in the descriptor table.
    ///
    /// # Panics
    /// Panics if the new offset is not aligned on an 8-byte boundary.
    pub fn set_offset(&mut self, offset: u16) {
        assert!(
            offset & !Self::OFFSET_MASK == 0,
            "Descriptor offset {} is not aligned on an 8-byte boundary",
            offset,
        );
        self.0 &= !Self::OFFSET_MASK;
        self.0 |= offset;
    }

    /// Calculate the address of the descriptor referenced by this selector
    pub fn descriptor_address(self) -> usize {
        let table_address = if self.local() {
            let local_table =
                unsafe { &*(LocalDescriptorTableRegister::current_descriptor()) };
            local_table.address()
        } else {
            GlobalDescriptorTableRegister::get().address
        };
        table_address + self.offset() as usize
    }
}


/// Settings common to [`SegmentDescriptor`]s and [`GateDescriptor`]s.
pub trait DescriptorFlags: Bitfield<u32> {
    bitfield_accessors! {
        /// If this is a system descriptor, indicates which type.
        ///
        /// Only applies if [`is_application`](Self::is_application) is false.
        [ 8..12] system_type: u8 as SystemDescriptorType,

        /// The processor sets this bit whenever a segment register points to this
        /// segment.
        ///
        /// The process never resets this itself, but it can be cleared manually.
        ///
        /// Only applies if [`is_application`](Self::is_application) is true.
        [ 8] application_accessed,

        /// If this is a code descriptor, indicates that the segment can be read.
        /// Otherwise, it is execute-only.
        ///
        /// Only applies if [`is_application`](Self::is_application) and
        /// [`is_code`](Self::is_code) are true.
        [ 9] code_readable,

        /// If this is a data descriptor, indicates that this segment is writable.
        ///
        /// Required for stack segments.
        ///
        /// Only applies if [`is_application`](Self::is_application) is true and
        /// [`code_descriptor`](Self::is_code) is false.
        [ 9] data_writable,

        /// If this is a code descriptor, indicates that this segment can be executed
        /// with lower privileges than [`privilege_level`](Self::privilege_level).
        ///
        /// Only applies if [`is_application`](Self::is_application) and
        /// [`is_code`](Self::is_code) are true.
        [10] code_conforming,

        /// If this is a data descriptor, indicates that the segment expands toward lower
        /// addresses (stack-like) if its limit is changed.
        ///
        /// Only applies if [`is_application`](Self::is_application) is true and
        /// [`is_code`](Self::is_code) is false.
        [10] data_expand_down,

        /// Indicates whether this is a code (true) or data (false) descriptor.
        ///
        /// Only applies if [`is_application`](Self::is_application) is true.
        [11] is_code,

        /// `S`: Indicates that this is an application section descriptor if true.
        /// Otherwise, this is a system descriptor.
        [12] is_application,

        /// `DPL`: The privilege level associated with the segment.
        ///
        /// This has several meanings depending on the segment type:
        ///   * For stack segments, this is the *exact* privilege level required to use
        ///     the segment as a stack.
        ///   * For data segments, this is the minimum privilege (maximum number) required
        ///     to access the segment.
        ///   * For call gates, this is the minimum privilege (maximum number) required to
        ///     use the gate.
        ///   * For code segments:
        ///     * If the segment is accessed through a call gate _or_
        ///       [`code_conforming`](Self::code_conforming) is true, this is the
        ///       *maximum* privilege level (minimum number) that can execute the code.
        ///     * Otherwise, it is the *exact* privilege level required to execute the
        ///       code.
        [13..15] privilege_level: u8,

        /// `P`: Indicates that the segment is defined.
        [15] present,

        /// `L`: If this is a code segment, indicates that it should be executed in 64-bit
        /// mode.
        ///
        /// Mutually exclusive with [`mode_32`](Self::mode_32).
        ///
        /// Only applies if all of the following are true:
        ///   * [`is_application`](Self::is_application)
        ///   * [`is_code`](Self::is_code)
        ///   * [`ExtendedFeatureEnableRegister::long_mode_active`]
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [21] code_mode_64,

        /// `D`/`B`: Indicates that the segment uses 32-bit mode. Otherwise, it is 16-bit,
        /// unless [`code_mode_64`](Self::code_mode_64) is set.
        ///
        /// Besides the address/operand sizes for instructions in code segments, this also
        /// affects the upper bound of stack-like data segments with the
        /// [`data_expand_down`](Self::data_expand_down) flag set.
        ///
        /// Only applies if [`is_application`](Self::is_application) is true.
        [22] application_mode_32,
    }

    /// Indicates that this is a [`GateDescriptor`].
    fn is_gate(&self) -> bool {
        !self.is_application() && self.system_type().is_gate()
    }
}


c_enum! {
    /// Discriminate types of segment descriptors that are not code or data.
    pub enum SystemDescriptorType(u8) {
        /// A [`SegmentDescriptor`] for a 16-bit task state segment (TSS) that is not
        /// currently running or waiting on a call to another task.
        #[cfg(any(target_arch = "x86", doc))]
        #[doc(cfg(target_arch = "x86"))]
        TaskStateAvailable16Bit = 1,

        /// A [`SegmentDescriptor`] for a segment that contains a local descriptor table.
        LocalDescriptorTable = 2,

        /// A [`SegmentDescriptor`] for a 16-bit task state segment (TSS) that is either
        /// running or waiting on a call to another task.
        #[cfg(any(target_arch = "x86", doc))]
        #[doc(cfg(target_arch = "x86"))]
        TaskStateBusy16Bit = 3,

        /// A [`GateDescriptor`] for a call to 16-bit code.
        #[cfg(any(target_arch = "x86", doc))]
        #[doc(cfg(target_arch = "x86"))]
        CallGate16Bit = 4,

        /// A [`GateDescriptor`] for task switching.
        #[cfg(any(target_arch = "x86", doc))]
        #[doc(cfg(target_arch = "x86"))]
        TaskGate = 5,

        /// A [`GateDescriptor`] for a 16-bit interrupt handler.
        #[cfg(any(target_arch = "x86", doc))]
        #[doc(cfg(target_arch = "x86"))]
        InterruptGate16Bit = 6,

        /// A [`GateDescriptor`] for a 16-bit trap handler.
        #[cfg(any(target_arch = "x86", doc))]
        #[doc(cfg(target_arch = "x86"))]
        TrapGate16Bit = 7,

        /// A [`SegmentDescriptor`] for a 32/64-bit task state segment (TSS) that is not
        /// currently running or waiting on a call to another task.
        TaskStateAvailable = 9,

        /// A [`SegmentDescriptor`] for a 32/64-bit task state segment (TSS) that is
        /// either running or waiting on a call to another task.
        TaskStateBusy = 11,

        /// A [`GateDescriptor`] for a call to 32/64-bit code.
        CallGate = 12,

        /// A [`GateDescriptor`] for a 32/64-bit interrupt handler.
        InterruptGate = 14,

        /// A [`GateDescriptor`] for a 32/64-bit interrupt handler.
        ///
        /// A trap gate works identically to an interrupt gate, except that the processor
        /// does not automatically clear [`FlagRegister::interrupt_enabled`] when it
        /// invokes the handler through a trap gate.
        TrapGate = 15,
    }
}

impl SystemDescriptorType {
    /// Indicates that this is a [`GateDescriptor`].
    pub fn is_gate(self) -> bool {
        #[cfg(target_arch = "x86")]
        {
            matches!(
                self,
                Self::CallGate
                    | Self::CallGate16Bit
                    | Self::InterruptGate
                    | Self::InterruptGate16Bit
                    | Self::TrapGate
                    | Self::TrapGate16Bit
                    | Self::TaskGate
            )
        }

        #[cfg(target_arch = "x86_64")]
        {
            matches!(self, Self::CallGate | Self::InterruptGate | Self::TrapGate)
        }
    }
}



/// Generic entry in a global/local/interrupt descriptor table. Can be a
/// [`SegmentDescriptor`] or [`GateDescriptor`], depending on the type flags.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GenericDescriptor {
    lower: u32,

    /// Common descriptor settings.
    pub flags: GenericDescriptorFlags,

    #[cfg(any(target_arch = "x86_64", doc))]
    #[doc(cfg(target_arch = "x86_64"))]
    upper: u32,

    // NOTE: In some cases, The processor verifies that this isn't a 32-bit descriptor by
    // looking for the type field (bits 8..13) in this DWord and making sure it is 0.
    #[cfg(any(target_arch = "x86_64", doc))]
    #[doc(cfg(target_arch = "x86_64"))]
    _reserved: u32,
}

#[cfg(target_arch = "x86")]
const_assert_eq!(8, core::mem::size_of::<GenericDescriptor>());

#[cfg(target_arch = "x86_64")]
const_assert_eq!(16, core::mem::size_of::<GenericDescriptor>());


bitfield_without_debug! {
    /// Settings for [`GenericDescriptor`]s.
    pub struct GenericDescriptorFlags(u32) {}
}

impl fmt::Debug for GenericDescriptorFlags {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut struct_fmt = f.debug_struct("GenericDescriptorFlags");
        struct_fmt.field("<value>", &self.0);
        self.fmt_fields(&mut struct_fmt);
        <Self as DescriptorFlags>::fmt_fields(&self, &mut struct_fmt);
        struct_fmt.finish()
    }
}

impl DescriptorFlags for GenericDescriptorFlags {}

/// An entry in a segment descriptor table that defines a new segment. This includes code,
/// data, task state (TSS), and local descriptor table (LDT) segments.
///
/// Notes on the size of this structure:
///   * In 32-bit mode, this structure is always 8 bytes and contains a 32-bit base
///     address.
///   * In 64-bit mode:
///     * Task state and local descriptor table segment descriptors are expanded to 16
///       bytes with 64-bit base addresses.
///     * From the processor's perspective, code and data segments remain 8 bytes and the
///       address and limit are both ignored. However, this structure is still defined as
///       16 bytes. This is fine, since the first 8 bytes are compatible and the rest will
///       be ignored.
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct SegmentDescriptor {
    lower: u32,

    /// Common segment descriptor settings.
    pub flags: SegmentDescriptorFlags,

    #[cfg(any(target_arch = "x86_64", doc))]
    #[doc(cfg(target_arch = "x86_64"))]
    upper: u32,

    // NOTE: In some cases, The processor verifies that this isn't a 32-bit descriptor by
    // looking for the type field (bits 8..13) in this DWord and making sure it is 0.
    #[cfg(any(target_arch = "x86_64", doc))]
    #[doc(cfg(target_arch = "x86_64"))]
    _reserved: u32,
}

#[cfg(target_arch = "x86")]
const_assert_eq!(8, core::mem::size_of::<SegmentDescriptor>());

#[cfg(target_arch = "x86_64")]
const_assert_eq!(16, core::mem::size_of::<SegmentDescriptor>());

impl SegmentDescriptor {
    /// The maximum supported value of the [`limit`](Self::limit) field (20 bits).
    pub const LIMIT_MAX: u32 = 0x000f_ffff;
    const LIMIT_MASK_LOWER: u32 = 0x0000_ffff;
    const LIMIT_MASK_FLAGS: u32 = 0x000f_0000;

    const ADDRESS_MASK_LOWER: u32 = 0xffff_0000;
    const ADDRESS_MASK_FLAGS_LOWER: u32 = 0x0000_00ff;
    const ADDRESS_MASK_FLAGS_UPPER: u32 = 0xff00_0000;

    /// Create a zero-initialized descriptor
    pub const fn new() -> Self {
        #[cfg(target_arch = "x86")]
        let value = Self { lower: 0, flags: SegmentDescriptorFlags(0) };

        #[cfg(target_arch = "x86_64")]
        let value =
            Self { lower: 0, flags: SegmentDescriptorFlags(0), upper: 0, _reserved: 0 };

        value
    }

    /// Base virtual address of the segment, to which offsets are added.
    ///
    /// In 64-bit mode, this is ignored and assumed to be 0 for code and data segments,
    /// but it still applies to task state and local descriptor table segments.
    pub fn address(self) -> usize {
        let mut address = ((self.lower & Self::ADDRESS_MASK_LOWER) >> 16) as usize;
        address |= ((Self::ADDRESS_MASK_FLAGS_LOWER & self.flags.value()) << 16) as usize;
        address |= (Self::ADDRESS_MASK_FLAGS_UPPER & self.flags.value()) as usize;
        #[cfg(target_arch = "x86_64")]
        {
            address |= (self.upper as usize) << 32;
        }
        address
    }

    /// Update the base address.
    pub fn set_address(&mut self, address: usize) {
        #![allow(clippy::cast_possible_truncation)]

        self.lower &= !Self::ADDRESS_MASK_LOWER;
        self.lower |= (address << 16) as u32 & Self::ADDRESS_MASK_LOWER;

        self.flags.0 &= !Self::ADDRESS_MASK_FLAGS_LOWER & !Self::ADDRESS_MASK_FLAGS_UPPER;
        self.flags.0 |= Self::ADDRESS_MASK_FLAGS_LOWER & (address >> 16) as u32;
        self.flags.0 |= Self::ADDRESS_MASK_FLAGS_UPPER & address as u32;

        #[cfg(target_arch = "x86_64")]
        {
            self.upper = (address >> 32) as u32;
        }
    }

    /// The "limit" of the segment, which is a maximum or minimum offset from the base
    /// address.
    ///
    /// If this is a stack-like data segment
    /// ([`data_expand_down`](DescriptorFlags::data_expand_down)), then this value is the
    /// *exclusive minimum* offset value. Otherwise, this is the *inclusive maximum*
    /// offset value (i.e., size - 1).
    ///
    /// This value may be in bytes or in 4KB units, depending on
    /// [`flags.granularity`](SegmentDescriptorFlags::granularity).
    ///
    /// In 64-bit mode, this is ignored (all limit checks are disabled) for code and data
    /// segments, but it still applies to task state and local descriptor table segments.
    pub fn limit(self) -> u32 {
        let mut limit = self.lower & Self::LIMIT_MASK_LOWER;
        limit |= self.flags.value() & Self::LIMIT_MASK_FLAGS;
        limit
    }

    /// Update the segment limit.
    ///
    /// # Panics
    /// Panics if the limit is greater than [`LIMIT_MAX`](Self::LIMIT_MAX).
    pub fn set_limit(&mut self, limit: u32) {
        assert!(limit <= Self::LIMIT_MAX, "Segment limit too large: {:#x}", limit);

        self.lower &= !Self::LIMIT_MASK_LOWER;
        self.lower |= Self::LIMIT_MASK_LOWER & limit;

        self.flags.0 &= !Self::LIMIT_MASK_FLAGS;
        self.flags.0 |= Self::LIMIT_MASK_FLAGS & limit;
    }
}

impl fmt::Debug for SegmentDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SegmentDescriptor")
            .field("address", &self.address())
            .field("limit", &self.limit())
            .field("flags", &self.flags)
            .finish()
    }
}

impl Default for SegmentDescriptor {
    fn default() -> Self {
        Self::new()
    }
}


bitfield_without_debug! {
    /// Settings for [`SegmentDescriptor`]s.
    pub struct SegmentDescriptorFlags(u32) {
        /// `AVL`: Ignored bit that can be used by the operating system.
        ///
        /// Does not apply to call gates.
        [20] pub os_defined,

        /// `G`: Indicates that the segment limit is in units of 4KB. Otherwise, it is in
        /// bytes.
        [23] pub granularity,
    }
}

impl fmt::Debug for SegmentDescriptorFlags {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut struct_fmt = f.debug_struct("SegmentDescriptorFlags");
        struct_fmt.field("<value>", &self.0);
        self.fmt_fields(&mut struct_fmt);
        <Self as DescriptorFlags>::fmt_fields(&self, &mut struct_fmt);
        struct_fmt.finish()
    }
}

impl DescriptorFlags for SegmentDescriptorFlags {}


/// An entry in a segment descriptor table that points to an existing segment rather than
/// defining a new one.
///
/// This includes:
///   * Call gates, which allow controlled access to routines defined in a code segment
///     with a different priority level or word size.
///   * Interrupt and trap gates, which define handlers for interrupt vectors.
///   * Task gates, which support hardware task switching. These are not supported in
///     64-bit mode.
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct GateDescriptor {
    lower: u32,

    /// Common gate descriptor settings.
    pub flags: GateDescriptorFlags,

    #[cfg(any(target_arch = "x86_64", doc))]
    #[doc(cfg(target_arch = "x86_64"))]
    upper: u32,

    // NOTE: In some cases, The processor verifies that this isn't a 32-bit descriptor by
    // looking for the type field (bits 8..13) in this DWord and making sure it is 0.
    #[cfg(any(target_arch = "x86_64", doc))]
    #[doc(cfg(target_arch = "x86_64"))]
    _reserved: u32,
}

#[cfg(target_arch = "x86")]
const_assert_eq!(8, core::mem::size_of::<GateDescriptor>());

#[cfg(target_arch = "x86_64")]
const_assert_eq!(16, core::mem::size_of::<GateDescriptor>());

impl GateDescriptor {
    const SELECTOR_MASK: u32 = 0xffff_0000;
    const OFFSET_MASK_LOWER: u32 = 0x0000_ffff;
    const OFFSET_MASK_UPPER: u32 = 0xffff_0000;

    /// Create a zero-initialized descriptor
    pub const fn new() -> Self {
        #[cfg(target_arch = "x86")]
        let value = Self { lower: 0, flags: GateDescriptorFlags(0) };

        #[cfg(target_arch = "x86_64")]
        let value =
            Self { lower: 0, flags: GateDescriptorFlags(0), upper: 0, _reserved: 0 };

        value
    }

    /// Selector that points to the code or task state segment to be accessed through this
    /// gate.
    ///
    /// For call, interrupt, and trap gates, this points to the code segment that contains
    /// the routine to be executed. For task gates, this points to a task state segment
    /// representing the task to activate.
    pub fn selector(self) -> Selector {
        #![allow(clippy::cast_possible_truncation)]
        Selector(((self.lower & Self::SELECTOR_MASK) >> 16) as u16)
    }

    /// Update the selector pointing to the segment to be accessed through this gate.
    pub fn set_selector(&mut self, selector: Selector) {
        self.lower &= !Self::SELECTOR_MASK;
        self.lower |= Self::SELECTOR_MASK & u32::from(selector.value()) << 16;
    }

    /// Offset of the entry point in code segment referenced by
    /// [`selector`](Self::selector).
    ///
    /// Only applies to call, interrupt, and trap gates.
    pub fn entry_point_offset(self) -> usize {
        let mut offset = (self.lower & Self::OFFSET_MASK_LOWER) as usize;
        offset |= (self.flags.value() & Self::OFFSET_MASK_UPPER) as usize;
        #[cfg(target_arch = "x86_64")]
        {
            offset |= (self.upper as usize) << 32;
        }
        offset
    }

    /// Update the offset of the entry point within the referenced code segment.
    pub fn set_entry_point_offset(&mut self, offset: usize) {
        #![allow(clippy::cast_possible_truncation)]

        self.lower &= !Self::OFFSET_MASK_LOWER;
        self.lower |= Self::OFFSET_MASK_LOWER & (offset as u32);

        self.flags.0 &= !Self::OFFSET_MASK_UPPER;
        self.flags.0 |= Self::OFFSET_MASK_UPPER & (offset as u32);

        #[cfg(target_arch = "x86_64")]
        {
            self.upper = (offset >> 32) as u32;
        }
    }
}

impl fmt::Debug for GateDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("GateDescriptor")
            .field("selector", &self.selector())
            .field("entry_point_offset", &self.entry_point_offset())
            .field("flags", &self.flags)
            .finish()
    }
}

impl Default for GateDescriptor {
    fn default() -> Self {
        Self::new()
    }
}


bitfield_without_debug! {
    /// Settings for [`GateDescriptor`]s.
    pub struct GateDescriptorFlags(u32) {
        /// Number of stack parameters to copy if the code segment referenced by a call
        /// gate uses a different stack segment.
        ///
        /// Only applies to call gates.
        [0..4] pub call_param_count: u8,

        /// One-based index of the
        /// [`interrupt_stack`](x86_64::TaskStateSegment::interrupt_stack) pointer to use
        /// when handling an interrupt though this gate.
        ///
        /// If this value is zero, then the stack segment is set to null.
        ///
        /// Only applies to interrupt and trap gates.
        #[cfg(any(target_arch = "x86_64", doc))]
        #[doc(cfg(target_arch = "x86_64"))]
        [0..2] pub interrupt_stack_index: u8,
    }
}

impl fmt::Debug for GateDescriptorFlags {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut struct_fmt = f.debug_struct("GateDescriptorFlags");
        struct_fmt.field("<value>", &self.0);
        self.fmt_fields(&mut struct_fmt);
        <Self as DescriptorFlags>::fmt_fields(&self, &mut struct_fmt);
        struct_fmt.finish()
    }
}

impl DescriptorFlags for GateDescriptorFlags {}


/// Bitmap that controls which I/O port addresses are covered by privilege checks.
///
/// For bit `N` of this structure (bit `N mod 8` of byte `floor(N / 8)`), up to
/// [`max_port`](Self::max_port):
///   * If the bit is set, then access to I/O port `N` is not allowed at privilege levels
///     below [`FlagRegister::io_privilege_level`] (greater numbers).
///   * If the bit is unset, then access to I/O port `N` is allowed from any privilege
///     level.
///
/// This structure is stored in the task state segment (TSS) at a variable location
/// indicated by its
/// [`io_permission_map_offset`](TaskStateSegmentHeader::io_permission_map_offset). It is
/// also variable-sized: if there are fewer than [`MAX_SIZE`](Self::MAX_SIZE) bytes
/// between the start of the permission map and the [`limit`](SegmentDescriptor::limit) of
/// the containing TSS, then the processor acts as though the bits for all ports past
/// `max_port_for_size(limit - offset)` are set.
#[derive(Debug, PartialEq, Eq)]
pub struct IOPermissionBitmap<T = [u8]>(T)
where
    T: AsRef<[u8]> + AsMut<[u8]> + Eq + ?Sized;

impl IOPermissionBitmap {
    /// Size in bytes required to map all I/O ports
    pub const MAX_SIZE: usize = Self::required_size(u16::MAX);

    /// Calculate the size in bytes of a map that has bits for ports up to and including
    /// `max_port`.
    ///
    /// Note that because of the way the processor reads this map, it requires an extra
    /// byte at the end that does not map any ports.
    ///
    /// ```
    /// # use tartan_arch::x86_common::protection::IOPermissionBitmap;
    /// #
    /// assert_eq!(IOPermissionBitmap::required_size(0), 2);
    /// assert_eq!(IOPermissionBitmap::required_size(7), 2);
    /// assert_eq!(IOPermissionBitmap::required_size(8), 3);
    /// assert_eq!(IOPermissionBitmap::required_size(0xa587), 0x14b2);
    /// assert_eq!(IOPermissionBitmap::required_size(0xffff), 0x2001);
    /// ```
    pub const fn required_size(max_port: u16) -> usize {
        max_port as usize / 8 + 2
    }

    /// Calculate the last I/O port that is covered by a map of the given size.
    ///
    /// Note that because of the way the processor reads this map, it requires an extra
    /// byte at the end that does not map any ports. Therefore, empty or single-byte maps
    /// cannot represent _any_ ports, and this function returns `None` in those cases.
    ///
    /// ```
    /// # use core::num::NonZeroU16;
    /// # use tartan_arch::x86_common::protection::IOPermissionBitmap;
    /// #
    /// assert_eq!(IOPermissionBitmap::max_port_for_size(0), None);
    /// assert_eq!(IOPermissionBitmap::max_port_for_size(1), None);
    /// assert_eq!(IOPermissionBitmap::max_port_for_size(2), NonZeroU16::new(7));
    /// assert_eq!(IOPermissionBitmap::max_port_for_size(4), NonZeroU16::new(23));
    /// assert_eq!(IOPermissionBitmap::max_port_for_size(0x2000), NonZeroU16::new(0xfff7));
    /// assert_eq!(IOPermissionBitmap::max_port_for_size(0x2001), NonZeroU16::new(0xffff));
    /// assert_eq!(IOPermissionBitmap::max_port_for_size(0x2002), NonZeroU16::new(0xffff));
    /// assert_eq!(IOPermissionBitmap::max_port_for_size(usize::MAX), NonZeroU16::new(0xffff));
    /// ```
    pub const fn max_port_for_size(size: usize) -> Option<NonZeroU16> {
        #[allow(clippy::cast_possible_truncation)]
        if size <= 1 {
            None
        } else if size >= Self::MAX_SIZE {
            NonZeroU16::new(u16::MAX)
        } else {
            NonZeroU16::new((((size - 1) * 8) - 1) as u16)
        }
    }
}

impl<T> IOPermissionBitmap<T>
where
    T: AsRef<[u8]> + AsMut<[u8]> + Eq + ?Sized,
{
    /// The number of bytes in this structure.
    pub fn size(&self) -> usize {
        self.0.as_ref().len()
    }

    /// The last I/O port that is covered by this map. The bits for all ports greater than
    /// this value are assumed to be set.
    pub fn max_port(&self) -> Option<NonZeroU16> {
        IOPermissionBitmap::max_port_for_size(self.size())
    }

    fn is_port_mapped(&self, port: u16) -> bool {
        match self.max_port() {
            None => false,
            Some(max) => port <= max.get(),
        }
    }

    /// Get the value in this bitmap that indicates whether the port should be subject to
    /// privilege level checks.
    ///
    /// If the given port is beyond the range covered by this map
    /// [`max_port`](Self::max_port), this will return true, in line with the processor's
    /// behavior.
    pub fn is_port_checked(&self, port: u16) -> bool {
        if self.is_port_mapped(port) {
            let byte = (port / 8) as usize;
            #[allow(clippy::cast_possible_truncation)]
            let bit = (port % 8) as u8;
            get_bit(self.0.as_ref()[byte], bit)
        } else {
            // All remaining ports are assumed to be covered by privilege checks
            true
        }
    }

    /// Set the value in this bitmap that indicates whether the port should be subject
    /// to privilege level checks.
    ///
    /// # Panics
    /// Panics if the given port is greater than [`max_port`](Self::max_port).
    pub fn set_port_checked(&mut self, port: u16, value: bool) {
        assert!(
            self.is_port_mapped(port),
            "Port {:x} is beyond the maximum value {:x?} supported by this map",
            port,
            self.max_port(),
        );
        let byte = (port / 8) as usize;
        #[allow(clippy::cast_possible_truncation)]
        let bit = (port % 8) as u8;
        let map = self.0.as_mut();
        map[byte] = set_bit(map[byte], bit, value);
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use core::mem;

    #[cfg(target_arch = "x86_64")]
    fn seg_desc_from_bytes(bytes: [u8; 16]) -> SegmentDescriptor {
        unsafe { mem::transmute(bytes) }
    }

    #[cfg(target_arch = "x86_64")]
    fn bytes_from_seg_desc(desc: SegmentDescriptor) -> [u8; 16] {
        unsafe { mem::transmute(desc) }
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    #[rustfmt::skip]
    fn test_descriptor_limit() {
        assert_eq!(0x0000_0000, seg_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]).limit());
        assert_eq!(0x0000_0000, seg_desc_from_bytes([
            0x00, 0x00, 0xff, 0xff,
            0xff, 0xff, 0xf0, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]).limit());
        assert_eq!(0x000f_ffff, seg_desc_from_bytes([
            0xff, 0xff, 0x00, 0x00,
            0x00, 0x00, 0x0f, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]).limit());
        assert_eq!(0x000a_bcde, seg_desc_from_bytes([
            0xde, 0xbc, 0x74, 0x48,
            0x52, 0x93, 0x8a, 0x51,
            0x72, 0x89, 0x73, 0x21,
            0x28, 0x05, 0x86, 0x85,
        ]).limit());

        let mut desc = seg_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);
        desc.set_limit(0x000f_ffff);
        assert_eq!(bytes_from_seg_desc(desc), [
            0xff, 0xff, 0x00, 0x00,
            0x00, 0x00, 0x0f, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);

        let mut desc = seg_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);
        desc.set_limit(0x000a_bcde);
        assert_eq!(bytes_from_seg_desc(desc), [
            0xde, 0xbc, 0x00, 0x00,
            0x00, 0x00, 0x0a, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);

        let mut desc = seg_desc_from_bytes([
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);
        desc.set_limit(0x0000_0000);
        assert_eq!(bytes_from_seg_desc(desc), [
            0x00, 0x00, 0xff, 0xff,
            0xff, 0xff, 0xf0, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);

        let mut desc = seg_desc_from_bytes([
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);
        desc.set_limit(0x000a_bcde);
        assert_eq!(bytes_from_seg_desc(desc), [
            0xde, 0xbc, 0xff, 0xff,
            0xff, 0xff, 0xfa, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);
    }

    #[test]
    #[should_panic]
    #[cfg(target_arch = "x86_64")]
    #[rustfmt::skip]
    fn test_descriptor_limit_out_of_range() {
        let mut desc = seg_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);
        desc.set_limit(0x0010_0000);
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    #[rustfmt::skip]
    fn test_descriptor_address() {
        assert_eq!(0x0000_0000_0000_0000_usize, seg_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]).address());
        assert_eq!(0x0000_0000_0000_0000_usize, seg_desc_from_bytes([
            0xff, 0xff, 0x00, 0x00,
            0x00, 0xff, 0xff, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0xff, 0xff, 0xff, 0xff,
        ]).address());
        assert_eq!(0xffff_ffff_ffff_ffff_usize, seg_desc_from_bytes([
            0x00, 0x00, 0xff, 0xff,
            0xff, 0x00, 0x00, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0x00, 0x00, 0x00, 0x00,
        ]).address());
        assert_eq!(0xaabb_ccdd_1122_3344_usize, seg_desc_from_bytes([
            0x8f, 0x97, 0x44, 0x33,
            0x22, 0x68, 0x5e, 0x11,
            0xdd, 0xcc, 0xbb, 0xaa,
            0xf8, 0x76, 0x89, 0xe5,
        ]).address());

        let mut desc = seg_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);
        desc.set_address(0xffff_ffff_ffff_ffff_usize);
        assert_eq!(bytes_from_seg_desc(desc), [
            0x00, 0x00, 0xff, 0xff,
            0xff, 0x00, 0x00, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0x00, 0x00, 0x00, 0x00,
        ]);

        let mut desc = seg_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);
        desc.set_address(0xaabb_ccdd_1122_3344_usize);
        assert_eq!(bytes_from_seg_desc(desc), [
            0x00, 0x00, 0x44, 0x33,
            0x22, 0x00, 0x00, 0x11,
            0xdd, 0xcc, 0xbb, 0xaa,
            0x00, 0x00, 0x00, 0x00,
        ]);

        let mut desc = seg_desc_from_bytes([
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);
        desc.set_address(0x0000_0000_0000_0000_usize);
        assert_eq!(bytes_from_seg_desc(desc), [
            0xff, 0xff, 0x00, 0x00,
            0x00, 0xff, 0xff, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0xff, 0xff, 0xff, 0xff,
        ]);

        let mut desc = seg_desc_from_bytes([
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);
        desc.set_address(0xaabb_ccdd_1122_3344_usize);
        assert_eq!(bytes_from_seg_desc(desc), [
            0xff, 0xff, 0x44, 0x33,
            0x22, 0xff, 0xff, 0x11,
            0xdd, 0xcc, 0xbb, 0xaa,
            0xff, 0xff, 0xff, 0xff,
        ]);
    }

    #[cfg(target_arch = "x86_64")]
    fn gate_desc_from_bytes(bytes: [u8; 16]) -> GateDescriptor {
        unsafe { mem::transmute(bytes) }
    }

    #[cfg(target_arch = "x86_64")]
    fn bytes_from_gate_desc(desc: GateDescriptor) -> [u8; 16] {
        unsafe { mem::transmute(desc) }
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    #[rustfmt::skip]
    fn test_descriptor_offset() {
        assert_eq!(0x0000_0000_0000_0000_usize, gate_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]).entry_point_offset());
        assert_eq!(0x0000_0000_0000_0000_usize, gate_desc_from_bytes([
            0x00, 0x00, 0xff, 0xff,
            0xff, 0xff, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0xff, 0xff, 0xff, 0xff,
        ]).entry_point_offset());
        assert_eq!(0xffff_ffff_ffff_ffff_usize, gate_desc_from_bytes([
            0xff, 0xff, 0x00, 0x00,
            0x00, 0x00, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0x00, 0x00, 0x00, 0x00,
        ]).entry_point_offset());
        assert_eq!(0xaabb_ccdd_1122_3344_usize, gate_desc_from_bytes([
            0x44, 0x33, 0x8f, 0x97,
            0x68, 0x5e, 0x22, 0x11,
            0xdd, 0xcc, 0xbb, 0xaa,
            0xf8, 0x76, 0x89, 0xe5,
        ]).entry_point_offset());

        let mut desc = gate_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);
        desc.set_entry_point_offset(0xffff_ffff_ffff_ffff_usize);
        assert_eq!(bytes_from_gate_desc(desc), [
            0xff, 0xff, 0x00, 0x00,
            0x00, 0x00, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0x00, 0x00, 0x00, 0x00,
        ]);

        let mut desc = gate_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);
        desc.set_entry_point_offset(0xaabb_ccdd_1122_3344_usize);
        assert_eq!(bytes_from_gate_desc(desc), [
            0x44, 0x33, 0x00, 0x00,
            0x00, 0x00, 0x22, 0x11,
            0xdd, 0xcc, 0xbb, 0xaa,
            0x00, 0x00, 0x00, 0x00,
        ]);

        let mut desc = gate_desc_from_bytes([
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);
        desc.set_entry_point_offset(0x0000_0000_0000_0000_usize);
        assert_eq!(bytes_from_gate_desc(desc), [
            0x00, 0x00, 0xff, 0xff,
            0xff, 0xff, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0xff, 0xff, 0xff, 0xff,
        ]);

        let mut desc = gate_desc_from_bytes([
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);
        desc.set_entry_point_offset(0xaabb_ccdd_1122_3344_usize);
        assert_eq!(bytes_from_gate_desc(desc), [
            0x44, 0x33, 0xff, 0xff,
            0xff, 0xff, 0x22, 0x11,
            0xdd, 0xcc, 0xbb, 0xaa,
            0xff, 0xff, 0xff, 0xff,
        ]);
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    #[rustfmt::skip]
    fn test_descriptor_selector() {
        assert_eq!(Selector(0x0000), gate_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]).selector());
        assert_eq!(Selector(0x0000), gate_desc_from_bytes([
            0xff, 0xff, 0x00, 0x00,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]).selector());
        assert_eq!(Selector(0xffff), gate_desc_from_bytes([
            0x00, 0x00, 0xff, 0xff,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]).selector());
        assert_eq!(Selector(0xabcd), gate_desc_from_bytes([
            0x94, 0x82, 0xcd, 0xab,
            0x52, 0x93, 0x83, 0x51,
            0x72, 0x89, 0x73, 0x21,
            0x28, 0x05, 0x86, 0x85,
        ]).selector());

        let mut desc = gate_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);
        desc.set_selector(0xffff.into());
        assert_eq!(bytes_from_gate_desc(desc), [
            0x00, 0x00, 0xff, 0xff,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);

        let mut desc = gate_desc_from_bytes([
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);
        desc.set_selector(0xabcd.into());
        assert_eq!(bytes_from_gate_desc(desc), [
            0x00, 0x00, 0xcd, 0xab,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ]);

        let mut desc = gate_desc_from_bytes([
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);
        desc.set_selector(0x0000.into());
        assert_eq!(bytes_from_gate_desc(desc), [
            0xff, 0xff, 0x00, 0x00,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);

        let mut desc = gate_desc_from_bytes([
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);
        desc.set_selector(0xabcd.into());
        assert_eq!(bytes_from_gate_desc(desc), [
            0xff, 0xff, 0xcd, 0xab,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff,
        ]);
    }

    #[test]
    #[allow(clippy::bool_assert_comparison)]
    fn test_is_port_checked() {
        let map = IOPermissionBitmap([]);
        assert_eq!(map.is_port_checked(0), true);
        assert_eq!(map.is_port_checked(1), true);
        assert_eq!(map.is_port_checked(u16::MAX), true);

        let map = IOPermissionBitmap([0xff]);
        assert_eq!(map.is_port_checked(0), true);
        assert_eq!(map.is_port_checked(1), true);
        assert_eq!(map.is_port_checked(8), true);
        assert_eq!(map.is_port_checked(u16::MAX), true);

        let map = IOPermissionBitmap([0x00, 0xff]);
        assert_eq!(map.is_port_checked(0), false);
        assert_eq!(map.is_port_checked(1), false);
        assert_eq!(map.is_port_checked(7), false);
        assert_eq!(map.is_port_checked(8), true);
        assert_eq!(map.is_port_checked(u16::MAX), true);

        let map = IOPermissionBitmap([0xaa, 0xff]);
        assert_eq!(map.is_port_checked(0), false);
        assert_eq!(map.is_port_checked(1), true);
        assert_eq!(map.is_port_checked(2), false);
        assert_eq!(map.is_port_checked(3), true);
        assert_eq!(map.is_port_checked(4), false);
        assert_eq!(map.is_port_checked(5), true);
        assert_eq!(map.is_port_checked(6), false);
        assert_eq!(map.is_port_checked(7), true);
        assert_eq!(map.is_port_checked(8), true);
        assert_eq!(map.is_port_checked(u16::MAX), true);

        let map = IOPermissionBitmap([0x55, 0xff]);
        assert_eq!(map.is_port_checked(0), true);
        assert_eq!(map.is_port_checked(1), false);
        assert_eq!(map.is_port_checked(2), true);
        assert_eq!(map.is_port_checked(3), false);
        assert_eq!(map.is_port_checked(4), true);
        assert_eq!(map.is_port_checked(5), false);
        assert_eq!(map.is_port_checked(6), true);
        assert_eq!(map.is_port_checked(7), false);
        assert_eq!(map.is_port_checked(8), true);
        assert_eq!(map.is_port_checked(u16::MAX), true);

        let map = IOPermissionBitmap([0xff, 0x00, 0xff]);
        assert_eq!(map.is_port_checked(0), true);
        assert_eq!(map.is_port_checked(7), true);
        assert_eq!(map.is_port_checked(8), false);
        assert_eq!(map.is_port_checked(15), false);
        assert_eq!(map.is_port_checked(16), true);
        assert_eq!(map.is_port_checked(u16::MAX), true);

        // The processor expects the last byte to be 0xff, and it doesn't map any ports.
        // For our purposes, we ignore it and treat it as though it were 0xff.
        let map = IOPermissionBitmap([0x00]);
        assert_eq!(map.is_port_checked(0), true);
        assert_eq!(map.is_port_checked(1), true);
        assert_eq!(map.is_port_checked(7), true);
        assert_eq!(map.is_port_checked(8), true);
        assert_eq!(map.is_port_checked(u16::MAX), true);

        let map = IOPermissionBitmap([0x00, 0x00]);
        assert_eq!(map.is_port_checked(0), false);
        assert_eq!(map.is_port_checked(1), false);
        assert_eq!(map.is_port_checked(7), false);
        assert_eq!(map.is_port_checked(8), true);
        assert_eq!(map.is_port_checked(15), true);
        assert_eq!(map.is_port_checked(16), true);
        assert_eq!(map.is_port_checked(u16::MAX), true);
    }

    #[test]
    fn test_set_port_checked() {
        let mut map = IOPermissionBitmap([0x00, 0x00, 0xff]);
        map.set_port_checked(0, true);
        assert_eq!(map.0, [0x01, 0x00, 0xff]);

        let mut map = IOPermissionBitmap([0x00, 0x00, 0xff]);
        map.set_port_checked(15, true);
        assert_eq!(map.0, [0x00, 0x80, 0xff]);

        let mut map = IOPermissionBitmap([0xff, 0xff, 0xff]);
        map.set_port_checked(0, false);
        assert_eq!(map.0, [0xfe, 0xff, 0xff]);

        let mut map = IOPermissionBitmap([0xff, 0xff, 0xff]);
        map.set_port_checked(15, false);
        assert_eq!(map.0, [0xff, 0x7f, 0xff]);
    }

    #[test]
    #[should_panic]
    fn test_set_port_checked_out_of_range_3byte() {
        let mut map = IOPermissionBitmap([0x00, 0x00, 0xff]);
        map.set_port_checked(16, true);
    }

    #[test]
    #[should_panic]
    fn test_set_port_checked_out_of_range_2byte() {
        let mut map = IOPermissionBitmap([0x00, 0xff]);
        map.set_port_checked(8, true);
    }

    #[test]
    #[should_panic]
    fn test_set_port_checked_out_of_range_1byte() {
        let mut map = IOPermissionBitmap([0xff]);
        map.set_port_checked(0, true);
    }

    #[test]
    #[should_panic]
    fn test_set_port_checked_out_of_range_empty() {
        let mut map = IOPermissionBitmap([]);
        map.set_port_checked(0, true);
    }
}
