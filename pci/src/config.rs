use crate::INVALID_VENDOR;
use tartan_bitfield::bitfield;


/// A register type that is always located at a specific offset
pub trait FixedConfigRegister: From<u32> {
    /// Fixed offset for this register type
    const REGISTER_NUMBER: u8;
}


bitfield! {
    /// The first register of the standard configuration header for all PCI devices.
    pub struct HeaderRegister0(u32) {
        /// Vendor-defined device ID
        [16..32] pub device: u16,
        /// ID of the chipset manufacturer for this function, assigned by PCI-SIG
        [ 0..16] pub vendor: u16,
    }
}

impl HeaderRegister0 {
    /// Return false if the function is not present, as indicated by an invalid vendor ID.
    pub fn valid(self) -> bool {
        self.vendor() != INVALID_VENDOR
    }
}

impl FixedConfigRegister for HeaderRegister0 {
    const REGISTER_NUMBER: u8 = 0;
}


bitfield! {
    /// The second register of the standard configuration header for all PCI devices.
    pub struct HeaderRegister1(u32) {
        /// Status bits indicated by the function
        [16..32] pub status:  u16 as StatusRegister,
        /// Control bits set by the host
        [ 0..16] pub command: u16 as CommandRegister,
    }
}

impl FixedConfigRegister for HeaderRegister1 {
    const REGISTER_NUMBER: u8 = 1;
}


bitfield! {
    /// The third register of the standard configuration header for all PCI devices.
    pub struct HeaderRegister2(u32) {
        /// Indicates the general purpose of the function, from a list of IDs defined by
        /// PCI-SIG.
        [24..32] pub class: u8,
        /// Indicates the purpose of the function more specifically, from a list of IDs
        /// defined by PCI-SIG for the corresponding [`class`]. Some classes do not have
        /// any meaningful subclasses defined.
        [16..24] pub subclass: u8,
        /// Indicates a standard programming interface that can be used by a generic
        /// driver. The possible IDs are defined by PCI-SIG for the corresponding
        /// [`class`] and [`subclass`]. Some subclasses do not have any meaningful
        /// interfaces defined.
        [ 8..16] pub interface: u8,
        /// Vendor-defined revision of the hardware
        [ 0.. 8] pub revision: u8,
    }
}

impl FixedConfigRegister for HeaderRegister2 {
    const REGISTER_NUMBER: u8 = 2;
}


bitfield! {
    /// The fourth register of the standard configuration header for all PCI devices.
    pub struct HeaderRegister3(u32) {
        /// Control and status flags for a function's self test capability, if present.
        [24..32] pub self_test: u8 as SelfTest,
        /// If true, then this device responds to at least some function numbers >= 1.
        /// The flag is only meaningful on function zero.
        [23    ] pub multi_function,
        /// Indicates which of the standard PCI configuration header formats that this
        /// function supports, and indirectly, whether this function is a PCI or CardBus
        /// bridge.
        ///
        /// Currently, the defined values are:
        ///   * `0x00` - general purpose
        ///   * `0x01` - PCI bridge
        ///   * `0x02` - PCI-to-CardBus bridge
        [16..23] pub header_type: u8,
        /// The value of the bus master's latency timer in PCI clock cycles
        [ 8..16] pub latency_timer: u8,
        /// Informs the device of the size of the system's cache lines, in 32-bit units.
        [ 0.. 8] pub cache_line_size: u8,
    }
}

impl FixedConfigRegister for HeaderRegister3 {
    const REGISTER_NUMBER: u8 = 3;
}


bitfield! {
    /// Twelfth register of the standard configuration header for general-purpose devices
    /// (header type `0x00`).
    pub struct Type0HeaderRegister11(u32) {
        /// ID of the device as defined by the card manufacturer.
        [16..32] pub subsystem: u16,
        /// ID of the card manufacturer, which may be different than the chipset
        /// manufacturer. Uses the same values defined by PCI-SIG for the vendor ID.
        [ 0..16] pub subsystem_vendor: u16,
    }
}

impl FixedConfigRegister for Type0HeaderRegister11 {
    const REGISTER_NUMBER: u8 = 11;
}


bitfield! {
    /// Fourtheenth register of the standard configuration header for general-purpose
    /// devices.
    pub struct Type0HeaderRegister13(u32) {
        /// Offset within this function's configuration space that points to the first
        /// of a linked list of "capabilities" supported by the device. Only valid if the
        /// appropriate bit is set in the [`StatusRegister`].
        [0..8] pub capabilities_offset: u8,
    }
}

impl FixedConfigRegister for Type0HeaderRegister13 {
    const REGISTER_NUMBER: u8 = 13;
}


bitfield! {
    /// Sixteenth register of the standard configuration header for general-purpose
    /// devices.
    pub struct Type0HeaderRegister15(u32) {
        /// Indicates the longest period the device can wait to access the PCI bus, in
        /// units of 0.25 µs.
        [24..32] pub max_latency: u8,
        /// Indicates the desired length of bursts in units of 0.25 µs.
        [16..24] pub min_grant: u8,
        /// Indicates which of the four interrupt pins on the PCI bus that the device uses
        /// (0 for `INTA#` through 3 for `INTC#`). Single-function devices always use
        /// `INTA#`.
        [ 8..16] pub interrupt_pin: u8,
        /// Indicates which system interrupt number the function uses, as defined by the
        /// system's interrupt controller.
        [ 0.. 8] pub interrupt_line: u8,
    }
}

impl FixedConfigRegister for Type0HeaderRegister15 {
    const REGISTER_NUMBER: u8 = 15;
}


bitfield! {
    /// Control bits set by the host
    pub struct CommandRegister(u16) {
        /// Controls whether the device is prohibited from asserting its interrupt pin.
        /// Does not apply to MSI interrupts.
        [10] pub interrupt_disabled,
        /// Controls whether the device may use fast back-to-back transactions to multiple
        /// devices when acting as a bus master.
        [ 9] pub fast_back_to_back_enabled,
        /// Controls whether the device may signal critical errors in addresses or
        /// special cycle operations.
        [ 8] pub system_error_enabled,
        /// Controls whether the device responds normally to parity errors (`true`) or
        /// whether it must simply set the parity error bit in the status register and
        /// ignore it (`false`).
        [ 6] pub parity_error_response,
        /// Controls whether a VGA or graphics card device may "snoop" on VGA palette
        /// buffer writes sent to a different device.
        [ 5] pub vga_palette_snoop,
        /// Controls whether the device may send a "Memory Write and Invalidate" command
        /// when acting as a bus master.
        [ 4] pub write_and_invalidate_enable,
        /// Controls whether the device may respond to "special cycle" messages, used to
        /// communicate system state to the device (e.g., shutting down, halted)
        [ 3] pub special_cycle,
        /// Controls whether the device may act as a master of the PCI bus
        [ 2] pub bus_master,
        /// Controls whether the device may respond to accesses through memory space
        [ 1] pub memory_space,
        /// Controls whether the device may respond to accesses through I/O space
        [ 0] pub io_space,
    }
}


bitfield! {
    /// Status bits indicated by the function
    pub struct StatusRegister(u16) {
        /// Indicates taht the device has detected a parity error, even if it is ignored.
        [15    ] pub parity_error_detected,
        /// Indicates that the device has detected a critical error in an address or
        /// special cycle operation.
        [14    ] pub system_error_signaled,
        /// Indicates whether the transaction was aborted with a Target-Abort while this
        /// device was acting as the bus master.
        [13    ] pub master_abort_received,
        /// Indicates whether the transaction was aborted with a Target-Abort.
        [12    ] pub target_abort_received,
        /// Indicates that this function generated an abort on the transaction (as opposed
        /// to a master abort).
        [11    ] pub target_abort_signaled,
        /// Indicates how quickly the device can assert the `DEVSEL#` pin.
        ///
        /// Valid values are:
        ///   * `0x0` - fast
        ///   * `0x1` - medium
        ///   * `0x2` - slow
        [ 9..11] pub device_select_timing: u8,
        /// Indicates whether a partity error was detected while this device was acting as
        /// the bus master.
        [ 8    ] pub master_parity_error,
        /// Indicates whether the device supports receiving back-to-back transactions that
        /// are targeted at multiple devices.
        [ 7    ] pub fast_back_to_back_capable,
        /// Indicates whether the device can run at 66 MHz (true).
        [ 5    ] pub double_clock_capable,
        /// Indicates whether the configuration space contains a linked list of
        /// capabilities, beginning at the offset listed in [`Type0HeaderRegister13`].
        [ 4    ] pub capabilities_list_available,
        /// Indicates whether the device wants to signal an interrupt, even if it is
        /// prohibited from asserting its interrupt pin.
        [ 3    ] pub interrupt_status,
    }
}


bitfield! {
    /// Control and status flags for a function's self test capability, if present.
    pub struct SelfTest(u8) {
        /// Indicates whether the function has a self-test capability.
        [7   ] pub capable,
        /// Setting this bit triggers the device to start its self test. Once the test is
        /// finished (successful or not), this is reset by the device.
        [6   ] pub start,
        /// If a self-test is complete, then any non-zero value in this field indicates
        /// that the test failed.
        [0..4] pub completion_code: u8,
    }
}


/// A pointer to a memory or I/O space that is used to interact with the function.
///
/// The meaning of the addressed space is defined by the specific function's interface.
#[repr(transparent)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct BaseAddressRegister(u32);

impl BaseAddressRegister {
    /// Indicates whether this BAR points to memory or I/O space
    pub fn address_space(self) -> AddressSpace {
        if self.0 & 1 == 0 {
            AddressSpace::Memory
        } else {
            AddressSpace::IO
        }
    }

    /// Indicates whether the host can safely cache the memory addressed by this BAR
    pub fn prefetchable(self) -> bool {
        if self.address_space() == AddressSpace::Memory {
            self.0 & 0b1000 != 0
        } else {
            false
        }
    }

    /// Size of address bus supported by this PCI function.
    ///
    /// Addresses in I/O space are always 32-bits wide.
    pub fn address_width(self) -> AddressWidth {
        if self.address_space() == AddressSpace::IO {
            AddressWidth::U32
        } else {
            match (self.0 & 0b0110) >> 1 {
                0 => AddressWidth::U32,
                2 => AddressWidth::U64,
                _ => AddressWidth::Invalid,
            }
        }
    }

    /// The address as a 32-bit number.
    ///
    /// If the [`address_width`](Self::address_width) for this BAR is 64 bits, then this
    /// will only return the lower half of the address.
    pub fn address_u32(self) -> u32 {
        if self.address_space() == AddressSpace::IO {
            self.0 & 0xffff_fffc
        } else {
            self.0 & 0xffff_fff0
        }
    }

    /// The address as a 64-bit number, when combined with the value of the following
    /// register.
    ///
    /// If the `address_width` of this BAR is 32 bits, then the value of the next register
    /// will be ignored. Thus, this is safe to use with any address width.
    pub fn address_u64(self, next_register: u32) -> u64 {
        let mut address = u64::from(self.address_u32());
        if self.address_width() == AddressWidth::U64 {
            address &= u64::from(next_register) << 32
        }
        address
    }
}

/// Differentiates between memory and I/O addresses
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressSpace {
    /// This address is in the normal memory address space
    Memory,
    /// This address is in I/O space (supported on x86/x86-64 only)
    IO,
}

/// Size of address bus supported by a PCI function
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressWidth {
    /// 32-bit address bus
    U32,
    /// 64-bit address bus
    U64,
    /// Unknown address bus width
    Invalid,
}


bitfield! {
    /// Structure shared by all capability registers
    pub struct GenericCapabilityRegister(u32) {
        /// Capability ID defined by PCI-SIG
        [ 0.. 8] pub id: u8,
        /// Byte offset within capability space of next capability register, or 0 if none.
        [ 8..16] pub next_offset: u8,
    }
}
