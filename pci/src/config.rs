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
