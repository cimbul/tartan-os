#![allow(clippy::module_name_repetitions)]

use crate::config::FixedConfigRegister;
use crate::{MAX_DEVICE, MAX_FUNCTION};
use core::ops::RangeInclusive;


/// Provides methods to access PCI configuration space.
///
/// Implementations may only offer access to a limited number of registers on a specific
/// range of segments/busses.
pub trait ConfigAccess {
    /// Get a specific 32-bit register in PCI configuration space.
    fn get_register(&self, selector: ConfigSelector, register: u16) -> u32;

    /// Set a specific 32-bit register in PCI configuration space.
    fn set_register(&self, selector: ConfigSelector, register: u16, value: u32);

    /// Get a register in PCI configuration space with an offset defined by the output
    /// type.
    fn get_fixed_register<T: FixedConfigRegister>(&self, selector: ConfigSelector) -> T {
        self.get_register(selector, T::REGISTER_NUMBER.into()).into()
    }

    /// Set a register in PCI configuration space with an offset defined by the input
    /// type.
    fn set_fixed_register<T: FixedConfigRegister>(
        &self,
        selector: ConfigSelector,
        value: T,
    ) {
        self.set_register(selector, T::REGISTER_NUMBER.into(), value.into());
    }
}


/// Logical PCI address for a function of a specific segment/bus/device.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ConfigSelector {
    /// Segment group as defined by PCI Express. Always zero for PCI Local Bus devices.
    pub segment_group: u16,
    /// PCI bus number within the indicated segment group. The root bus is always bus
    /// zero.
    pub bus: u8,
    /// PCI device number on the indicated bus.
    pub device: u8,
    /// PCI function number on the indicated device. Usually zero, but may be nonzero for
    /// multi-function devices.
    pub function: u8,
}


/// Access to memory-mapped PCI configuration space
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemMapConfigAccess {
    /// Address of register 0 on bus
    pub base_address: usize,
    /// Bus numbers covered by this memory map
    pub bus_range: RangeInclusive<u8>,
}

impl MemMapConfigAccess {
    const MAP_SIZE: u16 = 1 << 10;

    fn register_address(&self, selector: ConfigSelector, register: u16) -> *mut u32 {
        assert!(
            self.bus_range.contains(&selector.bus),
            "Cannot access bus {} with this memory map (covers buses {}-{})",
            selector.bus,
            self.bus_range.start(),
            self.bus_range.end(),
        );
        assert!(
            selector.device <= MAX_DEVICE,
            "Device number out of range: {}",
            selector.device,
        );
        assert!(
            selector.function <= MAX_FUNCTION,
            "Function number out of range: {}",
            selector.function,
        );
        assert!(register < Self::MAP_SIZE, "Register number out of range: {}", register);

        let offset = usize::from(selector.bus) << 20
            | usize::from(selector.device) << 15
            | usize::from(selector.function) << 12
            | usize::from(register) << 2;
        let address = self.base_address + offset;

        address as *mut u32
    }
}

impl ConfigAccess for MemMapConfigAccess {
    fn get_register(&self, selector: ConfigSelector, register: u16) -> u32 {
        let address = self.register_address(selector, register);
        let register_data_le = unsafe { core::ptr::read_volatile(address) };
        u32::from_le(register_data_le)
    }

    fn set_register(&self, selector: ConfigSelector, register: u16, value: u32) {
        let address = self.register_address(selector, register);
        unsafe { core::ptr::write_volatile(address, value.to_le()) };
    }
}


/// Support for the I/O based configuration access method on x86/x86-64.
///
/// Note that other architectures have no concept of I/O space.
#[cfg(any(target_arch = "x86", target_arch = "x86_64", doc))]
#[doc(cfg(any(target_arch = "x86", target_arch = "x86_64")))]
pub mod io {
    use crate::access::{ConfigAccess, ConfigSelector};
    use crate::MAX_DEVICE;
    use core::convert::TryFrom;
    use tartan_arch::x86_common::io;
    use tartan_bitfield::bitfield;

    /// I/O based configuration access method on x86/x86-64.
    ///
    /// This method only supports the 256 bits of I/O space defined by the PCI Local Bus
    /// specification, and it has no concept of segment groups, which must always be zero.
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
    pub struct IOConfigAccess;

    impl IOConfigAccess {
        const CONFIG_ADDRESS_PORT: u16 = 0xcf8;
        const CONFIG_DATA_PORT: u16 = 0xcfc;

        fn write_config_address(selector: ConfigSelector, register: u16) {
            assert!(
                selector.segment_group == 0,
                "IO-based PCI config only supports segment group 0 (requested {})",
                selector.segment_group,
            );
            let register_offset = u8::try_from(register << 2)
                .expect("IO-based PCI config only supports register numbers <= 0x3f");

            let config_address = IOConfigAddress::new(selector, register_offset);
            let config_address_le = config_address.0.to_le();

            unsafe {
                io::out_u32(Self::CONFIG_ADDRESS_PORT, config_address_le);
            };
        }
    }

    impl ConfigAccess for IOConfigAccess {
        fn get_register(&self, selector: ConfigSelector, register: u16) -> u32 {
            Self::write_config_address(selector, register);
            let register_data_le = unsafe { io::in_u32(Self::CONFIG_DATA_PORT) };
            u32::from_le(register_data_le)
        }

        fn set_register(&self, selector: ConfigSelector, register: u16, value: u32) {
            Self::write_config_address(selector, register);
            unsafe { io::out_u32(Self::CONFIG_DATA_PORT, value.to_le()) }
        }
    }


    bitfield! {
        /// Index value written to the `CONFIG_ADDRESS` port
        struct IOConfigAddress(u32) {
            [31    ] pub enable,
            [16..24] pub bus:             u8,
            [11..16] pub device:          u8,
            [ 8..11] pub function:        u8,
            [ 0.. 8] pub register_offset: u8,
        }
    }

    impl IOConfigAddress {
        fn new(selector: ConfigSelector, register_offset: u8) -> Self {
            assert!(selector.device <= MAX_DEVICE);
            assert!(selector.function <= MAX_DEVICE);

            let mut address = Self::default();
            address.set_enable(true);
            address.set_bus(selector.bus);
            address.set_device(selector.device);
            address.set_function(selector.function);
            address.set_register_offset(register_offset);
            address
        }
    }


    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_io_config_address() {
            let mut address = IOConfigAddress::default();
            assert_eq!(address.0.to_le_bytes(), *b"\x00\x00\x00\x00");

            address = IOConfigAddress::default();
            address.set_enable(true);
            assert_eq!(address.0.to_le_bytes(), *b"\x00\x00\x00\x80");

            address = IOConfigAddress::default();
            address.set_bus(0xff);
            assert_eq!(address.0.to_le_bytes(), *b"\x00\x00\xff\x00");

            address = IOConfigAddress::default();
            address.set_bus(0x01);
            assert_eq!(address.0.to_le_bytes(), *b"\x00\x00\x01\x00");

            address = IOConfigAddress::default();
            address.set_device(0x1f);
            assert_eq!(address.0.to_le_bytes(), *b"\x00\xf8\x00\x00");

            address = IOConfigAddress::default();
            address.set_device(0x01);
            assert_eq!(address.0.to_le_bytes(), *b"\x00\x08\x00\x00");

            address = IOConfigAddress::default();
            address.set_function(0x07);
            assert_eq!(address.0.to_le_bytes(), *b"\x00\x07\x00\x00");

            address = IOConfigAddress::default();
            address.set_function(0x01);
            assert_eq!(address.0.to_le_bytes(), *b"\x00\x01\x00\x00");

            address = IOConfigAddress::default();
            address.set_register_offset(0xfc);
            assert_eq!(address.0.to_le_bytes(), *b"\xfc\x00\x00\x00");

            address = IOConfigAddress::default();
            address.set_register_offset(0x04);
            assert_eq!(address.0.to_le_bytes(), *b"\x04\x00\x00\x00");
        }

        #[test]
        fn test_io_config_address_new() {
            let address = IOConfigAddress::new(
                ConfigSelector { segment_group: 0, bus: 1, device: 2, function: 3 },
                4,
            );
            assert_eq!(address.0.to_le_bytes(), *b"\x04\x13\x01\x80");
        }
    }
}
