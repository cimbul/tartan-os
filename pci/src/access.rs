use crate::{FixedConfigRegister, MAX_DEVICE, MAX_FUNCTION};
use core::convert::TryFrom;
use core::ops::RangeInclusive;
use tartan_arch as arch;
use tartan_util::bitfield;


pub trait ConfigAccess {
    fn get_register(&self, selector: ConfigSelector, register: u16) -> u32;

    fn get_fixed_register<T: FixedConfigRegister>(&self, selector: ConfigSelector) -> T {
        self.get_register(selector, T::REGISTER_NUMBER.into()).into()
    }
}


#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ConfigSelector {
    pub segment_group: u16,
    pub bus: u8,
    pub device: u8,
    pub function: u8,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemMapConfigAccess {
    base_address: usize,
    bus_range: RangeInclusive<u8>,
}

impl MemMapConfigAccess {
    const MAP_SIZE: u16 = 1 << 10;
}

impl ConfigAccess for MemMapConfigAccess {
    fn get_register(&self, selector: ConfigSelector, register: u16) -> u32 {
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

        let register_data_le = unsafe { *(address as *const u32) };
        u32::from_le(register_data_le)
    }
}


#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct IOConfigAccess;

impl IOConfigAccess {
    const CONFIG_ADDRESS_PORT: u16 = 0xcf8;
    const CONFIG_DATA_PORT: u16 = 0xcfc;
}

impl ConfigAccess for IOConfigAccess {
    fn get_register(&self, selector: ConfigSelector, register: u16) -> u32 {
        assert!(
            selector.segment_group == 0,
            "IO-based PCI config only supports segment group 0 (requested {})",
            selector.segment_group,
        );
        let register_offset = u8::try_from(register << 2)
            .expect("IO-based PCI config only supports register numbers <= 0x3f");

        let config_address = IOConfigAddress::new(selector, register_offset);
        let config_address_le = config_address.0.to_le();

        let register_data_le = unsafe {
            arch::io::out_u32(Self::CONFIG_ADDRESS_PORT, config_address_le);
            arch::io::in_u32(Self::CONFIG_DATA_PORT)
        };

        u32::from_le(register_data_le)
    }
}


bitfield! {
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
