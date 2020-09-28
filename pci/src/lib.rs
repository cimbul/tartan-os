//! Support for Peripheral Component Interconnect (PCI) and PCI Express (`PCIe`) devices.

#![no_std]
#![feature(doc_cfg)]
#![warn(missing_docs)]
#![warn(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

use access::{ConfigAccess, ConfigSelector};
use core::iter;

/// Access methods for PCI configuration space.
pub mod access;

/// Data exposed in PCI configuration space.
pub mod config;

/// Highest device number allowed by the PCI specification.
pub const MAX_DEVICE: u8 = (1 << 5) - 1;

/// Highest function number allowed by the PCI specification.
pub const MAX_FUNCTION: u8 = (1 << 3) - 1;

/// Placeholder value that will always be returned in the vendor field when querying PCI
/// configuration space for a device that does not exist.
pub const INVALID_VENDOR: u16 = 0xffff;


/// Iterate over the devices and functions present on the specified bus.
pub fn enumerate_bus<'a, A>(
    access: &'a A,
    bus_selector: ConfigSelector,
) -> impl Iterator<Item = ConfigSelector> + 'a
where
    A: ConfigAccess,
{
    enumerate_bus_devices(access, bus_selector)
        .flat_map(move |device| enumerate_device_functions(access, device))
}


/// Iterate over the devices present on the specified bus.
pub fn enumerate_bus_devices<'a, A>(
    access: &'a A,
    bus_selector: ConfigSelector,
) -> impl Iterator<Item = ConfigSelector> + 'a
where
    A: ConfigAccess,
{
    (0..=MAX_DEVICE).filter_map(move |device| {
        let selector = ConfigSelector { device, function: 0, ..bus_selector };
        if check_valid(access, selector) {
            Some(selector)
        } else {
            None
        }
    })
}

/// Iterate over the functions available on the specified device.
pub fn enumerate_device_functions<'a, A>(
    access: &'a A,
    device_selector: ConfigSelector,
) -> impl Iterator<Item = ConfigSelector> + 'a
where
    A: ConfigAccess,
{
    let fn_0_register: config::HeaderRegister3 =
        access.get_fixed_register(device_selector);
    let function_range =
        if fn_0_register.multi_function() { 0..MAX_FUNCTION } else { 0..1 };
    function_range.filter_map(move |function| {
        let fn_selector = ConfigSelector { function, ..device_selector };
        if check_valid(access, fn_selector) {
            Some(fn_selector)
        } else {
            None
        }
    })
}

/// Return true if a function is available at the specified selector.
pub fn check_valid<A>(access: &A, selector: ConfigSelector) -> bool
where
    A: ConfigAccess,
{
    let id_register: config::HeaderRegister0 = access.get_fixed_register(selector);
    id_register.valid()
}

/// Information to identify and locate capability registers
pub struct CapabilityEntry {
    /// Capability ID defined by PCI-SIG
    pub id: u8,
    /// Register number in config space (offset in 4-byte units) of the start of the
    /// capability data.
    pub register: u16,
}

/// Iterate over all capabilities defined in the configuration space for a PCI function
pub fn iter_capabilities<'a, A>(
    access: &'a A,
    selector: ConfigSelector,
) -> impl Iterator<Item = CapabilityEntry> + 'a
where
    A: ConfigAccess,
{
    let capability_header: config::Type0HeaderRegister13 =
        access.get_fixed_register(selector);
    let mut next_offset = capability_header.capabilities_offset();
    iter::from_fn(move || {
        if next_offset == 0 {
            None
        } else {
            let register = u16::from(next_offset / 4);
            let capability: config::GenericCapabilityRegister =
                access.get_register(selector, register).into();
            next_offset = capability.next_offset();
            Some(CapabilityEntry { id: capability.id(), register })
        }
    })
}
