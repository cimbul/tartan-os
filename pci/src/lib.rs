#![no_std]

use config::{HeaderRegister0, HeaderRegister3};
use access::{ConfigAccess, ConfigSelector};

pub mod config;
pub mod access;

pub const MAX_DEVICE: u8 = (1 << 5) - 1;
pub const MAX_FUNCTION: u8 = (1 << 3) - 1;
pub const INVALID_VENDOR: u16 = 0xffff;


pub fn enumerate_bus<'a, A>(
    access: &'a A,
    bus_selector: ConfigSelector,
) -> impl Iterator<Item = ConfigSelector> + 'a
where
    A: ConfigAccess
{
    enumerate_bus_devices(access, bus_selector)
        .flat_map(move |device| enumerate_device_functions(access, device))
}


pub fn enumerate_bus_devices<'a, A>(
    access: &'a A,
    bus_selector: ConfigSelector,
) -> impl Iterator<Item = ConfigSelector> + 'a
where
    A: ConfigAccess,
{
    (0..=MAX_DEVICE).filter_map(move |device| {
        let selector = ConfigSelector {
            device,
            function: 0,
            ..bus_selector
        };
        if check_valid(access, selector) {
            Some(selector)
        } else {
            None
        }
    })
}

pub fn enumerate_device_functions<'a, A>(
    access: &'a A,
    device_selector: ConfigSelector,
) -> impl Iterator<Item = ConfigSelector> + 'a
where
    A: ConfigAccess,
{
    let fn_0_register: HeaderRegister3 = access.get_fixed_register(device_selector);
    let function_range = if fn_0_register.header_type().multi_function() {
        0..MAX_FUNCTION
    } else {
        0..1
    };
    function_range.filter_map(move |function| {
        let fn_selector = ConfigSelector { function, ..device_selector };
        if check_valid(access, fn_selector) {
            Some(fn_selector)
        } else {
            None
        }
    })
}

pub fn check_valid<A>(access: &A, selector: ConfigSelector) -> bool
where
    A: ConfigAccess,
{
    let id_register: HeaderRegister0 = access.get_fixed_register(selector);
    id_register.valid()
}
