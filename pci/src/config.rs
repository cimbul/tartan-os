use crate::INVALID_VENDOR;
use tartan_bitfield::bitfield;


pub trait FixedConfigRegister: From<u32> {
    const REGISTER_NUMBER: u8;
}


bitfield! {
    pub struct HeaderRegister0(u32) {
        [16..32] pub device: u16,
        [ 0..16] pub vendor: u16,
    }
}

impl HeaderRegister0 {
    pub fn valid(&self) -> bool {
        self.vendor() != INVALID_VENDOR
    }
}

impl FixedConfigRegister for HeaderRegister0 {
    const REGISTER_NUMBER: u8 = 0;
}


bitfield! {
    pub struct HeaderRegister1(u32) {
        [16..32] pub status:  u16 as StatusRegister,
        [ 0..16] pub command: u16 as CommandRegister,
    }
}

impl FixedConfigRegister for HeaderRegister1 {
    const REGISTER_NUMBER: u8 = 1;
}


bitfield! {
    pub struct HeaderRegister2(u32) {
        [24..32] pub class: u8,
        [16..24] pub subclass: u8,
        [ 8..16] pub interface: u8,
        [ 0.. 8] pub revision: u8,
    }
}

impl FixedConfigRegister for HeaderRegister2 {
    const REGISTER_NUMBER: u8 = 2;
}


bitfield! {
    pub struct HeaderRegister3(u32) {
        [24..32] pub self_test: u8 as SelfTest,
        [16..24] pub header_type: u8 as HeaderType,
        [ 8..16] pub latency_timer: u8,
        [ 0.. 8] pub cache_line_size: u8,
    }
}

impl FixedConfigRegister for HeaderRegister3 {
    const REGISTER_NUMBER: u8 = 3;
}


bitfield! {
    pub struct CommandRegister(u16) {
        [10] pub interrupt_disabled,
        [ 9] pub fast_back_to_back_enabled,
        [ 8] pub system_error_enabled,
        [ 6] pub parity_error_response,
        [ 5] pub vga_palette_snoop,
        [ 4] pub write_and_invalidate_enable,
        [ 3] pub special_cycle,
        [ 2] pub bus_master,
        [ 1] pub memory_space,
        [ 0] pub io_space,
    }
}


bitfield! {
    pub struct StatusRegister(u16) {
        [15    ] pub parity_error_detected,
        [14    ] pub system_error_signaled,
        [13    ] pub master_abort_received,
        [12    ] pub target_abort_received,
        [11    ] pub target_abort_signaled,
        [ 9..11] pub device_select_timing: u8,
        [ 8    ] pub master_parity_error,
        [ 7    ] pub fast_back_to_back_capable,
        [ 5    ] pub double_clock_capable,
        [ 4    ] pub capabilities_list_available,
        [ 3    ] pub interrupt_status,
    }
}


bitfield! {
    pub struct SelfTest(u8) {
        [7   ] pub capable,
        [6   ] pub start,
        [0..4] pub completion: u8,
    }
}


bitfield! {
    pub struct HeaderType(u8) {
        [7   ] pub multi_function,
        [0..7] pub header_type: u8,
    }
}
