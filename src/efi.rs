// See EDK MdePkg/Include/Uefi/UefiSpec.h

#![allow(unused)]

pub type Handle = usize;

pub enum Revision {
    V2_80 = (2 << 16) | 80,
    V2_70 = (2 << 16) | 70,
    V2_60 = (2 << 16) | 60,
    V2_50 = (2 << 16) | 50,
    V2_40 = (2 << 16) | 40,
    V2_31 = (2 << 16) | 31,
    V2_30 = (2 << 16) | 30,
    V2_20 = (2 << 16) | 20,
    V2_10 = (2 << 16) | 10,
    V2_00 = (2 << 16) | 00,
    V1_10 = (1 << 16) | 10,
    V1_02 = (1 << 16) | 02,
}

impl Revision {
    const LATEST: Revision = Revision::V2_80;
}

pub type Result = core::result::Result<Status, Status>;

#[repr(transparent)]
#[derive(PartialEq, Eq)]
pub struct Status(usize);

impl Status {
    // Appendix D
    pub const SUCCESS: Status = Status(0);

    pub const WARN_UNKNOWN_GLYPH: Status = Status(1);
    pub const WARN_DELETE_FAILURE: Status = Status(2);
    pub const WARN_WRITE_FAILURE: Status = Status(3);
    pub const WARN_BUFFER_TOO_SMALL: Status = Status(4);
    pub const WARN_STALE_DATA: Status = Status(5);
    pub const WARN_FILE_SYSTEM: Status = Status(6);
    pub const WARN_RESET_REQUIRED: Status = Status(7);

    pub const ERROR_BIT: usize = 0x1usize.reverse_bits(); // high bit
    pub const LOAD_ERROR: Status = Status(Status::ERROR_BIT | 1);
    pub const INVALID_PARAMETER: Status = Status(Status::ERROR_BIT | 2);
    // TODO: ... more defined

    pub fn is_error(&self) -> bool {
        (self.0 & Status::ERROR_BIT) == 0
    }

    pub fn is_warning(&self) -> bool {
        *self != Status::SUCCESS && !self.is_error()
}

    pub fn into_result(self) -> Result {
        if self.is_error() {
            Err(self)
}
        else {
            Ok(self)
        }
    }
}

impl From<Status> for Result {
    fn from(status: Status) -> Self {
        status.into_result()
    }
}

#[repr(C)]
pub struct TableHeader {
    pub signature: u64,
    pub revision: u32,
    pub header_size: u32,
    pub crc32: u32,
    reserved: u32,
}

#[repr(C)]
pub struct SystemTable {
    pub header: TableHeader,
    pub firmware_vendor: *const u16,
    pub firmware_revision: u32,
    pub console_in_handle: Handle,
    pub console_in: *const proto::SimpleTextInput,
    pub console_out_handle: Handle,
    pub console_out: *const proto::SimpleTextOutput,
    pub std_err_handle: Handle,
    pub std_err: proto::SimpleTextOutput,
    pub runtime_services: RuntimeServices,
    pub boot_services: BootServices,
    pub config_entry_count: usize,
    pub config_table: *const ConfigurationTable,
}

impl SystemTable {
    const SIGNATURE: u64 = 0x5453595320494249;
}

#[repr(C)]
pub struct RuntimeServices {
    pub header: TableHeader,

    // Time Services
    get_time: usize,
    set_time: usize,
    get_wakeup_time: usize,
    set_wakeup_time: usize,

    // Virtual Memory Services
    set_virtual_address_map: usize,
    convert_pointer: usize,

    // Variable Services
    get_variable: usize,
    get_next_variable_name: usize,
    set_variable: usize,

    // Miscellaneous Services
    get_next_high_monotonic_count: usize,
    reset_system: usize,

    // UEFI 2.0 Capsule Services
    update_capsule: usize,
    query_capsule_capabilities: usize,

    // Miscellaneous UEFI 2.0 Service
    query_variable_info: usize,
}

impl RuntimeServices {
    const SIGNATURE: u64 = 0x56524553544e5552;
}

#[repr(C)]
pub struct BootServices {
    pub header: TableHeader,

    // Task Priority Services
    raise_tpm: usize,
    restore_tpl: usize,

    // Memory Services
    allocate_pages: usize,
    free_pages: usize,
    get_memory_map: usize,
    allocate_pool: usize,
    free_pool: usize,

    // Event & Timer Services
    create_event: usize,
    set_timer: usize,
    wait_for_event: usize,
    signal_event: usize,
    close_event: usize,
    check_event: usize,

    // Protocol Handler Services
    install_protocol_interface: usize,
    reinstall_protocol_interface: usize,
    uninstall_protocol_interface: usize,
    handle_protocol: usize,
    reserved: usize,
    register_protocol_notify: usize,
    locate_handle: usize,
    locate_device_path: usize,
    install_configuration_table: usize,

    // Image Services
    load_image: usize,
    start_image: usize,
    exit: usize,
    unload_image: usize,
    exit_boot_services: usize,

    // Miscellaneous Services
    get_next_monotonic_count: usize,
    stall: usize,
    set_watchdog_timer: usize,

    // Driver Support Services
    connect_controller: usize,
    disconnect_controller: usize,

    // Open and Close Protocol Services
    open_protocol: usize,
    close_protocol: usize,
    open_protocol_information: usize,

    // Library Services
    protocols_per_handle: usize,
    locate_handle_buffer: usize,
    locate_protocol: usize,
    install_multiple_protocol_interfaces: usize,
    uninstall_multiple_protocol_interfaces: usize,

    // 32-bit CRC Services
    calculate_crc32: usize,

    // Miscellaneous Services
    copy_mem: usize,
    set_mem: usize,
    create_event_ex: usize,
}

impl BootServices {
    const SIGNATURE: u64 = 0x56524553544f4f42;
}

#[repr(C)]
pub struct GUID(
    u32,
    u16,
    u16,
    [u8; 4]
);

impl GUID {
    pub const fn from(n: u128) -> Self {
        GUID(
            (n >> 96) as u32,
            (n >> 80) as u16,
            (n >> 64) as u16,
            (n as u32).to_be_bytes(),
        )
    }
}

impl From<u128> for GUID {
    fn from(n: u128) -> Self { GUID::from(n) }
}

#[repr(C)]
pub struct ConfigurationTable {
    pub vendor_guid: GUID,
    vendor_table: usize,
}

impl ConfigurationTable {
    pub const ACPI_20_GUID:    GUID = GUID::from(0x8868e871_e4f1_11d3_bc22_0080c73c8881);
    pub const ACPI_GUID:       GUID = GUID::from(0xeb9d2d30_2d88_11d3_9a16_0090273fc14d);
    pub const SAL_SYSTEM_GUID: GUID = GUID::from(0xeb9d2d32_2d88_11d3_9a16_0090273fc14d);
    pub const SMBIOS_GUID:     GUID = GUID::from(0xeb9d2d31_2d88_11d3_9a16_0090273fc14d);
    pub const SMBIOS3_GUID:    GUID = GUID::from(0xf2fd1544_9794_4a2c_992e_e5bbcf20e394);
    pub const MPS_GUID:        GUID = GUID::from(0xeb9d2d2f_2d88_11d3_9a16_0090273fc14d);
    // TODO: ... more defined in sect. 4.6
}

pub mod proto {
    #[repr(C)]
    pub struct SimpleTextInput {
        reset: usize,
        read_key_stroke: usize,
        wait_for_key: usize,
        set_state: usize,
        register_key_notify: usize,
        unregister_key_notify: usize,
    }

    #[repr(C)]
    pub struct SimpleTextOutput {
        pub reset: extern "C" fn(this: &SimpleTextOutput, extended_verification: bool) -> Status,
        pub output_string: extern "C" fn(this: &SimpleTextOutput, string: *const u16) -> Status,
        pub test_string: extern "C" fn(this: &SimpleTextOutput, string: *const u16) -> Status,
        query_mode: usize,
        set_mode: usize,
        set_attribute: usize,
        clear_screen: usize,
        set_cursor_position: usize,
        enable_cursor: usize,
        mode: SimpleTextOutputMode,
    }

    #[repr(C)]
    pub struct SimpleTextOutputMode {
        max_mode: i32,
        mode: i32,
        attribute: i32,
        cursor_column: i32,
        cursor_row: i32,
        cursor_visible: bool,
    }
}
