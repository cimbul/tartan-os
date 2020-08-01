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

#[repr(usize)]
pub enum Status {
    // Appendix D
    Success = 0,

    WarnUnknownGlyph = 1,
    WarnDeleteFailure = 2,
    WarnWriteFailure = 3,
    WarnBufferTooSmall = 4,
    WarnStaleData = 5,
    WarnFileSystem = 6,
    WarnResetRequired = 7,

    LoadError = Status::ERROR_BIT | 1,
    InvalidParameter = Status::ERROR_BIT | 2,
    // TODO: ... more defined
}

impl Status {
    const ERROR_BIT: usize = 0x1usize.reverse_bits(); // high bit
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
#[allow(non_snake_case)]
pub struct RuntimeServices {
    pub header: TableHeader,

    // Time Services
    GetTime: usize,
    SetTime: usize,
    GetWakeupTime: usize,
    SetWakeupTime: usize,

    // Virtual Memory Services
    SetVirtualAddressMap: usize,
    ConvertPointer: usize,

    // Variable Services
    GetVariable: usize,
    GetNextVariableName: usize,
    SetVariable: usize,

    // Miscellaneous Services
    GetNextHighMonotonicCount: usize,
    ResetSystem: usize,

    // UEFI 2.0 Capsule Services
    UpdateCapsule: usize,
    QueryCapsuleCapabilities: usize,

    // Miscellaneous UEFI 2.0 Service
    QueryVariableInfo: usize,
}

impl RuntimeServices {
    const SIGNATURE: u64 = 0x56524553544e5552;
}

#[repr(C)]
#[allow(non_snake_case)]
pub struct BootServices {
    pub header: TableHeader,

    // Task Priority Services
    RaiseTPM: usize,
    RestoreTPL: usize,

    // Memory Services
    AllocatePages: usize,
    FreePages: usize,
    GetMemoryMap: usize,
    AllocatePool: usize,
    FreePool: usize,

    // Event & Timer Services
    CreateEvent: usize,
    SetTimer: usize,
    WaitForEvent: usize,
    SignalEvent: usize,
    CloseEvent: usize,
    CheckEvent: usize,

    // Protocol Handler Services
    InstallProtocolInterface: usize,
    ReinstallProtocolInterface: usize,
    UninstallProtocolInterface: usize,
    HandleProtocol: usize,
    reserved: usize,
    RegisterProtocolNotify: usize,
    LocateHandle: usize,
    LocateDevicePath: usize,
    InstallConfigurationTable: usize,

    // Image Services
    LoadImage: usize,
    StartImage: usize,
    Exit: usize,
    UnloadImage: usize,
    ExitBootServices: usize,

    // Miscellaneous Services
    GetNextMonotonicCount: usize,
    Stall: usize,
    SetWatchdogTimer: usize,

    // Driver Support Services
    ConnectController: usize,
    DisconnectController: usize,

    // Open and Close Protocol Services
    OpenProtocol: usize,
    CloseProtocol: usize,
    OpenProtocolInformation: usize,

    // Library Services
    ProtocolsPerHandle: usize,
    LocateHandleBuffer: usize,
    LocateProtocol: usize,
    InstallMultipleProtocolInterfaces: usize,
    UninstallMultipleProtocolInterfaces: usize,

    // 32-bit CRC Services
    CalculateCrc32: usize,

    // Miscellaneous Services
    CopyMem: usize,
    SetMem: usize,
    CreateEventEx: usize,
}

impl BootServices {
    const SIGNATURE: u64 = 0x56524553544f4f42;
}

#[repr(C)]
pub struct ConfigurationTable {
    vendor_guid: u128,
    vendor_table: usize,
}

impl ConfigurationTable {
    const ACPI_20_GUID: u128 = 0x8868e871e4f111d3bc220080c73c8881;
    const ACPI_GUID: u128 = 0xeb9d2d302d8811d39a160090273fc14d;
    const SAL_SYSTEM_GUID: u128 = 0xeb9d2d322d8811d39a160090273fc14d;
    const SMBIOS_GUID: u128 = 0xeb9d2d312d8811d39a160090273fc14d;
    const SMBIOS3_GUID: u128 = 0xf2fd154497944a2c992ee5bbcf20e394;
    const MPS_GUID: u128 = 0xeb9d2d2f2d8811d39a160090273fc14d;
    // TODO: ... more defined in sect. 4.6
}

pub mod proto {
    #[repr(C)]
    #[allow(non_snake_case)]
    pub struct SimpleTextInput {
        Reset: usize,
        ReadKeyStroke: usize,
        WaitForKey: usize,
        SetState: usize,
        RegisterKeyNotify: usize,
        UnregisterKeyNotify: usize,
    }

    #[repr(C)]
    #[allow(non_snake_case)]
    pub struct SimpleTextOutput {
        pub Reset: extern "C" fn(this: &SimpleTextOutput, extended_verification: bool) -> usize,
        pub OutputString: extern "C" fn(this: &SimpleTextOutput, string: *const u16) -> usize,
        pub TestString: extern "C" fn(this: &SimpleTextOutput, string: *const u16) -> usize,
        QueryMode: usize,
        SetMode: usize,
        SetAttribute: usize,
        ClearScreen: usize,
        SetCursorPosition: usize,
        EnableCursor: usize,
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
