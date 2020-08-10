use super::SystemTable;

pub static mut SYSTEM_TABLE: Option<*mut SystemTable> = None;
