//! Static pointer to the [`SystemTable`], used by features where we can't pass it
//! directly.

use super::SystemTable;

pub static mut SYSTEM_TABLE: Option<*mut SystemTable> = None;
