//! Architecture-specific bindings for 32-bit Intel x86-based processors

use super::x86_common::*;


/// Perform early architecture-specific setup
pub fn initialize() {
    initialize_control_registers();
    initialize_segments();
    initialize_interrupts();
}
