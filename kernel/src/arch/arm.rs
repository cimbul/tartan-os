//! Architecture-specific bindings for 32-bit Arm processors

use tartan_arch::arm::interrupt::{Kind, VectorBaseAddressRegister};
use tartan_arch::arm_exception_vector_table;


/// Perform early architecture-specific setup
pub fn initialize() {
    initialize_interrupts();
}



fn initialize_interrupts() {
    VectorBaseAddressRegister::set(unsafe { &exception_vector_table });
}


arm_exception_vector_table!(exception_vector_table, handle_exception);

fn handle_exception(kind: Kind) {
    panic!("Exception {:?}", kind);
}
