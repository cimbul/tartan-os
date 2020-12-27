//! Architecture-specific bindings for 64-bit Arm processors

use tartan_arch::aarch64::interrupt::{
    Kind, Source, SyndromeRegister, VectorBaseAddressRegister,
};
use tartan_arch::aarch64::ExceptionLevel;
use tartan_arch::aarch64_exception_vector_table;


/// Perform early architecture-specific setup
pub fn initialize() {
    initialize_interrupts();
}


fn initialize_interrupts() {
    VectorBaseAddressRegister::set(ExceptionLevel::One, unsafe {
        &exception_vector_table
    });
}


aarch64_exception_vector_table!(exception_vector_table, handle_exception);

fn handle_exception(kind: Kind, source: Source) {
    let syndrome = SyndromeRegister::get(ExceptionLevel::One);
    panic!("Exception {:?} ({:?} from {:?})", syndrome, kind, source);
}
