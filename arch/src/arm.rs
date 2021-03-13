//! Architecture-specific primitives for 32-bit Arm.

pub mod interrupt;


#[macro_export]
#[doc(hidden)]
macro_rules! cp15_register_get {
    [$op_1:literal, $cr_n:literal, $cr_m:literal, $op_2:literal] => {
        {
            let mut value: usize;
            unsafe {
                asm!(
                    concat!(
                        "mrc p15, ",
                        $op_1,
                        ", {}, ",
                        $cr_n,
                        ", ",
                        $cr_m,
                        ", ",
                        $op_2,
                    ),
                    out(reg) value,
                );
            }
            value
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! cp15_register_set {
    [$op_1:literal, $cr_n:literal, $cr_m:literal, $op_2:literal, $value:expr] => {
        {
            let value: usize = $value;
            asm!(
                concat!(
                    "mcr p15, ",
                    $op_1,
                    ", {}, ",
                    $cr_n,
                    ", ",
                    $cr_m,
                    ", ",
                    $op_2,
                ),
                in(reg) value,
            );
        }
    }
}
