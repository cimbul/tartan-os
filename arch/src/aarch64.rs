//! Architecture-specific primitives for 64-bit Arm.

pub mod float;
pub mod interrupt;


#[macro_export]
#[doc(hidden)]
macro_rules! system_register_access {
    [$struct:ident, $register:literal $(, $setter_type:tt)?] => {
        impl $struct {
            /// Retrieve the current value of this register
            pub fn get() -> Self {
                let mut value = Self(0);
                unsafe {
                    asm!(
                        concat!("mrs {}, ", $register),
                        out(reg) value.0,
                    );
                }
                value
            }

            system_register_access!(@setter $register $($setter_type)?);
        }
    };

    [@setter $register:literal readonly] => { };

    [@setter $register:literal] => {
        /// Update the register to the given value.
        pub fn set(value: Self) {
            unsafe {
                asm!(
                    concat!("msr ", $register, ", {}"),
                    in(reg) value.0,
                );
            }
        }
    };

    [@setter $register:literal unsafe] => {
        /// Update the register to the given value.
        ///
        /// # Safety
        /// Altering certain system flags can have dramatic effects on the execution
        /// of this and other programs, including memory safety.
        pub unsafe fn set(value: Self) {
            asm!(
                concat!("msr ", $register, ", {}"),
                in(reg) value.0,
            );
        }
    };
}


/// `EL`: Defines the privilege level of executing code. Higher values have more
/// privileges.
#[repr(u8)]
pub enum ExceptionLevel {
    /// `EL0`, used for unprivileged user code.
    Zero,
    /// `EL1`, used for OS kernel code.
    One,
    /// `EL2`, used for hypervisors in virtualized systems.
    Two,
    /// `EL3`, used for the secure manager.
    Three,
}

impl ExceptionLevel {
    /// Get the current exception level. Only accessible from EL1 or higher.
    pub fn get() -> Self {
        let mut raw_value: u64;
        unsafe {
            asm!(
                "mrs {}, CurrentEL",
                out(reg) raw_value,
            );
        }
        Self::from((raw_value >> 2) as u8)
    }
}

impl From<u8> for ExceptionLevel {
    fn from(value: u8) -> Self {
        // SAFETY: When truncating to two bits, the defined enum values are exhaustive
        unsafe { core::mem::transmute(value & 0b11) }
    }
}


/// Controls which stack pointer register is used when executing code at EL1 or higher.
pub enum StackPointerSelect {
    /// `T` (thread) mode: use the stack pointer register from EL0.
    Level0,
    /// `H` (handler) mode: use the stack pointer register for the current exception
    /// level.
    CurrentLevel,
}

impl StackPointerSelect {
    /// Get the value of the flag for the current exception level. Only accessible from
    /// EL1 or higher.
    pub fn get() -> Self {
        let mut raw_value: u64;
        unsafe {
            asm!(
                "mrs {}, SPSel",
                out(reg) raw_value,
            );
        }
        if raw_value & 1 == 0 {
            Self::Level0
        } else {
            Self::CurrentLevel
        }
    }

    /// Update the value of the flag for the current exception level.
    ///
    /// # Safety
    /// This can alter the current stack pointer, which can have dramatic effects on the
    /// execution of the current function.
    #[inline(always)]
    pub unsafe fn set(value: Self) {
        asm!(
            "msr SPSel, {}",
            in(reg) value as u64,
        );
    }
}
