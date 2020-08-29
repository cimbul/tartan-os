pub mod io {
    /// Read an 8-bit value from the port at the given address in I/O space.
    ///
    /// # Safety
    /// This is an extremely low-level operation that can do anything: trigger an
    /// interrupt, overwrite arbitrary memory with DMA, shut down the system, launch a
    /// missile. Use with caution.
    pub unsafe fn in_u8(addr: u16) -> u8 {
        let val: u8;
        llvm_asm!(
            "in al, dx"
            : "={al}"(val)
            : "{dx}"(addr)
            :: "intel", "volatile"
        );
        val
    }

    /// Read a 16-bit value from the port at the given address in I/O space.
    ///
    /// # Safety
    /// This is an extremely low-level operation that can do anything: trigger an
    /// interrupt, overwrite arbitrary memory with DMA, shut down the system, launch a
    /// missile. Use with caution.
    pub unsafe fn in_u16(addr: u16) -> u16 {
        let val: u16;
        llvm_asm!(
            "in ax, dx"
            : "={ax}"(val)
            : "{dx}"(addr)
            :: "intel", "volatile"
        );
        val
    }

    /// Read a 32-bit value from the port at the given address in I/O space.
    ///
    /// # Safety
    /// This is an extremely low-level operation that can do anything: trigger an
    /// interrupt, overwrite arbitrary memory with DMA, shut down the system, launch a
    /// missile. Use with caution.
    pub unsafe fn in_u32(addr: u16) -> u32 {
        let val: u32;
        llvm_asm!(
            "in eax, dx"
            : "={eax}"(val)
            : "{dx}"(addr)
            :: "intel", "volatile"
        );
        val
    }

    /// Write an 8-bit value to the port at the given address in I/O space.
    ///
    /// # Safety
    /// This is an extremely low-level operation that can do anything: trigger an
    /// interrupt, overwrite arbitrary memory with DMA, shut down the system, launch a
    /// missile. Use with caution.
    pub unsafe fn out_u8(addr: u16, data: u8) {
        llvm_asm!(
            "out dx, al"
            :: "{al}"(data), "{dx}"(addr)
            :: "intel", "volatile"
        );
    }

    /// Write a 16-bit value to the port at the given address in I/O space.
    ///
    /// # Safety
    /// This is an extremely low-level operation that can do anything: trigger an
    /// interrupt, overwrite arbitrary memory with DMA, shut down the system, launch a
    /// missile. Use with caution.
    pub unsafe fn out_u16(addr: u16, data: u16) {
        llvm_asm!(
            "out dx, ax"
            :: "{ax}"(data), "{dx}"(addr)
            :: "intel", "volatile"
        );
    }

    /// Write a 16-bit value to the port at the given address in I/O space.
    ///
    /// # Safety
    /// This is an extremely low-level operation that can do anything: trigger an
    /// interrupt, overwrite arbitrary memory with DMA, shut down the system, launch a
    /// missile. Use with caution.
    pub unsafe fn out_u32(addr: u16, data: u32) {
        llvm_asm!(
            "out dx, eax"
            :: "{eax}"(data), "{dx}"(addr)
            :: "intel", "volatile"
        );
    }
}
