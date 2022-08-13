//! Access to I/O space

use core::arch::asm;

/// Read an 8-bit value from the port at the given address in I/O space.
///
/// # Safety
/// This is an extremely low-level operation that can do anything: trigger an interrupt,
/// overwrite arbitrary memory with DMA, shut down the system, launch a missile. Use with
/// caution.
pub unsafe fn in_u8(addr: u16) -> u8 {
    let data: u8;
    asm!(
        "in al, dx",
        out("al") data,
        in("dx") addr,
    );
    data
}

/// Read a 16-bit value from the port at the given address in I/O space.
///
/// # Safety
/// This is an extremely low-level operation that can do anything: trigger an interrupt,
/// overwrite arbitrary memory with DMA, shut down the system, launch a missile. Use with
/// caution.
pub unsafe fn in_u16(addr: u16) -> u16 {
    let data: u16;
    asm!(
        "in ax, dx",
        out("ax") data,
        in("dx") addr,
    );
    data
}

/// Read a 32-bit value from the port at the given address in I/O space.
///
/// # Safety
/// This is an extremely low-level operation that can do anything: trigger an interrupt,
/// overwrite arbitrary memory with DMA, shut down the system, launch a missile. Use with
/// caution.
pub unsafe fn in_u32(addr: u16) -> u32 {
    let data: u32;
    asm!(
        "in eax, dx",
        out("eax") data,
        in("dx") addr,
    );
    data
}

/// Write an 8-bit value to the port at the given address in I/O space.
///
/// # Safety
/// This is an extremely low-level operation that can do anything: trigger an interrupt,
/// overwrite arbitrary memory with DMA, shut down the system, launch a missile. Use with
/// caution.
pub unsafe fn out_u8(addr: u16, data: u8) {
    asm!(
        "out dx, al",
        in("al") data,
        in("dx") addr,
    );
}

/// Write a 16-bit value to the port at the given address in I/O space.
///
/// # Safety
/// This is an extremely low-level operation that can do anything: trigger an interrupt,
/// overwrite arbitrary memory with DMA, shut down the system, launch a missile. Use with
/// caution.
pub unsafe fn out_u16(addr: u16, data: u16) {
    asm!(
        "out dx, ax",
        in("ax") data,
        in("dx") addr,
    );
}

/// Write a 16-bit value to the port at the given address in I/O space.
///
/// # Safety
/// This is an extremely low-level operation that can do anything: trigger an interrupt,
/// overwrite arbitrary memory with DMA, shut down the system, launch a missile. Use with
/// caution.
pub unsafe fn out_u32(addr: u16, data: u32) {
    asm!(
        "out dx, eax",
        in("eax") data,
        in("dx") addr,
    );
}
