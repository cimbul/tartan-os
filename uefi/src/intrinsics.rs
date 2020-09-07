/// This module contains some compiler-rt intrinsics that are missing from the
/// compiler-builtins crate, or otherwise don't function properly.


// compiler-builtins doesn't mangle this name correctly for Windows's cdecl convention on
// x86, which adds a leading underscore. It would be better to alias this symbol directly,
// but I can't get LLVM ASM to handle that right.
#[cfg(all(target_os = "uefi", target_arch = "x86"))]
#[no_mangle]
#[naked]
pub unsafe fn __rust_probestack() {
    // This looks recursive, but isn't. This function is ___rust_probestack (triple).
    asm!("jmp __rust_probestack");
}


/// This is a Microsoft C Runtime Library function that LLVM expects to be available when
/// it is making PE files for Arm.
#[cfg(all(target_os = "uefi", target_arch = "aarch64"))]
#[no_mangle]
#[naked]
pub unsafe extern "C" fn __chkstk() {
    // Input:    x15 = number of 16-byte units in stack
    // Output:   x15 = same as input
    // Clobbers: x16, x17
    asm!(
        "
        mov  x16, 0    // Stack offset
        mov  x17, x15  // Remaining 16-byte units

    1:  // Repeatedly touch the guard page to trigger faults and allocate more stack
        sub  x16, x16, 4096   // Page size in bytes
        ldr  xzr, [sp, x16]
        subs x17, x17, 256    // Page size in 16-byte units
        b.gt 1b

        ret
        "
    )
}


/// This is a Microsoft C Runtime Library function that LLVM expects to be available when
/// it is making PE files for Arm.
#[cfg(all(target_os = "uefi", target_arch = "arm"))]
#[no_mangle]
#[naked]
pub unsafe extern "C" fn __chkstk() {
    // Input:    r4 = number of 4-byte units in stack
    // Output:   r4 = number of *individual* bytes in stack
    // Clobbers: r12
    asm!(
        "
        push {{r0, r4}} // r0 be used as scratch register for throw-away loads
        mov  r12, #-8   // Stack offset; initial value accounts for saved registers

    1:  // Repeatedly touch the guard page to trigger faults and allocate more stack
        sub  r12, r12, 4096  // Page size in bytes
        ldr  r0,  [sp, r12]
        subs r4,  r4,  1024  // Page size in 4-byte units
        bgt  1b

        pop  {{r0, r4}}
        lsl  r4,  2     // Convert 4-byte units to single bytes, as expected by caller
        blx  lr
        "
    )
}


// These are Microsoft C Runtime Library functions that LLVM expects to be available when
// it is making PE files for Arm.
cfg_if::cfg_if! {
    if #[cfg(all(target_os = "uefi", target_arch = "arm"))] {
        /// Convert 64-bit unsigned int to double-precision float
        #[no_mangle]
        pub unsafe extern "C" fn __u64tod(i: u64) -> f64 {
            __floatundidf(i)
        }

        /// Convert 64-bit unsigned int to single-precision float
        #[no_mangle]
        pub unsafe extern "C" fn __u64tos(i: u64) -> f32 {
            __floatundisf(i)
        }

        /// Division with remainder for unsigned 32-bit integers
        #[no_mangle]
        #[naked]
        pub unsafe fn __rt_udiv() {
            asm!(
                "
                // Swap arguments, because MS CRT and ARM RTABI use opposite orders
                // r0 <-> r1
                mov  r12, r0
                mov  r0,  r1
                mov  r1,  r12
                // Jump directly to the corresponding ARM RTABI function
                b    __aeabi_uidivmod
                "
            );
        }

        /// Division with remainder for unsigned 64-bit integers
        #[no_mangle]
        #[naked]
        pub unsafe fn __rt_udiv64() {
            asm!(
                "
                // Swap arguments, because MS CRT and ARM RTABI use opposite orders
                // r0 <-> r2 (lower 32b)
                mov  r12, r0
                mov  r0,  r2
                mov  r2,  r12
                // r1 <-> r3 (upper 32b)
                mov  r12, r1
                mov  r1,  r3
                mov  r3,  r12
                // Jump directly to the corresponding ARM RTABI function
                b    __aeabi_uldivmod
                "
            );
        }

        /// Division with remainder for signed 32-bit integers
        #[no_mangle]
        #[naked]
        pub unsafe fn __rt_sdiv() {
            asm!(
                "
                // Swap arguments, because MS CRT and ARM RTABI use opposite orders
                // r0 <-> r1
                mov  r12, r0
                mov  r0,  r1
                mov  r1,  r12
                // Jump directly to the corresponding ARM RTABI function
                b    __aeabi_idivmod
                "
            );
        }

        /// Division with remainder for unsigned 64-bit integers
        #[no_mangle]
        #[naked]
        pub unsafe fn __rt_sdiv64() {
            asm!(
                "
                // Swap arguments, because MS CRT and ARM RTABI use opposite orders
                // r0 <-> r2 (lower 32b)
                mov  r12, r0
                mov  r0,  r2
                mov  r2,  r12
                // r1 <-> r3 (upper 32b)
                mov  r12, r1
                mov  r1,  r3
                mov  r3,  r12
                // Jump directly to the corresponding ARM RTABI function
                b    __aeabi_ldivmod
                "
            );
        }

        extern "C" {
            // Functions from compiler-builtins that corespond to the MS CRT __u64to*
            // functions above.
            fn __floatundidf(i: u64) -> f64;
            fn __floatundisf(i: u64) -> f32;
        }
    }
}


pub fn test() {
    test::test_stack_probe();
    test::test_u64_division();
}


// Note that these aren't standard unit tests. They are run by the UEFI bootloader itself,
// which doesn't have access to the test crate.
#[allow(clippy::module_name_repetitions)]
mod test {
    use log::{debug, error};
    use core::hint::black_box;

    pub fn test_stack_probe() {
        // A convoluted identity function that has a stack size of over two pages
        fn allocate_big(x: u8) -> u8 {
            let mut big = [0_u8; 0x2abc];
            big[0x2abb] = black_box(x);
            black_box(big[0x2abb])
        }

        assert_eq!(allocate_big(0x5d), 0x5d);
    }

    pub fn test_u64_division() {
        #[track_caller]
        fn check(n: u64, d: u64, q: u64, r: u64) {
            assert_eq!(n / black_box(d), q, "for {} / {}", n, d);
            assert_eq!(n % black_box(d), r, "for {} % {}", n, d);
        }

        // denominator = 1
        check(0, 1, 0, 0);
        check(1, 1, 1, 0);
        check(2, 1, 2, 0);
        check(0xc00d_4a33_849a_8cb3, 1, 0xc00d_4a33_849a_8cb3, 0);
        check(0xee20_a96a_263b_4f18, 1, 0xee20_a96a_263b_4f18, 0);
        check(0xffff_ffff_ffff_ffff, 1, 0xffff_ffff_ffff_ffff, 0);

        // denominator = 2
        check(0, 2, 0, 0);
        check(1, 2, 0, 1);
        check(2, 2, 1, 0);
        check(3, 2, 1, 1);
        check(4, 2, 2, 0);
        check(5, 2, 2, 1);
        check(0xc00d_4a33_849a_8cb3, 2, 0x6006_a519_c24d_4659, 1);
        check(0xee20_a96a_263b_4f18, 2, 0x7710_54b5_131d_a78c, 0);
        check(0xffff_ffff_ffff_ffff, 2, 0x7fff_ffff_ffff_ffff, 1);

        // denominator = 2^64 - 1
        check(0, 0xffff_ffff_ffff_ffff, 0, 0);
        check(1, 0xffff_ffff_ffff_ffff, 0, 1);
        check(2, 0xffff_ffff_ffff_ffff, 0, 2);
        check(0xc00d_4a33_849a_8cb3, 0xffff_ffff_ffff_ffff, 0, 0xc00d_4a33_849a_8cb3);
        check(0xee20_a96a_263b_4f18, 0xffff_ffff_ffff_ffff, 0, 0xee20_a96a_263b_4f18);
        check(0xffff_ffff_ffff_fffe, 0xffff_ffff_ffff_ffff, 0, 0xffff_ffff_ffff_fffe);
        check(0xffff_ffff_ffff_ffff, 0xffff_ffff_ffff_ffff, 1, 0);
    }
}
