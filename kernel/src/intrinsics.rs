//! This module contains some compiler-rt intrinsics that are missing from the
//! compiler-builtins crate, or otherwise don't function properly.

extern crate compiler_builtins;


// These are all provided by compiler-builtins, but they are hidden behind the "mem"
// features. More specifically, they are mangled so their symbols don't match the standard
// C library names. Since compiler-builtins is automatically built with the build-std
// feature, we don't have a good way to enable the "mem" feature.
//
// These are also provided by the rlibc crate, and some sources suggest to use that
// instead. However, the functions in that crate are NOT marked as 'extern "C"', meaning
// they only work on architectures where the Rust calling convention happens to match.
// Notably, that isn't true when compiling for i686-unknown-uefi (and there is no
// guarantee it will continue to match on any other targets).

#[no_mangle]
pub unsafe extern "C" fn memcpy(dest: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    compiler_builtins::mem::memcpy(dest, src, n)
}

#[no_mangle]
pub unsafe extern "C" fn memmove(dest: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    compiler_builtins::mem::memmove(dest, src, n)
}

#[no_mangle]
pub unsafe extern "C" fn memset(s: *mut u8, c: i32, n: usize) -> *mut u8 {
    compiler_builtins::mem::memset(s, c, n)
}

#[no_mangle]
pub unsafe extern "C" fn memcmp(s1: *const u8, s2: *const u8, n: usize) -> i32 {
    compiler_builtins::mem::memcmp(s1, s2, n)
}

#[no_mangle]
pub unsafe extern "C" fn bcmp(s1: *const u8, s2: *const u8, n: usize) -> i32 {
    compiler_builtins::mem::bcmp(s1, s2, n)
}


// For some reason, the complier generates references to these intrinsics when handling
// alloc errors, even though we abort on panic.

#[no_mangle]
#[cfg(target_arch = "arm")]
pub fn __aeabi_unwind_cpp_pr0() {}

#[no_mangle]
#[cfg(target_arch = "arm")]
pub fn __aeabi_unwind_cpp_pr1() {}
