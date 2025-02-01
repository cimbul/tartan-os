//! Driver for Arm's `PrimeCell` PL011 UART

use super::{baud_from_divisor, divisor_from_baud, LineMode, Parity, WordLength, UART};
use core::ptr;
use tartan_bitfield::bitfield;
use tartan_c_enum::c_enum;



/// Driver for the PL011 UART.
pub struct UARTPL011<'a> {
    /// Memory-mapped registers for this device.
    pub register_block: &'a mut RegisterBlock,
    /// Frequency, in hertz, of the clock input to the UART.
    pub base_clock_rate: u32,
}

impl UARTPL011<'_> {
    fn max_baud(&self) -> u32 {
        // See RegisterBlock.divisor()
        4 * self.base_clock_rate
    }
}

impl UART for UARTPL011<'_> {
    fn reset(&mut self) {
        // Disable and flush before reprogramming
        let mut control = self.register_block.control();
        control.set_enabled(false);
        self.register_block.set_control(control);
        // FIXME (sync)
        while self.register_block.flags().busy() {}
        self.register_block.clear_rx_status();
        let mut line_control = self.register_block.line_control();
        line_control.set_fifo_enabled(false);
        self.register_block.set_line_control(line_control);

        // Now reprogram the control registers
        self.set_line_mode(LineMode::default());
        self.register_block.set_infrared_low_power_divisor(0);
        self.register_block.set_fifo_control(FIFOControlRegister::default());
        self.register_block.set_dma_control(DMAControlRegister::default());
        self.register_block.set_interrupt_mask(InterruptFlags::default());
        self.register_block.clear_interrupt_status(InterruptFlags::ALL);

        // Re-enable the UART last
        let mut control = ControlRegister::default();
        control.set_enabled(true);
        control.set_tx_enabled(true);
        control.set_rx_enabled(true);
        self.register_block.set_control(control);
    }

    fn line_mode(&mut self) -> LineMode {
        let line_control = self.register_block.line_control();
        let divisor = self.register_block.divisor();
        LineMode {
            baud: baud_from_divisor(self.max_baud(), divisor),
            data_bits: line_control.word_length().bits(),
            parity: line_control.parity(),
            extended_stop: line_control.extended_stop(),
        }
    }

    fn set_line_mode(&mut self, line_mode: LineMode) {
        let divisor = divisor_from_baud(self.max_baud(), line_mode.baud);
        self.register_block.set_divisor(divisor);

        let mut line_control = self.register_block.line_control();
        line_control.set_word_length(WordLength::from_bits(line_mode.data_bits).unwrap());
        line_control.set_parity(line_mode.parity);
        line_control.set_extended_stop(line_mode.extended_stop);
        self.register_block.set_line_control(line_control);
    }

    fn write(&mut self, data: &[u8]) {
        for &byte in data {
            // FIXME (sync)
            while !self.register_block.flags().tx_fifo_empty() {}
            self.register_block.push_tx_queue(byte);
        }
        // FIXME (sync)
        while !self.register_block.flags().tx_fifo_empty() {}
    }
}


/// Memory-mapped registers for PL011 UART.
#[repr(transparent)]
pub struct RegisterBlock {
    registers: [u8; 0x1000],
}

impl RegisterBlock {
    fn read<T: Copy>(&self, offset: usize) -> T {
        let size = core::mem::size_of::<T>();
        let slice = &self.registers[offset..offset + size];
        unsafe { ptr::read_volatile(slice.as_ptr().cast::<T>()) }
    }

    fn write<T: Copy>(&mut self, offset: usize, value: T) {
        let size = core::mem::size_of::<T>();
        let slice = &mut self.registers[offset..offset + size];
        unsafe { ptr::write_volatile(slice.as_mut_ptr().cast::<T>(), value) }
    }

    #[allow(dead_code)]
    fn pop_rx_queue(&self) -> DataRegister {
        self.read(0)
    }

    fn push_tx_queue(&mut self, value: u8) {
        self.write(0, value);
    }

    fn clear_rx_status(&mut self) {
        self.write(0x4, 0_u8);
    }

    fn flags(&self) -> FlagRegister {
        self.read(0x18)
    }

    #[allow(dead_code)]
    fn infrared_low_power_divisor(&self) -> u8 {
        self.read(0x20)
    }

    fn set_infrared_low_power_divisor(&mut self, value: u8) {
        self.write(0x20, value);
    }

    fn divisor(&self) -> u32 {
        // The PL011 splits the divisor into two registers, an "integer part" and a
        // "fractional part" that is measured in 1/64ths. The base rate is divided by an
        // additional factor of 16.
        //
        // Instead, we treat it as one integer, split into lower 6 bits and upper 16 bits,
        // and multiply the nominal base rate by 4.
        let mut divisor = u32::from(self.read::<u8>(0x28) & 0x3f);
        divisor |= u32::from(self.read::<u16>(0x24)) << 6;
        divisor
    }

    // NOTE: Must always write to the line control register after this.
    // TODO: Should update the IR low power divisor too
    fn set_divisor(&mut self, value: u32) {
        // See divisor()
        #![allow(clippy::cast_possible_truncation)]
        self.write(0x28, (value & 0x3f) as u8);
        self.write(0x24, (value >> 6) as u16);
    }

    fn line_control(&self) -> LineControlRegister {
        self.read(0x2c)
    }

    // NOTE: Requires disabling the UART
    fn set_line_control(&mut self, value: LineControlRegister) {
        self.write(0x2c, value);
    }

    fn control(&self) -> ControlRegister {
        self.read(0x30)
    }

    // NOTE: Requires disabling AND flushing the UART
    fn set_control(&mut self, value: ControlRegister) {
        self.write(0x30, value);
    }

    #[allow(dead_code)]
    fn fifo_control(&self) -> FIFOControlRegister {
        self.read(0x34)
    }

    fn set_fifo_control(&mut self, value: FIFOControlRegister) {
        self.write(0x34, value);
    }

    #[allow(dead_code)]
    fn interrupt_mask(&self) -> InterruptFlags {
        self.read(0x38)
    }

    fn set_interrupt_mask(&mut self, value: InterruptFlags) {
        self.write(0x38, value);
    }

    #[allow(dead_code)]
    fn raw_interrupt_status(&self) -> InterruptFlags {
        self.read(0x3c)
    }

    #[allow(dead_code)]
    fn masked_interrupt_status(&self) -> InterruptFlags {
        self.read(0x40)
    }

    fn clear_interrupt_status(&mut self, value: InterruptFlags) {
        self.write(0x44, value);
    }

    #[allow(dead_code)]
    fn dma_control(&self) -> DMAControlRegister {
        self.read(0x48)
    }

    fn set_dma_control(&mut self, value: DMAControlRegister) {
        self.write(0x48, value);
    }
}


bitfield! {
    struct DataRegister(u16) {
        [ 0.. 8] pub data: u8,
        [ 8..12] pub status: u8 as RxStatusRegister,
    }
}

bitfield! {
    struct RxStatusRegister(u8) {
        [0] pub framing_error,
        [1] pub parity_error,
        [2] pub break_error,
        [3] pub overrun_error,
    }
}

bitfield! {
    struct FlagRegister(u16) {
        [0] pub clear_to_send,
        [1] pub data_sent_ready,
        [2] pub data_carrier_detect,
        [3] pub busy,
        [4] pub rx_fifo_empty,
        [5] pub tx_fifo_full,
        [6] pub rx_fifo_full,
        [7] pub tx_fifo_empty,
        [8] pub ring_indicator,
    }
}


bitfield! {
    struct LineControlRegister(u8) {
        [7] pub parity_sticky,
        [5..7] pub word_length: u8 as WordLength,
        [4] pub fifo_enabled,
        [3] pub extended_stop,
        [2] pub parity_even,
        [1] pub parity_enabled,
        [0] pub break_enabled,
    }
}

impl LineControlRegister {
    fn parity(self) -> Parity {
        Parity::from_flags(
            self.parity_enabled(),
            self.parity_even(),
            self.parity_sticky(),
        )
    }

    pub fn set_parity(&mut self, value: Parity) {
        self.set_parity_enabled(value.enabled_flag());
        self.set_parity_even(value.even_flag());
        self.set_parity_sticky(value.sticky_flag());
    }
}


bitfield! {
    struct ControlRegister(u16) {
        [15] pub clear_to_send_flow_enabled,
        [14] pub request_to_send_flow_enabled,
        [13] pub aux_out_2,
        [12] pub aux_out_1,
        [11] pub request_to_send,
        [10] pub data_transmit_ready,
        [ 9] pub rx_enabled,
        [ 8] pub tx_enabled,
        [ 7] pub loopback,
        [ 2] pub infrared_low_power_mode,
        [ 1] pub infrared_enabled,
        [ 0] pub enabled,
    }
}

bitfield! {
    struct FIFOControlRegister(u16) {
        [3..6] pub rx_interrupt_trigger: u8 as FIFOInterruptTriggerLevel,
        [0..3] pub tx_interrupt_trigger: u8 as FIFOInterruptTriggerLevel,
    }
}

c_enum! {
    enum FIFOInterruptTriggerLevel(u8) {
        FourBytes,
        EightBytes,
        SixteenBytes,
        TwentyFourBytes,
        TwentyEightBytes,
    }
}


bitfield! {
    struct InterruptFlags(u16) {
        [10] pub overrun_error,
        [ 9] pub break_error,
        [ 8] pub parity_error,
        [ 7] pub framing_error,
        [ 6] pub rx_timeout,
        [ 5] pub tx,
        [ 4] pub rx,
        [ 3] pub modem_data_set_ready,
        [ 2] pub modem_carrier_detect,
        [ 1] pub modem_clear_to_send,
        [ 0] pub modem_ring,
    }
}

impl InterruptFlags {
    const ALL: InterruptFlags = InterruptFlags(0x07ff);
}


bitfield! {
    struct DMAControlRegister(u16) {
        [2] pub enabled_on_error,
        [1] pub tx_enabled,
        [0] pub rx_enabled,
    }
}
