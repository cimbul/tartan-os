//! Driver for the common UART interface provided on PCs that use the National
//! Semiconductor 16550 chip and derivatives (itself based on the NS 8250).

// TODO: Remove this once full functionality is implemented
#![allow(unused)]

use crate::{baud_from_divisor, divisor_from_baud, LineMode, Parity, WordLength, UART};
use core::convert::TryInto;
use tartan_arch::x86_common::io;
use tartan_bitfield::bitfield;
use tartan_c_enum::c_enum;


/// Strategy that provides access to registers for a 16550 UART.
pub trait RegisterAccess {
    /// Read 8-bit register at the given port
    fn read(&self, port: u16) -> u8;
    /// Write 8-bit register at the given port
    fn write(&mut self, port: u16, data: u8);
}


/// Strategy that reads and writes registers from I/O space.
#[derive(Default)]
pub struct ActualRegisterAccess;

impl RegisterAccess for ActualRegisterAccess {
    fn read(&self, port: u16) -> u8 {
        unsafe { io::in_u8(port) }
    }

    fn write(&mut self, port: u16, data: u8) {
        unsafe { io::out_u8(port, data) }
    }
}


/// Driver for a PC UART based on the National Semiconductor 16550.
pub struct UART16550<A: RegisterAccess = ActualRegisterAccess> {
    access: A,
    base_port: u16,
}

impl<A: RegisterAccess> UART16550<A> {
    const MAX_BAUD: u32 = 115_200;

    fn pop_rx_queue(&mut self) -> u8 {
        self.set_divisor_access(false);
        self.access.read(self.base_port)
    }

    fn push_tx_queue(&mut self, data: u8) {
        self.set_divisor_access(false);
        self.access.write(self.base_port, data);
    }

    fn interrupt_enable(&mut self) -> InterruptEnableRegister {
        self.set_divisor_access(false);
        InterruptEnableRegister(self.access.read(self.base_port + 1))
    }

    fn set_interrupt_enable(&mut self, value: InterruptEnableRegister) {
        self.set_divisor_access(false);
        self.access.write(self.base_port + 1, value.into());
    }

    fn divisor(&mut self) -> u16 {
        self.set_divisor_access(true);
        let mut divisor = u16::from(self.access.read(self.base_port));
        divisor |= u16::from(self.access.read(self.base_port + 1)) << 8;
        divisor
    }

    fn set_divisor(&mut self, value: u16) {
        #![allow(clippy::cast_possible_truncation)]
        self.set_divisor_access(true);
        self.access.write(self.base_port, value as u8);
        self.access.write(self.base_port + 1, (value >> 8) as u8);
    }

    fn interrupt_id(&self) -> InterruptID {
        InterruptID(self.access.read(self.base_port + 2))
    }

    fn set_fifo_control(&mut self, value: FIFOControlRegister) {
        self.access.write(self.base_port + 2, value.into());
    }

    fn line_control(&self) -> LineControlRegister {
        LineControlRegister(self.access.read(self.base_port + 3))
    }

    fn set_line_control(&mut self, value: LineControlRegister) {
        self.access.write(self.base_port + 3, value.into());
    }

    fn modem_control(&self) -> ModemControlRegister {
        ModemControlRegister(self.access.read(self.base_port + 4))
    }

    fn set_modem_control(&mut self, value: ModemControlRegister) {
        self.access.write(self.base_port + 4, value.into());
    }

    fn line_status(&self) -> LineStatusRegister {
        LineStatusRegister(self.access.read(self.base_port + 5))
    }

    fn modem_status(&self) -> ModemStatusRegister {
        ModemStatusRegister(self.access.read(self.base_port + 6))
    }

    fn set_divisor_access(&mut self, value: bool) {
        // TODO: This is very inefficient. Should cache the current state or assume that
        // false is the default. Just need to avoid synchronization issues...
        let mut line_control = self.line_control();
        line_control.set_divisor_access(value);
        self.set_line_control(line_control);
    }
}

impl UART16550<ActualRegisterAccess> {
    /// Create a driver for the UART with the given base I/O address
    pub fn new(base_port: u16) -> Self {
        Self { access: ActualRegisterAccess::default(), base_port }
    }
}

impl<A: RegisterAccess> UART for UART16550<A> {
    fn reset(&mut self) {
        self.set_interrupt_enable(InterruptEnableRegister::default());
        self.set_fifo_control(FIFOControlRegister::default());
        self.set_modem_control(ModemControlRegister::default());
        self.set_line_control(LineControlRegister::default());
    }

    fn line_mode(&mut self) -> LineMode {
        let line_control = self.line_control();
        let mut line_mode = LineMode::default();
        line_mode.baud = baud_from_divisor(Self::MAX_BAUD, u32::from(self.divisor()));
        line_mode.data_bits = line_control.word_length().bits();
        line_mode.parity = line_control.parity();
        line_mode.extended_stop = line_control.extended_stop();
        line_mode
    }

    fn set_line_mode(&mut self, line_mode: LineMode) {
        #[allow(clippy::cast_possible_truncation)]
        self.set_divisor(divisor_from_baud(Self::MAX_BAUD, line_mode.baud) as u16);

        let mut line_control = self.line_control();
        line_control.set_word_length(
            WordLength::from_bits(line_mode.data_bits).expect("Unsupported word length"),
        );
        line_control.set_parity(line_mode.parity);
        line_control.set_extended_stop(line_mode.extended_stop);
        self.set_line_control(line_control);
    }

    fn write(&mut self, data: &[u8]) {
        for &byte in data {
            // FIXME (sync)
            while !self.line_status().tx_empty() {}
            self.push_tx_queue(byte);
        }
        // FIXME (sync)
        while !self.line_status().tx_empty() {}
    }
}


bitfield! {
    struct InterruptEnableRegister(u8) {
        [0] pub rx_data_available,
        [1] pub tx_empty,
        [2] pub line_status_change,
        [3] pub modem_status_change,
        [4] pub sleep_mode, // 16750
        [5] pub low_power_mode, // 16750
    }
}

bitfield! {
    struct InterruptID(u8) {
        [0]    pub interrupt_pending,
        [1..4] pub interrupt_type: u8 as InterruptType,
        [5]    pub fifo_64byte_enabled, // 16750
        [6..8] pub fifo_status: u8 as FIFOStatus,
    }
}

c_enum! {
    enum InterruptType(u8) {
        ModemStatusChange,
        TxEmpty,
        RxDataAvailable,
        LineStatusChange,
        CharacterTimeout = 6,
    }
}

c_enum! {
    enum FIFOStatus(u8) {
        NotEnabled,
        Unusable = 2,
        Enabled,
    }
}

bitfield! {
    struct FIFOControlRegister(u8) {
        [0]    pub enabled,
        [1]    pub clear_rx,
        [2]    pub clear_tx,
        [3]    pub dma_mode,
        [5]    pub fifo_64byte_enabled, // 16750
        [6..8] pub interrupt_trigger: u8 as FIFOInterruptTrigger,
    }
}

c_enum! {
    enum FIFOInterruptTrigger(u8) {
        OneByte,
        FourBytes,
        EightBytes,
        FourteenBytes,
    }
}

bitfield! {
    struct LineControlRegister(u8) {
        [0..2] pub word_length: u8 as WordLength,
        [2]    pub extended_stop,
        [3]    pub parity_enabled,
        [4]    pub parity_even,
        [5]    pub parity_sticky,
        [6]    pub break_enabled,
        [7]    pub divisor_access,
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
    struct ModemControlRegister(u8) {
        [0] pub data_terminal_ready,
        [1] pub request_to_send,
        [2] pub aux_out_1,
        [3] pub aux_out_2,
        [4] pub loopback,
        [5] pub auto_flow_control, // 16750
    }
}

bitfield! {
    struct LineStatusRegister(u8) {
        [0] pub data_available,
        [1] pub overrun_error,
        [2] pub parity_error,
        [3] pub framing_error,
        [4] pub break_received,
        [5] pub tx_empty,
        [6] pub tx_empty_line_idle,
        [7] pub fifo_error,
    }
}

bitfield! {
    struct ModemStatusRegister(u8) {
        [0] pub clear_to_send_changed,
        [1] pub data_set_ready_changed,
        [2] pub ring_trailing_edge,
        [3] pub carrier_detect_changed,
        [4] pub clear_to_send,
        [5] pub data_set_ready,
        [6] pub ring,
        [7] pub carrier_detect,
    }
}
