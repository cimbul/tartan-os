use core::alloc::{GlobalAlloc, Layout};
use core::cell::RefCell;
use core::mem::{align_of, size_of, transmute, MaybeUninit};
use core::pin::Pin;
use static_assertions::const_assert;
use tartan_bitfield::bitfield;


/// Simple kernel allocator that keeps a singly-linked list of blocks and finds the first
/// fit.
pub struct Allocator<'a> {
    block_list: Option<RefCell<BlockList<'a>>>,
}

impl<'a> Allocator<'a> {
    pub const fn uninitialized() -> Self {
        Self { block_list: None }
    }

    pub fn init(&mut self, block_list: BlockList<'a>) {
        self.block_list = Some(RefCell::new(block_list));
    }
}

unsafe impl<'a> GlobalAlloc for Allocator<'a> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let layout = layout.align_to(align_of::<BlockHeader>()).unwrap();
        let mut cursor = self.block_list.as_ref().unwrap().borrow_mut().front_mut();
        while !cursor.current().is_end() {
            if cursor.current().as_ref().can_fit(layout) {
                cursor.split_to_align(layout);
                cursor.split_to_minimum(layout.size());
                return cursor.current().as_ref().data_ptr();
            }
            cursor.move_next();
        }
        core::ptr::null_mut()
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _: Layout) {
        let mut block_list = self.block_list.as_ref().unwrap().borrow_mut();
        let mut cursor = block_list.cursor_from_data_ptr(ptr);
        cursor.current().set_free(true);

        // TODO: This would be much faster with a doubly-linked list
        let mut cursor = block_list.front_mut();
        while !cursor.current().is_end() {
            cursor.merge_next_if_free();
            cursor.move_next();
        }
    }

    // TODO: Implement realloc
}


pub struct BlockList<'a> {
    data: &'a mut [MaybeUninit<usize>],
}

impl<'a> BlockList<'a> {
    pub fn from_block(data: &'a mut [MaybeUninit<usize>]) -> Self {
        let mut start_header = BlockHeader::default();
        start_header.set_data_size((data.len() - 2) * size_of::<BlockHeader>());
        start_header.set_free(true);
        data[0] = MaybeUninit::new(start_header.into());

        let mut end_header = BlockHeader::default();
        end_header.set_free(false);
        data[data.len() - 1] = MaybeUninit::new(end_header.into());

        Self { data }
    }

    unsafe fn cursor_from_data_ptr(&mut self, data_ptr: *mut u8) -> CursorMut<'a> {
        let header_addr = data_ptr as usize - size_of::<BlockHeader>();
        debug_assert!(header_addr % align_of::<BlockHeader>() == 0);

        let header_ptr = header_addr as *mut BlockHeader;
        debug_assert!(self.data.as_ptr_range().contains(&(header_addr as *const _)));

        CursorMut(Pin::new_unchecked(&mut *header_ptr))
    }

    #[cfg(test)]
    fn front(&self) -> Cursor<'a> {
        #![allow(clippy::transmute_ptr_to_ptr)]
        Cursor(unsafe { Pin::new_unchecked(transmute(&self.data[0])) })
    }

    fn front_mut(&mut self) -> CursorMut<'a> {
        #![allow(clippy::transmute_ptr_to_ptr)]
        CursorMut(unsafe { Pin::new_unchecked(transmute(&mut self.data[0])) })
    }
}


#[cfg(test)]
struct Cursor<'a>(Pin<&'a BlockHeader>);

#[cfg(test)]
impl<'a> Cursor<'a> {
    fn current(&self) -> Pin<&'a BlockHeader> {
        self.0
    }

    fn move_next(&mut self) {
        if !self.0.is_end() {
            self.0 = self.0.next().unwrap();
        }
    }
}


struct CursorMut<'a>(Pin<&'a mut BlockHeader>);

impl<'a> CursorMut<'a> {
    fn current(&mut self) -> &mut Pin<&'a mut BlockHeader> {
        &mut self.0
    }

    fn move_next(&mut self) {
        if !self.0.is_end() {
            self.0 = self.0.next_mut().unwrap();
        }
    }

    #[cfg(test)]
    fn into_cursor(self) -> Cursor<'a> {
        Cursor(self.0.into_ref())
    }

    fn split_exact(&mut self, new_data_size: usize) {
        assert!(self.0.free());
        assert!(new_data_size % align_of::<BlockHeader>() == 0);

        assert!(new_data_size <= self.0.data_size());
        let spare_space = self.0.data_size() - new_data_size;
        if spare_space == 0 {
            // Nothing to do
            return;
        }

        assert!(spare_space >= size_of::<BlockHeader>());
        let mut spare_header = BlockHeader::default();
        spare_header.set_free(true);
        spare_header.set_data_size(spare_space - size_of::<BlockHeader>());
        let spare_header_addr = self.0.as_ref().data_ptr() as usize + new_data_size;
        let spare_header_ptr = spare_header_addr as *mut MaybeUninit<BlockHeader>;
        unsafe {
            *spare_header_ptr = MaybeUninit::new(spare_header);
        }

        // Update our own size last so that this is still in a safe state if anything
        // panics above.
        self.0.as_mut().set_data_size(new_data_size);
    }

    fn split_to_minimum(&mut self, min_data_size: usize) {
        self.split_exact(align_up(min_data_size, Layout::new::<BlockHeader>()));
    }

    fn split_to_align(&mut self, layout: Layout) {
        assert!(layout.align() >= align_of::<BlockHeader>());
        let start = self.0.as_ref().data_ptr() as usize;
        let aligned_start = align_up(start, layout);
        if start != aligned_start {
            let shift = aligned_start - start;
            assert!(shift >= size_of::<BlockHeader>());
            let new_data_size = shift - size_of::<BlockHeader>();
            self.split_exact(new_data_size);
            self.move_next();
        }
    }

    fn merge_next_if_free(&mut self) {
        match self.0.as_ref().next() {
            Some(next) if next.free() => {
                let additional_space = next.data_size() + size_of::<BlockHeader>();
                let new_size = self.0.as_ref().data_size() + additional_space;
                self.0.set_data_size(new_size);
            }
            _ => {}
        };
    }
}


bitfield! {
    struct BlockHeader(usize) {
        [0] free,
    }
}

impl BlockHeader {
    const SIZE_MASK: usize = !0b1;

    fn data_size(self) -> usize {
        self.0 & Self::SIZE_MASK
    }

    fn set_data_size(&mut self, value: usize) {
        assert!(value & !Self::SIZE_MASK == 0, "Size is not 2-byte aligned");
        self.0 &= !Self::SIZE_MASK;
        self.0 |= value;
    }

    fn data_ptr(self: Pin<&Self>) -> *mut u8 {
        let self_addr = unsafe { Pin::into_inner_unchecked(self) as *const _ as usize };
        let data_addr = self_addr + size_of::<Self>();
        data_addr as *mut u8
    }

    fn is_end(self) -> bool {
        self.data_size() == 0 && !self.free()
    }

    fn next_ptr(self: &Pin<&Self>) -> Option<*mut Self> {
        if self.is_end() {
            None
        } else {
            let next_addr = self.data_ptr() as usize + self.data_size();
            Some(next_addr as *mut Self)
        }
    }

    fn next(self: Pin<&Self>) -> Option<Pin<&Self>> {
        self.next_ptr().map(|p| unsafe { Pin::new_unchecked(&*p) })
    }

    fn next_mut(self: &Pin<&mut Self>) -> Option<Pin<&mut Self>> {
        self.as_ref().next_ptr().map(|p| unsafe { Pin::new_unchecked(&mut *p) })
    }

    fn can_fit(self: Pin<&Self>, layout: Layout) -> bool {
        if !self.free() {
            return false;
        }

        // Check space without factoring in alignment
        let self_size = self.data_size();
        let requested_size = layout.size();
        if requested_size > self_size {
            return false;
        }

        // Check whether alignment is required
        assert!(layout.align() >= align_of::<Self>());
        let start = self.data_ptr() as usize;
        let aligned_start = align_up(start, layout);
        if aligned_start == start {
            return true;
        }

        // If alignment is needed, check that the split block will still be large enough
        let shift = aligned_start - start;
        shift <= self_size && requested_size <= self_size - shift
    }
}

// Several calculations in BlockHeader's methods assume this
const_assert!(size_of::<BlockHeader>() % align_of::<BlockHeader>() == 0);


fn align_up(addr: usize, layout: Layout) -> usize {
    let align_mask = align_mask(layout);
    let aligned = layout.align().wrapping_add(addr.wrapping_sub(1) & align_mask);
    // TODO: Return Option<usize> instead?
    assert!(
        addr <= aligned,
        "Aligning {:#x} to {:#x} would overflow",
        addr,
        layout.align()
    );
    aligned
}

fn align_mask(layout: Layout) -> usize {
    // align is always a power of 2
    !(layout.align() - 1)
}


#[cfg(test)]
mod test {
    extern crate alloc;

    use super::*;

    #[track_caller]
    fn assert_aligns_up(alignment: usize, input: usize, expected_output: usize) {
        let layout = Layout::from_size_align(1, alignment).unwrap();
        assert_eq!(align_up(input, layout), expected_output);
    }

    #[test]
    fn test_align_up() {
        let max_align = 1_usize.reverse_bits();
        for &a in &[1, 2, 4, 8, 16, 32, 64, max_align] {
            assert_aligns_up(a, 0, 0);
        }

        for i in 1..0x100_usize {
            assert_aligns_up(1, i, i);
            assert_aligns_up(1, usize::MAX - i, usize::MAX - i);
        }
        assert_aligns_up(1, 0x8080_c46d, 0x8080_c46d);
        assert_aligns_up(1, 0xc3e4_ccc2, 0xc3e4_ccc2);
        assert_aligns_up(1, usize::MAX, usize::MAX);

        assert_aligns_up(2, 1, 2);
        assert_aligns_up(2, 2, 2);
        assert_aligns_up(2, 3, 4);
        assert_aligns_up(2, 4, 4);
        assert_aligns_up(2, 0x8080_c46d, 0x8080_c46e);
        assert_aligns_up(2, 0xc3e4_ccc2, 0xc3e4_ccc2);
        assert_aligns_up(2, usize::MAX - 2, usize::MAX - 1);
        assert_aligns_up(2, usize::MAX - 1, usize::MAX - 1);

        assert_aligns_up(0x200, 1, 0x200);
        assert_aligns_up(0x200, 2, 0x200);
        assert_aligns_up(0x200, 0x1ff, 0x200);
        assert_aligns_up(0x200, 0x200, 0x200);
        assert_aligns_up(0x200, 0x201, 0x400);
        assert_aligns_up(0x200, 0x8080_c46d, 0x8080_c600);
        assert_aligns_up(0x200, 0xc3e4_ccc2, 0xc3e4_ce00);
        assert_aligns_up(0x200, usize::MAX - 0x200, usize::MAX - 0x1ff);
        assert_aligns_up(0x200, usize::MAX - 0x1ff, usize::MAX - 0x1ff);

        assert_aligns_up(max_align, 1, max_align);
        assert_aligns_up(max_align, max_align - 1, max_align);
        assert_aligns_up(max_align, max_align, max_align);
    }

    #[test]
    #[should_panic]
    fn test_align_up_overflow_2() {
        let layout = Layout::from_size_align(1, 2).unwrap();
        align_up(usize::MAX, layout);
    }

    #[test]
    #[should_panic]
    fn test_align_up_overflow_512() {
        let layout = Layout::from_size_align(1, 0x200).unwrap();
        align_up(usize::MAX - 0x1fe, layout);
    }

    #[test]
    #[should_panic]
    fn test_align_up_overflow_max() {
        let max_align = 1_usize.reverse_bits();
        let layout = Layout::from_size_align(1, max_align).unwrap();
        align_up(max_align + 1, layout);
    }

    mod block_list {
        use super::*;
        use alloc::vec::Vec;

        #[track_caller]
        fn assert_same_address<A>(a: *const A, b: usize) {
            assert_eq!(a as usize, b);
        }

        #[test]
        fn test_from_block() {
            // When creating a block from a slice
            let mut data = [MaybeUninit::<usize>::zeroed(); 0x20];
            let addrs: Vec<_> = data.iter().map(|p| p as *const _ as usize).collect();
            let blocks = BlockList::from_block(&mut data);
            let mut cursor = blocks.front();

            #[cfg(target_pointer_width = "32")]
            let expected_data_size = 120; // (0x20 - 2) * 4

            #[cfg(target_pointer_width = "64")]
            let expected_data_size = 240; // (0x20 - 2) * 8

            // Then the first header should be placed at the beginning and have all the
            // free space, minus the overhead of the two headers.
            assert_eq!(cursor.0.data_size(), expected_data_size);
            assert!(cursor.0.free());
            assert_same_address(cursor.0.get_ref(), addrs[0]);
            assert_same_address(cursor.0.data_ptr(), addrs[1]);

            // And the next header should be the empty end marker
            cursor.move_next();
            assert!(cursor.current().is_end());
            assert_same_address(cursor.current().get_ref(), addrs[0x1f]);
        }

        #[test]
        #[should_panic]
        fn test_from_block_empty() {
            // When creating from a slice without enough space for either header
            BlockList::from_block(&mut [MaybeUninit::zeroed(); 0]);

            // Then the call should panic
        }

        #[test]
        #[should_panic]
        fn test_from_block_size_1() {
            // When creating from a slice without enough space for the end header
            BlockList::from_block(&mut [MaybeUninit::zeroed(); 1]);

            // Then the call should panic
        }

        #[test]
        fn test_from_block_size_2() {
            // When creating from a slice with exactly enough space for the start and
            // end headers
            let mut data = [MaybeUninit::zeroed(); 2];
            let addrs: Vec<_> = data.iter().map(|p| p as *const _ as usize).collect();
            let blocks = BlockList::from_block(&mut data);
            let mut cursor = blocks.front();

            // Then the first header should be free but empty
            assert!(cursor.current().free());
            assert_eq!(cursor.current().data_size(), 0);
            assert_same_address(cursor.current().get_ref(), addrs[0]);

            // And the next header should be the empty end marker
            cursor.move_next();
            assert!(cursor.current().is_end());
            assert_same_address(cursor.current().get_ref(), addrs[1]);
        }

        #[test]
        fn test_split() {
            let mut data = [MaybeUninit::zeroed(); 0x20];
            let addrs: Vec<_> = data.iter().map(|p| p as *const _ as usize).collect();
            let mut blocks = BlockList::from_block(&mut data);
            let mut cursor = blocks.front_mut();

            // When splitting a sufficiently-sized free block
            cursor.split_exact(0x12 * size_of::<usize>());

            // Then the first header should shrink to the requested data size
            let mut cursor = cursor.into_cursor();
            assert_eq!(cursor.current().data_size(), 0x12 * size_of::<usize>());
            assert!(cursor.current().free());
            assert_same_address(cursor.current().get_ref(), addrs[0]);
            assert_same_address(cursor.current().data_ptr(), addrs[1]);

            // And a new header should be added after it with the remaining space from the
            // first block, minus the overhead of an additional header
            cursor.move_next();
            assert_eq!(cursor.current().data_size(), 0xb * size_of::<usize>());
            assert!(cursor.current().free());
            assert_same_address(cursor.current().get_ref(), addrs[0x13]);
            assert_same_address(cursor.current().data_ptr(), addrs[0x14]);

            // And the next header should be unmodified
            cursor.move_next();
            assert!(cursor.current().is_end());
            assert_same_address(cursor.current().get_ref(), addrs[0x1f]);
        }

        #[test]
        fn test_split_minimal() {
            let mut data = [MaybeUninit::zeroed(); 3];
            let addrs: Vec<_> = data.iter().map(|p| p as *const _ as usize).collect();
            let mut blocks = BlockList::from_block(&mut data);
            let mut cursor = blocks.front_mut();

            // When splitting a block that has exactly enough space for an additional
            // header
            cursor.split_exact(0);

            // Then the first header should be empty
            let mut cursor = cursor.into_cursor();
            assert!(cursor.0.free());
            assert_eq!(cursor.0.data_size(), 0);
            assert_same_address(cursor.0.get_ref(), addrs[0]);

            // And so should the new header
            cursor.move_next();
            assert!(cursor.0.free());
            assert_eq!(cursor.0.data_size(), 0);
            assert_same_address(cursor.0.get_ref(), addrs[1]);

            // And the next header should be unmodified
            cursor.move_next();
            assert!(cursor.0.is_end());
            assert_same_address(cursor.0.get_ref(), addrs[2]);
        }

        #[test]
        fn test_split_no_op() {
            let mut data = [MaybeUninit::zeroed(); 0x20];
            let mut blocks = BlockList::from_block(&mut data);
            let mut cursor = blocks.front_mut();
            let original_size = 0x1e * size_of::<usize>();
            assert_eq!(cursor.0.data_size(), original_size);

            // When calling split with the current size
            cursor.split_exact(original_size);

            // Then nothing about this block should change
            assert_eq!(cursor.0.data_size(), original_size);
            assert!(cursor.0.free());
            assert!(!cursor.0.is_end());

            // And no new header should be created, so the next header will be the end
            cursor.move_next();
            assert!(cursor.current().is_end());
        }

        #[test]
        #[should_panic]
        fn test_split_unaligned() {
            let mut data = [MaybeUninit::zeroed(); 0x20];
            let mut blocks = BlockList::from_block(&mut data);
            let mut cursor = blocks.front_mut();

            // When calling split with a size that is not a multiple of the required
            // alignment
            cursor.split_exact(9);

            // Then the call should panic
        }

        #[test]
        #[should_panic]
        fn test_split_too_large() {
            let mut data = [MaybeUninit::zeroed(); 0x20];
            let mut blocks = BlockList::from_block(&mut data);
            let mut cursor = blocks.front_mut();

            // When calling split with a size that is larger than the current size
            cursor.split_exact(0x1f * size_of::<usize>());

            // Then the call should panic
        }
    }
}
