# See https://docs.microsoft.com/en-us/windows/win32/debug/pe-format

from os import SEEK_CUR

PE_SIGNATURE = b'PE\0\0'

EXECUTABLE_TYPES = {
    0x10b: 'PE',
    0x107: 'ROM',
    0x20b: 'PE+',
}

def seek_file_header(pe_file):
    """
    Seek to the start of the COFF file header, as defined in the spec_.

    .. _spec: https://docs.microsoft.com/en-us/windows/win32/debug/pe-format#coff-file-header-object-and-image
    """

    # Offset to PE signature is stored at 0x3c. Microsoft's spec doesn't say how long
    # this field is, but in other places I've seen 4 bytes.
    pe_file.seek(0x3c)
    file_offset = int.from_bytes(pe_file.read(4), byteorder='little')
    pe_file.seek(file_offset)

    # Verify that the PE signature is where we think it should be
    actual_signature = pe_file.read(4)
    if actual_signature != PE_SIGNATURE:
        raise Exception(
            'Invalid PE signature at offset 0x{:x}: {}'.format(
                file_offset,
                actual_signature,
            )
        )


def seek_executable_header(pe_file):
    """
    Seek to the start of the "optional" header, as defined in the spec_. This header is
    always present for executable images. Returns the header size, or raises an exception
    if the header is not present.

    .. _spec: https://docs.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-image-only
    """

    seek_file_header(pe_file)

    # SizeOfOptionalHeader is a two-byte field 16 bytes into the file header. This
    # "optional" header is required for executables.
    pe_file.seek(16, SEEK_CUR)
    optional_header_size = int.from_bytes(pe_file.read(2), byteorder='little')
    if optional_header_size == 0:
        raise Exception('PE file appears to be an object file, not an executable')

    # Skip the rest of the file header. The executable header should be immediately after.
    pe_file.seek(2, SEEK_CUR)

    return optional_header_size
