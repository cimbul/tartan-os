#!/usr/bin/env python3

# Add the script directory to the PYTHONPATH
import os.path
import sys
sys.path.append(os.path.dirname(__file__))

from os import SEEK_CUR
from pe_format import seek_executable_header, EXECUTABLE_TYPES

# (offset, size)
IMAGE_BASE_LOCATIONS = {
    'PE': (28, 4),
    'PE+': (24, 8),
}


def get_image_base(pe_path):
    with open(pe_path, 'rb') as pe_file:
        header_size = seek_executable_header(pe_file)

        # The location of the ImageBase field depends on whether this is a PE (32-bit) or
        # PE+ (64-bit) executable. That is determined by a 2-byte magic number at the
        # start of the executable header.
        executable_type_magic = int.from_bytes(pe_file.read(2), byteorder='little')
        executable_type = EXECUTABLE_TYPES.get(executable_type_magic)
        image_base_location = IMAGE_BASE_LOCATIONS.get(executable_type)
        if image_base_location is None:
            raise Exception(
                'Cannot determine image base for executable type {:#x}'.format(
                    executable_type_magic))

        # If this happens to be a COFF file without the Windows-specific fields, the
        # ImageBase field won't be present.
        image_base_offset, image_base_size = image_base_location
        if image_base_offset + image_base_size > header_size:
            raise Exception(
                'Optional header does not contain Windows-specific fields, including' +
                'the image base')

        # Read the image base. Account for the two bytes we read for the magic number.
        pe_file.seek(image_base_offset - 2, SEEK_CUR)
        image_base = int.from_bytes(pe_file.read(image_base_size), byteorder='little')

    return image_base


def main():
    from argparse import ArgumentParser

    parser = ArgumentParser(description='Read the ImageBase field from a PE (.exe) file')
    parser.add_argument('pe_path')
    args = parser.parse_args()

    image_base = get_image_base(args.pe_path)
    print('{:#x}'.format(image_base))


if __name__ == '__main__':
    main()
