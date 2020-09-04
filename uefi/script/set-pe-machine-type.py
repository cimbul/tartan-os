#!/usr/bin/env python3


def set_machine_type(pe_path, machine_type):
    # See https://docs.microsoft.com/en-us/windows/win32/debug/pe-format

    with open(pe_path, 'r+b') as pe_file:
        # Offset to PE signature stored at 0x3c. Microsoft's spec doesn't say how long
        # this field is, but in other places I've seen 4 bytes.
        pe_file.seek(0x3c)
        file_offset = int.from_bytes(pe_file.read(4), byteorder='little')
        pe_file.seek(file_offset)

        # Verify that the PE header is where we think it should be
        actual_signature = pe_file.read(4)
        if actual_signature != b'PE\0\0':
            raise Exception(
                'Invalid PE signature at offset 0x{:x}: {}'.format(
                    file_offset,
                    actual_signature,
                )
            )

        # The machine type is a two-byte field immediately after the PE header
        pe_file.write(machine_type.to_bytes(2, byteorder='little'))


def main():
    from argparse import ArgumentParser

    parser = ArgumentParser(
        description = 'Overwrite the machine type field in a PE (.exe) file')
    parser.add_argument('pe_path')
    parser.add_argument('machine_type', type=lambda x: int(x, base=0))
    args = parser.parse_args()

    set_machine_type(args.pe_path, args.machine_type)


if __name__ == '__main__':
    main()
