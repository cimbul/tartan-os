#!/usr/bin/env python3

# Add the script directory to the PYTHONPATH
import os.path
import sys
sys.path.append(os.path.dirname(__file__))

from pe_format import seek_file_header


def set_machine_type(pe_path, machine_type):
    with open(pe_path, 'r+b') as pe_file:
        seek_file_header(pe_file)

        # The machine type is a two-byte field at the start of the PE header
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
