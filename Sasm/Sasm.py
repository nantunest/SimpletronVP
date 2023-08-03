#!/usr/bin/env python
import fire
import re

class Sasm:
    """The Simpletron Assembler Class."""

    OP_CODES = {
        "READ"  : 0x10,
        "WRITE" : 0x11,
        "LOAD"  : 0x20,
        "STORE" : 0x21,
        "ADD"   : 0x30,
        "SUB"   : 0x31,
        "DIV"   : 0x32,
        "MUL"   : 0x33,
        "JUMP"  : 0x40,
        "BLZ"   : 0x41,
        "BEZ"   : 0x42,
        "HALT"  : 0x43
    }

    # Define the regular expression pattern
    pattern = r"^(?:(\b[A-Z]+\b))?\s*(0x[0-9a-fA-F]+)?\s*(#.*)?"

    def assemble(self, files):
        with open(files, "r") as file:
            for line in file:
                # Use the regex pattern to extract the desired information
                if not line.replace(" ","").startswith("#"):
                    # Use the regex pattern to extract the desired information
                    match = re.match(self.pattern, line)

                    if match:
                        keyword = match.group(1)
                        address = match.group(2)
                        description = match.group(3)

                        print("Keyword:", keyword)
                        print("Address:", address)
                        print("Description:", description)
                    else:
                        print("No match found.")


if __name__ == '__main__':
  fire.Fire(Sasm)
