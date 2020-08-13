# 512k-rom-512k-ram

Code to write data to the ROM on the RC2014 official 
512K ROM 512K RAM module using a Z80 processor

For full background and explanation see: http://bread80.com/?p=153

This code probably works with other similar modules but hasn't been tested with other hardware.
Specifically this code is writing to the SST39SF040 NOR flash IC. It may work with other chips which use software write protection.

Originally written by Mike Sutton - Bread80.com
Licenced under the MIT licence

The first section is some test routines which may be useful but you'll have to use the symbol table or disassembler to find the entry addresses (sorry).
Change the include_test_code constant to assemble without them.

Written and tested with the RASM assembler

Files:
NORflash.asm   - The Z80 Assember source
rasmoutput.bin - Raw binary output from the assembler
output.hex     - Hex file version of the output
