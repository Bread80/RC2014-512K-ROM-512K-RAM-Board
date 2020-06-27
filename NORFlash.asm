;**********************************
; Code to write data to the ROM on the RC2014 official 
; 512K ROM 512K RAM module using a Z80 processor
;
; For full background and explanation see: http://bread80.com/?p=153
;
; This code probably works with other similar modules but hasn;t been tested 
; with other hardware.
; Specifically this code is writing to the SST39SF040 NOR flash IC. It may work 
; with other chips which use software write protection.
;
; Originally written by Mike Sutton - Bread80.com
; Licences under the MIT licence
;
; The first section is some test routines which may be useful but you'll have to
; use the symbol table or disassembler to find the entry addresses (sorry)
; Change the include_test_code constant to assemble without them.
;
; Written and tested with the RASM assembler
;**********************************
org $8000

include_test_code equ 1		;1= Assemble with test code. 0 = Assemble without test code

;Start of test routines
if include_test_code

;*******************************
;Test code: clear a sector and write a test byte.
;To verify in SCM (Small Computer Monitor)
;o79 1 - Page ROM bank in
;m4000 - Memory dump. First byte should be $EA and other bytes $FF
;*******************************
write_test_byte:
	ld d,1		;Target physical ROM bank
	ld e,0		;Target sector within ROM bank (0..3)
	call prog_sector_erase
	
	ld hl,$0000	;Target address
	ld d,1		;Target ROM bank
	ld a,$ea	;Data to write
	call prog_write_byte
	ret
	
;************************
; Test code: Erase a sector and write an ASCIIZ test string into the first few bytes
;
; To verify in SCM:
;o79 1  - Page in ROM bank	
;m4000  - Memory dump. Data should be written in first few bytes, other bytes in sector set to $ff
;************************
write_test_string:
	ld d,1		;Physical ROM bank to write to
	ld e,0		;Sector index within ROM bank (0..3)
	call prog_sector_erase
	
	ld hl,$000	;Target address
	ld bc,message	;Message source address
write_loop:
	ld a,(bc)
	and a
	ret z		;Return when done
	call prog_write_byte
	
	inc bc
	inc hl
	jr write_loop

	;Test string
message: db "Hello, World!",0
	nop		;To get the disassembler back on track


;*************	
;Test routine - write (copy data to) a single 4Kb sector
;Copy 4Kb of data pointed to by HL to the sector with index E (0..3) within ROM bank D
test_rom_write_sector:
	ld hl,$8000		;Source address
	ld d,1			;Target physical ROM bank
	ld e,0			;Target sector (0..3)
	call rom_write_sector
	ret
	
;************
;Test routine - write (copy data to) an entire bank
;Copy 16Kb of data pointed at by HL to ROM bank in D ($1f)
test_rom_write_bank:
	ld hl,0		;Source address
	ld d,2		;Target physical ROM bank
	call rom_write_bank
	ret

	
;***********************************
;
;!!!!!!!!!!!!!!!!!!!!!!!!!THIS OVERWRITES PRETTY MUCH YOUR ENTIRE ROM!!!!!!!!!!
;
; Test routine: Writes test data into every sector of every bank (except ROM bank 0)
; Data written: 
;  Byte 0: ROM bank index (+ ASCII 'A')
;  Byte 1: Sector index (+ ASCII '0')
;  Byte 2: High byte of logical address
;
; Entry: None
; Exit: AF,BC,DE,HL corrupt.
;
; To verify in SCM:
;o79 <n> - where n is the ROM bank number ($0..$1f)
;m4000 - memory dump (or m5000, m6000, m7000). The first three bytes should be written and everything else $ff
;***********************************
write_test_all_sectors:
	ld d,1		;Block (start at one because block zero is our monitor!)
write_test_loop_outer:
	ld e,0		;Sector
write_test_loop_inner:
	call prog_sector_erase
	
	ld a,e		;Convert sector index to an address: D bits 1..0 to HL bits 13..12
	rlca		
	rlca
	rlca
	rlca
	ld h,a
	ld l,0
	
	ld a,d		;Write block index
	add 'A'
	call prog_write_byte
	
	ld a,e		;Write sector index
	add '0'
	inc hl
	call prog_write_byte
	
	ld a,h		;Write high byte of address
	inc hl
	call prog_write_byte
	
	inc e		;Next sector
	bit 2,e
	jr z,write_test_loop_inner	;Loop if sector < 4
	
	inc d		;Otherwise, next block
	bit 5,d
	jr z,write_test_loop_outer	;Loop if sector < 32
	ret
	
;End of test code	
endif

;*****************************************************
; Program flash memory used as ROM in the RC2014 512k ROM 512k RAM memory card
;
; The Z80/RC2014 Board Address Space
; ----------------------------------
;
; The chip used by the RC2014 is the SST39SF040.
; The write protections mechanisms are standard across the industry so this code 
; should work with other chips with minor modifications (e.g to the port address(es).
;
; The 512k ROM 512k RAM board divides memory up into 16k banks. Thus, there are 32 banks of
; RAM and 32 pages of ROM. The ROM banks have index $00..$1F and the RAM banks $20..$3F
;
; The board divides Z80s address space into four 16kb memory bank. Any bank of RAM or 
; ROM can be paged into any page of the Z80s address space. (Side note: the same physical 
; bank can be mapped into more than one logical bank at the same time.)
;
; The board uses four port addresses to control which physical banks and paged into which 
; physical banks.
;
; This table maps Z80 logical banks map to memory addresses and port numbers as follows.
;	0	$0000..$3fff	$78
;	1	$4000..$7fff	$79
;	2	$8000..$bfff	$7a
;	3	$c000..$ffff	$7b
;
; Thus, writing $00 to port $78 will map the first physical bank of ROM into the first logical
; bank of memory. Writing $20 to port $7b will map the first physical bank of RAM into memory
; addresses $c000 to $ffff
;
; (PS: port $7c is used to turn pagin on (value 1) or off (value 0). In the off state the Z80s
; address space is 'mapped' directly to ROM banks 0 to 3. This is the default state after reset.
;
; Programming the Norflash IC
; ---------------------------
;
; The Norflash IC used divides memory up into 4kb sectors. Before writing it is necessary to
; the entire sector into which data is to be written.
;
; The IC has a mechanism to protect it from accidental data writes and accidental erasure.
; in practice this means writing a specific sequence of data bytes to a specific sequence of 
; memeory addresses. It is then necessary to wait until the IC has completed the operation 
; before attempting to use the IC again (whether to read, write or erase.)
;
; The command sequence to erase a sector is:
;
; Sector erase (4K sector size)
; Addr    Data
; $5555 = $AA
; $2AAA = $55
; $5555 = $80
; $5555 = $AA
; $2AAA = $55
; SAx = $30 - SAx = sector address on address lines Amax to A12. (Amax is the highest address line).
; <busy-wait> - wait to operation to complete
;
; The command sequence to write a byte is:
; Addr    Data
; $5555 = $AA
; $2AAA = $55
; $5555 = $A0
; <addr> = <data> - Write the required byte to the required memory address
; <busy-wait> - wait for the sequence to complete
;
; The write protection command sequence must be followed before each and every byte written.
;
; (There are other operations which aren't implemente here, notably erasing the entire chip.)
;
; There are a couple of ways to sense when the operation is complete, the one used here is
; as follows:
; For writing: read back data from data from the same address. Data byte 7 will be inverted until
; the operation is complete.
; For sector erase: read data from the sector address being erased. Data byte 7 will be a zero
; until the operation is complete (Erased data is set to $ff, so this is also reading back inverted 
; data).
;
; WARNING: During the command sequnce you MUST NOT WRITE TO MEMORY. Any memory writes will cancel
; the operation and may cuase system instability. This includes PUSHing data on the stack and CALLing
; subroutines. Reading from memory and both reading and writing to output ports is not an issue. 
; This may also mean the using a debugger for operations such as breakpoints or single stepping
; on the programming code can cause it to fail and can cause system instability.
;
; WARNING: Using a monitor which is stored in the ROM itself will cause problems when single stepping 
; through a programming operation since the ROM will be unavailable during the 'busy-wait' period.
;
; The Programming Code
; --------------------
;
; The code below maps banks of ROM into logical bank 1 (addresses $4000 to $ffff) as required. This
; includes the addresses required for the command sequence as well as the address actually being 
; programmed. The memory mapping scheme, however, means that this can become somewhat confusing.
;
; For example, the command sequence includes requiring data to be written to address $2AAA. This is in
; physical ROM bank zero. The code does all writes to logical bank 1 as described above. Thus the 
; address $2AAA is mapped to address $6AAA. Address lines A14 and above of the physical IC are 
; provided from the banking hardware, using the values of logical address lines A15 and A14. 
; Logical address lines A13 to A0 are passed directly to the ROM (or RAM).
;
; This the code below extracts the lower 14 bits of ROM addresses, sets A14 and clears A15 to get
; addresses in logical bank zero.
;
;******************************************

base_bank_port equ $78	;First of the four sequential port addresses
logical_bank equ 1		;The logical bank we'll be using to address ROM
bank_port equ base_bank_port + logical_bank	;The port address we'll be using

logical_bank_mask equ $b000		;Mask for A15 and A14 - logical bank number
logical_address_mask equ $3fff	;Mask for A13 to A0 - address within a bank
logical_bank_address equ logical_bank << 14	;The logical bank as a logical address

;Software protection override constants/command sequences.
;Each step needs a physical bank number, logical address within the bank and data byte
prog_bank1 equ $5555 >> 14	;The physical bank containing our address
prog_addr1 equ $5555 and logical_address_mask or logical_bank_address

;prog_bank1 equ 1
;prog_addr1 equ $5555; and $3fff	;14 bit addresses! Hard coded logical banks
prog_data1 equ $aa

prog_bank2 equ $2aaa >> 14
prog_addr2 equ $2aaa and logical_address_mask or logical_bank_address
;prog_bank2 equ 0
;prog_addr2 equ $2aaa or $4000	;Hard code the logical bank
prog_data2 equ $55

prog_bank3 equ prog_bank1	;Address $5555 again
prog_addr3 equ prog_addr1
prog_data3_write equ $a0	;For writing a byte
prog_data3_erase equ $80	;For erasing a sector

prog_bank4 equ prog_bank1	;Address $5555 again
prog_addr4 equ prog_addr1
prog_data4 equ $aa

prog_bank5 equ prog_bank2	;Address $2aaa again
prog_addr5 equ prog_addr2
prog_data5 equ $55

prog_data6_erase equ $30	;Erase command to be sent along with the sector address

;************************
; Sector Erase for NOR flash
; Sectors are 4kb in size
; Entry: D = Logical ROM bank to write to (0..$1f). E = index of sector within the bank (0..3)
; I.e. sector 0 = addresses 0..0fff, 1=1000..1fff, 2=2000..2fff, 3=3000..3fff
;************************
prog_sector_erase:	
	push af
	push bc
	push hl
	
	push de
	;Memory bank we'll be switching blocks into
	ld c,bank_port	
	
	;!!!!!!DO NOT DO ANY MEMORY WRITES UNTIL PROGRAMMING IS FINISHED (all bytes sent)
	;This includes DO NOT PUSH VALUES ON THE STACK!!!!!
	
	;Overcoming write protection means writing some specific bytes to specific memory addresses in a specified sequence

	;Write data byte 1
	ld e,prog_bank1		;Switch the phsical bank into the logical bank
	out (c),e
	ld hl,prog_addr1	;Address to write to
	ld (hl),prog_data1	;Write

	;Write data byte 2
	ld e,prog_bank2
	out (c),e
	ld hl,prog_addr2
	ld (hl),prog_data2
	
	;Write data byte 3
	ld e,prog_bank3
	out (c),e
	ld hl,prog_addr3
	ld (hl),prog_data3_erase
	
	;Write data byte 4
	ld e,prog_bank4
	out (c),e
	ld hl,prog_addr4
	ld (hl),prog_data4
	
	;Write data byte 5
	ld e,prog_bank5
	out (c),e
	ld hl,prog_addr5
	ld (hl),prog_data5
	;End of write protection override sequence

	pop de		;Retrieve
	
	;Set sector to erase
	ld a,d		;Physical bank index
	and $1f		;Validate it
	out (c),a	;Switch bank in
	
	;Convert 2-bit sector address in E to bits 13 and 12 of HL
	ld a,e		;Move sector address into A
	and $03		;And make sure it's valid
	rlca		;Sector index is in A register, valid range 0..3, (bits 1 and 0)
	rlca		;Move bits 1,0 to bits 5,4 (two bit sector number to address 0x(xx)..3x(xx))
	rlca
	rlca
	or logical_bank << 6
;	or $40		;Move address to memory bank 1 (addresses 4x(xx)..7x(xx))
	ld h,a		;Move to a 16-but address in HL
	ld l,0
	
	;**** DO NOT SINGLE STEP THIS INSTRUCTION (if the monitor is in ROM) 
	; - the ROM is not available until /after/ the busy wait loop
	ld (hl),prog_data6_erase	;Send the sector address and protection override data byte
	
	;Wait until operation completes
prog_erase_busy_loop:
	ld a,(hl)	;Read back data
	rlca		;Bit 7 will return a zero until done
	jr nc,prog_erase_busy_loop
	
	pop hl
	pop bc
	pop af
	ret
	
;*****************************
; Write a single byte to NOR flash
; Entry: HL = Address to write to within the 16kb bank (0 - $3FFF) - bits 15 and 14 will be ignored.
;        A = Byte to write. D = Logical ROM bank to write to (0..$1f)
; Exit: Corrupt: AF. All other registers preserved
;*****************************
prog_write_byte:
	push bc	;Preserve
	push de
	push hl		;Push HL to preserve return value
	
	push hl		;Push HL again so we can retrieve it
	;Memory bank we'll be switching blocks into
	ld c,bank_port	

	;!!!!!!DO NOT DO ANY MEMORY WRITES UNTIL PROGRAMMING IS FINISHED (all bytes sent)
	;This includes DO NOT PUSH VALUES ON THE STACK!!!!!
	
	;Overcoming write protection means writing some specific bytes to specific memory addresses in a specified sequence
	;Write data byte 1
	ld e,prog_bank1	;Switch bank in
	out (c),e
	ld hl,prog_addr1	;Address to program
	ld (hl),prog_data1	;Byte to program

	;Write data byte 2
	ld e,prog_bank2
	out (c),e
	ld hl,prog_addr2
	ld (hl),prog_data2
	
	;Write data byte 3
	ld e,prog_bank3
	out (c),e
	ld hl,prog_addr3
	ld (hl),prog_data3_write
	;End of write protection override sequence
	
	;Write actual data
	out (c),d	;Switch bank in
	
	pop hl		;Retrieve address
	ld e,a		;Preserve A
	ld a,logical_address_mask >> 8
;	ld a,$3f	;Mask to a 14 bit address
	and h
	or logical_bank << 6
;	or $40		;Map to address block $4000..$7fff - memory bank 1
	ld h,a
	ld a,e		;Retrieve data
	
	;**** DO NOT SINGLE STEP THIS INSTRUCTION (if the monitor is in ROM) 
	; - the ROM is not available until /after/ the busy wait loop
	ld (hl),a	;Write our new byte
	
	;Busy loop - wait for chip to finish
	;Bit 7 will be inverted until operation is complete
	ld b,a		;Preserve
prog_write_busy_loop:
	ld a,(hl)	;Fetch written data
	xor b       ;XOR bit 7 - will be zero when done
	rlca
	jr c,prog_write_busy_loop
	
	pop hl
	pop de
	pop bc
	ret

;*********************************
; Write (copy) data to a single sector. Each sector is $1000 (4kb) long
;
; Entry: HL = Source addr of first byte of block to copy
; D = Target logical ROM bank (0..$1f)
; E = Target 4k sector (0..3)
; Exit: HL = addr of byte after last byte copied (ie. entry HL + $1000).
;       All other registers preserved.
;*********************************
rom_write_sector:
	push af		;Preserve
	push bc
	push de
	push ix
	
	call prog_sector_erase	;Erase sector

	push hl		;Move Source to IX
	pop ix

	ld a,e		;Sector
	rlca		;Convert to address
	rlca
	rlca
	rlca
	ld h,a		;HL = Target addr
	ld l,0
	
	ld bc,$1000	;Byte count
	
				;D=ROM bank
				;BC=Counter
				;IX=Source
				;HL=Dest
rom_write_sector_loop:
	ld a,(ix)	;Get byte to copy
	call prog_write_byte	;Write byte
	inc ix		;Next source
	inc hl		;Next dest

	dec bc		;Dec counter
	ld a,b		;Is it zero?
	or c
	jr nz,rom_write_sector_loop	;Loop if not

	push ix		;Source back to HL
	pop hl
	
	pop ix		;Restore
	pop de
	pop bc
	pop af
	ret

;******************************
; Write (copy) an entire 16kb ROM bank	
; (Simply repeats ROM_write_sector four times)
; Entry: HL=Source address to start copying from. D=Target physical ROM bank (0..$1f)
; Exit: HL=Address one byte after the last byte read. All other registers preserved.
;******************************
rom_write_bank:
	push bc	;Preserve
	push de
	
	ld b,4		;Counter
	ld e,0		;Sector
	
rom_write_bank_loop:
	call rom_write_sector	;Write sector
	
	inc e		;Next sector
	djnz rom_write_bank_loop
	
	pop de		;Restore
	pop bc
	ret