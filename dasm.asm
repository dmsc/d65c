; =====================================================================
;
; A tiny disassembler that decodes all 256 opcodes for 65c02
; including an option for Rockwell/WDC bit operators
;

.comment

Requires a kernel_putc routine that outputs the character in A
while preserving X and Y.  A c65/py65mon compatible routine
is included with the test code below.

Compile like:

    64tass --nostart --list=dasm.lst --output dasm.bin --labels=dasm.sym dasm.asm

Run like:
    c65 -gg -r dasm.bin -a 0x200 -l dasm.sym

.endcomment

        .cpu "65c02"
        .enc "none"

; -------------------------------------------------------------
; Optionally enable support for the 4x8 WDC/Rockwell bit op instructions
; This adds about 64 bytes.  Otherwise these instructions are disassembled
; as NOP with equivalent addressing mode/size.

INCLUDE_BITOPS :?= 0            ; SMB0-7 etc add about 60 bytes

; -------------------------------------------------------------
; helper macros

; encode "ABC" as %aaa aabbb  bbc ccccf
s3w .sfunction s, f=0, (((s[0] & $1f)<<11) | ((s[1] & $1f)<<6) | ((s[2] & $1f)<<1) | f )

; encode two nibbles into one byte
n2b .sfunction lo, hi, ((hi<<4) | lo)

; -------------------------------------------------------------
; zero page storage, specific location is not important

        * = 0

pc      .word ?                 ; current address (word)
opcode  .byte ?                 ; opcode
args    .word ?                 ; operand bytes (must follow opcode location)
oplen   .byte ?                 ; # of operand bytes (0, 1 or 2)
format  .byte ?                 ; formatting bit pattern
tmp     .word ?                 ; tmp storage within several routines


; =====================================================================
;
; Call with <pc> set to the address to disassemble.
; The routine emits a single line of disassembly to kernel_putc
; terminated by a newline character.
; The address in <pc> is incremented just past the disassembled
; opcode and any operands so that dasm can be called repeatedly to
; produce a contiguos disassembly listing.

        * = $200                ; arbitrary location

dasm:
        ; -------------------------------------------------------------
        ; display current address followed by three spaces

        ldy pc
        lda pc+1
        jsr prword
        jsr pr3spc

        ; -------------------------------------------------------------
        ; determine addressing mode
        ; in: pc
        ; out: A = 0..15

        ldx #1                  ; set base length of 1
        stx oplen

        lda (pc)

        ; check if opcode has special mode
        ldx #n_normal_mode      ; use offset relative to mode_tbl
-
        cmp op_special_mode-n_normal_mode,x
        beq _found_mode
        inx
        cpx #n_normal_mode + n_special_mode
        bne -

        ; otherwise fetch mode from lookup table using bits ...bbbcc
        ; two mode nibbles are packed in each byte so first
        ; determine which byte and then which half
        and #$1f
        tax
_found_mode:
        txa
        lsr                     ; C=1 for odd (high nibble)
        tax
        lda mode_tbl,x
        bcc +
        lsr                     ; shift the high nibble down
        lsr
        lsr
        lsr
+
        and #$f

        ; -------------------------------------------------------------
        ; extract length (including opcode) and formatting pattern

        clv                     ; mode_R sets V=1
        beq _impl               ; mode 0 is implied (len 1, format 0)
        dea
        lsr                     ; A is format, C is length (0 => 2 bytes, 1 => 3 bytes)
        rol oplen               ; roll C to lsb of oplen leaving %10 or %11, leaving C=0
        tax                     ; save the format offset
        adc #121                ; 121+7 => 128, triggering V=1 for mode_R
        lda mode_fmt,x          ; grab the format byte
 _impl:
        sta format              ; store format (or zero)
        php                     ; save overflow status

        ; -------------------------------------------------------------
        ; print the opcode and each operand byte,
        ; copying them to opcode/args in the process

        ; loop Y=0,1,oplen-1, oplen,..3
        ldy #0
-
        jsr prpadnext           ; copy *pc++, showing "XX " then pad with "   "
        iny
        cpy #4
        bne -

        ; -------------------------------------------------------------
        ; determine mnemonic from opcode e.g. LDA or BBR2
        ; in: opcode
        ; out: X index to mnemonic table

test_mnemonic:                  ; entry point for testing mnemonic table

        lda opcode

        ; -------------------------------------------------------------
        ; First check opcodes that don't follow a clear pattern

        ldx #n_special-1
-
        cmp op_special,x
        beq _found_mnem
        dex
        bpl -

        ; -------------------------------------------------------------
        ; Then try matching one of several bitmasked slice
        ; This only falls through if we're in INCLUDE_BITOPS mode
        ldx #0
-
        lda slice_mask,x
        and opcode
        cmp slice_match,x
        beq _found_slice
        inx
        cpx #n_slice
        bne -

.if INCLUDE_BITOPS
        ; -------------------------------------------------------------
        ; otherwise it's a bitop instruction xaaby111
        ; where xy selects the base opcode and aab gives the bit index
        ; y=0 selects RMB/SMB, y=1 selects BBR/BBS
        ; x selects first or second of the pair

        ldx #mBITOPS
        lda opcode
        bit #%1000              ; check bit 3 (y)
        beq +
        inx
        ; update format to mode_ZR (it wasn't mode_R so stashed V flag is OK)
        ldy #format_ZR
        sty format
+
        asl                     ; check bit 7 (x), leaving A = aaby1110
        bcc +
        inx
        inx
+
        ldy #5                  ; roll top 3 bits from A down to index
-
        lsr
        dey
        bne -
        pha                     ; stash bit index for later
        txa
        bra _decode
.endif

_found_mnem:
        lda ix_special,x
        bra _decode

_found_slice:
        ; -------------------------------------------------------------
        ; found a matching slice, now calculate index into mnemonic table

        lda opcode              ; aaabbbcc
        stx tmp                 ; X is 0, 1,2,3, 4,5,6,7,8
        cpx #1                  ; X < 1 ?
        bmi _x0                 ; For X=0 we want index aaabb

        cpx #4                  ; X < 4 ?
        bpl +
        lda #mNOP               ; slice 1,2,3 all map to NOP
        bra _decode
+
        ; the remaining five slices map to four groups of 8 opcodes
        ; with an index like 001vwaaa where vw are the two LSB of X

        lsr tmp                 ; C=w
        ror                     ; waaabbbc
        lsr tmp                 ; C=v
        ror                     ; vwaaabbb
        sec
_x0:
        ror                     ; 0aaabbbc or 1vwaaabb
        lsr                     ; 00aaabbb or 01vwaaab
        lsr                     ; 000aaabb or 001vwaaa
        ; fall through to decode

_decode:
        ; -------------------------------------------------------------
        ; given the index of a packed word in our mnemonic table
        ; emit the corresponding three characters
        ; optionally add a bit index digit for bit ops

        ; A is an index to the matching word in mnemonic table

        asl                     ; double to get byte offset
        tax

        ; The three characters A=aaaaa, B, C are packed like
        ; (MSB) %faaa aabb  bbbc ccccc (LSB) and stored in little endian order
        ; we'll decode them in reverse and push to the stack

        lda mnemonics,x         ; bbcccccf
        sta tmp                 ; final char, C
        lda mnemonics+1,x       ; aaaaabbb
        sta tmp+1
        ldx #3
_unpack:
        ldy #5
        lda #%10                ; eventually 10xxxxx
_rol5:
        asl tmp
        rol tmp+1
        rol a
        dey
        bne _rol5

        jsr putc
        dex
        bne _unpack

.if INCLUDE_BITOPS
        bit tmp+1               ; tmp+1 is now %f0000000 or %00000000
        bpl +                   ; f flags a bit-indexed opcode e.g. BBS3
        pla
        jsr prnbl               ; ... so show the digit
+
.endif
        jsr prspc

        ; -------------------------------------------------------------
        ; show the operand value(s) if any

show_operand:
        plp                     ; recover V flag indicating relative address mode
        lda format
        beq _done               ; immediate mode?

.if INCLUDE_BITOPS
        cmp #format_ZR
        php                     ; save status for ?=format_ZR
        bne +
        dec oplen               ; we'll consume bitops args zp,r in two passes
+
.endif
        ldx #7                  ; loop through each bit in the template
-
        asl format
        bcc +                   ; display the corresponding character if bit is set
        lda s_mode_template,x
        jsr putc
+
        cpx #5                  ; insert operand byte/word or branch target after $ character
        bne +
        jsr prarg
+
        dex
        bpl -

.if INCLUDE_BITOPS
        ; -------------------------------------------------------------
        ; bitops like XZYn $zz, $rr are a pain..
        ; The first pass with mode ZR will emit "$zz,"
        ; since we decremented oplen above.
        ; Then we'll run a second pass, switching to mode R
        ; to emit the branch target "$hhll".

_pla:
        plp
        bne _done

        lda args+1              ; shift the second operand into position
        sta args
        lda #format_R           ; switch to mode R
        sta format
        bit jmp_putc            ; set V=1
        php
        bra show_operand        ; repeat
.endif
_done:
prnl:
        lda #$0a                ; add a newline and return via putc
        bra putc

; ---------------------------------------------------------------------
; various helper functions

prarg:
    ; print one or two operand bytes as byte, word or target address
    ; (from a relative branch) based on oplen and mode

        lda oplen               ; length is 2 or 3, meaning 1 or 2 operands
        lsr                     ; so LSB tells us one (0) or two (1) bytes
        lda args                ; fetch first arg
        bcc _one                ; one and done?

        tay                     ; stash LSB
        lda args+1              ; fetch MSB
        bra prword              ; print the address <A,Y>

_one:
        bvc prbyte              ; mode R is flagged with V=1

prrel:
    ; show the target address of a branch instruction
    ; we've already incremented PC past the operands
    ; so it is the baseline for the branch
        ldy pc+1                ; fetch MSB
        cmp #0                  ; test A flags without using X
        bpl +
        dey                     ; if negative, dec MSB
+
        clc
        adc pc                  ; calc LSB of target
        bcc +
        iny                     ; handle carry
+
        pha                     ; swap Y, A
        tya
        ply
prword:
    ; print the word with LSB=Y, MSB=A in big-endian order as <A><Y>
        jsr prbyte
        tya
        ; fall through
prbyte:
    ; wozmon code to print a nibble as two bytes
        PHA                     ; Save A for LSD.
        LSR
        LSR
        LSR
        LSR
        JSR     prnbl           ; Output hex digit.
        PLA                     ; Restore A.
        AND     #$0F            ; Mask LSD for hex print.
prnbl:
        ORA     #$30            ; Add "0".
        CMP     #$3A            ; Digit?
        BMI     putc            ; Yes, output it.
        ADC     #$06            ; Add offset for letter. (Carry is set)
putc:
jmp_putc:
        jmp kernel_putc         ; redirect to kernel routine

prpadnext:
    ; consume the opcode and operand bytes
    ; printing them as we go, padding to right align

        cpy oplen               ; loop through Y=0,1,oplen-1, oplen,..3
        bpl pr3spc              ; pad after operands

        lda (pc)
        sta opcode,y            ; fill opcode, args
        jsr prbyte              ; show opcode/operand
        inc pc                  ; advance pc
        bne +
        inc pc+1
+
        bra prspc               ; add trailing space

        ; done operand, pad with spaces
pr3spc:
    ; print three spaces
        jsr prspc
        jsr prspc
prspc:
    ; print one space
        lda #' '
        bra putc


; =====================================================================
;
; We use several data tables to drive the disassembly
; First we have tables map opcodes to mnemonics,
; then mnemonic tables which pack three letter labels into two byte words,
; then tables to decode the addressing mode,
; and finally tables to format the operands for each address mode.
;

.comment

Excluding a few special cases, we can group the opcodes in slices based on fixed
combinations of the least signifcant bits.
Representing the opcode as (msb) aaabbbcc (lsb) we have the following patterns:

X       Pattern     Mask   Target   Opcodes Offset  Index
 000    aaabb000    %111   %000     32x1    %0      aaabb   A >> 3

 001    aaa00010    %11111 %00010   1x8     %1000000  0     NOP
 010    aaabb011    %111   %011     1x8     %1000000  0     NOP

 011    11a1b100    %11010111 %11010100 1x4 %1000000  0     NOP

These slices are indexed with low two bits of X along with aaa.
there are five slices with the first and last mapping to the same opcode,
so we can handily index by the two lower bits of X

 100    aaa10010    %11111 %10010   8x1     %110000 aaa     A >> 5  * same opcodes as X=2
 101    aaa11010    %11111 %11010   8x1     %111000 aaa     A >> 5
 110    aaabbb10    %11    %10      8x8     %100000 aaa     A >> 5
 111    aaabb100    %111   %100     8x4     %101000 aaa     A >> 5
1000    aaabbb01    %11    %01      8x8     %110000 aaa     A >> 5  * same opcodes as X=2

This covers all 224 (=32+8+8+8+8+64+32+64) opcodes ending in 00, 01, 10 (c=0, 1, 2), and NOP ennding 011.
The remaining 32 opcodes are WDC/Rockwell extensions with a slightly different structure,
i.e. xaaby111 where xy select the opcode and aab give the bit index.

        0aab0111    %10001111   %0...0111   %1000001    RMB
        0aab1111    %10001111   %0...1111   %1000010    BBR
        1aab0111    %10001111   %1...0111   %1000011    SMB
        1aab1111    %10001111   %1...1111   %1000100    BBS

.endcomment

; ---------------------------------------------------------------------
; opcode mnemonic slices

n_slice = 9
.if INCLUDE_BITOPS

slice_mask:
    .byte %111, %11111, %111, %11010111, %11111, %11111, %11, %111, %11
slice_match:
    .byte %000, %00010, %011, %11010100, %10010, %11010, %10, %100, %01

.else

; one mask differs -------+
;                         |
slice_mask:  ;            v
    .byte %111, %11111,  %11, %11010111, %11111, %11111, %11, %111, %11
slice_match:
    .byte %000, %00010,  %11, %11010100, %10010, %11010, %10, %100, %01

.endif

; ---------------------------------------------------------------------
; packed mnemonic labels

mnemonics:

; aaabb000 indexed by aaabb
    .word s3w("BRK"), s3w("PHP"), s3w("BPL"), s3w("CLC")
    .word s3w("JSR"), s3w("PLP"), s3w("BMI"), s3w("SEC")
    .word s3w("RTI"), s3w("PHA"), s3w("BVC"), s3w("CLI")
    .word s3w("RTS"), s3w("PLA"), s3w("BVS"), s3w("SEI")
    .word s3w("BRA"), s3w("DEY"), s3w("BCC"), s3w("TYA")
    .word s3w("LDY"), s3w("TAY"), s3w("BCS"), s3w("CLV")
    .word s3w("CPY"), s3w("INY"), s3w("BNE"), s3w("CLD")
    .word s3w("CPX"), s3w("INX"), s3w("BEQ"), s3w("SED")

; index +32

; aaa10010 and aaabbb01 indexed by aaa (each repeated 9x)
    .word s3w("ORA"), s3w("AND"), s3w("EOR"), s3w("ADC"), s3w("STA"), s3w("LDA"), s3w("CMP"), s3w("SBC")
; aaa11010 indexed by aaa (each repeated 1x)
    .word s3w("INC"), s3w("DEC"), s3w("PHY"), s3w("PLY"), s3w("TXS"), s3w("TSX"), s3w("PHX"), s3w("PLX")
; aaabbb10 indexed by aaa (each repeated 8x)
    .word s3w("ASL"), s3w("ROL"), s3w("LSR"), s3w("ROR"), s3w("STX"), s3w("LDX"), s3w("DEC"), s3w("INC")
; aaabb100 indexed by aaa (each repeated 8x)
    .word s3w("TSB"), s3w("BIT"), s3w("NOP"), s3w("STZ"), s3w("STY"), s3w("LDY"), s3w("CPY"), s3w("CPX")

; these indices are reused for specials, so depend on the ordering of slices above

mLDX = 53
mBIT = 57
mSTZ = 59

; index +64

; aaabb111 (NOP repeated 32x) and 11a1b100 (NOP repeated 4x)
    .word s3w("NOP")

mNOP = 64

.if INCLUDE_BITOPS

mBITOPS = 65
; bitops xaaby111 with op xy repeated 8x
    .word s3w("RMB",1), s3w("BBR",1), s3w("SMB",1), s3w("BBS",1)

.endif

; index +65 or +69
mSpecial = (* - mnemonics) / 2

; mnemonics only used as specials
    .word s3w("TRB"), s3w("JMP"), s3w("TXA"), s3w("TAX"), s3w("WAI"), s3w("STP"), s3w("DEX")

mTRB = mSpecial
mJMP = mSpecial + 1
mTXA = mSpecial + 2
mTAX = mSpecial + 3
mWAI = mSpecial + 4
mSTP = mSpecial + 5
mDEX = mSpecial + 6

; ---------------------------------------------------------------------
; lookup for opcodes that don't fit a simple pattern

n_special = 15

op_special:
    .byte $14, $1c, $4c, $6c, $7c, $89, $8a, $9c, $9e, $a2, $aa, $cb, $db, $ca, $ea
ix_special:
    .byte mTRB,mTRB,mJMP,mJMP,mJMP,mBIT,mTXA,mSTZ,mSTZ,mLDX,mTAX,mWAI,mSTP,mDEX,mNOP

; ---------------------------------------------------------------------
; address mode decoding

.comment

There are 15 addressing modes, three of which (ZY, WI, WXI) only appear as exceptions
we use a four bit index where the lsb indicates a length of 2 or 3 (m_IMPL is a special case)
and the other three bits index an operand formatting pattern

.endcomment

; helper macro to pack opcode length and operand format in four bits
; we'll unpack by decrementing and then recover the length bit and format
mnbl .sfunction n, fmt, ( (n%2) + (fmt << 1) + 1 )

mode_NIL    = 0	            ; (*) INC, RTS (note we don't emit INC A)
mode_ZP     = mnbl(2,0)     ; LDA $42
mode_W      = mnbl(3,0)     ; LDA $1234
mode_IMM    = mnbl(2,1)     ; LDA #$42
; 4 is unused
mode_ZX     = mnbl(2,2)     ; LDA $42,X
mode_WX     = mnbl(3,2)     ; LDA $1234,X
mode_ZY     = mnbl(2,3)     ; LDA $42,Y
mode_WY     = mnbl(3,3)     ; LDA $1234,Y
mode_ZI     = mnbl(2,4)     ; LDA ($42)
mode_WI     = mnbl(3,4)     ; JMP ($1234)   (*) one opcode
mode_ZXI    = mnbl(2,5)     ; LDA ($42,X)
mode_WXI    = mnbl(3,5)     ; JMP ($1234,X) (*) one opcode
mode_ZIY    = mnbl(2,6)     ; LDA ($42),Y
; 14 is unused
mode_R      = mnbl(2,7)     ; BRA $1234

; we'll deal with mode_ZR, e.g. RMB $42,$1234, as an exception in code

; ---------------------------------------------------------------------
; address mode formatting

; This string lists all the characters that can appear in a formatted operand
; in reverse order of appearance

s_mode_template:
    .text "Y,)X,$(#"            ; reversed: #($,X),Y

mode_fmt:

; The format bytes index the template string which is stored backwards in s_mode_template
; The operand payload (single byte, word or branch target) is always inserted after $
;                 v-------- operand inserted
; string mask "#($,X),Y"        ; 1 or 2 byte address or branch target always inserted after $
        .byte %00100000	        ; 0: $@
        .byte %10100000         ; 1: #$@
        .byte %00111000	        ; 2: $@,x
        .byte %00100011	        ; 3: $@,y
        .byte %01100100	        ; 4: ($@)
        .byte %01111100	        ; 5: ($@,x)
        .byte %01100111	        ; 6: ($@),y
format_R  =   %00100000
        .byte format_R          ; 7: $@     (duplicate of 0 for mode_R)
format_ZR =   %00110000         ;    $@,    (then repeat with format_R)

; ---------------------------------------------------------------------
; lookup tables mapping opcode slices to address modes

.comment

The five least significant bits of an opcode give the default
address mode. ie. for opcode aaabbbcc there are 32 distinct groups
of eight opcodes sharing the bits bbbcc.
In some cases we don't care (or almost don't care) about cc but
I haven't found an efficient way to encode that.

.endcomment

mode_tbl:

; with rows b=0..7 and columns c=0..3 packing two modes per byte
    .byte n2b(mode_IMM, mode_ZXI),  n2b(mode_IMM, mode_NIL)
    .byte n2b(mode_ZP,  mode_ZP),   n2b(mode_ZP,  mode_ZP)
    .byte n2b(mode_NIL, mode_IMM),  n2b(mode_NIL, mode_NIL)
    .byte n2b(mode_W,   mode_W),    n2b(mode_W,   mode_W)       ; <= last becomes ZR for BITOPS
    .byte n2b(mode_R,   mode_ZIY),  n2b(mode_ZI,  mode_NIL)
    .byte n2b(mode_ZX,  mode_ZX),   n2b(mode_ZX,  mode_ZP)
    .byte n2b(mode_NIL, mode_WY),   n2b(mode_NIL, mode_NIL)
    .byte n2b(mode_WX,  mode_WX),   n2b(mode_WX,   mode_WX)     ; <= last becomes ZR for BITOPS

; ---------------------------------------------------------------------
; address mode lookup for opcode that don't fit the pattern

.comment

Three of these just switch X=>Y for mnemonics ending with X,
which we could possibly use to our advantage.
The other nine all occur for c=0, a<=4 (5*8 = 40 possible locations)
so again there might be a more efficient representation.

.endcomment

n_normal_mode = 32
n_special_mode = 12

mode_special:
    .byte n2b(mode_W,   mode_NIL), n2b(mode_NIL, mode_R)
    .byte n2b(mode_WI,  mode_ZP),  n2b(mode_ZY,  mode_ZY)
    .byte n2b(mode_W,   mode_WXI), n2b(mode_W,   mode_WY)

op_special_mode:
    .byte $20, $40, $60, $80
    .byte $6c, $14, $96, $b6
    .byte $1c, $7c, $9c, $be

; ---------------------------------------------------------------------
; dasm ends
; ---------------------------------------------------------------------


; =====================================================================
;
; Test routines - the location is arbtirary but separate
; from dasm so we can see compiled size more easily
;

        * = $600

test_dasm_self:
    ; disassemble the first $100 bytes of dasm itself
        stz pc
        lda #2
        sta pc+1                ; pc = $200
-
        jsr dasm                ; keep calling until we hit $300
        lda pc+1
        cmp #3
        bne -

        brk                     ; dasm shows brk argument byte
        .byte 0

test_all:
    ; simulate disassembly of each opcode X at address $1000+X
        lda #$10
        sta pc+1                ; pc = $1000
        stz opcode              ; opcode = 0
-
        lda opcode              ; loop
        sta pc                  ; set pc = $1000 + opcode
        sta (pc)                ; write opcode there
        pha
        bit #$f
        bne +
        jsr prnl                ; add a line break every 16
+
        pla
        jsr dasm
        inc opcode
        bne -

        jsr prnl
        brk
        .byte 0

test_mnemonic_table:
    ; generate a compact table of mnenomics without address
    ; mode to test opcode decoding
        lda #$60
        sta show_operand      ;TODO hack: modify code to return after mnemonic

        stz pc                ; loop index
_loop:
        lda #$7
        bit pc
        bne +
        jsr prnl
+
        lda pc                ; reverse index to get opcode
        ldy #8
-
        lsr a
        rol opcode
        dey
        bne -
        jsr test_mnemonic
        inc pc
        bne _loop

        jsr prnl
        brk
        .byte 0

; -------------------------------------------------------------
; a c65/py65mon compatible output routine

kernel_putc:
        sta $f001
        rts

; ---------------------------------------------------------------------
; eof
; ---------------------------------------------------------------------
