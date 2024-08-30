; =====================================================================

.comment

A tiny disassembler that decodes all 256 opcodes for 65c02
including an option for Rockwell/WDC bit operators.

Requires a kernel_putc routine that outputs the character in A
while preserving X and Y.  A c65/py65mon compatible routine
is included with the test code below.

Compile like:

    64tass --nostart --list=dasm.lst --output dasm.bin --labels=dasm.sym dasm.asm

Run like:
    c65 -gg -r dasm.bin -a 0x200 -l dasm.sym

The test routines can be used to check expected output:

    g 600       ; test_dasm_self: disassemble first $100 bytes of self
    g 611       ; test_all: disassemble every opcode
    g 632       ; test_mnemonic_table: generate a compact mnemonic table

pivot.py generates the expected output from a data file of opcodes
which should match the latter two excluding extra bytes for extended NOPs.

.endcomment

        .cpu "65c02"
        .enc "none"

; -------------------------------------------------------------
; Optionally enable support for the 4x8 WDC/Rockwell bit op instructions
; This adds about 64 bytes.  Otherwise these instructions are disassembled
; as NOP with equivalent addressing mode/size.

INCLUDE_BITOPS :?= 0            ; SMB0-7 etc add about 60 bytes

; -------------------------------------------------------------
; zero page storage, specific location is not important

        * = 0

pc      .word ?                 ; current address (word)
opcode  .byte ?                 ; opcode
args    .word ?                 ; operand bytes (must follow opcode location)
oplen   .byte ?                 ; bytes to disassemble including opcode (1, 2 or 3)
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
        ; determine addressing mode
        ; in: pc
        ; out: A = 0..15

        lda (pc)

        ; mode_tbl has two disjoint parts: it starts with 32 entries in 16 bytes
        ; for normal modes indexed by the lower 5 bits of the opcode.
        ; Then we have several special modes which we index as 116..127
        ; and so map to six bytes stored at mode_tbl + 58..63.
        ; The intervening bytes are used for other purposes.
        ; This lets us use the same indexing code to retrieve the mode value
        ; and a more compact loop to check for them.

        ldx #128-n_special_mode      ; offset relative to mode_tbl
-
        cmp op_special_mode+n_special_mode-128,x
        beq _found_mode
        inx
        bpl -

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
        ; -------------------------------------------------------------
        ; extract length (including opcode) and formatting pattern

        ldy #1                  ; oplen is at least 1
        sty oplen

        clv                     ; we'll set V=1 for mode_R
        and #$f
        beq _impl               ; implied mode? (len 1, format 0)

        dea                     ; now A is pppn where format=ppp with n+1 operand bytes
        lsr                     ; A is format, C=n
        rol oplen               ; roll C to lsb of oplen leaving %10 or %11 and C=0
        tax                     ; save format index
        adc #121                ; set V=1 for mode_R since 121+7 = 128
        lda mode_fmt,x          ; grab the format byte
 _impl:
        sta format              ; store format (or zero for implied)
        php                     ; save overflow flag

        ; -------------------------------------------------------------
        ; Print the current address

        ldy pc
        lda pc+1
        jsr prword

        ; -------------------------------------------------------------
        ; Print the opcode and each operand byte, copying to
        ; opcode/args zp storage as we go.  We want them nicely
        ; padded whether they have 0, 1 or 2 operands so we use
        ; four fields of width 3.  The first and last are always
        ; empty, the others show either a byte with one space
        ; or three spaces.
        ;
        ;     000111222333444
        ; 1234   11          XYZ ...
        ; 1234   22 00       UVW ...
        ; 1234   33 00 00    RST ...

        ldy #$ff                ; count fields -1, 0, 1, 2, 3

_3spcs:
        ldx #2                  ; print three spaces (X=2,1,0)
_spcs:
        jsr prspc               ; print a space
        dex
        bpl _spcs

        iny                     ; next 3 char field
        cpy #4                  ; done?
        beq find_mnemonic

        cpy oplen               ; finished operands?
        bpl _3spcs              ; right justify

        lda (pc)                ; fetch next byte
        sta opcode,y            ; save to opcode, args
        inc pc                  ; advance pc
        bne +
        inc pc+1
+
        jsr prbyte              ; show it
        bra _spcs               ; X is already <= 0 so _spcs will emit one space


        ; -------------------------------------------------------------
        ; determine mnemonic from opcode e.g. LDA or BBR2
        ; in: opcode
        ; out: X index to mnemonic table

find_mnemonic:
        lda opcode

        ; -------------------------------------------------------------
        ; First check opcodes that don't follow a clear pattern

        ldx #n_special_mnem-1   ; loop backward to save cpx
-
        cmp op_special_mnem,x
        bne +
        lda ix_special_mnem,x
        bra _w2s
+
        dex
        bpl -

        ; -------------------------------------------------------------
        ; Then try matching one of several bitmasked slice
        ; Excluding bitops, these patterns cover all opcodes
        ; so we can skip the final check and just fall through
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
        bpl +                   ; N=x set indicates the second pair
        inx
        inx
+
        ; roll xaaby111 left to get C=y and lower nibble 0aab
        asl                     ; A=aaby1110 C=x
        asl                     ; A=aby11100 C=a
        rol                     ; A=by11100a C=a
        rol                     ; A=y11100aa C=b
        rol                     ; A=11100aab C=y

        pha                     ; stash bit index nibble for later

        bcc +
        inx                     ; it's BBR/BBS
        ; update format to mode_ZR.  Nb. the original mode
        ; wasn't mode_R so stashed V flag is still OK
        ldy #format_ZR
        sty format
+
        txa
        bra _w2s
.endif

_found_slice:
        ; -------------------------------------------------------------
        ; found a matching slice, calculate index into mnemonic table

        lda opcode              ; aaabbbcc
        stx tmp                 ; X is 0, 1,2,3, 4,5,6,7,8
        cpx #1                  ; check for X=0 with carry bit
        bcc _x0                 ; For X=0 we want index aaabb and leave C=0

        cpx #4                  ; X < 4 ?
        bmi _nop

        ; the remaining five slices map to four groups of 8 opcodes
        ; with an index like 001vwaaa where vw are the two LSB of X

        lsr tmp                 ; C=w
        ror                     ; waaabbbc
        lsr tmp                 ; C=v
        ror                     ; vwaaabbb
        sec                     ; C=1
_x0:                            ; note C=0 if we entered via X=0
        ror                     ; 0aaabbbc or 1vwaaabb
        lsr                     ; 00aaabbb or 01vwaaab
        lsr                     ; 000aaabb or 001vwaaa

        .byte $2C               ; bit llhh to skip past _nop, aka bra _w2s
_nop:
        lda #mNOP               ; slice 1,2,3 all map to NOP

        ; -------------------------------------------------------------
        ; given A indexing a packed word in our mnemonic table
        ; emit the corresponding three characters
        ; optionally add a bit index digit for bit ops

_w2s:
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
        ; we eventualy want %010xxxxx
        ; so start with A=%1010 and rol left until the top
        ; bit falls off into the carry leaving 010 from A
        ; and five bits from tmp/tmp+1
        lda #%1010
_rol5:
        asl tmp
        rol tmp+1
        rol a
        bcc _rol5

        jsr putc
        dex
        bne _unpack

.if INCLUDE_BITOPS
        bit tmp+1               ; tmp+1 is now %f0000000 where f flags
        bpl +                   ; is it a bit-indexed opcode e.g. BBS3?
        pla
        jsr prnblmsk            ; show the nibble we stashed (masking high bits)
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
        bne +

        ; oplen is %11 but we want to consume only one arg
        ; indicated by the parity bit.  We increment to oplen=%100
        ; so that both passes get C=0 from lsr oplen
        inc oplen               ; we'll consume bitops args zp,r in two passes
+
.endif
        ldx #7                  ; loop through each bit in the template
-
        asl format
        bcc +                   ; display the corresponding character if bit is set
_zr2:
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
        ; bitops like XZYn $zz, $rr are a pain...
        ; The first pass with mode ZR will emit "$zz,"
        ; since we decremented oplen above.
        ; If we finished with ',' we'll repeat in mode R
        ; to emit the branch target "$hhll".
        ; This assumes that putc preserves A.

        cmp #','                ; fell through after ',' (ZR) ?
        bne _done

        sbc #$80                ; set V=1 to indicate mode R
        ldx #5                  ; repeat prarg step (format is now 0)
        bvs _zr2
.endif
_done:
prnl:
        lda #$0a                ; add a newline and return via putc
        .byte $2C               ; bit llhh instead of bra putc
prspc:
    ; print one space
        lda #' '
putc:
        jmp kernel_putc         ; redirect to kernel routine

; ---------------------------------------------------------------------
; various helper functions

prarg:
    ; print one or two operand bytes as byte, word or target address
    ; (from a relative branch) based on oplen and mode

        ; oplen is 2 (%10) or 3 (%11) so lsr gives C=0 for 1 operand, C=1 for 2
        lsr oplen               ; length is 2 or 3, meaning 1 or 2 operands
        lda args+1              ; speculatively fetch second
        ldy args                ; fetch first operand
        bcs prword              ; two operands, print <A Y>
.if INCLUDE_BITOPS
        sta args                ; speculatively shuffle up 2nd operand for mode_ZR
.endif
        tya
        bvc prbyte              ; one operand, not mode_R
        ; fall through to mode R
prrel:
    ; show the target address of a branch instruction
    ; we've already incremented PC past the operands
    ; so it is the baseline for the branch
    ; we have the offset in A, with sign in N, C=0
        php                     ; save sign of offset
        adc pc                  ; C already clear from #args check
        tay                     ; Y is LSB
        lda pc+1
        adc #0                  ; add carry
        plp
        bpl +
        dea
+
prword:
    ; print the word with LSB=Y, MSB=A in big-endian order as <A Y>
        jsr prbyte              ; print MSB and fall through
prbytey:
        tya
prbyte:
    ; wozmon code to print a nibble as two bytes
        PHA                     ; Save A for LSD.
        LSR
        LSR
        LSR
        LSR
        JSR     prnbl           ; Output hex digit.
        PLA                     ; Restore A.
prnblmsk:
        AND     #$0F            ; Mask LSD for hex print.
prnbl:
        ORA     #$30            ; Add "0".
        CMP     #$3A            ; Digit?
        BMI     putc            ; Yes, output it.
        ADC     #$06            ; Add offset for letter. (Carry is set)
        BRA     putc

; =====================================================================
;
; We use several data tables to drive the disassembly
; First we have tables map opcodes to mnemonics,
; then mnemonic tables which pack three letter labels into two byte words,
; then tables to decode the addressing mode,
; and finally tables to format the operands for each address mode.
;

; -------------------------------------------------------------
; helper macros

; encode "ABC" as %aaa aabbb  bbc ccccf
s3w .sfunction s, f=0, (((s[0] & $1f)<<11) | ((s[1] & $1f)<<6) | ((s[2] & $1f)<<1) | f )

; encode two nibbles into one byte
n2b .sfunction lo, hi, ((hi<<4) | lo)

.comment

Excluding a few special cases, we can group the opcodes in slices based on fixed
combinations of the least signifcant bits.
Representing the opcode as (msb) aaabbbcc (lsb) we have the following patterns:

 X      Pattern     Mask   Target   Opcodes Offset  Index
 000    aaabb000    %111   %000     32x1    %0      aaabb   A >> 3

 001    aaa00010    %11111 %00010   1x8     %1000000  0     NOP
 010    aaabb011    %111   %011     1x8     %1000000  0     NOP

 011    11a1b100    %11010111 %11010100 1x4 %1000000  0     NOP

The next slices are indexed with the the two low bits of X along with aaa.
There are five slices with the first and last mapping to the same opcode,
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

.if INCLUDE_BITOPS
n_slice = 9

slice_mask:
    .byte %111, %11111, %111, %11010111, %11111, %11111, %11, %111, %11
slice_match:
    .byte %000, %00010, %011, %11010100, %10010, %11010, %10, %100, %01

.else
n_slice = 8
; we skip final check and just fall through without bitops ---------+
; one mask differs -------+                                         |
;                         |                                         |
slice_mask:  ;            v                                         v
    .byte %111, %11111,  %11, %11010111, %11111, %11111, %11, %111
slice_match:
    .byte %000, %00010,  %11, %11010100, %10010, %11010, %10, %100

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

.if INCLUDE_BITOPS

mBITOPS = 64
; bitops xaaby111 with op xy repeated 8x
    .word s3w("RMB",1), s3w("BBR",1), s3w("SMB",1), s3w("BBS",1)

.endif

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
; lookup tables mapping opcode slices to address modes

.comment

The five least significant bits of an opcode give the default
address mode. ie. for opcode aaabbbcc there are 32 distinct groups
of eight opcodes sharing the bits bbbcc.
In some cases we don't care (or almost don't care) about cc but
I haven't found an efficient way to encode that.

.endcomment

mode_tbl:
        ; *note* mode_tbl has two disjoint sections at mode_tbl+0..31 and mode_tbl+58..63
        ; see discussion above as to why/how that works...
        ; the intervening bytes are used for other purposes
        ; we use an assembly-time assertion to ensure this spacing is maintained

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
        .byte %00100000         ; 7: $@     (duplicate of 0 for mode_R)
format_ZR =   %00110000         ;    $@,    (special case for 2nd arg)

; ---------------------------------------------------------------------
; lookup for opcodes that don't fit a simple pattern

n_special_mnem = 15

op_special_mnem:
    .byte  $4c, $89, $8a, $9e, $a2, $aa, $cb, $db, $ca, $ea, $6c, $14, $1c, $7c, $9c

n_special_mode = 12

op_special_mode = *-5
;   Reuse last 5 bytes from table above
;   .byte $6c, $14, $1c, $7c
;   .byte $9c
    .byte      $be, $96, $b6
    .byte $20, $40, $60, $80

; aaabb111 (NOP repeated 32x) and 11a1b100 (NOP repeated 4x)
mNOP = (* - mnemonics) / 2
    .word s3w("NOP")

mDEX = (* - mnemonics) / 2
    .word s3w("DEX")

; *note* mode_special forms the end of mode_tbl, ending 64 bytes afterward

mode_special:
    .byte n2b(mode_WI,  mode_ZP),  n2b(mode_W,   mode_WXI)
    .byte n2b(mode_W,   mode_WY),  n2b(mode_ZY,  mode_ZY)
    .byte n2b(mode_W,   mode_NIL), n2b(mode_NIL, mode_R)

.cerror * - mode_tbl != $40, "mode_special must end just at +64 bytes from mode_tbl, not ",*-mode_tbl,".."

; indexed disjointly from earlier mnemonics
mSpecial = (* - mnemonics) / 2

; mnemonics only used as specials
    .word s3w("TRB"), s3w("JMP"), s3w("TXA"), s3w("TAX"), s3w("WAI"), s3w("STP")

mTRB = mSpecial
mJMP = mSpecial + 1
mTXA = mSpecial + 2
mTAX = mSpecial + 3
mWAI = mSpecial + 4
mSTP = mSpecial + 5

ix_special_mnem:
    .byte mJMP,mBIT,mTXA,mSTZ,mLDX,mTAX,mWAI,mSTP,mDEX,mNOP,mJMP,mTRB,mTRB,mJMP,mSTZ

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
        stz opcode              ; opcode = 0
-
        lda #$10
        sta pc+1                ; pc = $10xx
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
        jsr find_mnemonic
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
