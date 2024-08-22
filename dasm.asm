        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

; encode "ABC" as %aaa aabbb  bbc ccccf
s3w .sfunction s, f=0, (((s[0] & $1f)<<11) | ((s[1] & $1f)<<6) | ((s[2] & $1f)<<1) | f )

; encode two nibbles to one byte
n2b .sfunction lo, hi, ((hi<<4) | lo)

INCLUDE_BITOPS :?= 1

        * = 0

pc      .word ?                 ; current address (word)
opcode  .byte ?                 ; opcode
args    .word ?                 ; tmp storage within several routines
narg    .byte ?                 ; # of operand bytes (0, 1 or 2)
flags   .byte ?                 ; mode flags (N=1 for ZR; V=1 for R)
fmt     .byte ?                 ; operand output template; see s_mode_template
tmp     .word ?

.if INCLUDE_BITOPS
bix     .byte ?                 ; bit numbered opcode suffix, eg. BBR7
.endif

        * = $200

; =====================================================================
;
; main entry point, with <pc> giving the address to disassemble
; we emit a single line of disassembly to putc and increase
; pc address by the number of bytes consumed

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
        ; out: mode = 0..15

        lda (pc)

        ; check if opcode has special mode
        ldx #n_special_mode-1
-
        cmp op_special_mode,x
        beq _get_mode
        dex
        bpl -

        ; otherwise fetch mode from lookup table using bits ...bbbcc
        ; two mode nibbles are packed in each byte so first
        ; determine which byte and then which half
        and #$1f
        clc
        adc #n_special_mode
        tax
_get_mode:
        txa
        lsr                     ; C=1 for odd (high nibble)
        tax
        lda mode_tbl,x
        bcc _mask

        lsr
        lsr
        lsr
        lsr                     ; shift high nibble down
_mask:
        and #$f

        ; -------------------------------------------------------------
        ; decode the mode nibble to get the number of operands (narg)
        ; along with the fmt bitmask and flags
        ; Y = narg (0,1,2)
        ; X = fmt index of bitmask for s_mode_template
        ; flags: flag special modes R (V=1) and ZR (N=1) else 0

        cmp #13
        bpl _special            ; mode_NIL, mode_R, mode_ZR

        lsr                     ; nibble is fffn so A=fff and C=n
        tax                     ; X = fmt index
        lda #1
        rol                     ; A = narg+1 (2, 3)
        tay                     ; Y = narg+1
        stz flags               ; no special flags
        bra _continue

_special:
        and #3                  ; 13 = %1101 .. 15 = %1111 so A = narg+1
        tay                     ; Y = narg+1 (1, 2, 3)
        lda mode_extended-1,y
        sta flags
        and #$f
        tax                     ; X = fmt index

_continue:
        sty tmp                 ; keep narg+1 for prpad loop (TODO remove?)
        dey
        sty narg
        lda mode_fmt,x          ; copy the format byte whose bits map to s_mode_template
        sta fmt

        ; -------------------------------------------------------------
        ; print the opcode and each operand byte,
        ; copying them to opcode/args in the process

        ; loop Y=0,1,narg, narg+1,..3
        ldy #0
-
        jsr prpad_incpc         ; show "XX " or "   " if Y > narg, and PC++
        iny
        cpy #4
        bne -

        ; -------------------------------------------------------------
        ; determine mnemonic from opcode e.g. LDA or BBR2
        ; in: opcode
        ; out: X index to mnemonic table

        lda opcode              ; recover opcode and fall through

show_mnemonic:
        ; First check specials
        ;TODO not shorter than table lookup? do it as a slice?
        ; d4/f4/dc/fc NOP block has bits 11x1x100
        eor     #%11010100
        and     #%11010111
        bne +
_nop:
        lda #mNOP
        bra _decode
+
        lda opcode

        ldx #n_special-1
-
        cmp op_special,x
        beq _special_lookup
        dex
        bpl -

        ; Then try matching slices
        ldx #0
-
        lda slice_mask,x
        and opcode
        cmp slice_match,x
        beq _slice_lookup
        inx
        cpx #n_slice
        bne -

.if INCLUDE_BITOPS
        ; otherwise it's a bitop instruction xaaby111 with xy choosing op and aab giving bit
        ldx #65
        lda opcode
        bit #%1000          ; check bit 3
        beq +
        inx
+
        asl                 ; check bit 7=x, leaving A = aaby1110
        bcc +
        inx
        inx
+
        ldy #4
        clc                 ; set bix to 0aab
-
        rol bix
        asl
        dey
        bne -
.endif

_found:
        txa
        bra _decode

_special_lookup:
        lda ix_special,x
        bra _decode

_slice_lookup:
        lda opcode      ; aaabbbcc
        stx tmp         ; X is 0, 1,2, 3,4,5,6,7 say X=%uvw
        cpx #1          ; C=0 for X=0, 1 otherwise
        bcc _x0         ; For X=0 we want index aaabb

        cpx #3
        bmi _nop

        lsr tmp         ; C=w
        ror             ; waaabbbc
        lsr tmp         ; C=v
        ror             ; vwaaabbb
        sec
_x0:
        ror             ; 0aaabbbc or 1vwaaabb
        lsr             ; 00aaabbb or 01vwaaab
        lsr             ; 000aaabb or 001vwaaa
        ; fall through to decode

_decode:
        ; -------------------------------------------------------------
        ; decode three character mnemonic packed into two bytes
        ; in: X
        ; out: emit three-letter mnemonic with bit index for bitops

        asl
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
        bit tmp                 ; tmp is now %f0000000 or %00000000
        bpl +                   ; f indicates a bit-indexed opcode e.g. BBS3
        lda bix
        jsr prnbl               ; so show the digit
+
.endif
        jsr prspc

        ; -------------------------------------------------------------
        ; show the operand value(s) if any

show_operand:
        lda narg                ; anything to do?
        beq _done

        ldx #7                  ; loop through the template
_template:
        asl fmt
        bcc +                   ; show this character?
        lda s_mode_template,x
        jsr putc
+
        cpx #5
        bne +
        jsr przwr
+
        dex
        bpl _template
.if INCLUDE_BITOPS
        bit flags                ; mode 15 has N flag set
        bpl _done

        lda args+1              ; bump the second operand down
        sta args
        lda mode_fmt+mode_R     ; reset fmt mask to mode_R
        sta fmt
        stz flags
        bra show_operand
.endif
_done:
prnl:
        lda #$0a                ; add a newline and return
        bra putc

; ---------------------------------------------------------------------
; various helpers

przwr:
    ; print one or two operand bytes as byte, word or target address (relative mode)
    ; based on narg and mode

    ; mode 7(R) prints one byte as target address
    ; mode 15(Z,R) prints Z, first then repeats as mode 7
    ; others print narg bytes

        lda narg
        lsr                     ; C=1 means one byte
        lda args                ; fetch first arg
        bcs _one

        tay                     ; stash LSB
        lda args+1              ; fetch MSB
        bra prword

_one:
        bit flags               ; check V=1 for relative
        bvc prbyte              ; regular byte

prrel:
    ; print the target address of a branch instruction
    ; we've already fetched the offset byte so current address
    ; points to next instruction which is the baseline for the branch
        ldy pc+1                ; fetch MSB
        cmp #0                  ; test A flags
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
    ; wozmon's code to print a nibble as two bytes
        PHA                    ; Save A for LSD.
        LSR
        LSR
        LSR                    ; MSD to LSD position.
        LSR
        JSR     prnbl          ; Output hex digit.
        PLA                    ; Restore A.
        AND     #$0F           ; Mask LSD for hex print.
prnbl:
        ORA     #$30           ; Add "0".
        CMP     #$3A           ; Digit?
        BMI     putc           ; Yes, output it.
        ADC     #$06           ; Add offset for letter. (Carry is set)
putc:
        sta $f001
        rts

prpad_incpc:
        ; loop Y=0,1,narg, narg+1,..3
        cpy tmp
        bpl pr3spc

        lda (pc)
        sta opcode,y            ; fill opcode, args
        jsr prbyte              ; show opcode/operand
        inc pc                  ; advance pc
        bne +
        inc pc+1
+
        bra prspc               ; show trailing space

        ; done operand, pad with spaces
pr3spc:
    ; print three spaces
        jsr prspc
        jsr prspc
prspc:
    ; print one space
        lda #' '
        bra putc


dasm_data_mnemonics:

.comment
Excluding a few special cases, we can group the opcodes in slices based on fixed
combinations of the least signifcant bits.
Representing the opcode as (msb) aaabbbcc (lsb) we have the following patterns:

X       Pattern     Mask   Target   Opcodes Offset  Index
000     aaabb000    %111   %000     32x1    %0      aaabb   A >> 3

001     aaa00010    %11111 %00010   1x8     %1000000  0     NOP
010     aaabb011    %111   %011     1x8     %1000000  0     NOP

These slices are indexed with low two bits of X along with aaa

011     aaa10010    %11111 %10010   8x1     %110000 aaa     A >> 5  * same opcodes as X=2
100     aaa11010    %11111 %11010   8x1     %111000 aaa     A >> 5
101     aaabbb10    %11    %10      8x8     %100000 aaa     A >> 5
110     aaabb100    %111   %100     8x4     %101000 aaa     A >> 5
111     aaabbb01    %11    %01      8x8     %110000 aaa     A >> 5  * same opcodes as X=2

This covers all 224 (=32+8+8+8+8+64+32+64) opcodes ending in 00, 01, 10 (c=0, 1, 2), and NOP ennding 011.
The remaining 32 opcodes are WDC/Rockwell extensions with a slightly different structure:

        0aab0111    %10001111   %0...0111   %1000001    RMB
        0aab1111    %10001111   %0...1111   %1000010    BBR
        1aab0111    %10001111   %1...0111   %1000011    SMB
        1aab1111    %10001111   %1...1111   %1000100    BBS

.endcomment

n_slice = 8
slice_mask:
.if INCLUDE_BITOPS
    .byte %111, %11111, %111, %11111, %11111, %11, %111, %11
slice_match:
    .byte %000, %00010, %011, %10010, %11010, %10, %100, %01
.else
    .byte %111, %11111,  %11, %11111, %11111, %11, %111, %11
slice_match:
    .byte %000, %00010,  %11, %10010, %11010, %10, %100, %01
.endif

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

; offset +32

; aaa11010 indexed by aaa (each repeated 1x)
    .word s3w("INC"), s3w("DEC"), s3w("PHY"), s3w("PLY"), s3w("TXS"), s3w("TSX"), s3w("PHX"), s3w("PLX")
; aaabbb10 indexed by aaa (each repeated 8x)
    .word s3w("ASL"), s3w("ROL"), s3w("LSR"), s3w("ROR"), s3w("STX"), s3w("LDX"), s3w("DEC"), s3w("INC")
; aaabb100 indexed by aaa (each repeated 8x)
    .word s3w("TSB"), s3w("BIT"), s3w("NOP"), s3w("STZ"), s3w("STY"), s3w("LDY"), s3w("CPY"), s3w("CPX")
; aaa10010 and aaabbb01 indexed by aaa (each repeated 9x)
    .word s3w("ORA"), s3w("AND"), s3w("EOR"), s3w("ADC"), s3w("STA"), s3w("LDA"), s3w("CMP"), s3w("SBC")

; offset +64

; aaabb111 (NOP repeated 32x)
    .word s3w("NOP")
.if INCLUDE_BITOPS
; bitops xaaby111 with op xy repeated 8x
    .word s3w("RMB"), s3w("BBR"), s3w("SMB"), s3w("BBS")
.endif

; offset +65 or +69
mSpecial = (* - mnemonics) / 2

; mnemonics only used as specials
    .word s3w("TRB"), s3w("JMP"), s3w("TXA"), s3w("TAX"), s3w("WAI"), s3w("STP"), s3w("DEX")

mNOP = 64

mLDX = 45
mBIT = 49
mSTZ = 51

mTRB = mSpecial
mJMP = mSpecial + 1
mTXA = mSpecial + 2
mTAX = mSpecial + 3
mWAI = mSpecial + 4
mSTP = mSpecial + 5
mDEX = mSpecial + 6

n_special = 15

op_special:
    .byte $14, $1c, $4c, $6c, $7c, $89, $8a, $9c, $9e, $a2, $aa, $cb, $db, $ca, $ea
ix_special:
    .byte mTRB,mTRB,mJMP,mJMP,mJMP,mBIT,mTXA,mSTZ,mSTZ,mLDX,mTAX,mWAI,mSTP,mDEX,mNOP


dasm_data_mode:

; there are 15 distinct addressing modes, three of which are only used as exceptions
; we use a four bit index where the top bit is (just about) the operand length
; and the low three bits (almost) index one of eight formatting patterns

modenbl .sfunction fmt, len, ((fmt << 1) | len)

mode_ZP     = modenbl(0, 0)     ; LDA $42
mode_W      = modenbl(0, 1)     ; LDA $1234
mode_IMM    = modenbl(1, 0)     ; LDA #$42
; unused    = modenbl(1, 1)
mode_ZX     = modenbl(2, 0)     ; LDA $42,X
mode_WX     = modenbl(2, 1)     ; LDA $1234,X
mode_ZY     = modenbl(3, 0)     ; LDA $42,Y
mode_WY     = modenbl(3, 1)     ; LDA $1234,Y
mode_ZI     = modenbl(4, 0)     ; LDA ($42)
mode_WI     = modenbl(4, 1)     ; JMP ($1234)   (*) one opcode
mode_ZXI    = modenbl(5, 0)     ; LDA ($42,X)
mode_WXI    = modenbl(5, 1)     ; JMP ($1234,X) (*) one opcode
mode_ZIY    = modenbl(6, 0)     ; LDA ($42),Y
; these extended cases that are encoded as 13/14/15
mode_NIL    = modenbl(6, 1)	    ; INC, RTS (note we don't write INC A)
mode_R      = modenbl(7, 0)		; BRA $1234
.if INCLUDE_BITOPS
mode_ZR     = modenbl(7, 1)     ; RMB $42,$1234 (*) pattern 7 then recurse to pattern 1
.endif

mode_extended:
    .byte $7, $60, $88           ; extended modes NIL fmt 7, R(V=1, fmt 0) and ZR(N=1, fmt 8)

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
        .byte 0                 ; 7: implied
.if INCLUDE_BITOPS
        .byte %00110000         ; 8: $@,       => first part of z,r follwed by pattern 1
.endif


n_special_mode = 12

op_special_mode:
    .byte $20, $40, $60, $80
    .byte $6c, $14, $96, $b6
    .byte $1c, $7c, $9c, $be

mode_tbl:

; 3 are for X=>Y when op ends with X, could detect and adjust mode or save Z status and add to ,x ?
; the other nine all occur for c=0, a<=4 (5*8 = 40 possible locations)
mode_special:
    .byte n2b(mode_W,   mode_NIL), n2b(mode_NIL, mode_R)
    .byte n2b(mode_WI,  mode_ZP),  n2b(mode_ZY,  mode_ZY)
    .byte n2b(mode_W,   mode_WXI), n2b(mode_W,   mode_WY)

; encode the default mode for 32 groups of xxxbbbcc
; with rows b=0..7 and columns c=0..3 packing two modes per byte
mode_default:
    .byte n2b(mode_IMM, mode_ZXI),  n2b(mode_IMM, mode_NIL)
    .byte n2b(mode_ZP,  mode_ZP),   n2b(mode_ZP,  mode_ZP)
    .byte n2b(mode_NIL, mode_IMM),  n2b(mode_NIL, mode_NIL)
    .byte n2b(mode_W,   mode_W)
.if INCLUDE_BITOPS
    .byte                           n2b(mode_W,   mode_ZR)
.else
    .byte                           n2b(mode_W,   mode_W)
.endif
    .byte n2b(mode_R,   mode_ZIY),  n2b(mode_ZI,  mode_NIL)
    .byte n2b(mode_ZX,  mode_ZX),   n2b(mode_ZX,  mode_ZP)
    .byte n2b(mode_NIL, mode_WY),   n2b(mode_NIL, mode_NIL)
    .byte n2b(mode_WX,  mode_WX)
.if INCLUDE_BITOPS
    .byte                           n2b(mode_WX,   mode_ZR)
.else
    .byte                           n2b(mode_WX,   mode_WX)
.endif

dasm_end:

* = $600

test_dasm_self:
    ; disassemble first $100 bytes of dasm
        stz pc
        lda #2
        sta pc+1
-
        jsr dasm
        lda pc+1
        cmp #3
        bne -

        brk
        .byte 0

test_all:
    ; disassemble each opcode X at $1000,X
        lda #$10
        sta pc+1
        stz opcode
-
        lda opcode
        sta pc
        sta (pc)
        pha
        bit #$f
        bne +
        jsr prnl
+
        pla
        jsr dasm
        inc opcode
        bne -

        jsr prnl
        brk
        .byte 0

test_mnemonic_table:
    ; generate a compact table of mnenomics (no address mode)
        lda #$60
        sta show_operand      ;TODO hack: modify code to just show mnemonic

        stz pc                ; loop index
_next:
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
        lda opcode
        jsr show_mnemonic
        inc pc
        bne _next

        jsr prnl
        brk
        .byte 0
