        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

; encode "ABC" as %fccc ccbb  bbba aaaa
s3w .sfunction s, f=0, ((s[0] & $1f) | ((s[1] & $1f)<<5) | ((s[2] & $1f)<<10) | (f ? 1 << 15 : 0))
; encode two nibbles to one byte
n2b .sfunction lo, hi, ((hi<<4) | lo)

.comment


$25f    logic               344 bytes  (incl putc 3 bytes)
            setup                75 bytes
    $2aa    mnemonic            126 bytes
    $328    operand              40 bytes
    $350    helpers             103 bytes
$3b7    mnemonics data      198 bytes
$47d    mode data            64 bytes
$4bd    end
            total               606
            target              512
            over                 94 bytes

$261
$4bb  602 bytes (+90)

.endcomment

        * = 0

cur     .word ?                 ; current address (word)
opc     .byte ?                 ; current opcode
len     .byte ?                 ; current # of operand bytes (0, 1 or 2)
bix     .byte ?                 ; bit numbered opcode suffix, eg. BBR7
mode    .byte ?                 ; current address mode $0-e
fmt     .byte ?                 ; operand output template; see s_mode_template
tmp     .byte ?                 ; temp storage within several routines

        * = $200

main:
        stz cur
        lda #2
        sta cur+1
-
        jsr dasm
        lda cur+1
        cmp #3
        bne -

        brk
        .byte 0

test_all:
        lda #$10
        sta cur+1
        stz opc
-
        lda opc
        bit #$f
        bne +
        lda #$0a
        jsr putc
        lda opc
+
        jsr dasm_test
        inc opc
        bne -

        lda #$0a
        jsr putc
        lda #$0a
        jsr putc

test_opc:
        lda #$60
        sta end_show_mnemonic   ; modify code to just show mnemonic

        stz cur                 ; loop index
_next:
        lda #$7
        bit cur
        bne +
        lda #$0a
        jsr putc
+
        lda cur                 ; reverse index to get opcode
        ldy #8
-
        lsr a
        rol opc
        dey
        bne -
        lda opc
        jsr show_mnemonic
        inc cur
        bne _next

        lda #$0a
        jsr putc
        brk
        .byte 0

; =====================================================================

dasm:
        ldy cur                 ; print current address followed by three spaces
        lda cur+1
        jsr prword
        jsr pr3spc

        jsr nxtbyte             ; fetch opcode and increment address
dasm_test:                      ; enter here with A=opcode for testing
        sta opc                 ; stash opcode

        ; check if opcode has special mode
        ldx #n_special_mode-1
-
        cmp op_special_mode,x
        beq _get_mode
        dex
        bpl -

        ; otherwise fetch mode from lookup table using bits ...bbbcc
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
        sta mode

        ; extract number of operand bytes
        ; mode  length
        ;  0      0
        ; 1-8     1
        ; 9+      2
        beq +                   ; mode 0 (impl) has length 0
        dea                     ; else len := (--A // 8 ) + 1
        lsr                     ;TODO maybe shorten with length in LSB?
        lsr
        lsr
        ina
+
        sta len                 ; store 0, 1, or 2

        ldy #$ff
        lda opc                 ; print the opcode followed by len operand bytes
-
        jsr prbyte
        jsr prspc
        iny
        lda (cur),y
        cpy len
        bne -
-
        jsr pr3spc              ; print three space blocks to pad to width 12 total
        iny
        cpy #4
        bne -

        lda opc                 ; recover opcode and fall through

; ---------------------------------------------------------------------
; display the mnemonic e.g. LDA or BBR2

show_mnemonic:
        stz bix                 ; default not bit-indexed opcode
        dec bix

        ; First check specials
        ;TODO this doesn't actually save space over repeated lookup
        ; d4/f4/dc/fc NOP block has bits 11x1x100
        eor     #%11010100
        and     #%11010111
        bne +
_nop:
        lda #mNOP
        bra _decode
+
        lda opc

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
        and opc
        cmp slice_match,x
        beq _slice_lookup
        inx
        cpx #n_slice
        bne -

        ; otherwise it's a Rockwell instruction xaaby111 with xy choosing op and aab giving bit
        ldx #65
        lda opc
        bit #%1000          ; check bit 3
        beq +
        inx
+
        asl                 ; check bit 7, A = aaby1110
        bcc +
        inx
        inx
+
        ldy #3
-
        asl
        rol bix
        dey
        bne -
_found:
        txa
        bra _decode

_special_lookup:
        lda ix_special,x
        bra _decode

_slice_lookup:
        lda opc         ; aaabbbcc
        stx tmp         ; X is 0, 1,2, 3,4,5,6,7 say X=%uvw
        cpx #1          ; C=0 for X=0, 1 otherwise
        bcc _x0         ; For X=0 we want index aaabb

        cpx #3
        bmi _nop

        lsr tmp         ; C=w
        ror a           ; waaabbbc
        lsr tmp         ; C=v
        ror a           ; vwaaabbb
        sec
_x0:
        ror a           ; 0aaabbbc or 1vwaaabb
        lsr a           ; 00aaabbb or 01vwaaab
        lsr a           ; 000aaabb or 001vwaaa
        ; fall through to decode

_decode:
        ; decode a 3 letter opcode packed into two bytes
        asl
        tax

        lda mnemonics,x
        pha
        jsr out
        lda mnemonics+1,x
        lsr a
        sta tmp
        pla
        ror a
        lsr tmp
        ror a
        lsr a
        lsr a
        lsr a
        jsr out2

        lda tmp
        jsr out                 ;TODO a trailing "X" could flag some mode exceptions, ie. ,Y vs ,X

        lda bix
        bmi +                   ; is it a bit-indexed opcode e.g. BBS3 ?
        jsr prnbl
+
        jsr prspc
end_show_mnemonic:
        bra show_operand

out:
        and #$1f
out2:
        ora #$40
        jmp putc

; ---------------------------------------------------------------------
; show the operand value(s) if any

show_operand:
        lda mode                ; mode is 0000nfff where fff is format index
        beq _done               ; mode_NIL has no operand

        ; We have a few special cases:
        ; R (mode 7) uses format 1 and shows branch target for operand
        ; ZR (mode 15) recurses back to mode 7 to handle the dual argument
        ; IMM (mode 8) uses pattern 0 which needs initial C=1 (all others have C=0)

        tax                     ; save mode
        ina                     ; 7 and 15 => %1000 and %10000
        bit #7                  ; mode 7 or 15 iff bit 0,1 and 2 all clear
        bne _normal
        asl                     ; %1000 => %100000, %10000 = %1000000
        asl
        asl
        tsb mode                ; flag bit 6(V) for mode 7 and bit 7(N) for mode 15
        bmi _mode15
        ldx #1                  ; switch to format 1 for mode 7
        .byte $2c               ; bit llhh to skip dec
_mode15:
        dec len                 ; mode 15 shows one byte twice
_normal:
        txa
        and #7                  ; extract fff bits
        tax                     ; use fff bits as index to format table

        lda mode_fmt,x          ; copy the format byte whose bits map to s_mode_template
        sta fmt

        ldx #7                  ; loop through the template
_template:
        asl fmt
        bcc +                   ; show this character?
        lda s_mode_template,x
        jsr putc
+
        cpx #5
        bne +
        phx                     ; TODO get rid of push/pull
        jsr przwr
        plx
+
        dex
        bpl _template

        bit mode                ; mode 15 has N flag set
        bpl _done
        lda #mode_R             ; we've printed e.g. "SMB $42,"
        sta mode                ; so repeat as mode 7 to get branch target
        bra show_operand
_done:
        lda #$0a                ; add a newline
        bra putc

; ---------------------------------------------------------------------
; various helpers

nxtbyte:
    ; fetch next byte, incrementing current address
        lda (cur)
        inc cur
        bne +
        inc cur+1
+
        rts

przwr:
    ; print one or two operand bytes as byte, word or target address (relative mode)
    ; based on len and mode

    ; mode 7(R) prints one byte as target address
    ; mode 15(Z,R) prints single byte and recurse as mode 7
    ; others print len bytes

        ldx #0
-
        tay
        jsr nxtbyte             ; single byte or LSB of pair
        inx
        cpx len
        bne -
        dex
        bne prword
        bit mode
        bvc prbyte              ; mode flagged as V=1 for relative
        ; fall through
prrel:
    ; print the target address of a branch instruction
    ; we've already fetched the offset byte so current address
    ; points to next instruction which is the baseline for the branch
        ldx cur+1               ; fetch MSB
        tay                     ; check A flags
        bpl +
        dex                     ; if negative, dec MSB
+
        clc
        adc cur                 ; calc LSB of target
        bcc +
        inx                     ; deal with overflow
+
        tay
        txa
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

pr3spc:
    ; print three spaces
    ;TODO
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
The remaining 32 opcodes are Rockwell extensions with a slightly different structure:

        0aab0111    %10001111   %0...0111   %1000001    RMB
        0aab1111    %10001111   %0...1111   %1000010    BBR
        1aab0111    %10001111   %1...0111   %1000011    SMB
        1aab1111    %10001111   %1...1111   %1000100    BBS

.endcomment

n_slice = 8
slice_mask:
    .byte %111, %11111, %111, %11111, %11111, %11, %111, %11
slice_match:
    .byte %000, %00010, %011, %10010, %11010, %10, %100, %01

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

; aaabb111 (NOP repeated 32x) and xaaby111 with op xy repeated 8x
    .word s3w("NOP"), s3w("RMB"), s3w("BBR"), s3w("SMB"), s3w("BBS")

; offset +69
; mnemonics only used as specials
    .word s3w("TRB"), s3w("JMP"), s3w("TXA"), s3w("TAX"), s3w("WAI"), s3w("STP"), s3w("DEX")

mNOP = 64

mLDX = 45
mBIT = 49
mSTZ = 51

mTRB = 69
mJMP = 70
mTXA = 71
mTAX = 72
mWAI = 73
mSTP = 74
mDEX = 75

n_special = 15

op_special:
    .byte $14, $1c, $4c, $6c, $7c, $89, $8a, $9c, $9e, $a2, $aa, $cb, $db, $ca, $ea
ix_special:
    .byte mTRB,mTRB,mJMP,mJMP,mJMP,mBIT,mTXA,mSTZ,mSTZ,mLDX,mTAX,mWAI,mSTP,mDEX,mNOP


dasm_data_mode:

; there are 15 distinct addressing modes, three of which are only used as exceptions
; we use a four bit index where the top bit is (just about) the operand length
; and the low three bits (almost) index one of eight formatting patterns

n1 = 0
n2 = 1 << 3

mode_NIL    = n1 | 0        ; INC, RTS (note we don't write INC A)
mode_ZP     = n1 | 1        ; LDA $42
mode_ZX	    = n1 | 2        ; LDA $42,X
mode_ZY     = n1 | 3        ; LDA $42,Y
mode_ZI     = n1 | 4        ; LDA ($42)
mode_ZXI    = n1 | 5        ; LDA ($42,X)
mode_ZIY    = n1 | 6        ; LDA ($42),Y
mode_R      = n1 | 7        ; BRA $1234     (*) special: pattern => 1
mode_IMM    = n2 | 0        ; LDA #$42      (*) length=1 after decrement
mode_W      = n2 | 1        ; LDA $1234
mode_WX     = n2 | 2        ; LDA $1234,X
mode_WY     = n2 | 3        ; LDA $1234,Y
mode_WI     = n2 | 4        ; JMP ($1234)   (*) one opcode
mode_WXI    = n2 | 5        ; JMP ($1234,X) (*) one opcode
; unused	= n2 | 6
mode_ZR     = n2 | 7        ; RMB $42,$1234 (*) pattern 7 then recurse to pattern 1

s_mode_template:
    .text "Y,)X,$(#"            ; reversed: #($,X),Y

mode_fmt:
;              #($,X),Y         ; 1 or 2 byte address or branch target always inserted after $
        .byte %10100000         ; 0: #$@
        .byte %00100000	        ; 1: $@
        .byte %00111000	        ; 2: $@,x
        .byte %00100011	        ; 3: $@,y
        .byte %01100100	        ; 4: ($@)
        .byte %01111100	        ; 5: ($@,x)
        .byte %01100111	        ; 6: ($@),y
        .byte %00110000         ; 7: $@,       => first part of z,r follwed by pattern 1

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
    .byte n2b(mode_W,   mode_W),    n2b(mode_W,   mode_ZR)
    .byte n2b(mode_R,   mode_ZIY),  n2b(mode_ZI,  mode_NIL)
    .byte n2b(mode_ZX,  mode_ZX),   n2b(mode_ZX,  mode_ZP)
    .byte n2b(mode_NIL, mode_WY),   n2b(mode_NIL, mode_NIL)
    .byte n2b(mode_WX,  mode_WX),   n2b(mode_WX,  mode_ZR)

dasm_data_end: