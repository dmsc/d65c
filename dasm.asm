        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

; encode "ABC" as %fccc ccbb  bbba aaaa
s3w .sfunction s, f=0, ((s[0] & $1f) | ((s[1] & $1f)<<5) | ((s[2] & $1f)<<10) | (f ? 1 << 15 : 0))

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

$25f    setup



.endcomment

tmp = $0        ; used as temp storage during 2->3 mnemonic decoding
rix = $1        ; rockwell numbered opcode suffix, eg. BBR7
cur = $2        ; current address (word)
opc = $4        ; current opcode
mode= $5        ; current mode template (8 bit pattern, see s_mode_template)
len = $6        ; current # of operand bytes (0, 1 or 2)

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

        stz cur     ; loop index
_next:
        lda #$7
        bit cur
        bne +
        lda #$0a
        jsr putc
+
        lda cur     ; reverse index to get opcode
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

; =====================================================================

dasm:
        lda cur                 ; print current address followed by three spaces
        ldy cur+1
        jsr prword
        jsr pr3spc

        jsr nxtbyte             ; fetch opcode and increment address
        sta opc                 ; stash opcode

dasm_test:                      ; test entrypoint so we can feed in opcode in opc + A
        ; check if opcode has special mode
        ldx #n_special_mode-1
-
        cmp op_special_mode,x
        bne +
        lda mode_special,x
        bra _special
+
        dex
        bpl -

        ; otherwise just get mode from lookup table using bits ...bbbcc
        and #$1f
        tax
        lda mode_tbl,x
_special:
        sta mode                ; bitmask has bits for '#(wzrx)y'
        lsr                     ; shift and mask to get 000000wz C=r
        lsr
        lsr
        lsr
        and #3
        adc #0                  ; so adc 0 gives operand byte count 2w + z + r
        sta len                 ; stash it
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
        ; First check specials
        ;TODO this doesn't actually save space over repeated lookup
        ; d4/f4/dc/fc NOP block has bits 11x1x100
        eor     #%11010100
        and     #%11010111
        bne +
_nop:
        lda #64
        bra _decode
+
        lda opc

        ldx #n_special-1
-
        cmp op_special,x
        beq _special_lookup
        dex
        bpl -

        ; Then match slices
        ldx #0
-
        lda slice_mask,x
        and opc
        cmp slice_match,x
        beq _slice_lookup
        inx
        cpx #n_slice
        bne -

        ; otherwise it's a rockwell instruction xaaby111 with xy choosing which
        ldx #65
        lda opc
        bit #%1000          ; check bit 3
        beq +
        inx
+
        asl                 ; check bit 7
        bcc +
        inx
        inx
+
        ;TODO shift right 5 and save index
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
        jsr out                 ; TODO could flag X here to handle some mode exceptions
.comment
TODO
        lda rix                 ; is there a trailing digit like BBS3 ?
        bmi +
        jsr prnbl
+
.endcomment
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
        ; mode has bit pattern corresponding to characters in s_mode_template
        ldx #$ff
        clv                     ; flag for ,r
        bra _next               ; TODO reorganize this loop

_put:
        lda s_mode_template,x   ; bit set means show this character
        phx
        bpl _char               ; is it a regular character?

        jsr przwr               ; otherwise show one or more operand bytes
        bra _cont

_char:
        cmp #'X'                ; X or Y wants a preceding comma
        bcc +                   ;TODO this is ugly
        pha
        jsr prcma
        pla
+
        jsr putc
_cont:
        plx
_next:
        inx
        asl mode
        bcs _put                ; show this template character?
        bne _next               ; done?

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
    ; on entry A has $80(r), $81(zp), $82(w)
    ;TODO simplify one vs two?
        and #$7f
        pha                     ; push 0, 1, or 2
        bvc +                   ; already called for this template?

        jsr prcma               ; add a comma for the rockwell z,r case
+
        lda #'$'                ; show $
        jsr putc
        jsr nxtbyte             ; get next byte

        plx                     ; fetch 0, 1, 2
        beq prrel               ; relative?

        dex
        beq _one                ; one and done?
        pha                     ; stash lsb
        jsr nxtbyte             ; fetch msb
_one:
        jsr prbyte              ; show byte
        dex
        bne _done
        pla
        bra _one
_done:
        bit _rts                ; set overflow flag for rockwell case
_rts:
        rts

prrel:
    ; print the target address of a branch instruction
    ; we've already fetched the offset byte so current address
    ; points to next instruction which is the baseline for the branch
        ldy cur+1               ; fetch MSB
        tax                     ; check A flags
        bpl +
        dey                     ; if negative, dec MSB
+
        clc
        adc cur                 ; calc LSB of target
        bcc +
        iny                     ; deal with overflow
+
prword:
    ; print the word with LSB=A, MSB=Y
        pha
        tya
        jsr prbyte
        pla
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
prcma:
    ; print a comma
        lda #','
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

s_mode_template:
    .text "#(", $82, $81, $80, "X)Y"

; there are 15 distinct addressing modes, three of which are only used as exceptions
; there's a reasonable four bit representation which isn't currently used since
; it seems more expensive to decode
;
;           =  #(wzrx)y              ind? ~1?  idx? x/y?  Example
mode_NIL    = %00000000     ; 0   *   1    1    1    1    INC A as well as rts

mode_IMM    = %10010000     ; 1       0    0    0    0    LDA #$42
mode_ZP     = %00010000     ; 2       0    0    0    1    LDA $42
mode_ZX     = %00010100     ; 3       0    0    1    0    LDA $42,x
mode_ZY     = %00010001     ; 4       0    0    1    1    LDA $42,y         (3x exceptions only)
mode_W      = %00100000     ; 5       0    1    0    0    LDA $1234
mode_R      = %00001000     ; 6   *   0    1    0    1    BCC $1234
mode_WX     = %00100100     ; 7       0    1    1    0    LDA $1234,x
mode_WY     = %00100001     ; 8       0    1    1    1    LDA $1234,y

mode_ZI     = %01010010     ; 9       1    0    0    0    LDA ($42)

mode_ZXI    = %01010110     ; 10      1    0    1    0    LDA ($42,x)
mode_ZIY    = %01010011     ; 11      1    0    1    1    LDA ($42),y
mode_WI     = %01100010     ; 12      1    1    0    0    JMP ($1234)       (singleton exception)
mode_ZR     = %00011000     ; 13  *   1    1    0    1    RMB $42,$1234
mode_WXI    = %01100110     ; 14      1    1    1    0    JMP ($1234,x)     (singleton exception)



mode_tbl:
    .byte mode_IMM, mode_ZXI, mode_IMM, mode_NIL
    .byte mode_ZP,  mode_ZP,  mode_ZP,  mode_ZP
    .byte mode_NIL, mode_IMM, mode_NIL, mode_NIL
    .byte mode_W,   mode_W,   mode_W,   mode_ZR
    .byte mode_R,   mode_ZIY, mode_ZI,  mode_NIL
    .byte mode_ZX,  mode_ZX,  mode_ZX,  mode_ZP
    .byte mode_NIL, mode_WY,  mode_NIL, mode_NIL
    .byte mode_WX,  mode_WX,  mode_WX,  mode_ZR

n_special_mode = 12

op_special_mode:
    .byte $20, $40, $60, $80
    .byte $6c, $14, $96, $b6
    .byte $1c, $7c, $9c, $be

; 3 are for X=>Y when op ends with X, could detect and adjust mode or save Z status and add to ,x ?
; the other nine all occur for c=0, a<=4 (5*8 = 40 possible locations)
mode_special:
    .byte mode_W,   mode_NIL, mode_NIL, mode_R
    .byte mode_WI,  mode_ZP,  mode_ZY,  mode_ZY
    .byte mode_W,   mode_WXI, mode_W,   mode_WY

dasm_data_end: