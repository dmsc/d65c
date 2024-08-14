        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

mnem .macro op, f=0
; encode "ABC" as %fccc ccbb  bbba aaaa
_s = str(.\op)
    .word  (_s[0] & $1f) + ((_s[1] & $1f)<<5) + ((_s[2] & $1f)<<10) + (\f ? 1 << 15 : 0)
.endmacro

tmp = $0
cur = $2

* = $200

dasm:

        ldy #0
-
        tya
        bit #$f
        bne +
        lda #$0a
        jsr putc
        tya
+
        jsr show_mnemonic
        lda #$20
        jsr putc
        iny
        bne -
        lda #$0a
        jsr putc
        brk


show_mnemonic:
        ; opcode has bit pattern aaabbbcc aka a2a1a0b2b1b0c1c0

        ldx #n_special-1
-
        cmp op_special,x
        bne +
        lda mnem_special,x
        bra _decode
+       dex
        bpl -

        lsr
        ror         ; A=c0aaabbb, with N=c0, C=c1
        bcc _c0x
        bpl _c10

_c11:
        ; bit 2 (b0) clear is nop
        lsr
        bcs +

_nop:   lda #op_NOP
        bra _decode
+
        ; otherwise bit 3 (b1) clear means rmb/smb, set is bbr/bbs
        ldx #op_RMB
        lsr
        bcc +
        inx
+
        ; with bit 7 (a2) choosing between them and a1a0b2 gives index
        bit #8
        beq +
        inx
        inx
        and #7
+
        sta tmp
        txa
        bra _decode


_c10:   ; aaa lookup w/ fall thru on b=4
        ; we enter with 00aaabbb

        bit #7          ; b=0 => nop
        beq _nop

        ; b = 4 or 6?   100 or 110

        eor #%100       ; flip b2 so 4 or 6 is 0x0
        bit #%101       ;
        bne +
        bit #%10
        bne _c01        ; b=6, leave 00aaabbb

        ora #%10000000  ; b=4, set bit 7 leaving 10aaabbb
        bra _c01
+
        ora #%01000000  ; else set bit 6 leaving 01aaabbb
        bra _c01

_c0x:
        bpl _c00

_c01:
        ; we normally enter with 10aaabbb, or via _c10 with other patterns
        ; we want to do a xxaaa lookup
        lsr
        lsr
        lsr
        tax
        lda op_c12,x
        bra _decode

_c00:   ; exceptions
        tax
        lda op_c00,x

_decode:
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
    jsr out+2

    lda tmp
out:
    and #$1f
    ora #$40
    jmp putc

putc:
    sta $f001
    rts


.comment

BRK ORA NOP NOP TSB ORA ASL RMB0 PHP ORA ASL NOP TSB ORA ASL BBR0
BPL ORA ORA NOP TRB ORA ASL RMB1 CLC ORA INC NOP TRB ORA ASL BBR1
JSR AND NOP NOP BIT AND ROL RMB2 PLP AND ROL NOP BIT AND ROL BBR2
BMI AND AND NOP BIT AND ROL RMB3 SEC AND DEC NOP BIT AND ROL BBR3
RTI EOR NOP NOP NOP EOR LSR RMB4 PHA EOR LSR NOP JMP EOR LSR BBR4
BVC EOR EOR NOP NOP EOR LSR RMB5 CLI EOR PHY NOP NOP EOR LSR BBR5
RTS ADC NOP NOP STZ ADC ROR RMB6 PLA ADC ROR NOP JMP ADC ROR BBR6
BVS ADC ADC NOP STZ ADC ROR RMB7 SEI ADC PLY NOP JMP ADC ROR BBR7
BRA STA NOP NOP STY STA STX SMB0 DEY BIT TXA NOP STY STA STX BBS0
BCC STA STA NOP STY STA STX SMB1 TYA STA TXS NOP STZ STA STZ BBS1
LDY LDA LDX NOP LDY LDA LDX SMB2 TAY LDA TAX NOP LDY LDA LDX BBS2
BCS LDA LDA NOP LDY LDA LDX SMB3 CLV LDA TSX NOP LDY LDA LDX BBS3
CPY CMP NOP NOP CPY CMP DEC SMB4 INY CMP DEX WAI CPY CMP DEC BBS4
BNE CMP CMP NOP NOP CMP DEC SMB5 CLD CMP PHX STP NOP CMP DEC BBS5
CPX SBC NOP NOP CPX SBC INC SMB6 INX SBC NOP NOP CPX SBC INC BBS6
BEQ SBC SBC NOP NOP SBC INC SMB7 SED SBC PLX NOP NOP SBC INC BBS7
.endcomment

n_special = 9
op_special:
    .byte $89, $a2, $8a, $aa, $ca, $ea, $9e, $cb, $db
mnem_special:
    .byte op_BIT, op_LDX, op_TXA, op_TAX, op_DEX, op_NOP, op_STZ, op_WAI, op_STP

; 11 x 8 = 88 bytes of lookup with 61 distinct values (need to reorder)
; 6 bit / value => 6*88/8 = 66 bytes of lookup (saving 22 bytes but need extract code?)
; 70 distinct mnemonics altogether

.comment
; aaaaaabb bbbbcccc ccdddddd

    ; x => x % 4  and *3/4
    x & #3 gives x % 4
    x >> 1 + x >> 2; e.g 17 => 8 + 4 = 12
    4,4,4,4 (12 bytes), 17%4 is 1

dec1:
    lda tbl-1,x
    sta tmp
    lda tbl,x
    lsr tmp
    ror
    lsr tmp
    ror
    bra +
dec0:
    lda tbl,x
+
    lsr
    lsr
    rts

dec2:
    lda tbl+1,x
    sta tmp
    lda tbl,x
    asl tmp
    rol
    asl tmp
    rol
    bra +
dec3:
    lda tbl,x
    and #$3f
    rts

.endcomment



op_c12:
    .byte op_INC, op_DEC, op_PHY, op_PLY, op_TXS, op_TSX, op_PHX, op_PLX    ; c=2, b=6
    .byte op_ASL, op_ROL, op_LSR, op_ROR, op_STX, op_LDX, op_DEC, op_INC    ; c=2, b=4
    .byte op_ORA, op_AND, op_EOR, op_ADC, op_STA, op_LDA, op_CMP, op_SBC    ; c=1

op_c00:
    .byte op_BRK, op_TSB, op_PHP, op_TSB, op_BPL, op_TRB, op_CLC, op_TRB
    .byte op_JSR, op_BIT, op_PLP, op_BIT, op_BMI, op_BIT, op_SEC, op_BIT
    .byte op_RTI, op_NOP, op_PHA, op_JMP, op_BVC, op_NOP, op_CLI, op_NOP
    .byte op_RTS, op_STZ, op_PLA, op_JMP, op_BVS, op_STZ, op_SEI, op_JMP
    .byte op_BRA, op_STY, op_DEY, op_STY, op_BCC, op_STY, op_TYA, op_STZ
    .byte op_LDY, op_LDY, op_TAY, op_LDY, op_BCS, op_LDY, op_CLV, op_LDY
    .byte op_CPY, op_CPY, op_INY, op_CPY, op_BNE, op_NOP, op_CLD, op_NOP
    .byte op_CPX, op_CPX, op_INX, op_CPX, op_BEQ, op_NOP, op_SED, op_NOP

mnemonics:
op_id := 0

; specials that can occur as exceptions, index <64
op_BCC = op_id
    .mnem BCC
op_id += 1
op_BCS = op_id
    .mnem BCS
op_id += 1
op_BEQ = op_id
    .mnem BEQ
op_id += 1
op_BIT = op_id
    .mnem BIT
op_id += 1
op_BMI = op_id
    .mnem BMI
op_id += 1
op_BNE = op_id
    .mnem BNE
op_id += 1
op_BPL = op_id
    .mnem BPL
op_id += 1
op_BRA = op_id
    .mnem BRA
op_id += 1
op_BRK = op_id
    .mnem BRK
op_id += 1
op_BVC = op_id
    .mnem BVC
op_id += 1
op_BVS = op_id
    .mnem BVS
op_id += 1
op_CLC = op_id
    .mnem CLC
op_id += 1
op_CLD = op_id
    .mnem CLD
op_id += 1
op_CLI = op_id
    .mnem CLI
op_id += 1
op_CLV = op_id
    .mnem CLV
op_id += 1
op_CPX = op_id
    .mnem CPX
op_id += 1
op_CPY = op_id
    .mnem CPY
op_id += 1
op_DEX = op_id
    .mnem DEX
op_id += 1
op_DEY = op_id
    .mnem DEY
op_id += 1
op_INX = op_id
    .mnem INX
op_id += 1
op_INY = op_id
    .mnem INY
op_id += 1
op_JMP = op_id
    .mnem JMP
op_id += 1
op_JSR = op_id
    .mnem JSR
op_id += 1
op_LDX = op_id
    .mnem LDX
op_id += 1
op_LDY = op_id
    .mnem LDY
op_id += 1
op_NOP = op_id
    .mnem NOP
op_id += 1
op_PHA = op_id
    .mnem PHA
op_id += 1
op_PHP = op_id
    .mnem PHP
op_id += 1
op_PHX = op_id
    .mnem PHX
op_id += 1
op_PHY = op_id
    .mnem PHY
op_id += 1
op_PLA = op_id
    .mnem PLA
op_id += 1
op_PLP = op_id
    .mnem PLP
op_id += 1
op_PLX = op_id
    .mnem PLX
op_id += 1
op_PLY = op_id
    .mnem PLY
op_id += 1
op_RTI = op_id
    .mnem RTI
op_id += 1
op_RTS = op_id
    .mnem RTS
op_id += 1
op_SEC = op_id
    .mnem SEC
op_id += 1
op_SED = op_id
    .mnem SED
op_id += 1
op_SEI = op_id
    .mnem SEI
op_id += 1
op_STP = op_id
    .mnem STP
op_id += 1
op_STY = op_id
    .mnem STY
op_id += 1
op_STZ = op_id
    .mnem STZ
op_id += 1
op_TAX = op_id
    .mnem TAX
op_id += 1
op_TAY = op_id
    .mnem TAY
op_id += 1
op_TRB = op_id
    .mnem TRB
op_id += 1
op_TSB = op_id
    .mnem TSB
op_id += 1
op_TSX = op_id
    .mnem TSX
op_id += 1
op_TXA = op_id
    .mnem TXA
op_id += 1
op_TXS = op_id
    .mnem TXS
op_id += 1
op_TYA = op_id
    .mnem TYA
op_id += 1
op_WAI = op_id
    .mnem WAI


; never appear as exceptions
op_id += 1
op_ADC = op_id
    .mnem ADC
op_id += 1
op_AND = op_id
    .mnem AND
op_id += 1
op_ASL = op_id
    .mnem ASL
op_id += 1
op_CMP = op_id
    .mnem CMP
op_id += 1
op_DEC = op_id
    .mnem DEC
op_id += 1
op_EOR = op_id
    .mnem EOR
op_id += 1
op_INC = op_id
    .mnem INC
op_id += 1
op_LDA = op_id
    .mnem LDA
op_id += 1
op_LSR = op_id
    .mnem LSR
op_id += 1
op_ORA = op_id
    .mnem ORA
op_id += 1
op_ROL = op_id
    .mnem ROL
op_id += 1
op_ROR = op_id
    .mnem ROR
op_id += 1
op_SBC = op_id
    .mnem SBC
op_id += 1
op_STA = op_id
    .mnem STA
op_id += 1
op_STX = op_id
    .mnem STX


; flag for trailing bit index
op_id += 1
op_RMB = op_id
    .mnem RMB, 1
op_id += 1
op_BBR = op_id
    .mnem BBR, 1
op_id += 1
op_SMB = op_id
    .mnem SMB, 1
op_id += 1
op_BBS = op_id
    .mnem BBS, 1
