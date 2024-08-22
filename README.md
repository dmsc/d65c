../tali/c65/c65 -gg -r dasm.bin -a 0x200

64tass --nostart --list=dasm.lst --output dasm.bin --labels=dasm.sym dasm.asm

https://docs.google.com/spreadsheets/d/1wf9PgigE5G9hAW63dF5ATjTwNdXEup0tbsFexUzrQEc/edit?gid=825377478#gid=825377478


https://www.applefritter.com/files/Apple1WozDrDobbsDisasm.pdf

much of the code shared here https://github.com/jblang/supermon64/blob/master/supermon64.asm


TODO

- excl other specials with bitops
- excl bitops adr mode lines
- don't consume opc until operand, use one inline nextbyte (mode 15?)
)

;TODO a trailing "X" could flag some mode exceptions, ie. ,Y vs ,X

- switch length bit to LSB, then mode 7 & 15 are neighbors
- fix c65 label reading
- combine asl / rol and lsr / ror loop subroutine?

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





        sta len                 ; store 0, 1, or 2
        ldy #$ff
-
        iny
        lda (dadr),y            ; print the opcode followed by len operand bytes
        jsr prbyte
        jsr prspc
        cpy len
        bne -
-
        jsr pr3spc              ; print three space blocks to pad to width 12 total
        iny
        cpy #3
        bne -

(21)

-
jsr prpadbyte
iny
cpy #3
bne -

prpadbyte:
    cpy len
    bpl pr3spc
    lda (dadr),y
    bra prbyte

(19)