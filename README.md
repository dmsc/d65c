This is smallish disassembler that decodes all 256 opcodes for 65c02
including an option for Rockwell/WDC bit operators.

By default we show the extended bit operators RMBn, SMBn, BBRn, and BBSn
as NOPs with equivalent length and addressing mode.   This compiles to 540 bytes
including all data tables.  Enabling support for RMBn and friends adds another 64 bytes.

Background and further reading:

- Source code for
[Wozniak & Baum's 6502 disassembler](https://www.applefritter.com/files/Apple1WozDrDobbsDisasm.pdf)
in 473 bytes(!).  You can find machine readable version a derived version of this code
[here](https://github.com/jblang/supermon64/blob/master/supermon64.asm) though I haven't found a link to
a clean version of the original.

- Discussion in the [6502.org forum](http://forum.6502.org/viewtopic.php?f=2&t=8147)

- A spreadsheet with my
    [exploration of the 65c02 opcode structure](https://docs.google.com/spreadsheets/d/1wf9PgigE5G9hAW63dF5ATjTwNdXEup0tbsFexUzrQEc/edit?gid=825377478#gid=825377478)
    used for designing data tables

- My [periodic table of 65c02 opcodes](https://patricksurry.github.io/periodic-65c02/)
includes several useful reference links.