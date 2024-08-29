This is smallish disassembler that decodes all 256 opcodes for 65c02
including an option for Rockwell/WDC bit operators.

The minimal version compiles to 500 bytes (all code and tables) but shows
the extended bit operators RMBn, SMBn, BBRn, and BBSn as NOP with
appropriate length and addressing mode.  Enabling support for RMBn and friends
adds 60 bytes for a total of 560 bytes.

Background and further reading:

- Source code for
[Wozniak & Baum's 6502 disassembler](https://www.applefritter.com/files/Apple1WozDrDobbsDisasm.pdf)
in 473 bytes(!).  You can find machine readable version a derived version of this code
[here](https://github.com/jblang/supermon64/blob/master/supermon64.asm) though I haven't found a link to
a clean version of the original.

- Great discussion and suggestions in the
[6502.org forum thread](http://forum.6502.org/viewtopic.php?f=2&t=8147)

- A spreadsheet with my
    [exploration of the 65c02 opcode structure](https://docs.google.com/spreadsheets/d/1wf9PgigE5G9hAW63dF5ATjTwNdXEup0tbsFexUzrQEc/edit?gid=825377478#gid=825377478)
    used for designing data tables

- My [periodic table of 65c02 opcodes](https://patricksurry.github.io/periodic-65c02/)
includes several useful reference links.